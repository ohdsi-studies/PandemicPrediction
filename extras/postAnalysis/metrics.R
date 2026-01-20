clampProbs <- function(p, eps = .Machine$double.xmin) {
  p <- as.numeric(p)
  p[p <= 0] <- eps
  p[p >= 1] <- 1 - eps
  p
}

auroc <- function(y, p) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required for auroc(). Install it first.")
  }
  y <- as.integer(y)
  p <- clampProbs(p)
  r <- pROC::roc(response = y, predictor = p, quiet = TRUE, direction = "auto")
  as.numeric(pROC::auc(r))
}

auprc <- function(y, p) {
  if (!requireNamespace("PRROC", quietly = TRUE)) {
    stop("Package 'PRROC' is required for auprc(). Install it first.")
  }
  y <- as.integer(y)
  p <- clampProbs(p)
  pos <- p[y == 1]
  neg <- p[y == 0]
  if (length(pos) == 0 || length(neg) == 0) {
    return(NA_real_)
  }
  as.numeric(
    PRROC::pr.curve(
      scores.class0 = pos,
      scores.class1 = neg,
      curve = FALSE
    )$auc.integral
  )
}

brier <- function(y, p) {
  y <- as.numeric(y)
  p <- clampProbs(p)
  mean((p - y)^2)
}

eavgLoess <- function(y, p, span = 0.4) {
  y <- as.numeric(y)
  p <- clampProbs(p)
  # Guard against tiny samples
  if (length(p) < 20 || length(unique(p)) < 5) {
    return(NA_real_)
  }
  fit <- stats::loess(
    y ~ p,
    span = span,
    degree = 1,
    surface = "direct",
    control = stats::loess.control(iterations = 1)
  )
  mhat <- stats::predict(fit, newdata = data.frame(p = p))
  if (all(!is.finite(mhat))) {
    return(NA_real_)
  }
  mean(abs(p - mhat), na.rm = TRUE)
}

eavgGAM <- function(y, p, k = NULL, gamma = 1.2, discrete = TRUE, nthreads = 1L) {
  if (length(p) < 20L) {
    return(NA_real_)
  }
  ok <- is.finite(p) & is.finite(y)
  if (!all(ok)) {
    p <- p[ok]
    y <- y[ok]
    if (length(p) < 20L) return(NA_real_)
  }
  y <- as.numeric(y)
  if (!all(y %in% c(0, 1))) {
    y <- as.numeric(y > 0.5)
  }

  p <- clampProbs(p)
  n <- length(p)
  nevt <- sum(y)
  if (nevt == 0L || nevt == n) {
    return(NA_real_)
  }

  # transform and guard uniqueness
  lp <- qlogis(p)
  nuniq <- length(unique(lp))
  if (nuniq < 5L) {
    return(NA_real_)
  }

  # choose k automatically if not provided
  if (is.null(k)) {
    # scale with events; clamp to [12, 24]
    k <- max(12L, min(24L, ceiling(6 * log10(max(20, nevt)))))
  }
  kCap <- max(3L, min(k, nuniq - 1L))
  if (kCap < 3L) {
    return(NA_real_)
  }

  # fit bam; fall back to logistic recal if something goes wrong
  fitOk <- TRUE
  mhat <- NULL
  if (requireNamespace("mgcv", quietly = TRUE)) {
    dat <- data.frame(y = y, lp = lp)
    ctrl <- mgcv::gam.control(maxit = 200, epsilon = 1e-8)
    # Try to pass nthreads if supported; fall back to no nthreads
    fit <- try({
      do.call(
        mgcv::bam,
        list(
          formula = stats::as.formula("y ~ s(lp, bs = 'cr', k = kCap)"),
          data = dat,
          family = stats::binomial(link = "logit"),
          method = "fREML",
          discrete = discrete,
          select = TRUE,
          gamma = gamma,
          control = ctrl,
          nthreads = as.integer(nthreads)
        )
      )
    }, silent = TRUE)
    if (inherits(fit, "try-error")) {
      fit <- try(
        mgcv::bam(
          y ~ s(lp, bs = "cr", k = kCap),
          data = dat,
          family = stats::binomial(link = "logit"),
          method = "fREML",
          discrete = discrete,
          select = TRUE,
          gamma = gamma,
          control = ctrl
        ),
        silent = TRUE
      )
    }
    if (!inherits(fit, "try-error")) {
      mhat <- as.numeric(predict(fit, newdata = dat, type = "response"))
      if (!any(is.finite(mhat))) fitOk <- FALSE
    } else {
      fitOk <- FALSE
    }
  } else {
    fitOk <- FALSE
  }

  if (!fitOk) {
    # Fallback: logistic recalibration (slope + intercept)
    gl <- try(glm(y ~ qlogis(p), family = binomial()), silent = TRUE)
    if (inherits(gl, "try-error")) {
      return(NA_real_)
    }
    mhat <- plogis(predict(gl))
  }

  mean(abs(p - mhat), na.rm = TRUE)
}
netBenefit <- function(y, p, threshold) {
  y <- as.integer(y)
  p <- clampProbs(p)
  t <- as.numeric(threshold)
  if (!is.finite(t) || t <= 0 || t >= 1) {
    return(NA_real_)
  }
  n <- length(y)
  pred <- p >= t
  tp <- sum(pred & (y == 1))
  fp <- sum(pred & (y == 0))
  tp / n - fp / n * t / (1 - t)
}

integratedNetBenefit <- function(y, p, thresholds) {
  th <- as.numeric(thresholds)
  th <- th[is.finite(th) & th > 0 & th < 1]
  if (!length(th)) {
    return(NA_real_)
  }
  nb <- vapply(th, function(t) netBenefit(y, p, t), numeric(1))
  mean(nb, na.rm = TRUE)
}

# Area under net benefit curve (A-NBC) using trapezoidal numerical integration.
# Returns raw area (not normalized by threshold range).
areaNetBenefitCurve <- function(y, p, thresholds) {
  th <- as.numeric(thresholds)
  th <- th[is.finite(th) & th > 0 & th < 1]
  th <- sort(unique(th))
  if (length(th) < 2) {
    return(NA_real_)
  }
  nb <- vapply(th, function(t) netBenefit(y, p, t), numeric(1))
  ok <- is.finite(th) & is.finite(nb)
  th <- th[ok]
  nb <- nb[ok]
  if (length(th) < 2) {
    return(NA_real_)
  }
  sum(diff(th) * (head(nb, -1) + tail(nb, -1)) / 2)
}
