bootMakePlan <- function(nCases, nControls, B = 2000L, seedBase = 1L) {
  list(
    B = as.integer(B),
    nCases = as.integer(nCases),
    nControls = as.integer(nControls),
    seedBase = as.integer(seedBase)
  )
}

bootDrawOnce <- function(plan, repId) {
  stopifnot(is.list(plan), !is.null(plan$nCases), !is.null(plan$nControls))
  set.seed(plan$seedBase + as.integer(repId))
  # Sample with replacement within strata; redraw if any stratum empty
  ok <- FALSE
  tries <- 0L
  while (!ok && tries < 1000L) {
    idxCases <- sample.int(plan$nCases, size = plan$nCases, replace = TRUE)
    idxCtrls <- sample.int(
      plan$nControls,
      size = plan$nControls,
      replace = TRUE
    )
    ok <- length(unique(idxCases)) > 0 && length(unique(idxCtrls)) > 0
    tries <- tries + 1L
  }
  if (!ok) {
    stop("Failed to draw a valid stratified bootstrap sample after many tries.")
  }
  list(cases = idxCases, ctrls = idxCtrls)
}

# metricFns: named list of functions f(y, p) -> scalar
bootPairedDeltas <- function(
  y,
  pA,
  pB,
  metricFns,
  B = 2000L,
  seedBase = 1L,
  showProgress = FALSE
) {
  y <- as.integer(y)
  stopifnot(length(y) == length(pA), length(y) == length(pB))
  nCases <- sum(y == 1L)
  nCtrls <- sum(y == 0L)
  plan <- bootMakePlan(nCases, nCtrls, B, seedBase)

  caseIdx <- which(y == 1L)
  ctrlIdx <- which(y == 0L)

  origA <- vapply(
    metricFns,
    function(f) tryCatch(f(y, pA), error = function(e) NA_real_),
    numeric(1)
  )
  origB <- vapply(
    metricFns,
    function(f) tryCatch(f(y, pB), error = function(e) NA_real_),
    numeric(1)
  )
  orig <- origA - origB

  # Collect bootstrap deltas (sequential with optional progress)
  mNames <- names(metricFns)
  mat <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  matA <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  matB <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  colnames(mat) <- mNames
  colnames(matA) <- mNames
  colnames(matB) <- mNames

  evalRep <- function(b) {
    draw <- bootDrawOnce(plan, b)
    idx <- c(caseIdx[draw$cases], ctrlIdx[draw$ctrls])
    yb <- y[idx]
    a <- pA[idx]
    bvec <- pB[idx]
    v <- numeric(length(metricFns))
    va <- numeric(length(metricFns))
    vb <- numeric(length(metricFns))
    for (j in seq_along(metricFns)) {
      vA <- tryCatch(metricFns[[j]](yb, a), error = function(e) NA_real_)
      vB <- tryCatch(metricFns[[j]](yb, bvec), error = function(e) NA_real_)
      v[j] <- vA - vB
      va[j] <- vA
      vb[j] <- vB
    }
    list(d = v, a = va, b = vb)
  }

  if (isTRUE(showProgress)) {
    pb <- utils::txtProgressBar(min = 0, max = B, style = 3)
    on.exit(
      {
        try(close(pb), silent = TRUE)
      },
      add = TRUE
    )
  }
  for (b in seq_len(B)) {
    res <- evalRep(b)
    mat[b, ] <- res$d
    matA[b, ] <- res$a
    matB[b, ] <- res$b
    if (isTRUE(showProgress)) utils::setTxtProgressBar(pb, b)
  }

  res <- lapply(seq_along(metricFns), function(j) {
    v <- mat[, j]
    v <- v[is.finite(v)]
    vA <- matA[, j]
    vA <- vA[is.finite(vA)]
    vB <- matB[, j]
    vB <- vB[is.finite(vB)]
    if (!length(v)) {
      return(data.frame(
        metric = names(metricFns)[j],
        delta = NA_real_,
        lo = NA_real_,
        hi = NA_real_,
        p = NA_real_,
        sd = NA_real_,
        se = NA_real_,
        validB = 0L,
        B = as.integer(B),
        metricA = NA_real_,
        metricA_lo = NA_real_,
        metricA_hi = NA_real_,
        metricA_sd = NA_real_,
        metricA_se = NA_real_,
        metricB = NA_real_,
        metricB_lo = NA_real_,
        metricB_hi = NA_real_,
        metricB_sd = NA_real_,
        metricB_se = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    delta <- mean(v)
    lo <- stats::quantile(v, probs = 0.025, names = FALSE)
    hi <- stats::quantile(v, probs = 0.975, names = FALSE)
    p <- 2 * min(mean(v <= 0), mean(v >= 0))
    sdv <- stats::sd(v)
    sev <- sdv / sqrt(length(v))
    # A and B
    mA <- mean(vA)
    mALo <- stats::quantile(vA, 0.025, names = FALSE)
    mAHi <- stats::quantile(vA, 0.975, names = FALSE)
    sdA <- stats::sd(vA)
    seA <- sdA / sqrt(length(vA))
    mB <- mean(vB)
    mBLo <- stats::quantile(vB, 0.025, names = FALSE)
    mBHi <- stats::quantile(vB, 0.975, names = FALSE)
    sdB <- stats::sd(vB)
    seB <- sdB / sqrt(length(vB))
    data.frame(
      metric = names(metricFns)[j],
      delta = delta,
      lo = lo,
      hi = hi,
      p = p,
      sd = sdv,
      se = sev,
      validB = as.integer(length(v)),
      B = as.integer(B),
      metricA = mA,
      metricA_lo = mALo,
      metricA_hi = mAHi,
      metricA_sd = sdA,
      metricA_se = seA,
      metricB = mB,
      metricB_lo = mBLo,
      metricB_hi = mBHi,
      metricB_sd = sdB,
      metricB_se = seB,
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, res)
  res$deltaFullSample <- as.numeric(orig[match(res$metric, names(orig))])
  res$metricAFullSample <- as.numeric(origA[match(res$metric, names(origA))])
  res$metricBFullSample <- as.numeric(origB[match(res$metric, names(origB))])
  res
}

delongDelta <- function(y, pA, pB) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required for delongDelta(). Install it first.")
  }
  y <- as.integer(y)
  rA <- pROC::roc(y, pA, quiet = TRUE, direction = "auto")
  rB <- pROC::roc(y, pB, quiet = TRUE, direction = "auto")
  aucA <- as.numeric(pROC::auc(rA))
  aucB <- as.numeric(pROC::auc(rB))
  delta <- aucA - aucB
  tst <- tryCatch(
    pROC::roc.test(rA, rB, method = "delong", paired = TRUE),
    error = function(e) NULL
  )
  p <- if (is.null(tst)) NA_real_ else as.numeric(tst$p.value)
  data.frame(
    metric = "AUROC",
    delta = delta,
    p = p,
    aucA = aucA,
    aucB = aucB,
    stringsAsFactors = FALSE
  )
}

# Benchmark a single comparison to estimate time per replicate
benchBootstrap <- function(y, pA, pB, metricFns, bPilot = 200L) {
  t <- system.time({
    invisible(bootPairedDeltas(
      y,
      pA,
      pB,
      metricFns,
      B = bPilot,
      seedBase = 123,
      showProgress = FALSE
    ))
  })["elapsed"]
  list(
    elapsed = as.numeric(t),
    perRep = as.numeric(t) / bPilot,
    Bpilot = bPilot
  )
}
