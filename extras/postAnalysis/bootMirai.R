bootPairedDeltasMirai <- function(
  y,
  pA,
  pB,
  metricFns,
  B,
  seedBase,
  workers = NULL,
  chunkSize = 100L,
  threads = 1L,
  showProgress = TRUE
) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("bootPairedDeltasMirai requires 'mirai'. Please install it.")
  }

  y <- as.integer(y)
  stopifnot(length(y) == length(pA), length(y) == length(pB))
  nCases <- sum(y == 1L)
  nCtrls <- sum(y == 0L)
  if (nCases == 0L || nCtrls == 0L) {
    stop("Cannot bootstrap with 0 cases or 0 controls.")
  }

  # Precompute full-sample values
  origA <- vapply(metricFns, function(f) tryCatch(f(y, pA), error = function(e) NA_real_), numeric(1))
  origB <- vapply(metricFns, function(f) tryCatch(f(y, pB), error = function(e) NA_real_), numeric(1))
  orig <- origA - origB

  # Chunk replicate ids
  repIds <- seq_len(as.integer(B))
  chunkSize <- max(1L, as.integer(chunkSize))
  chunks <- split(repIds, ceiling(seq_along(repIds) / chunkSize))

  # Start daemons
  if (is.null(workers)) {
    workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  }
  mirai::daemons(workers)

  # Submit jobs
  jobs <- vector("list", length(chunks))
  nm <- names(metricFns)
  for (i in seq_along(chunks)) {
    reps <- as.integer(chunks[[i]])
    jobs[[i]] <- mirai::mirai(
      {
        # Cap threads inside worker
        try(source("extras/postAnalysis/setThreads.R"), silent = TRUE)
        if (exists("setAnalysisThreads")) {
          try(
            setAnalysisThreads(as.integer(threads_i), verbose = FALSE),
            silent = TRUE
          )
        }

        # Load code
        source("extras/postAnalysis/metrics.R")
        source("extras/postAnalysis/pairedTests.R")

        # Prepare
        caseIdx <- which(y_i == 1L)
        ctrlIdx <- which(y_i == 0L)
        plan <- bootMakePlan(sum(y_i == 1L), sum(y_i == 0L), B_i, seedBase_i)

        # Build metrics locally from passed functions (closures) after sourcing metrics
        # metricFns_i contains serialized functions; they should now find needed symbols
        mnames <- names(metricFns_i)
        out <- matrix(
          NA_real_,
          nrow = length(reps_i),
          ncol = length(metricFns_i)
        )
        colnames(out) <- mnames
        outA <- out; outB <- out
        rpos <- 0L
        for (b in reps_i) {
          draw <- bootDrawOnce(plan, b)
          idx <- c(caseIdx[draw$cases], ctrlIdx[draw$ctrls])
          yb <- y_i[idx]
          a <- pA_i[idx]
          bb <- pB_i[idx]
          rpos <- rpos + 1L
          for (j in seq_along(metricFns_i)) {
            vA <- tryCatch(metricFns_i[[j]](yb, a), error = function(e) {
              NA_real_
            })
            vB <- tryCatch(metricFns_i[[j]](yb, bb), error = function(e) {
              NA_real_
            })
            out[rpos, j] <- vA - vB
            outA[rpos, j] <- vA
            outB[rpos, j] <- vB
          }
        }
        list(ids = reps_i, mat = out, matA = outA, matB = outB)
      },
      .args = list(
        y_i = y,
        pA_i = pA,
        pB_i = pB,
        B_i = as.integer(B),
        seedBase_i = as.integer(seedBase),
        reps_i = reps,
        metricFns_i = metricFns,
        threads_i = as.integer(threads)
      )
    )
  }

  # Aggregate results
  if (isTRUE(showProgress)) {
    pb <- utils::txtProgressBar(min = 0, max = length(jobs), style = 3)
    on.exit(
      {
        try(close(pb), silent = TRUE)
      },
      add = TRUE
    )
  }
  results <- vector("list", length(jobs))
  done <- rep(FALSE, length(jobs))
  nDone <- 0L
  while (!all(done)) {
    for (i in seq_along(jobs)) {
      if (!done[i] && !mirai::unresolved(jobs[[i]])) {
        results[[i]] <- jobs[[i]][]
        done[i] <- TRUE
        nDone <- nDone + 1L
        if (isTRUE(showProgress)) utils::setTxtProgressBar(pb, nDone)
      }
    }
    if (!all(done)) Sys.sleep(0.05)
  }

  # Reassemble in replicate id order
  B <- as.integer(B)
  mat  <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  matA <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  matB <- matrix(NA_real_, nrow = B, ncol = length(metricFns))
  colnames(mat) <- nm
  colnames(matA) <- nm
  colnames(matB) <- nm
  for (res in results) {
    ids <- as.integer(res$ids)
    mat[ids, ]  <- res$mat
    matA[ids, ] <- res$matA
    matB[ids, ] <- res$matB
  }

  # Summarize (match bootPairedDeltas output)
  res <- lapply(seq_along(nm), function(j) {
    v  <- mat[, j];  v  <- v[is.finite(v)]
    vA <- matA[, j]; vA <- vA[is.finite(vA)]
    vB <- matB[, j]; vB <- vB[is.finite(vB)]
    if (!length(v)) {
      return(data.frame(metric = nm[j], delta = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, sd = NA_real_, se = NA_real_, validB = 0L, B = as.integer(B), metricA = NA_real_, metricA_lo = NA_real_, metricA_hi = NA_real_, metricA_sd = NA_real_, metricA_se = NA_real_, metricB = NA_real_, metricB_lo = NA_real_, metricB_hi = NA_real_, metricB_sd = NA_real_, metricB_se = NA_real_, stringsAsFactors = FALSE))
    }
    delta <- mean(v)
    lo <- stats::quantile(v, probs = 0.025, names = FALSE)
    hi <- stats::quantile(v, probs = 0.975, names = FALSE)
    p <- 2 * min(mean(v <= 0), mean(v >= 0))
    sdv <- stats::sd(v)
    sev <- sdv / sqrt(length(v))
    mA <- mean(vA); mA_lo <- stats::quantile(vA, 0.025, names = FALSE); mA_hi <- stats::quantile(vA, 0.975, names = FALSE)
    sdA <- stats::sd(vA); seA <- sdA / sqrt(length(vA))
    mB <- mean(vB); mB_lo <- stats::quantile(vB, 0.025, names = FALSE); mB_hi <- stats::quantile(vB, 0.975, names = FALSE)
    sdB <- stats::sd(vB); seB <- sdB / sqrt(length(vB))
    data.frame(metric = nm[j], delta = delta, lo = lo, hi = hi, p = p, sd = sdv, se = sev, validB = as.integer(length(v)), B = as.integer(B), metricA = mA, metricA_lo = mA_lo, metricA_hi = mA_hi, metricA_sd = sdA, metricA_se = seA, metricB = mB, metricB_lo = mB_lo, metricB_hi = mB_hi, metricB_sd = sdB, metricB_se = seB, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, res)
  res$deltaFullSample <- as.numeric(orig[match(res$metric, names(orig))])
  res$metricAFullSample <- as.numeric(origA[match(res$metric, names(origA))])
  res$metricBFullSample <- as.numeric(origB[match(res$metric, names(origB))])
  res
}
