# Drivers for building comparisons, running quarter-wise paired analyses,
# and pooling results.

sourceIfExists <- function(path) {
  if (file.exists(path)) try(source(path), silent = TRUE)
}

# Load helpers from extras/postAnalysis by relative path
sourceIfExists("extras/postAnalysis/metrics.R")
sourceIfExists("extras/postAnalysis/pairedTests.R")
sourceIfExists("extras/postAnalysis/extractPredictions.R")
sourceIfExists("extras/postAnalysis/bootMirai.R")
sourceIfExists("extras/postAnalysis/setThreads.R")

ensureDir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

buildBootstrapOutFile <- function(outDir, row) {
  file.path(
    outDir,
    paste0(
      row$outcomeName,
      "_",
      row$featureSet,
      "_",
      row$W,
      "_",
      row$comparator,
      "_",
      row$quarterId,
      ".csv"
    )
  )
}

readExistingBootstrapIfCompatible <- function(path, B) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) {
    return(NULL)
  }
  if (!"B" %in% names(df)) {
    return(NULL)
  }
  existingB <- unique(as.integer(df$B))
  if (length(existingB) != 1L || is.na(existingB) || existingB != as.integer(B)) {
    return(NULL)
  }
  # Expect 5 metrics per file from our writers:
  # AUROC, AUPRC, Brier, ICI, INB
  if (!"metric" %in% names(df) || length(unique(as.character(df$metric))) != 5L) {
    return(NULL)
  }
  df
}

featureShort <- function(featureSet) {
  ifelse(tolower(featureSet) == "parsimonious", "pars", "full")
}

mapDevPeriodToW <- function(devPeriod) {
  switch(
    as.character(devPeriod),
    "First 3 Months" = "3m",
    "First 6 Months" = "6m",
    "First 9 Months" = "9m",
    "Full Year 2020 (Sampled)" = "12m_150k",
    "Full Year 2020 (Full Pop.)" = "12m_full",
    NA_character_
  )
}

makeModelKey <- function(modelOrigin, featureSet, w) {
  fs <- featureShort(featureSet)
  mo <- tolower(modelOrigin)
  ww <- w
  if (is.na(ww) || ww == "NA" || !nzchar(ww)) ww <- ""
  if (mo == "new covid") {
    paste0("covid_", fs, "_", ww)
  } else if (mo == "original influenza") {
    paste0("proxy_frozen_", fs)
  } else if (mo == "original influenza (recalibrated)") {
    paste0("proxy_recal_", fs, "_", ww)
  } else if (grepl("rolling recalibrated", mo, fixed = TRUE)) {
    paste0("proxy_roll_", fs, if (nzchar(ww)) paste0("_", ww) else "")
  } else {
    NA_character_
  }
}

computeQuarterId <- function(startDate, endDate, outcomeName) {
  paste0(
    format(as.Date(startDate), "%Y%m%d"),
    "_",
    format(as.Date(endDate), "%Y%m%d"),
    "_",
    outcomeName
  )
}

buildComparisonsFromAllResults <- function(
  allResultsPath = "results/allResults.csv"
) {
  if (!file.exists(allResultsPath)) {
    stop("allResults.csv not found at ", allResultsPath)
  }
  ar <- utils::read.csv(allResultsPath, stringsAsFactors = FALSE)
  ar$startDate <- as.Date(ar$startDate)
  ar$endDate <- as.Date(ar$endDate)
  ar$devEndDate <- as.Date(ar$devEndDate)
  ar$w <- vapply(ar$devPeriod, mapDevPeriodToW, character(1))

  quarters <- unique(ar[, c("outcomeName", "startDate", "endDate")])
  quarters$quarterId <- computeQuarterId(
    quarters$startDate,
    quarters$endDate,
    quarters$outcomeName
  )

  # Build COVID and proxy entries by W and featureSet
  covid <- subset(ar, modelOrigin == "New Covid")
  proxyFrozen <- subset(ar, modelOrigin == "Original Influenza")
  proxyRecal <- subset(
    ar,
    grepl("recal", tolower(recalibrationPeriod)) |
      modelOrigin == "Original Influenza (Recalibrated)"
  )

  # Define comparators
  combos <- unique(covid[, c("outcomeName", "featureSet", "w", "devEndDate")])
  res <- list()
  k <- 1L
  for (i in seq_len(nrow(combos))) {
    oc <- combos$outcomeName[i]
    fs <- combos$featureSet[i]
    w <- combos$w[i]
    devEnd <- combos$devEndDate[i]

    qv <- subset(quarters, outcomeName == oc & startDate > devEnd)
    if (!nrow(qv)) {
      next
    }
    qv$W <- w
    qv$featureSet <- fs
    qv$outcomeName <- oc

    modelACovid <- makeModelKey("New Covid", fs, w)
    modelBProxyFrozen <- makeModelKey("Original Influenza", fs, w)
    modelBProxyRecal <- makeModelKey("Original Influenza (Recalibrated)", fs, w)

    # Add covid vs proxy_frozen
    res[[k]] <- data.frame(
      outcomeName = oc,
      featureSet = fs,
      W = w,
      comparator = "covid_vs_proxy_frozen",
      modelAKey = modelACovid,
      modelBKey = modelBProxyFrozen,
      quarterId = qv$quarterId,
      stringsAsFactors = FALSE
    )
    k <- k + 1L

    # Add covid vs proxy_recal
    res[[k]] <- data.frame(
      outcomeName = oc,
      featureSet = fs,
      W = w,
      comparator = "covid_vs_proxy_recal",
      modelAKey = modelACovid,
      modelBKey = modelBProxyRecal,
      quarterId = qv$quarterId,
      stringsAsFactors = FALSE
    )
    k <- k + 1L

    # Add proxy_frozen vs proxy_rolling_recal (Q-1 recal applied to Q)
    modelBRolling <- makeModelKey("Original Influenza (Rolling Recalibrated)", fs, NA)
    res[[k]] <- data.frame(
      outcomeName = oc,
      featureSet = fs,
      W = w,
      comparator = "proxy_frozen_vs_proxy_rolling_recal",
      modelAKey = modelBProxyFrozen,
      modelBKey = modelBRolling,
      quarterId = qv$quarterId,
      stringsAsFactors = FALSE
    )
    k <- k + 1L

    # Add proxy_recal vs proxy_rolling_recal
    res[[k]] <- data.frame(
      outcomeName = oc,
      featureSet = fs,
      W = w,
      comparator = "proxy_recal_vs_proxy_rolling_recal",
      modelAKey = modelBProxyRecal,
      modelBKey = modelBRolling,
      quarterId = qv$quarterId,
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }
  out <- do.call(rbind, res)

  # Add "frozen-only" comparisons for quarters where a frozen proxy model exists
  # but a rolling recalibrated proxy model does not (for the same outcome + feature set).
  #
  # We use a self-comparison (frozen vs frozen) so that bootstrapping yields absolute
  # metric values for the frozen model in those quarters.
  buildFrozenOnlyComparisons <- function(allResults) {
    needed <- c("outcomeName", "featureSet", "startDate", "endDate", "modelOrigin")
    if (length(setdiff(needed, names(allResults)))) {
      return(data.frame())
    }
    tmp <- allResults
    tmp$startDate <- as.Date(tmp$startDate)
    tmp$endDate <- as.Date(tmp$endDate)
    tmp$quarterId <- computeQuarterId(tmp$startDate, tmp$endDate, tmp$outcomeName)

    origin <- tolower(trimws(as.character(tmp$modelOrigin)))
    frozenMask <- origin == "original influenza"
    rollingMask <- grepl("rolling recalibrated", origin, fixed = TRUE)

    frozen <- unique(tmp[frozenMask, c("outcomeName", "featureSet", "quarterId")])
    if (!nrow(frozen)) return(data.frame())

    rolling <- unique(tmp[rollingMask, c("outcomeName", "featureSet", "quarterId")])
    frozenKey <- paste(frozen$outcomeName, frozen$featureSet, frozen$quarterId, sep = "||")
    rollingKey <- if (nrow(rolling)) paste(rolling$outcomeName, rolling$featureSet, rolling$quarterId, sep = "||") else character(0)

    only <- frozen[!(frozenKey %in% rollingKey), , drop = FALSE]
    if (!nrow(only)) return(data.frame())

    only$W <- "na"
    only$comparator <- "proxy_frozen_only"
    only$modelAKey <- vapply(
      seq_len(nrow(only)),
      function(i) makeModelKey("Original Influenza", only$featureSet[i], NA),
      character(1)
    )
    only$modelBKey <- only$modelAKey
    only <- only[, c("outcomeName", "featureSet", "W", "comparator", "modelAKey", "modelBKey", "quarterId")]
    rownames(only) <- NULL
    only
  }

  frozenOnly <- buildFrozenOnlyComparisons(ar)
  if (nrow(frozenOnly)) {
    out <- rbind(out, frozenOnly)
  }
  out
}

loadQuarterPredictions <- function(
  quarterId,
  outcomeName,
  modelKeys,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv"
) {
  # Always extract from results/runs to ensure consistency
  tmpRow <- data.frame(
    outcomeName = outcomeName,
    quarterId = quarterId,
    modelAKey = modelKeys[1],
    modelBKey = modelKeys[2],
    stringsAsFactors = FALSE
  )
  loadQuarterPairFromRuns(
    tmpRow,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath
  )
}

getPairedPredictions <- function(
  row,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  locateQuarterRunFn = locateQuarterRun,
  loadRunPredictionsFn = loadRunPredictions,
  loadQuarterPairFromRunsFn = loadQuarterPairFromRuns
) {
  sameModel <- identical(as.character(row$modelAKey), as.character(row$modelBKey))
  if (isTRUE(sameModel)) {
    tmpRow <- data.frame(
      outcomeName = row$outcomeName,
      quarterId = row$quarterId,
      modelAKey = row$modelAKey,
      modelBKey = row$modelBKey,
      stringsAsFactors = FALSE
    )
    run <- locateQuarterRunFn(
      modelKey = row$modelAKey,
      row = tmpRow,
      runsRoot = runsRoot,
      allResultsPath = allResultsPath
    )
    preds <- loadRunPredictionsFn(run$runPlpPath, evalType = "Validation")
    list(
      y = preds$y,
      pA = preds$p,
      pB = preds$p
    )
  } else {
    modelKeys <- c(row$modelAKey, row$modelBKey)
    df <- loadQuarterPairFromRunsFn(
      row,
      runsRoot = runsRoot,
      allResultsPath = allResultsPath
    )
    cols <- c("patient_id", "y", paste0("p_", modelKeys))
    df <- df[, cols]
    df <- df[stats::complete.cases(df), ]
    list(
      y = df$y,
      pA = df[[paste0("p_", row$modelAKey)]],
      pB = df[[paste0("p_", row$modelBKey)]]
    )
  }
}

runSingleComparison <- function(
  row,
  inbThresholds,
  B = 2000L,
  seedBase = 1L,
  outDir = NULL,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  threads = 1L,
  resume = FALSE
) {
  if (!is.null(outDir)) {
    ensureDir(outDir)
    outFile <- buildBootstrapOutFile(outDir, row)
    if (isTRUE(resume)) {
      cached <- readExistingBootstrapIfCompatible(outFile, B = B)
      if (!is.null(cached)) {
        return(cached)
      }
    }
  }

  pr <- getPairedPredictions(
    row = row,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath
  )
  y <- pr$y
  pA <- pr$pA
  pB <- pr$pB

  metricFns <- list(
    AUROC = auroc,
    AUPRC = auprc,
    Brier = brier,
    ICI = function(y, p) eavgGAM(y, p, nthreads = threads),
    INB = function(y, p) integratedNetBenefit(y, p, thresholds = inbThresholds[[row$outcomeName]])
  )

  bootRes <- bootPairedDeltas(
    y,
    pA,
    pB,
    metricFns,
    B = B,
    seedBase = seedBase
  )
  dl <- delongDelta(y, pA, pB)
  bootRes$delongP <- ifelse(bootRes$metric == "AUROC", dl$p, NA_real_)

  bootRes$outcomeName <- row$outcomeName
  bootRes$featureSet <- row$featureSet
  bootRes$W <- row$W
  bootRes$comparator <- row$comparator
  bootRes$modelAKey <- row$modelAKey
  bootRes$modelBKey <- row$modelBKey
  bootRes$quarterId <- row$quarterId
  bootRes$N <- length(y)
  bootRes$nEvents <- sum(y == 1L)

  if (!is.null(outDir)) {
    outFile <- buildBootstrapOutFile(outDir, row)
    utils::write.csv(bootRes, outFile, row.names = FALSE)
  }
  bootRes
}

runQuarterwise <- function(
  comparisons,
  outDir = NULL,
  B = 2000L,
  seedBase = 1L,
  inbThresholds = NULL,
  parallel = c("none", "mirai"),
  workers = NULL,
  showProgress = TRUE,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  threads = 1L,
  resume = FALSE,
  bootParallel = c("none", "mirai"),
  bootWorkers = NULL,
  bootChunkSize = 100L
) {
  if (!is.null(outDir)) ensureDir(outDir)
  if (is.null(inbThresholds)) {
    inbThresholds <- list(
      Fatality = seq(0.005, 0.20, by = 0.005),
      Hospitalization = seq(0.01, 0.30, by = 0.005),
      RespiratoryFailure = seq(0.01, 0.30, by = 0.005)
    )
  }
  parallel <- match.arg(parallel)
  bootParallel <- match.arg(bootParallel)
  # Cap threads in sequential mode using a single control knob
  if (parallel == "none" && exists("setAnalysisThreads", mode = "function")) {
    try(setAnalysisThreads(as.integer(threads), verbose = FALSE), silent = TRUE)
  }
  # No additional cap here; handled above based on gamThreads

  taskFun <- function(i, prog = NULL) {
    row <- comparisons[i, ]
    pr <- getPairedPredictions(
      row = row,
      runsRoot = runsRoot,
      allResultsPath = allResultsPath
    )
    y <- pr$y
    pA <- pr$pA
    pB <- pr$pB

	  metricFns <- list(
	    AUROC = auroc,
	    AUPRC = auprc,
    Brier = brier,
    ICI = function(y, p) eavgGAM(y, p, nthreads = threads),
    INB = function(y, p) integratedNetBenefit(y, p, thresholds = inbThresholds[[row$outcomeName]])
  )

    # Unique seed per task; if bootParallel=='mirai' and not already using mirai across comparisons,
    # use mirai chunking for B within this single comparison.
    if (bootParallel == "mirai" && parallel == "none") {
      bootRes <- bootPairedDeltasMirai(
        y = y,
        pA = pA,
        pB = pB,
        metricFns = metricFns,
        B = B,
        seedBase = seedBase + i,
        workers = bootWorkers,
        chunkSize = bootChunkSize,
        threads = threads,
        showProgress = showProgress
      )
    } else {
      bootRes <- bootPairedDeltas(
        y,
        pA,
        pB,
        metricFns,
        B = B,
        seedBase = seedBase + i,
        showProgress = showProgress
      )
    }
    dl <- delongDelta(y, pA, pB)
    bootRes$delongP <- ifelse(bootRes$metric == "AUROC", dl$p, NA_real_)

    # Attach identifiers
    bootRes$outcomeName <- row$outcomeName
    bootRes$featureSet <- row$featureSet
    bootRes$W <- row$W
    bootRes$comparator <- row$comparator
    bootRes$modelAKey <- row$modelAKey
    bootRes$modelBKey <- row$modelBKey
    bootRes$quarterId <- row$quarterId
    bootRes$N <- length(y)
    bootRes$nEvents <- sum(y == 1L)

    if (!is.null(outDir)) {
      outFile <- file.path(
        outDir,
        paste0(
          row$outcomeName,
          "_",
          row$featureSet,
          "_",
          row$W,
          "_",
          row$comparator,
          "_",
          row$quarterId,
          ".csv"
        )
      )
      utils::write.csv(bootRes, outFile, row.names = FALSE)
    }
    if (!is.null(prog)) {
      prog()
    }
    bootRes
  }

  n <- nrow(comparisons)
  if (n == 0L) return(data.frame())

  # Parallel across comparisons using mirai
  if (parallel == "mirai") {
    if (!requireNamespace("mirai", quietly = TRUE)) {
      warning("Package 'mirai' not installed; running sequentially.")
    } else {
      if (is.null(workers)) {
        workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
      }
      mirai::daemons(workers)
      if (isTRUE(showProgress)) {
        pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
        on.exit(
          {
            try(close(pb), silent = TRUE)
          },
          add = TRUE
        )
      }

      jobs <- vector("list", n)
      for (ii in seq_len(n)) {
        rowi <- comparisons[ii, ]
        # Launch a job that sources helpers inside the worker to ensure availability
        jobs[[ii]] <- mirai::mirai(
          {
            # Cap BLAS/OpenMP inside worker to avoid oversubscription
            try(source('extras/postAnalysis/setThreads.R'), silent = TRUE)
            if (exists('setAnalysisThreads')) {
              try(setAnalysisThreads(as.integer(threads_i), verbose = FALSE), silent = TRUE)
            }
            source('extras/postAnalysis/metrics.R')
            source('extras/postAnalysis/pairedTests.R')
            source('extras/postAnalysis/extractPredictions.R')
            source('extras/postAnalysis/drivers.R')
            runSingleComparison(
              row = rowi,
              inbThresholds = inbThresholds,
              B = B,
              seedBase = seedBase_i,
              outDir = outDir,
              runsRoot = runsRoot,
              allResultsPath = allResultsPath,
              threads = threads_i,
              resume = resume_i
            )
          },
          .args = list(
            rowi = rowi,
            inbThresholds = inbThresholds,
            B = B,
            seedBase_i = seedBase + ii,
            outDir = outDir,
            runsRoot = runsRoot,
            allResultsPath = allResultsPath,
            threads_i = threads,
            resume_i = resume
          )
        )
      }

      # Poll for completion and update progress (mirai API)
      done <- rep(FALSE, n)
      while (!all(done)) {
        for (k in seq_len(n)) {
          if (!done[k] && !mirai::unresolved(jobs[[k]])) {
            done[k] <- TRUE
          }
        }
        if (isTRUE(showProgress)) {
          utils::setTxtProgressBar(pb, sum(done))
        }
        if (!all(done)) Sys.sleep(0.1)
      }
      # Collect values (propagate any errors)
      vals <- lapply(jobs, function(m) m[])
      outDf <- do.call(rbind, vals)
      return(outDf)
    }
  }

  # Sequential: show only bootstrap progress (inside each task)
  vals <- vector("list", n)
  for (i in seq_len(n)) {
    vals[[i]] <- taskFun(i, prog = NULL)
  }
  outDf <- do.call(rbind, vals)
  outDf
}

poolResults <- function(
  quarterwiseDir = "results/quarterwise",
  outDir = "results/pooled",
  K = 6L
) {
  ensureDir(outDir)
  files <- list.files(quarterwiseDir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) {
    return(invisible(FALSE))
  }
  qw <- do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
  qw <- qw[stats::complete.cases(qw$delta), ]
  keyCols <- c("outcomeName", "featureSet", "W", "comparator", "metric")
  splitIdx <- split(seq_len(nrow(qw)), interaction(qw[keyCols], drop = TRUE))

  if (!requireNamespace("metafor", quietly = TRUE)) {
    warning(
      "Package 'metafor' not installed; falling back to fixed-effects averages."
    )
  }

  for (idx in splitIdx) {
    df <- qw[idx, ]
    # Keep only first K post-dev quarters if available
    # Assume chronological order by quarterId prefix (startDate)
    df <- df[order(df$quarterId), ]
    if (nrow(df) > K) {
      df <- df[seq_len(K), ]
    }

    # Estimate SE from percentile CI
    se <- (df$hi - df$lo) / (2 * 1.96)
    w <- 1 / (se^2)
    pooled <- sum(df$delta * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    seP <- sqrt(1 / sum(w, na.rm = TRUE))
    ciLo <- pooled - 1.96 * seP
    ciHi <- pooled + 1.96 * seP
    pVal <- 2 * stats::pnorm(-abs(pooled / seP))

    out <- data.frame(
      outcomeName = df$outcomeName[1],
      featureSet = df$featureSet[1],
      W = df$W[1],
      comparator = df$comparator[1],
      metric = df$metric[1],
      deltaPooled = pooled,
      ciLo = ciLo,
      ciHi = ciHi,
      pValue = pVal,
      kQuarters = nrow(df),
      quarters = paste(df$quarterId, collapse = ";")
    )
    outFile <- file.path(
      outDir,
      paste0(
        out$outcomeName,
        "_",
        out$featureSet,
        "_",
        out$W,
        "_",
        out$comparator,
        "_",
        out$metric,
        "_pooled.csv"
      )
    )
    utils::write.csv(out, outFile, row.names = FALSE)
  }
  invisible(TRUE)
}
