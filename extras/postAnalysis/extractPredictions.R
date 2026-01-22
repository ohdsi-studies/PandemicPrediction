# Utilities to read raw PLP predictions from results/runs and map them to
# quarter IDs and model keys. CamelCase naming.

safeReadJson <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

parseModelKey <- function(modelKey) {
  # Returns list(family, featureSet, W) where family in {covid, proxy_frozen, proxy_recal, proxy_roll}
  x <- tolower(modelKey)
  parts <- strsplit(x, "_")[[1]]
  if (length(parts) < 2) {
    return(list(
      family = NA_character_,
      featureSet = NA_character_,
      W = NA_character_
    ))
  }
  family <- paste(parts[1:2], collapse = "_")
  if (family == "covid_pars" || family == "covid_full") {
    return(list(
      family = "covid",
      featureSet = if (parts[2] == "pars") "Parsimonious" else "Full",
      W = if (length(parts) >= 3) paste(parts[3:length(parts)], collapse = "_") else NA_character_
    ))
  }
  if (family == "proxy_frozen") {
    return(list(
      family = "proxy_frozen",
      featureSet = if (parts[3] == "pars") "Parsimonious" else "Full",
      W = NA_character_
    ))
  }
  if (family == "proxy_recal") {
    return(list(
      family = "proxy_recal",
      featureSet = if (parts[3] == "pars") "Parsimonious" else "Full",
      W = if (length(parts) >= 4) paste(parts[4:length(parts)], collapse = "_") else NA_character_
    ))
  }
  if (family == "proxy_roll") {
    return(list(
      family = "proxy_roll",
      featureSet = if (parts[3] == "pars") "Parsimonious" else "Full",
      W = if (length(parts) >= 4) paste(parts[4:length(parts)], collapse = "_") else NA_character_
    ))
  }
  list(family = NA_character_, featureSet = NA_character_, W = NA_character_)
}

inferFeatureSetFromModel <- function(modelDesign) {
  covariateSettings <- modelDesign$covariateSettings
  covariateSettings <- covariateSettings %||% list()
  len <- length(covariateSettings)
  if (!len) {
    return(NA_character_)
  }
  if (len == 1) {
    "Full"
  } else {
    "Parsimonious"
  }
}

findRunRoots <- function(runsRoot = "results/runs") {
  # Returns data.frame with runPlpPath, modelPath, runType
  patt <- c(
    file.path(
      runsRoot,
      "validation",
      "strategusWork",
      "PatientLevelPredictionValidationModule",
      "**",
      "Analysis_*",
      "validationResult",
      "runPlp.rds"
    ),
    file.path(
      runsRoot,
      "new_models_validation",
      "strategusWork",
      "PatientLevelPredictionValidationModule",
      "**",
      "Analysis_*",
      "validationResult",
      "runPlp.rds"
    ),
    file.path(
      runsRoot,
      "recalibrated_models_validation",
      "strategusWork",
      "PatientLevelPredictionValidationModule",
      "**",
      "Analysis_*",
      "validationResult",
      "runPlp.rds"
    ),
    file.path(
      runsRoot,
      "rolling_recalibrated_validation",
      "strategusWork",
      "PatientLevelPredictionValidationModule",
      "**",
      "Analysis_*",
      "validationResult",
      "runPlp.rds"
    )
  )
  files <- unlist(lapply(patt, Sys.glob), use.names = FALSE)
  if (!length(files)) {
    return(data.frame())
  }
  modelPath <- file.path(dirname(files), "model")
  typ <- ifelse(
    grepl("/new_models_validation/", files),
    "new_models_validation",
    ifelse(
      grepl("/recalibrated_models_validation/", files),
      "recalibrated_models_validation",
      ifelse(
        grepl("/rolling_recalibrated_validation/", files),
        "rolling_recalibrated_validation",
        "validation"
      )
    )
  )
  data.frame(
    runPlpPath = files,
    modelPath = modelPath,
    runType = typ,
    stringsAsFactors = FALSE
  )
}

parseQuarterDates <- function(quarterId) {
  # quarterId = YYYYMMDD_YYYYMMDD_OutcomeName
  parts <- strsplit(quarterId, "_", fixed = TRUE)[[1]]
  startDate <- as.Date(parts[1], "%Y%m%d")
  endDate <- as.Date(parts[2], "%Y%m%d")
  list(
    startDate = gsub("-", "", as.character(startDate)),
    endDate = gsub("-", "", as.character(endDate))
  )
}

getRunMeta <- function(modelPath) {
  vdPath <- file.path(modelPath, "validationDetails.json")
  vd <- safeReadJson(vdPath)
  if (is.null(vd)) {
    stop(
      "Missing validationDetails.json under ",
      modelPath,
      "; rerun Strategus with validation metadata or restore the file."
    )
  }
  outcomeId <- vd$outcomeId %||% NA_integer_
  targetId <- vd$targetId %||% NA_integer_
  rs <- vd$restrictPlpDataSettings
  startDate <- rs$studyStartDate
  endDate <- rs$studyEndDate
  mdPath <- file.path(modelPath, "modelDesign.json")
  if (!file.exists(mdPath)) {
    stop("Missing modelDesign.json under ", modelPath, "; cannot infer feature set.")
  }
  md <- jsonlite::fromJSON(mdPath, simplifyVector = FALSE)
  featureSet <- inferFeatureSetFromModel(md)
  list(
    outcomeId = as.integer(outcomeId),
    targetId = as.integer(targetId),
    startDate = startDate,
    endDate = endDate,
    featureSet = featureSet
  )
}

loadRunPredictions <- function(
  runPlpPath,
  evalType = c("Validation", "Test"),
  includeLinearPredictor = FALSE
) {
  evalType <- match.arg(evalType)
  x <- readRDS(runPlpPath)
  pred <- x$prediction
  pred <- pred[pred$evaluationType == evalType, , drop = FALSE]
  df <- data.frame(
    patientId = pred$subjectId,
    y = pred$outcomeCount,
    p = pred$value,
    stringsAsFactors = FALSE
  )
  if (isTRUE(includeLinearPredictor) && "linearPredictor" %in% colnames(pred)) {
    df$lp <- pred$linearPredictor
  }
  df
}

computeAuroc <- function(y, p) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    return(NA_real_)
  }
  r <- try(pROC::roc(y, p, quiet = TRUE, direction = "auto"), silent = TRUE)
  if (inherits(r, "try-error")) {
    return(NA_real_)
  }
  as.numeric(pROC::auc(r))
}

locateQuarterRun <- function(
  modelKey,
  row,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4
) {
  stopifnot(all(c("outcomeName", "quarterId") %in% names(row)))
  info <- parseModelKey(modelKey)
  if (is.na(info$family)) {
    stop("Unknown model key: ", modelKey)
  }
  q <- parseQuarterDates(row$quarterId)
  ar <- utils::read.csv(allResultsPath, stringsAsFactors = FALSE)
  ar$startDate <- as.Date(ar$startDate)
  ar$endDate <- as.Date(ar$endDate)

  outcomeId <- unique(ar$outcomeId[ar$outcomeName == row$outcomeName])
  if (!length(outcomeId)) {
    stop("Cannot map outcomeName to outcomeId from allResults.csv")
  }
  outcomeId <- outcomeId[1]

  roots <- findRunRoots(runsRoot)
  if (!nrow(roots)) {
    stop("No runPlp.rds files found under ", runsRoot)
  }

  runType <- switch(
    info$family,
    covid = "new_models_validation",
    proxy_frozen = "validation",
    proxy_recal = "recalibrated_models_validation",
    proxy_roll = "rolling_recalibrated_validation",
    stop("Unknown family for modelKey: ", modelKey)
  )
  cand <- roots[roots$runType == runType, , drop = FALSE]
  if (!nrow(cand)) {
    stop("No runs of type ", runType, " found for ", modelKey)
  }
  meta <- lapply(cand$modelPath, getRunMeta)
  ok <- vapply(
    meta,
    function(m) {
      if (is.null(m)) return(FALSE)
      targetFeature <- info$featureSet
      matchesFeature <- is.na(targetFeature) ||
        (!is.null(m$featureSet) &&
          !isTRUE(is.na(m$featureSet)) &&
          identical(tolower(m$featureSet), tolower(targetFeature)))
      identical(m$startDate, q$startDate) &&
        identical(m$endDate, q$endDate) &&
        identical(as.integer(m$outcomeId), as.integer(outcomeId)) &&
        matchesFeature
    },
    logical(1)
  )
  cand <- cand[ok, , drop = FALSE]
  meta <- meta[ok]
  if (!nrow(cand)) {
    stop("No matching run for ", modelKey, " in quarter ", row$quarterId)
  }

  # If multiple candidates, disambiguate via AUROC match to allResults.csv
  ar2 <- subset(
    ar,
    outcomeName == row$outcomeName &
      startDate == as.Date(q$startDate, "%Y%m%d") &
      endDate == as.Date(q$endDate, "%Y%m%d")
  )
  if (!is.na(info$featureSet)) {
    ar2 <- subset(ar2, featureSet == info$featureSet)
  }
  familyName <- switch(
    info$family,
    covid = "New Covid",
    proxy_frozen = "Original Influenza",
    proxy_recal = "Recalibrated Influenza",
    proxy_roll = "Rolling Recalibrated Influenza"
  )
  ar2 <- subset(ar2, modelOrigin == familyName)
  if (nrow(ar2)) {
    if (info$family == "covid") {
      ar2$W <- ar2$wDev
    } else if (info$family == "proxy_recal") {
      ar2$W <- ar2$wRecal
    } else if (info$family == "proxy_roll") {
      ar2$W <- "na"
    } else {
      ar2$W <- NA
    }
    if (!is.na(info$W)) {
      ar2 <- subset(ar2, W == info$W)
    }
  }
  expAucs <- unique(ar2$AUROC)

  for (i in seq_len(nrow(cand))) {
    p <- loadRunPredictions(
      cand$runPlpPath[i],
      evalType = "Validation",
      includeLinearPredictor = FALSE
    )
    auc <- computeAuroc(p$y, p$p)
    if (any(abs(auc - expAucs) < tolerance, na.rm = TRUE)) {
      return(list(
        runPlpPath = cand$runPlpPath[i],
        meta = meta[[i]],
        auc = auc
      ))
    }
  }
  # Fallback: first candidate
  list(runPlpPath = cand$runPlpPath[1], meta = meta[[1]], auc = NA_real_)
}

loadQuarterPairFromRuns <- function(
  row,
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4,
  includeLinearPredictor = FALSE
) {
  # row: one row from comparisons with fields: outcomeName, W, featureSet, quarterId, modelAKey, modelBKey
  # Returns list(df, meta), where df has columns patient_id, y, p_<A>, p_<B>
  stopifnot(all(
    c("outcomeName", "quarterId", "modelAKey", "modelBKey") %in% names(row)
  ))

  a <- locateQuarterRun(
    modelKey = row$modelAKey,
    row = row,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  b <- locateQuarterRun(
    modelKey = row$modelBKey,
    row = row,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  pa <- loadRunPredictions(
    a$runPlpPath,
    evalType = "Validation",
    includeLinearPredictor = includeLinearPredictor
  )
  pb <- loadRunPredictions(
    b$runPlpPath,
    evalType = "Validation",
    includeLinearPredictor = includeLinearPredictor
  )

  # Inner join by patientId; assume y consistent, use A's y
  names(pa)[names(pa) == "p"] <- paste0("p_", row$modelAKey)
  names(pb)[names(pb) == "p"] <- paste0("p_", row$modelBKey)
  if (includeLinearPredictor && "lp" %in% names(pa)) {
    names(pa)[names(pa) == "lp"] <- paste0("lp_", row$modelAKey)
  }
  if (includeLinearPredictor && "lp" %in% names(pb)) {
    names(pb)[names(pb) == "lp"] <- paste0("lp_", row$modelBKey)
  }
  keepA <- c(
    "patientId",
    "y",
    paste0("p_", row$modelAKey),
    if (includeLinearPredictor && paste0("lp_", row$modelAKey) %in% names(pa)) {
      paste0("lp_", row$modelAKey)
    }
  )
  keepB <- c(
    "patientId",
    "y",
    paste0("p_", row$modelBKey),
    if (includeLinearPredictor && paste0("lp_", row$modelBKey) %in% names(pb)) {
      paste0("lp_", row$modelBKey)
    }
  )
  m <- merge(
    pa[, keepA, drop = FALSE],
    pb[, keepB, drop = FALSE],
    by = "patientId"
  )
  # Harmonize
  names(m)[names(m) == "patientId"] <- "patient_id"
  # Optionally, check y equality
  if (!all(m$y.x == m$y.y)) {
    warning("Outcome labels differ across models for quarter ", row$quarterId)
  }
  m$y <- m$y.x
  m$y.x <- NULL
  m$y.y <- NULL
  m
}

loadQuarterPlpResult <- function(
  row,
  role = c("A", "B"),
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4
) {
  role <- match.arg(role)
  key <- if (identical(role, "A")) row$modelAKey else row$modelBKey
  run <- locateQuarterRun(
    modelKey = key,
    row = row,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  readRDS(run$runPlpPath)
}

loadQuarterPredictions <- function(
  row,
  role = c("A", "B"),
  evalType = c("Validation", "Test", "Train"),
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4,
  includeLinearPredictor = FALSE
) {
  evalType <- match.arg(evalType)
  role <- match.arg(role)
  key <- if (identical(role, "A")) row$modelAKey else row$modelBKey
  run <- locateQuarterRun(
    modelKey = key,
    row = row,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  loadRunPredictions(
    runPlpPath = run$runPlpPath,
    evalType = evalType,
    includeLinearPredictor = includeLinearPredictor
  )
}

computeNetBenefitCurve <- function(
  row,
  role = c("B", "A"),
  thresholds = seq(0.01, 0.5, by = 0.01),
  evalType = c("Validation", "Test", "Train"),
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4
) {
  role <- match.arg(role)
  evalType <- match.arg(evalType)
  preds <- loadQuarterPredictions(
    row = row,
    role = role,
    evalType = evalType,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  # Column names: patientId, y, p
  y <- as.integer(preds$y)
  p <- as.numeric(preds$p)
  prevalence <- mean(y)
  nb <- function(th) {
    pred <- p >= th
    tp <- sum(pred & (y == 1))
    fp <- sum(pred & (y == 0))
    n <- length(y)
    tp / n - fp / n * th / (1 - th)
  }
  treatAll <- function(th) {
    prevalence - (1 - prevalence) * th / (1 - th)
  }
  data.frame(
    threshold = thresholds,
    netBenefit = vapply(thresholds, nb, numeric(1)),
    treatAll = vapply(thresholds, treatAll, numeric(1)),
    treatNone = 0,
    eventRate = prevalence,
    role = role,
    modelKey = if (identical(role, "A")) row$modelAKey else row$modelBKey,
    quarterId = row$quarterId,
    stringsAsFactors = FALSE
  )
}

plotQuarterNetBenefitCurve <- function(
  row,
  role = c("B", "A"),
  thresholds = seq(0.01, 0.5, by = 0.01),
  evalType = c("Validation", "Test", "Train"),
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4
) {
  role <- match.arg(role)
  evalType <- match.arg(evalType)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for net benefit plotting.")
  }
  df <- computeNetBenefitCurve(
    row = row,
    role = role,
    thresholds = thresholds,
    evalType = evalType,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  ggplot2::ggplot(df, ggplot2::aes(x = threshold)) +
    ggplot2::geom_hline(yintercept = 0, color = "#777777", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = netBenefit, color = "Model"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = treatAll, color = "Treat all"), linewidth = 0.6, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = treatNone, color = "Treat none"), linewidth = 0.6, linetype = "dotdash") +
    ggplot2::labs(
      title = paste0("Net benefit curve (", evalType, ")"),
      subtitle = paste(df$modelKey[1], df$quarterId[1]),
      x = "Threshold",
      y = "Net benefit",
      color = NULL
    ) +
    ggplot2::scale_color_manual(values = c(
      "Model" = "#1f77b4",
      "Treat all" = "#ff7f0e",
      "Treat none" = "#555555"
    )) +
    ggplot2::theme_minimal(base_size = 12)
}

plotQuarterNetBenefit <- function(
  row,
  role = c("B", "A"),
  evalType = c("Validation", "Test", "Train"),
  runsRoot = "results/runs",
  allResultsPath = "results/allResults.csv",
  tolerance = 1e-4,
  ...
) {
  evalType <- match.arg(evalType)
  role <- match.arg(role)
  if (!requireNamespace("PatientLevelPrediction", quietly = TRUE)) {
    stop("Package 'PatientLevelPrediction' is required for net benefit plotting.")
  }
  plpResult <- loadQuarterPlpResult(
    row = row,
    role = role,
    runsRoot = runsRoot,
    allResultsPath = allResultsPath,
    tolerance = tolerance
  )
  PatientLevelPrediction::plotNetBenefit(
    plpResult = plpResult,
    type = evalType,
    ...
  )
}
