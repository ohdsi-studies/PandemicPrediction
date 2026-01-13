#!/usr/bin/env Rscript

# This script extracts weak-recalibration coefficients from Strategus validation runs
# and writes them to results/weakRecalibration_coeffs.csv.
# Tip: run with `Rscript --no-init-file extras/exportWeakRecalibration.R`
# to avoid slow renv auto-activation if that is an issue.

# Make renv autoload a no-op if the user's .Rprofile is sourced
Sys.setenv("RENV_CONFIG_AUTOLOADER_ENABLED" = "FALSE")

suppressPackageStartupMessages({
  library(jsonlite)
})

# ---- Config ---
runsRoot <- "results/runs/validation/strategusWork/PatientLevelPredictionValidationModule"
dataSource <- "OPTUM Extended DOD" # adjust if your data source folder differs
allResultsPath <- "results/allResults.csv"
outFile <- "results/weakRecalibration_coeffs.csv"
aucTolerance <- 1e-4

# ---- Helpers ----

safeReadJson <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

matchQuarterId <- function(startDate, endDate, outcomeName) {
  paste0(
    format(as.Date(startDate), "%Y%m%d"), "_",
    format(as.Date(endDate), "%Y%m%d"), "_",
    outcomeName
  )
}

makeModelKey <- function(modelOrigin, featureSet, w = NA_character_) {
  fs <- ifelse(tolower(featureSet) == "parsimonious", "pars", "full")
  mo <- tolower(modelOrigin)
  if (mo == "new covid") {
    paste0("covid_", fs, "_", w)
  } else if (mo == "original influenza") {
    paste0("proxy_frozen_", fs)
  } else if (mo == "original influenza (recalibrated)") {
    paste0("proxy_recal_", fs, "_", w)
  } else {
    NA_character_
  }
}

inferFeatureAndKey <- function(startDate, endDate, outcomeId, aucVal, ar, tolerance = 1e-4) {
  ar$startDateChr <- gsub("-", "", as.character(ar$startDate))
  ar$endDateChr <- gsub("-", "", as.character(ar$endDate))
  mask <- ar$startDateChr == startDate &
    ar$endDateChr == endDate &
    as.integer(ar$outcomeId) == as.integer(outcomeId) &
    grepl("Original Influenza", ar$modelOrigin, fixed = TRUE)
  cand <- ar[mask, , drop = FALSE]
  if (!nrow(cand)) return(list(featureSet = NA_character_, modelKey = NA_character_))
  cand$AUROC <- as.numeric(cand$AUROC)
  diff <- abs(cand$AUROC - aucVal)
  idx <- which(diff == min(diff, na.rm = TRUE) & diff <= tolerance)
  if (!length(idx)) return(list(featureSet = NA_character_, modelKey = NA_character_))
  row <- cand[idx[1], ]
  modelKey <- makeModelKey(row$modelOrigin, row$featureSet, row$wDev %||% row$wRecal)
  list(featureSet = row$featureSet, modelKey = modelKey, outcomeName = row$outcomeName)
}

# ---- Load ground truth ----
allRes <- NULL
if (file.exists(allResultsPath)) {
  allRes <- try(
    utils::read.csv(
      allResultsPath,
      stringsAsFactors = FALSE,
      colClasses = c(
        startDate = "character",
        endDate = "character",
        outcomeId = "integer",
        AUROC = "numeric",
        modelOrigin = "character",
        featureSet = "character",
        wDev = "character",
        wRecal = "character",
        outcomeName = "character"
      )
    ),
    silent = TRUE
  )
}

# ---- Main ----

root <- file.path(runsRoot, dataSource)
analyses <- list.dirs(root, recursive = FALSE, full.names = TRUE)
analyses <- analyses[grepl("Analysis_", basename(analyses))]
if (!length(analyses)) {
  stop("No Analysis_* folders found under ", root)
}

rows <- list()
for (dir in analyses) {
  aId <- sub("^Analysis_", "", basename(dir))
  # validationDetails are stored inside validationResult/model/
  vdPath <- file.path(dir, "validationResult", "model", "validationDetails.json")
  runPath <- file.path(dir, "validationResult", "runPlp.rds")
  if (!file.exists(runPath)) next

  vd <- safeReadJson(vdPath)
  if (is.null(vd) || is.null(vd$restrictPlpDataSettings)) next
  startDate <- vd$restrictPlpDataSettings$studyStartDate
  endDate <- vd$restrictPlpDataSettings$studyEndDate
  outcomeId <- vd$outcomeId
  targetId <- vd$targetId

  quarterId <- NA_character_
  outcomeName <- NA_character_
  featureSet <- NA_character_
  modelKey <- NA_character_

  run <- readRDS(runPath)
  wr <- attr(run$prediction, "metaData")$weakRecalibration
  if (is.null(wr)) next

  # AUROC from evaluationStatistics (Validation rows)
  aucVal <- NA_real_
  if ("performanceEvaluation" %in% names(run)) {
    es <- run$performanceEvaluation$evaluationStatistics
    if (!is.null(es)) {
      mask <- es$evaluation == "Validation" & es$metric == "AUROC"
      if (any(mask, na.rm = TRUE)) {
        aucVal <- as.numeric(es$value[which(mask)[1]])
      }
    }
  }

  if (!inherits(allRes, "try-error") && !is.null(allRes) && !is.na(aucVal)) {
    res <- inferFeatureAndKey(startDate, endDate, outcomeId, aucVal, allRes, tolerance = aucTolerance)
    featureSet <- res$featureSet
    modelKey <- res$modelKey
    outcomeName <- res$outcomeName %||% outcomeName
    if (!is.na(outcomeName)) {
      quarterId <- matchQuarterId(as.Date(startDate, "%Y%m%d"), as.Date(endDate, "%Y%m%d"), outcomeName)
    }
  }

  rows[[length(rows) + 1]] <- data.frame(
    analysisId = as.integer(aId),
    modelFolder = NA_character_,
    modelKey = modelKey,
    family = ifelse(is.na(modelKey), NA_character_, ifelse(grepl("proxy", modelKey), "proxy", "covid")),
    featureSet = featureSet,
    outcomeId = outcomeId,
    outcomeName = outcomeName,
    targetId = targetId,
    quarterStart = startDate,
    quarterEnd = endDate,
    quarterId = quarterId,
    intercept = as.numeric(wr$adjustIntercept %||% wr[[1]]),
    slope = as.numeric(wr$adjustGradient %||% wr[[2]]),
    stringsAsFactors = FALSE
  )
}

if (!length(rows)) {
  stop("No weakRecalibration coefficients found.")
}

out <- do.call(rbind, rows)

dir.create(dirname(outFile), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, outFile, row.names = FALSE)
message("Wrote weak recalibration coefficients to: ", normalizePath(outFile))
