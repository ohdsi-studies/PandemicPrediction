library(PatientLevelPrediction)
library(Strategus)
library(dplyr)
library(lubridate)
library(jsonlite)

# Mirrors your pipeline for a single model and two adjacent periods:
# 1) Recalibrate on P1 (weakRecalibration) -> writes JSON
# 2) Extract recalibrated model from runPlp.rds into inst/recalibratedModels
# 3) Validate on P2 (same window for original vs recalibrated) -> writes JSON
# Only required cohorts are included (target, outcome, and model feature cohorts).

packageName <- "PandemicPrediction"
modelFolder <- "models/dato" # packaged model relative to inst/
targetId <- 31 # target for validation
outcomeId <- 11 # Death

# Periods: recalibrate on P1, validate on the next period P2
recalStart <- ymd("2020-01-01")
recalEnd <- ymd("2020-03-31")
valStart <- recalEnd + days(1)
valEnd <- valStart + months(3) - days(1)

# Where you will run the recalibration JSON (recommended)
recalibrationResultsDir <- "./results/pipeline_simple_recalibration" # set this in Strategus execution settings

# Helper to reference packaged models
createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  return(result)
}

# Identify feature cohort dependencies used by the model
getFeatureCohortIds <- function(modelDir) {
  designPath <- file.path("inst", modelDir, "modelDesign.json")
  if (!file.exists(designPath)) {
    return(integer(0))
  }
  md <- jsonlite::fromJSON(designPath, simplifyVector = FALSE)
  covs <- md$covariateSettings
  if (is.null(covs)) {
    return(integer(0))
  }
  ids <- c()
  for (cset in covs) {
    attr_fun <- if (is.null(cset$attr_fun)) "" else cset$attr_fun
    if (isTRUE(grepl("getCohortCovariateData", attr_fun))) {
      ids <- c(ids, unlist(cset$cohortIds))
    }
  }
  ids <- ids[!is.na(ids)]
  as.integer(unique(ids))
}

# Minimal cohort set
featureCohortIds <- getFeatureCohortIds(modelFolder)
requiredCohortIds <- sort(unique(c(targetId, outcomeId, featureCohortIds)))

# Load and filter cohortDefinitionSet BEFORE creating shared resources
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

idCol <- dplyr::case_when(
  "cohortId" %in% names(cohortDefinitionSet) ~ "cohortId",
  "cohort_id" %in% names(cohortDefinitionSet) ~ "cohort_id",
  "atlasId" %in% names(cohortDefinitionSet) ~ "atlasId",
  TRUE ~ NA_character_
)
if (is.na(idCol)) {
  stop("Could not detect cohort id column in cohortDefinitionSet")
}

cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data[[idCol]] %in% requiredCohortIds)
message(
  "Cohorts to generate (",
  idCol,
  "): ",
  paste(sort(unique(cohortDefinitionSet[[idCol]])), collapse = ", ")
)

cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(
  generateStats = TRUE
)

# Recalibration design (P1)
restrictP1 <- list(
  PatientLevelPrediction::createRestrictPlpDataSettings(
    studyStartDate = format(recalStart, "%Y%m%d"),
    studyEndDate = format(recalEnd, "%Y%m%d")
  )
)

plpModelOriginal <- createPackageModel(modelFolder, packageName)

val_recalibration <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL,
  restrictPlpDataSettings = restrictP1,
  plpModelList = list(plpModelOriginal),
  recalibrate = "weakRecalibration",
  runCovariateSummary = FALSE
)

predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predRecSpec <- predictionValidationModule$createModuleSpecifications(
  validationList = list(val_recalibration)
)

analysisSpec_recal <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predRecSpec)

recalJsonPath <- "./inst/study_execution_jsons/pipeline_simple_recalibration.json"
ParallelLogger::saveSettingsToJson(analysisSpec_recal, recalJsonPath)
message("Saved recalibration JSON: ", normalizePath(recalJsonPath))
message(
  "Run this JSON with Strategus, using output folder: ",
  normalizePath(recalibrationResultsDir)
)

# Attempt to promote a recalibrated model from results (if available)
promoteRecalibratedModel <- function(
  resultsDir,
  originalModelPath,
  restrictSettings
) {
  # Default expected path for single-analysis run
  defaultRunPlp <- file.path(
    resultsDir,
    "Analysis_1",
    "validationResult",
    "runPlp.rds"
  )
  runPlpPath <- defaultRunPlp
  if (!file.exists(runPlpPath)) {
    # Fallback: search for runPlp.rds within resultsDir
    cand <- list.files(
      resultsDir,
      pattern = "^runPlp\\.rds$",
      recursive = TRUE,
      full.names = TRUE
    )
    cand <- cand[grepl("Analysis_", cand, fixed = TRUE)]
    if (length(cand) > 0) runPlpPath <- cand[[1]]
  }
  if (!file.exists(runPlpPath)) {
    message(
      "No runPlp.rds found yet under ",
      resultsDir,
      ". Skipping promotion step."
    )
    return(NULL)
  }

  originalModel <- PatientLevelPrediction::loadPlpModel(originalModelPath)
  runPlpObject <- readRDS(runPlpPath)
  coeffs <- attr(runPlpObject$prediction, "metaData")$weakRecalibration
  if (is.null(coeffs)) {
    warning("Missing weakRecalibration coefficients in ", runPlpPath)
    return(NULL)
  }

  # Sanity check: gradient should be > 0 for rank invariance
  if (
    !is.null(coeffs$adjustGradient) &&
      is.finite(coeffs$adjustGradient) &&
      coeffs$adjustGradient <= 0
  ) {
    warning(sprintf(
      "Weak recalibration gradient <= 0 (%.4f). Ranking may change and AUROC may differ.",
      coeffs$adjustGradient
    ))
  }

  promoted <- originalModel
  attr(promoted, "originalPredictionFunction") <- attr(
    originalModel,
    "predictionFunction"
  )
  attr(promoted, "recalibration") <- list(
    coefficients = coeffs,
    recalibratedOn = paste0(
      format(recalStart, "%Y%m%d"),
      "_to_",
      format(recalEnd, "%Y%m%d")
    )
  )
  attr(promoted, "predictionFunction") <- paste0(
    packageName,
    "::predictWithRecalibration"
  )

  # Optional: record period in model design for traceability
  if (!is.null(promoted$modelDesign$modelSettings)) {
    promoted$modelDesign$modelSettings$recalibrationPeriod <- list(
      studyStartDate = restrictSettings[[1]]$studyStartDate,
      studyEndDate = restrictSettings[[1]]$studyEndDate
    )
  }

  # Construct a descriptive folder name
  featureSet <- if (grepl("dataDriven", originalModelPath, fixed = TRUE)) {
    "Full"
  } else {
    "Parsimonious"
  }
  outName <- paste0(
    "Death_",
    featureSet,
    "_SimplePipeline_RecalOn_",
    format(recalStart, "%Y%m%d"),
    "_to_",
    format(recalEnd, "%Y%m%d")
  )
  destDir <- file.path("./inst/recalibratedModels", outName)
  dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
  PatientLevelPrediction::savePlpModel(promoted, destDir)
  message("Promoted recalibrated model to: ", normalizePath(destDir))
  return(destDir)
}

# Try promotion; if not yet available, user can rerun this script after executing the recalibration JSON
recalibratedModelPath <- promoteRecalibratedModel(
  resultsDir = recalibrationResultsDir,
  originalModelPath = file.path("./inst", modelFolder),
  restrictSettings = restrictP1
)

# If we promoted a recalibrated model, build a compare JSON (P2) with both models
if (!is.null(recalibratedModelPath)) {
  relRecalModelPath <- sub("^./inst/", "", recalibratedModelPath)

  restrictP2 <- list(
    PatientLevelPrediction::createRestrictPlpDataSettings(
      studyStartDate = format(valStart, "%Y%m%d"),
      studyEndDate = format(valEnd, "%Y%m%d")
    )
  )

  modelOriginal <- createPackageModel(modelFolder, packageName)
  modelRecalibrated <- createPackageModel(relRecalModelPath, packageName)

  val_origP2 <- PatientLevelPrediction::createValidationDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    populationSettings = NULL,
    restrictPlpDataSettings = restrictP2,
    plpModelList = list(modelOriginal),
    recalibrate = NULL,
    runCovariateSummary = FALSE
  )

  val_recalP2 <- PatientLevelPrediction::createValidationDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    populationSettings = NULL,
    restrictPlpDataSettings = restrictP2,
    plpModelList = list(modelRecalibrated),
    recalibrate = NULL,
    runCovariateSummary = FALSE
  )

  predValModule <- Strategus::PatientLevelPredictionValidationModule$new()
  predValSpecs <- predValModule$createModuleSpecifications(
    validationList = list(val_origP2, val_recalP2)
  )

  analysisSpec_compare <- Strategus::createEmptyAnalysisSpecificiations() |>
    Strategus::addSharedResources(cohortDefinitionShared) |>
    Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
    Strategus::addModuleSpecifications(predValSpecs)

  compareJsonPath <- "./inst/study_execution_jsons/pipeline_simple_validate_compare.json"
  ParallelLogger::saveSettingsToJson(analysisSpec_compare, compareJsonPath)
  message(
    "Saved validation compare JSON (P2): ",
    normalizePath(compareJsonPath)
  )

  message(sprintf(
    "Compare AUROC for original vs recalibrated on %s to %s.",
    format(valStart, "%Y-%m-%d"),
    format(valEnd, "%Y-%m-%d")
  ))
} else {
  message(
    "Re-run this script after executing recalibration to create the P2 compare JSON."
  )
}
