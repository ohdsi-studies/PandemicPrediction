library(PatientLevelPrediction)
library(Strategus)
library(dplyr)
library(jsonlite)

# Simple, minimal Strategus JSON to:
# 1) Validate a single model on a single time period (no recalibration)
# 2) Validate the same model on the same time period with weak recalibration
#
# This lets you directly check that ranking metrics (e.g., AUC) are identical
# between original and weak-recalibrated validations for the same population
# and time window.

# --- Cohorts (mirrors your existing setup) ---
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

# Helper to extract feature cohort IDs from a packaged model's modelDesign.json
getFeatureCohortIds <- function(modelDir) {
  designPath <- file.path("inst", modelDir, "modelDesign.json")
  if (!file.exists(designPath)) return(integer(0))
  md <- jsonlite::fromJSON(designPath, simplifyVector = FALSE)
  covs <- md$covariateSettings
  if (is.null(covs)) return(integer(0))
  ids <- c()
  for (cset in covs) {
    # getCohortCovariateData indicates a feature cohort dependency
    attr_fun <- if (is.null(cset$attr_fun)) "" else cset$attr_fun
    if (isTRUE(grepl("getCohortCovariateData", attr_fun))) {
      ids <- c(ids, unlist(cset$cohortIds))
    }
  }
  ids <- ids[!is.na(ids)]
  as.integer(unique(ids))
}

## Note: do NOT build shared resources yet; we first filter cohorts below.


# --- Helper to reference packaged models the same way as elsewhere ---
createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  return(result)
}


# --- Choose one model, one outcome, one time window ---
# Adjust these three lines if needed:
targetId <- 31                  # COVID target used in this repo
outcomeId <- 11                 # Death (11); use 13 (Critical) or 14 (Hospital) if desired
modelFolder <- "models/dato"
model <- createPackageModel(modelFolder, "PandemicPrediction")  # One parsimonious model

# Restrict cohort generation to: target, outcome, and only feature cohorts this model requires
featureCohortIds <- getFeatureCohortIds(modelFolder)
requiredCohortIds <- sort(unique(c(targetId, outcomeId, featureCohortIds)))

filterIds <- requiredCohortIds

# Be robust to differing id column names from CohortGenerator
idCol <- dplyr::case_when(
  "cohortId" %in% names(cohortDefinitionSet) ~ "cohortId",
  "cohort_id" %in% names(cohortDefinitionSet) ~ "cohort_id",
  "atlasId" %in% names(cohortDefinitionSet) ~ "atlasId",
  TRUE ~ NA_character_
)
if (is.na(idCol)) stop("Could not detect cohort id column in cohortDefinitionSet")

cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data[[idCol]] %in% filterIds)

message("Cohorts to generate (", idCol, "): ", paste(sort(unique(cohortDefinitionSet[[idCol]])), collapse = ", "))

# Now that the cohortDefinitionSet is filtered, build the CohortGenerator resources
cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)

# One time period (Q1 2020 here); change dates to your desired window
restrictSettings <- list(
  PatientLevelPrediction::createRestrictPlpDataSettings(
    studyStartDate = "20200101",
    studyEndDate   = "20200331"
  )
)


# --- Build two validation designs on identical cohorts + window ---
# 1) Original validation (no recalibration)
val_original <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL,            # use model's pop settings
  restrictPlpDataSettings = restrictSettings,
  plpModelList = list(model),
  recalibrate = NULL,
  runCovariateSummary = FALSE
)

# 2) Weak-recalibrated validation on the same time window
val_recal <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL,
  restrictPlpDataSettings = restrictSettings,
  plpModelList = list(model),
  recalibrate = "weakRecalibration",
  runCovariateSummary = FALSE
)

predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModule$createModuleSpecifications(
  validationList = list(val_original[[1]], val_recal[[1]])
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

outputJson <- "./inst/study_execution_jsons/simple_recalibration_consistency.json"
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputJson
)

message("Saved Strategus JSON to: ", normalizePath(outputJson))
