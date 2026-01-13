# Create a Strategus validation spec that applies quarter-shifted (Q-1 -> Q)
# recalibrated proxy models produced by extras/createRollingRecalibratedModels.R.

library(dplyr)
library(purrr)
library(PatientLevelPrediction)
library(Strategus)

manifestPath <- "inst/rollingRecalibratedModels/manifest.csv"
outputFileName <- "inst/study_execution_jsons/rolling_recalibrated_validation.json"
packageName <- "PandemicPrediction"

getOutcomeId <- function(outcomeName) {
  switch(
    tolower(outcomeName),
    "fatality" = 11L,
    "critical" = 13L,
    "respiratoryfailure" = 13L,
    "hospitalization" = 14L,
    stop("Unknown outcomeName: ", outcomeName)
  )
}

parseQuarterId <- function(quarterId) {
  parts <- strsplit(as.character(quarterId), "_", fixed = TRUE)[[1]]
  if (length(parts) < 3) {
    stop("Invalid quarterId: ", quarterId)
  }
  list(
    start = parts[1],
    end = parts[2],
    outcome = parts[3]
  )
}

createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  result
}

if (!file.exists(manifestPath)) {
  stop(
    "Manifest not found at ",
    manifestPath,
    ". Run extras/createRollingRecalibratedModels.R first."
  )
}
man <- utils::read.csv(manifestPath, stringsAsFactors = FALSE)
if (!nrow(man)) {
  stop("Manifest is empty.")
}

# Cohorts shared resources
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)
cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data$cohortId != 30) # dev-only cohort
cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(
  generateStats = TRUE
)

# Build validation designs: one per manifest row
validationList <- purrr::pmap(
  man,
  function(
    outcomeName,
    featureSet,
    modelKey,
    applyQuarter,
    recalibratedOn,
    dest,
    ...
  ) {
    q <- parseQuarterId(applyQuarter)
    outcomeId <- getOutcomeId(outcomeName)
    targetId <- 31L
    restrictSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
      studyStartDate = q$start,
      studyEndDate = q$end
    )
    relFolder <- dest
    relFolder <- sub("^\\./", "", relFolder)
    relFolder <- sub("^inst/", "", relFolder) # inst/ is stripped on install
    pkgModel <- createPackageModel(
      modelFolder = relFolder,
      package = packageName
    )
    PatientLevelPrediction::createValidationDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      populationSettings = NULL,
      restrictPlpDataSettings = restrictSettings,
      plpModelList = list(pkgModel),
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  }
)

predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModule$createModuleSpecifications(
  validationList = validationList
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

dir.create(dirname(outputFileName), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputFileName
)
message(
  "\nRolling recalibrated validation spec saved to:\n",
  normalizePath(outputFileName)
)
