library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(PatientLevelPrediction)
library(Strategus)

recalibratedModelsDir <- "./inst/recalibratedModels/"
packageName <- "PandemicPrediction"

createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  return(result)
}

getOutcomeId <- function(outcomeName) {
  switch(outcomeName,
    "Death" = 11,
    "Critical" = 13,
    "Hospital" = 14
  )
}

getTargetId <- function(outcomeName) {
  return(31)
}

getRecalibrationPeriodSettings <- function(recalPeriodName) {
  settings <- dplyr::case_when(
    recalPeriodName == "RecalOn_First_3_Months" ~
      list(PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200331")),
    recalPeriodName == "RecalOn_First_6_Months" ~
      list(PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200630")),
    recalPeriodName == "RecalOn_First_9_Months" ~
      list(PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200930")),
    recalPeriodName == "RecalOn_FullYear_FullPop" ~
      list(PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20201231")),
    recalPeriodName == "RecalOn_FullYear_Sampled" ~
      list(PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20201231", sampleSize = 150000)),
    TRUE ~ list(NULL)
  )
  return(settings[[1]]) 
}

discoverAndParseRecalibratedModels <- function(modelsDir) {
  modelFolders <- list.dirs(modelsDir, full.names = TRUE, recursive = FALSE)
  if (length(modelFolders) == 0) {
    warning("No model folders found in: ", modelsDir)
    return(tibble::tibble())
  }

  message("Found ", length(modelFolders), " recalibrated models to process.")

  parsedModels <- purrr::map_dfr(modelFolders, function(folderPath) {
    folderName <- basename(folderPath)
    regexPattern <- "^([^_]+)_([^_]+)_(OriginalInfluenza)_(RecalOn_.*)$"
    matches <- stringr::str_match(folderName, regexPattern)

    if (is.na(matches[1, 1])) {
      warning("Folder name does not match pattern and will be skipped: ", folderName)
      return(NULL)
    }

    outcomeName <- matches[1, 2]
    featureSet <- matches[1, 3]
    recalPeriodName <- matches[1, 5]

    recalSettings <- getRecalibrationPeriodSettings(recalPeriodName)

    if (is.null(recalSettings)) {
      warning("Could not map recal period name: ", recalPeriodName)
      return(NULL)
    }

    tibble::tibble(
      path = folderPath,
      outcomeName = outcomeName,
      featureSet = featureSet,
      recalSettings = list(recalSettings), 
      targetId = getTargetId(outcomeName),
      outcomeId = getOutcomeId(outcomeName)
    )
  })
  return(parsedModels)
}

createSanityCheckDesigns <- function(modelDetails) {
  validationDesigns <- purrr::pmap(modelDetails, function(path, outcomeId, targetId, recalSettings, ...) {
    relativeModelPath <- stringr::str_remove(path, "^./inst/")
    packageModel <- createPackageModel(
      modelFolder = relativeModelPath,
      package = packageName
    )

    PatientLevelPrediction::createValidationDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      populationSettings = NULL,
      restrictPlpDataSettings = recalSettings,
      plpModelList = list(packageModel),
      recalibrate = NULL, 
      runCovariateSummary = FALSE
    )
  })
  return(validationDesigns)
}

recalibratedModelDetails <- discoverAndParseRecalibratedModels(modelsDir = recalibratedModelsDir)
cat("\n--- Discovered and Parsed Recalibrated Model Details ---\n")
print(recalibratedModelDetails)

validationList <- createSanityCheckDesigns(modelDetails = recalibratedModelDetails)
cat("\n--- Successfully created", length(validationList), " sanity check validation designs ---\n")

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)

predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModule$createModuleSpecifications(
  validationList = validationList
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

outputFileName <- "./inst/study_execution_jsons/sanity_check_recalibration_validation.json"
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputFileName
)

message("\nAnalysis specifications for sanity check successfully saved to:\n", normalizePath(outputFileName))
