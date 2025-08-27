library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

newModelsDir <- "./inst/newModels/"
package <- "PandemicPrediction"

createPackageModel <- function(modelFolder, package) {
  result <- list(
    type = "package",
    modelFolder = modelFolder,
    package = package
  )
  class(result) <- "plpModel"

  return(result)
}

getOutcomeId <- function(outcomeName) {
  switch(outcomeName,
    "Death" = 11,
    "Critical" = 13,
    "Hospital" = 14,
    NA
  )
}

getTargetId <- function(outcomeName) {
  return(31)
}
# Scans the models directory and parses folder names to extract metadata.
discoverAndParseModels <- function(modelsDir) {
  modelFolder <- list.dirs(modelsDir, full.names = TRUE, recursive = FALSE)
  if (length(modelFolder) == 0) {
    warning("No model folders found in: ", modelsDir)
    return(tibble())
  }
  message("Found ", length(modelFolder), " models to process for validation design.")
  parsedModels <- map_dfr(modelFolder, function(folderPath) {
    folderName <- basename(folderPath)
    matches <- str_match(folderName, "^([^_]+)_([^_]+)_(\\d{8})_to_(\\d{8})$")
    if (is.na(matches[1, 1])) {
      warning("Folder name does not match expected format and will be skipped: ", folderName)
      return(NULL)
    }
    tibble(
      path = folderPath,
      outcomeName = matches[1, 2],
      featureSet = matches[1, 3],
      devStartDate = ymd(matches[1, 4]),
      devEndDate = ymd(matches[1, 5]),
      targetId = getTargetId(matches[1, 2]),
      outcomeId = getOutcomeId(matches[1, 2])
    )
  })
  if (any(is.na(parsedModels$outcomeId))) {
    warning("Some models had outcome names that could not be mapped to an ID and will be excluded.")
    parsedModels <- parsedModels %>% filter(!is.na(.data$outcomeId))
  }
  return(parsedModels)
}


newModelDetails <- discoverAndParseModels(modelsDir = newModelsDir)
cat("\n--- Discovered and Parsed Model Details ---\n")
print(newModelDetails)
cat("\n")

overallEndDate <- "2023-06-01"

# Generates a list of rolling 3-month RestrictPlpDataSettings objects
setDynamicRestrictSettings <- function(validationStartDate, overallEndDate, interval = months(3)) {
  startDate <- ymd(validationStartDate)
  endDate <- ymd(overallEndDate)
  if (startDate >= endDate) {
    return(list())
  }
  settingsList <- list()
  currentStart <- startDate
  while (currentStart < endDate) {
    currentEnd <- currentStart + interval - days(1)

    if (currentEnd > endDate) {
      currentEnd <- endDate
    }
    settings <- PatientLevelPrediction::createRestrictPlpDataSettings(
      studyStartDate = format(currentStart, "%Y%m%d"),
      studyEndDate = format(currentEnd, "%Y%m%d")
    )

    settingsList <- append(settingsList, list(settings))
    currentStart <- currentStart + interval
  }
  return(settingsList)
}

createValidationDesigns <- function(modelDetails) {
  validationDesigns <- pmap(modelDetails, function(path, outcomeId, targetId, devEndDate, ...) {
    validationStartDate <- devEndDate + days(1)
    message(sprintf("Creating validation design for model '%s', starting %s", basename(path), validationStartDate))
    restrictPlpDataSettingsList <- setDynamicRestrictSettings(
      validationStartDate = validationStartDate,
      overallEndDate = overallEndDate
    )
    if (length(restrictPlpDataSettingsList) == 0) {
      warning("No validation windows for model ", basename(path), ". It will not be included in the analysis.")
      return(NULL)
    }
    relativeModelPath <- stringr::str_remove(path, "^./inst/")
    packageModel <- createPackageModel(
      modelFolder = relativeModelPath,
      package = package
    )
    validationDesign <- PatientLevelPrediction::createValidationDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      populationSettings = NULL, # Use settings from the trained model
      restrictPlpDataSettings = restrictPlpDataSettingsList,
      plpModelList = list(packageModel),
      recalibrate = "weakRecalibration",
      runCovariateSummary = TRUE
    )
    return(validationDesign)
  })
  validationDesigns <- purrr::list_flatten(validationDesigns)

  validationDesigns <- compact(validationDesigns)
  return(validationDesigns)
}

# --- Execute the validation design creation ---
validationList <- createValidationDesigns(modelDetails = newModelDetails)
cat("\n--- Successfully created", length(validationList), "validation designs ---\n")

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)
cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data$cohortId != 30) # only used for development

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

# --- Save the final JSON object ---
outputFileName <- "./inst/study_execution_jsons/new_models_validation.json"
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputFileName
)

message("\nAnalysis specifications for new models successfully saved to:\n", normalizePath(outputFileName))
