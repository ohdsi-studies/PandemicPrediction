library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(PatientLevelPrediction)
library(Strategus)

newModelsDir <- "./inst/newModels/"
packageName <- "PandemicPrediction"
overallEndDate <- "2023-06-01"

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

discoverAndParseModels <- function(modelsDir) {
  modelFolders <- list.dirs(modelsDir, full.names = TRUE, recursive = FALSE)
  if (length(modelFolders) == 0) {
    warning("No model folders found in: ", modelsDir)
    return(tibble::tibble())
  }

  message("Found ", length(modelFolders), " models to process for validation design.")

  parsedModels <- purrr::map_dfr(modelFolders, function(folderPath) {
    folderName <- basename(folderPath)

    regexPattern <- "^([^_]+)_([^_]+)_([^_]+_)?(\\d{8})_to_(\\d{8})$"
    matches <- stringr::str_match(folderName, regexPattern)

    if (is.na(matches[1, 1])) {
      warning("Folder name does not match expected format and will be skipped: ", folderName)
      return(NULL)
    }

    outcomeName <- matches[1, 2]
    featureSet <- matches[1, 3]
    devStartDate <- lubridate::ymd(matches[1, 5])
    devEndDate <- lubridate::ymd(matches[1, 6])

    tibble::tibble(
      path = folderPath,
      outcomeName = outcomeName,
      featureSet = featureSet,
      devStartDate = devStartDate,
      devEndDate = devEndDate,
      targetId = getTargetId(outcomeName),
      outcomeId = getOutcomeId(outcomeName)
    )
  })

  if (any(is.na(parsedModels$outcomeId))) {
    warning("Some models had outcome names that could not be mapped to an ID and will be excluded.")
    parsedModels <- parsedModels |> dplyr::filter(!is.na(.data$outcomeId))
  }

  return(parsedModels)
}

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
  validationDesigns <- purrr::pmap(modelDetails, function(path, outcomeId, targetId, devEndDate, ...) {
    validationStartDate <- devEndDate + lubridate::days(1)

    restrictPlpDataSettingsList <- setDynamicRestrictSettings(
      validationStartDate = validationStartDate,
      overallEndDate = overallEndDate
    )

    if (length(restrictPlpDataSettingsList) == 0) {
      return(NULL)
    }

    relativeModelPath <- stringr::str_remove(path, "^./inst/")
    packageModel <- createPackageModel(
      modelFolder = relativeModelPath,
      package = packageName
    )

    PatientLevelPrediction::createValidationDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      populationSettings = NULL,
      restrictPlpDataSettings = restrictPlpDataSettingsList,
      plpModelList = list(packageModel),
      recalibrate = "weakRecalibration",
      runCovariateSummary = TRUE
    )
  })

  validationDesignsFlat <- purrr::list_flatten(validationDesigns)

  return(purrr::compact(validationDesignsFlat))
}

newModelDetails <- discoverAndParseModels(modelsDir = newModelsDir)
cat("\n--- Discovered and Parsed Model Details ---\n")
print(newModelDetails, n = Inf)

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
  dplyr::filter(.data$cohortId != 30)

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

outputFileName <- "./inst/study_execution_jsons/new_models_validation.json"
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputFileName
)

message("\nAnalysis specifications for all new models successfully saved to:\n", normalizePath(outputFileName))
