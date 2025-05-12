library(PatientLevelPrediction)
library(Strategus)


cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)
cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data$cohortId != 30) # only used for development

# add cohortGenerator
cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)


# UNIVERSAL ANALYSIS SETTINGS --------------------------------------------------
generateRestrictDataSettings <- function(start, end, interval = months(3)) {
  start_date <- lubridate::ymd(start)
  end_date <- lubridate::ymd(end)

  settings_list <- list()
  current_start <- start_date

  while (current_start < end_date) {
    current_end <- current_start + interval - lubridate::days(1)

    if (current_end > end_date) {
      current_end <- end_date
    }

    settings <- createRestrictPlpDataSettings(
      studyStartDate = format(current_start, "%Y%m%d"),
      studyEndDate = format(current_end, "%Y%m%d")
    )

    settings_list <- append(settings_list, list(settings))
    current_start <- current_start + interval
  }

  return(settings_list)
}

restrictPlpDataSettings <- generateRestrictDataSettings("2020-01-01", "2023-06-01")

# maybe move this into PLP?
createPackageModel <- function(modelFolder, package) {
  result <- list(
    type = "package",
    modelFolder = modelFolder,
    package = package
  )
  class(result) <- "plpModel"

  return(result)
}
validationList <- list()

validationList[[1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 31, # covid
  outcomeId = 11, # death. - 14 severe, 13 critial
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = "models/dato",
      package = "PandemicPrediction"
    ),
    createPackageModel(
      modelFolder = "models/dataDrivenF",
      package = "PandemicPrediction"
    )
  ), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[2]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 31, # covid
  outcomeId = 14, # 11 death. - 14 severe, 13 critial
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = "models/sato",
      package = "PandemicPrediction"
    ),
    createPackageModel(
      modelFolder = "models/dataDrivenH",
      package = "PandemicPrediction"
    )
  ),
  recalibrate = "weakRecalibration"
)
validationList[[3]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 31, # covid
  outcomeId = 13, # 11 death. - 14 severe, 13 critial
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = "models/cato",
      package = "PandemicPrediction"
    ),
    createPackageModel(
      modelFolder = "models/dataDrivenI",
      package = "PandemicPrediction"
    )
  ),
  recalibrate = "weakRecalibration"
)

allValList <- do.call("c", validationList)

predictionValidationModuleSpecifications <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModuleSpecifications$createModuleSpecifications(
  validationList = allValList
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "./inst/study_execution_jsons/cover_validation.json"
)
