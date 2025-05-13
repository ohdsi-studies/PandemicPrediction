library(PatientLevelPrediction)
library(Strategus)

# should be influenza only in 2019, this should get me as similar results for 
# internal validation as the original paper

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

# filter to flu
cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(.data$cohortId != 31, .data$cohortId != 12) # only used for development

# add cohortGenerator
cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)


# UNIVERSAL ANALYSIS SETTINGS --------------------------------------------------
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

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  studyStartDate = "20190101",
  studyEndDate = "20191231"
)
validationList[[1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 30, # influenza
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
  targetId = 30, # influenza
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
  targetId = 30, # influenza
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

allValList <- do.call(
  c,
  lapply(validationList, function(tmp) {
    if (inherits(tmp, "validationDesign")) {
      list(tmp)
    } else {
      tmp
    }
  })
)


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
  fileName = "./inst/study_execution_jsons/influenza2019_validation.json"
)
