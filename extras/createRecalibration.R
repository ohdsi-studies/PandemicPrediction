library(PatientLevelPrediction)
library(Strategus)
library(dplyr)
library(purrr)

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <-
  cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <-
  cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)

createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  return(result)
}

originalInfluenzaModels <- list(
  list(outcomeId = 11, model = createPackageModel("models/dato", "PandemicPrediction")),
  list(outcomeId = 11, model = createPackageModel("models/dataDrivenF", "PandemicPrediction")),
  list(outcomeId = 14, model = createPackageModel("models/sato", "PandemicPrediction")),
  list(outcomeId = 14, model = createPackageModel("models/dataDrivenH", "PandemicPrediction")),
  list(outcomeId = 13, model = createPackageModel("models/cato", "PandemicPrediction")),
  list(outcomeId = 13, model = createPackageModel("models/dataDrivenI", "PandemicPrediction"))
)

recalibrationPeriods <- list(
  createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200331"),
  createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200630"),
  createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20200930"),
  createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20201231"),
  createRestrictPlpDataSettings(studyStartDate = "20200101", studyEndDate = "20201231", sampleSize = 150000)
)


analysisParams <- tidyr::expand_grid(
  influenzaModel = originalInfluenzaModels,
  recalPeriod = recalibrationPeriods
)

recalibrationDesignList <- purrr::pmap(analysisParams, function(influenzaModel, recalPeriod) {
  PatientLevelPrediction::createValidationDesign(
    targetId = 31,
    outcomeId = influenzaModel$outcomeId,
    populationSettings = NULL,
    restrictPlpDataSettings = recalPeriod, 
    plpModelList = list(influenzaModel$model),
    recalibrate = "weakRecalibration",
    runCovariateSummary = FALSE 
  )
})
message(sprintf("Generated %d recalibration tasks.", length(recalibrationDesignList)))


predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModule$createModuleSpecifications(
  validationList = recalibrationDesignList
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "./inst/study_execution_jsons/recalibration.json"
)

message("Recalibration analysis specification saved to ./inst/study_execution_jsons/recalibration.json")
