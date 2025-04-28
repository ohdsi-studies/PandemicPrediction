library(PatientLevelPrediction)
library(Strategus)
library(lubridate)

# MODEL TRANSFER Module --------------------------------------------------------

localFileSettings <- data.frame(
  locations = "./inst/newModels/"
)

modelTransferModule <- ModelTransferModule$new()

modelTransferModuleSpecs <- modelTransferModule$createModuleSpecifications(localFileSettings = localFileSettings)


# COHORT GENERATOR MODULE ------------------------------------------------------

cohortDefinitions <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

# modify the cohort
cohortGeneratorModule <- CohortGeneratorModule$new()
cohortDefShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitions)

cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(
  generateStats = TRUE
)

# UNIVERSAL ANALYSIS SETTINGS --------------------------------------------------
# Function used to generate restrictPlpDataSettings for time windows of three months
generateRestrictDataSettings <- function(start, end, interval = months(3)) {
  startDate <- lubridate::ymd(start)
  endDate <- lubridate::ymd(end)
  
  settingsList <- list()
  currentStart <- startDate
  
  while (currentStart < endDate) {
    currentEnd <- currentStart + interval - days(1)
    
    if (currentEnd > endDate) {
      currentEnd <- endDate
    }
    
    settings <- createRestrictPlpDataSettings(
      studyStartDate = format(currentStart, "%Y%m%d"),
      studyEndDate = format(currentEnd, "%Y%m%d")
    )
    
    settingsList <- append(settingsList, list(settings))
    currentStart <- currentStart + interval
  }
  
  return(settingsList)
  
}

# start only 2021 since models were developed in 2020
restrictPlpDataSettings <- generateRestrictDataSettings("2021-01-01", "2023-06-01")

outpatientVisitCovid <- 12
outpatientVisitFlu <- 30
death <- 11
severe <- 14
critical <- 13
validationComponentsList <- list(
  list(
    targetId = outpatientVisitCovid,
    outcomeId = death,
    modelTargetId = outpatientVisitFlu,
    modelOutcomeId = death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisitCovid,
    outcomeId = severe,
    modelTargetId = outpatientVisitFlu,
    modelOutcomeId = severe,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisitCovid,
    outcomeId = critical,
    modelTargetId = outpatientVisitFlu,
    modelOutcomeId = critical,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ), 
  list(
    targetId = outpatientVisitCovid,
    outcomeId = death,
    modelTargetId = outpatientVisitCovid,
    modelOutcomeId = death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisitCovid,
    outcomeId = severe,
    modelTargetId = outpatientVisitCovid,
    modelOutcomeId = severe,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisitCovid,
    outcomeId = critical,
    modelTargetId = outpatientVisitCovid,
    modelOutcomeId = critical,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ) 
)

plpValidationModule <- PatientLevelPredictionValidationModule$new()

predictionValidationModuleSpecifications <- plpValidationModule$createModuleSpecifications(
  validationComponentsList = validationComponentsList,
  logLevel = "INFO"
)

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addModuleSpecifications(modelTransferModuleSpecs) |>
  addSharedResources(cohortDefShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(predictionValidationModuleSpecifications)

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path("study_execution_jsons", "validationNewModels.json"))
