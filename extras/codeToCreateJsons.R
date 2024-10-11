library(PatientLevelPrediction)
library(Strategus)
library(lubridate)

# MODEL TRANSFER Module --------------------------------------------------------

localFileSettings <- data.frame(
  locations = "./inst/models/"
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

restrictPlpDataSettings <- generateRestrictDataSettings("2020-01-01", "2023-06-01")

outpatientVisit <- 12
death <- 11
severe <- 14
critical <- 13
inPatientVisit <- 25
validationComponentsList <- list(
  list(
    targetId = outpatientVisit,
    outcomeId = death,
    modelTargetId = outpatientVisit,
    modelOutcomeId = death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisit,
    outcomeId = severe,
    modelTargetId = outpatientVisit,
    modelOutcomeId = severe,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = outpatientVisit,
    outcomeId = critical,
    modelTargetId = outpatientVisit,
    modelOutcomeId = critical,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ), 
  list(
    targetId = inPatientVisit,
    outcomeId = death,
    modelTargetId = inPatientVisit,
    modelOutcomeId = death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL, 
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = inPatientVisit,
    outcomeId = critical,
    modelTargetId = inPatientVisit,
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

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path("study_execution_jsons", "validation.json"))
