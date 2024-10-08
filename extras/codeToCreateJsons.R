library(PatientLevelPrediction)
library(Strategus)
library(lubridate)

# MODEL TRANSFER Module --------------------------------------------------------

githubSettings <- data.frame(user = "ohdsi-studies",
                             repository = "PandemicPrediction",
                             ref = "strategus_v1",
                             modelsFolder = "models")

modelTransferModule <- ModelTransferModule$new()

modelTransferModuleSpecs <- modelTransferModule$createModuleSpecifications(githubSettings = githubSettings)


# COHORT GENERATOR MODULE ------------------------------------------------------

cohortDefinitions <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = 'inst/Cohorts.csv',
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
  start_date <- lubridate::ymd(start)
  end_date <- lubridate::ymd(end)
  
  settings_list <- list()
  current_start <- start_date
  
  while (current_start < end_date) {
    current_end <- current_start + interval - days(1)
    
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

restrictPlpDataSettings <- generateRestrictDataSettings('2020-01-01', '2023-06-01')


validationComponentsList <- list(
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$death,
    modelTargetId = cohortIds$outpatientVisit,
    modelOutcomeId = cohortIds$death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$severe,
    modelTargetId = cohortIds$outpatientVisit,
    modelOutcomeId = cohortIds$severe,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$critical,
    modelTargetId = cohortIds$outpatientVisit,
    modelOutcomeId = cohortIds$critical,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL,
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ), 
  list(
    targetId = cohortIds$inPatientVisit,
    outcomeId = cohortIds$death,
    modelTargetId = cohortIds$inPatientVisit,
    modelOutcomeId = cohortIds$death,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL, 
    recalibrate = NULL,
    runCovariateSummary = TRUE
  ),
  list(
    targetId = cohortIds$inPatientVisit,
    outcomeId = cohortIds$critical,
    modelTargetId = cohortIds$inPatientVisit,
    modelOutcomeId = cohortIds$critical,
    restrictPlpDataSettings = restrictPlpDataSettings,
    populationSettings = NULL, 
    recalibrate = NULL,
    runCovariateSummary = TRUE
  )
)

plpValidationModule <- PatientLevelPredictionValidationModule$new()

predictionValidationModuleSpecifications <- plpValidationModule$createModuleSpecifications(
  validationComponentsList = validationComponentsList
)

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addModuleSpecifications(modelTransferModuleSpecs) |>
  addSharedResources(cohortDefShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(predictionValidationModuleSpecifications)

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path('study_execution_jsons', 'validation.json'))
