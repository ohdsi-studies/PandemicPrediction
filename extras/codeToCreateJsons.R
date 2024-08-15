library(PatientLevelPrediction)
library(Strategus)
library(lubridate)


# MODEL TRANSFER Module --------------------------------------------------------
source('https://raw.githubusercontent.com/OHDSI/ModelTransferModule/strategus_v1/Main.R')

githubSettings <- data.frame(user = "ohdsi-studies",
                             repository = "PandemicPrediction",
                             ref = "master",
                             modelsFolder = "models")

modelTransferCreator <- ModelTransferModule$new()
modelTransferModuleSpecifications <- modelTransferCreator$createModuleSpecifications(
  settings = githubSettings
)

# COHORT GENERATOR MODULE ------------------------------------------------------

# atlas Id's from my personal atlas ~ Egill
cohortIds <- list(outpatientVisit =  12,
                  inPatientVisit = 25,
                  death = 11,
                  severe = 14,
                  critical = 13,
                  cancer = 15,
                  copd = 16,
                  diabetes = 17,
                  heartDisease = 18,
                  hyperlipidemia = 19,
                  hypertension = 20,
                  kidneyDisease  = 21,
                  ulcer = 22,
                  hepaticFailure = 23,
                  respFailure = 24)

baseUrl <- keyring::key_get('webapi', 'baseurl')
ROhdsiWebApi::authorizeWebApi(
  baseUrl = baseUrl,
  authMethod = 'db',
  webApiUsername = keyring::key_get('webapi', 'username'),
  webApiPassword = keyring::key_get('webapi', 'password')
)

cohortDefinitions <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = unlist(cohortIds),
  generateStats = TRUE
)

cgModuleCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleCreator$createCohortSharedResourceSpecifications(cohortDefinitions)

cohortGeneratorModuleSpecifications <- cgModuleCreator$createModuleSpecifications(
  incremental = TRUE,
  generateStats = TRUE
)

# UNIVERSAL ANALYSIS SETTINGS --------------------------------------------------
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

source('https://raw.githubusercontent.com/OHDSI/PatientLevelPredictionValidationModule/strategus_v1/Main.R')

validationComponentsList <- list(
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$death,
    restrictPlpDataSettings = restrictPlpDataSettings, # vector
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  ),
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$severe,
    restrictPlpDataSettings = restrictPlpDataSettings, # vector
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  ),
  list(
    targetId = cohortIds$outpatientVisit,
    outcomeId = cohortIds$critical,
    restrictPlpDataSettings = restrictPlpDataSettings, # vector
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  ), 
  list(
    targetId = cohortIds$inPatientVisit,
    outcomeId = cohortIds$death,
    restrictPlpDataSettings = restrictPlpDataSettings, # vector
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  ),
  list(
    targetId = cohortIds$inPatientVisit,
    outcomeId = cohortIds$critical,
    restrictPlpDataSettings = restrictPlpDataSettings, # vector
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    )
  )
)

validationSettingsCreator <- PatientLevelPredictionValidationModule$new()

predictionValidationModuleSpecifications <- validationSettingsCreator$createModuleSpecifications(
  settings = validationComponentsList
)

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortDefinitionShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(modelTransferModuleSpecifications) |>
  addModuleSpecifications(predictionValidationModuleSpecifications)

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path('study_execution_jsons', 'validation.json'))
