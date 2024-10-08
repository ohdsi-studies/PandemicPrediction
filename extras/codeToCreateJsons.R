library(PatientLevelPrediction)
library(Strategus)
library(lubridate)

# MODEL TRANSFER Module --------------------------------------------------------

githubSettings <- data.frame(user = "ohdsi-studies",
                             repository = "PandemicPrediction",
                             ref = "master",
                             modelsFolder = "models")

modelTransferModule <- ModelTransferModule$new()

modelTransferModuleSpecs <- modelTransferModule$createModuleSpecifications(githubSettings = githubSettings)


# COHORT GENERATOR MODULE ------------------------------------------------------

# atlas Id's from my personal atlas ~ Egill
# maybe better for reproducibility to have the jsons in the repo and use them
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
