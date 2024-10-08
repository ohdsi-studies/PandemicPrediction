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

CohortGenerator::saveCohortDefinitionSet(
  cohortDefinitionSet = cohortDefinitions,
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
  )
