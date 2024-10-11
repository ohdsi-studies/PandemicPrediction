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

baseUrl <- Sys.getenv("WEBAPI_URL")
ROhdsiWebApi::authorizeWebApi(
  baseUrl = baseUrl,
  authMethod = "db",
  webApiUsername = Sys.getenv("WEBAPI_USER"),
  webApiPassword = Sys.getenv("WEBAPI_PASSWORD")
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
