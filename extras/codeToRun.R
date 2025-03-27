library(Strategus)
library(PandemicPrediction)
library(dplyr)

analysisSpecifications <- PandemicPrediction::loadStudySpec(
<<<<<<< HEAD:extras/codeToRun.R
  type = 'all_validation_new.json'
  )
||||||| parent of 84acd83 (stuff):codeToRun.R
=======
  type = "development.json"
)
>>>>>>> 84acd83 (stuff):codeToRun.R

# Inputs to run (edit these for your CDM):
# ========================================= #
# If your database requires temp tables being created in a specific schema
if (!Sys.getenv("DATABASE_TEMP_SCHEMA") == "") {
  options(sqlRenderTempEmulationSchema = Sys.getenv("DATABASE_TEMP_SCHEMA"))
}

# where to save the output - a directory in your environment
outputFolder <- "/results/development-new-strategus/"

# fill in your connection details and path to driver
# see ?DatabaseConnector::createConnectionDetails for help for your
# database platform
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = Sys.getenv("DATABASE_SERVER"),
  user = Sys.getenv("DATABASE_USER"),
  password = Sys.getenv("DATABASE_PASSWORD"),
  port = Sys.getenv("DATABASE_PORT"),
  connectionString = Sys.getenv("DATABASE_CONNECTION_STRING"),
  pathToDriver = Sys.getenv("DATABASE_DRIVER")
)

# A schema with write access to store cohort tables
workDatabaseSchema <- Sys.getenv("WORK_SCHEMA")

# name of cohort table that will be created for study
cohortTable <- Sys.getenv("COHORT_TABLE")

# schema where the cdm data is
cdmDatabaseSchema <- Sys.getenv("CDM_SCHEMA")

# Aggregated statistics with cell count less than this are removed before sharing results.
minCellCount <- 5

# =========== END OF INPUTS ========== #

Strategus::storeConnectionDetails(
  connectionDetails = connectionDetails,
  connectionDetailsReference = connectionDetailsReference
)

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTable),
  workFolder = file.path(outputFolder, "strategusWork"),
  resultsFolder = file.path(outputFolder, "strategusOutput"),
  minCellCount = minCellCount
)

<<<<<<< HEAD:extras/codeToRun.R
json <- paste(readLines("./study_execution_jsons/outpatient_critical_simple_validation.json"), collapse = "\n")
analysisSpecifications <- ParallelLogger::convertJsonToSettings(json)

||||||| parent of 84acd83 (stuff):codeToRun.R
  
=======
>>>>>>> 84acd83 (stuff):codeToRun.R
Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)
