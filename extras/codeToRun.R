library(Strategus)
library(PandemicPrediction)

analysisSpecifications <- PandemicPrediction::loadStudySpec(
  type = 'all_validation_new.json'
  )

# Inputs to run (edit these for your CDM):
# ========================================= #
if (!Sys.getenv("DATABASE_TEMP_SCHEMA") == "") {
  options(sqlRenderTempEmulationSchema = Sys.getenv("DATABASE_TEMP_SCHEMA"))
}

database <- "databaseName"

# reference for the connection used by Strategus
connectionDetailsReference <- paste0("PandemicPrediction", database)

# where to save the output - a directory in your environment
outputFolder <- "/output/folder/"

# fill in your connection details and path to driver
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


# Location to Strategus modules
# If you've ran Strategus studies before this directory should already exist.
# Note: this environmental variable should be set once for each compute node
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = "/path/to/strategus/modules/")


# =========== END OF INPUTS ========== #

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTable),
  workFolder = file.path(outputFolder, "strategusWork"),
  resultsFolder = file.path(outputFolder, "strategusOutput"),
  minCellCount = minCellCount
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)
