library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(PatientLevelPrediction)
library(Strategus)

# ============================================================================
# CONFIGURATION
# ============================================================================
# The definitive source of truth for this analysis
RECALIBRATION_JSON_PATH <- "./inst/study_execution_jsons/recalibration.json"
packageName <- "PandemicPrediction"

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
createPackageModel <- function(modelFolder, package) {
  result <- list(type = "package", modelFolder = modelFolder, package = package)
  class(result) <- "plpModel"
  return(result)
}

getOutcomeName <- function(outcomeId) {
  dplyr::case_when(
    as.character(outcomeId) == "11" ~ "Death",
    as.character(outcomeId) == "13" ~ "Critical",
    as.character(outcomeId) == "14" ~ "Hospital"
  )
}

getFeatureSetFromPath <- function(modelPath) {
  isDataDriven <- grepl("dataDriven", modelPath, fixed = TRUE)
  return(ifelse(isDataDriven, "Full", "Parsimonious"))
}

getRecalibrationPeriodName <- function(restrictSettings) {
  startDate <- lubridate::ymd(restrictSettings$studyStartDate)
  endDate <- lubridate::ymd(restrictSettings$studyEndDate)
  sampleSize <- restrictSettings$sampleSize
  dplyr::case_when(
    !is.null(sampleSize) & lubridate::interval(startDate, endDate) > lubridate::days(360) ~ "RecalOn_FullYear_Sampled",
    lubridate::interval(startDate, endDate) <= lubridate::days(92) ~ "RecalOn_First_3_Months",
    lubridate::interval(startDate, endDate) <= lubridate::days(184) ~ "RecalOn_First_6_Months",
    lubridate::interval(startDate, endDate) <= lubridate::days(275) ~ "RecalOn_First_9_Months",
    TRUE ~ "RecalOn_FullYear_FullPop"
  )
}

createSanityCheckDesignsFromJson <- function(recalJsonPath) {
  
  message("--- Reading definitive analysis list from: ", recalJsonPath, " ---")
  
  recalSpec <- ParallelLogger::loadSettingsFromJson(recalJsonPath)
  recalValidationList <- recalSpec$moduleSpecifications[[2]]$settings$validationList
  
  sanityCheckList <- purrr::map(recalValidationList, function(originalDesign) {
    promotedFolderName <- paste(
      getOutcomeName(originalDesign$outcomeId),
      getFeatureSetFromPath(originalDesign$plpModelList[[1]]$modelFolder),
      "OriginalInfluenza",
      getRecalibrationPeriodName(originalDesign$restrictPlpDataSettings),
      sep = "_"
    )
    
    promotedModelPath <- file.path("recalibratedModels", promotedFolderName)
    
    packageModel <- createPackageModel(
      modelFolder = promotedModelPath,
      package = packageName
    )
    
    PatientLevelPrediction::createValidationDesign(
      targetId = originalDesign$targetId,
      outcomeId = originalDesign$outcomeId,
      populationSettings = NULL,
      restrictPlpDataSettings = originalDesign$restrictPlpDataSettings, # Use the exact same settings
      plpModelList = list(packageModel),
      recalibrate = NULL, # Do not recalibrate again
      runCovariateSummary = FALSE
    )
  })
  
  return(sanityCheckList)
}

validationList <- createSanityCheckDesignsFromJson(recalJsonPath = RECALIBRATION_JSON_PATH)
cat("\n--- Successfully created", length(validationList), " sanity check validation designs IN THE CORRECT ORDER ---\n")

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)

predictionValidationModule <- Strategus::PatientLevelPredictionValidationModule$new()
predictionValidationModuleSpecifications <- predictionValidationModule$createModuleSpecifications(
  validationList = validationList,
  logLevel = "DEBUG"
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(predictionValidationModuleSpecifications)

outputFileName <- "./inst/study_execution_jsons/sanity_check_recalibration_validation.json"
ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = outputFileName
)

message("\nAnalysis specifications for sanity check successfully saved to:\n", normalizePath(outputFileName))
