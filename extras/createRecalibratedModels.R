recalibrationJsonPath <- "./inst/study_execution_jsons/recalibration.json"
recalibrationResultsDir <- "./results/recalibration/"
recalibratedModelsDir <- "./inst/recalibratedModels/"

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

createMasterMapFromJson <- function(jsonPath) {
  message("--- Creating a master map from the definitive source: ", jsonPath, " ---")
  analysisSpec <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  validationList <- analysisSpec$moduleSpecifications[[2]]$settings$validationList
  
  masterMap <- purrr::imap_dfr(validationList, function(design, index) {
    modelInfo <- design$plpModelList[[1]]
    
    tibble::tibble(
      analysisId = index,
      originalModelPath = file.path("./inst", modelInfo$modelFolder),
      outcomeId = design$outcomeId,
      recalPeriodSettings = list(design$restrictPlpDataSettings)
    )
  })
  
  message("Successfully created definitive map for ", nrow(masterMap), " analysis tasks.")
  return(masterMap)
}

promoteRecalibratedModels <- function(destinationDir, masterMap) {
  if (!dir.exists(destinationDir)) {
    message("\n--- Promoting models to: ", destinationDir, " ---")
    dir.create(destinationDir, recursive = TRUE)
  }

  summaryLog <- list()

  for (i in 1:nrow(masterMap)) {
    mapRow <- masterMap[i, ]
    analysisFolderName <- paste0("Analysis_", mapRow$analysisId)
    runPlpPath <- file.path(recalibrationResultsDir, analysisFolderName, "validationResult", "runPlp.rds")
    
    if (!file.exists(runPlpPath)) {
      warning("Skipping Analysis ", mapRow$analysisId, ": runPlp.rds not found.")
      next
    }
    
    originalModel <- PatientLevelPrediction::loadPlpModel(mapRow$originalModelPath)
    runPlpObject <- readRDS(runPlpPath)
    recalCoefficients <- attr(runPlpObject$prediction, "metaData")$weakRecalibration
    
    if (is.null(recalCoefficients)) {
      warning("Skipping Analysis ", mapRow$analysisId, ": Missing recalibration coefficients.")
      next
    }
    
    promotedModel <- originalModel
    
    promotedModel$predictionFunction <- attr(originalModel, "predictionFunction")
    
    promotedModel$recalibration <- list(
      coefficients = recalCoefficients,
      recalibratedOn = getRecalibrationPeriodName(mapRow$recalPeriodSettings[[1]])
    )
    
    attr(promotedModel, "predictionFunction") <- "PandemicPrediction::predictWithRecalibration"
    
    outcomeName <- getOutcomeName(mapRow$outcomeId)
    featureSet <- getFeatureSetFromPath(mapRow$originalModelPath)
    recalPeriodName <- getRecalibrationPeriodName(mapRow$recalPeriodSettings[[1]])
    
    newFolderName <- paste(
      outcomeName, featureSet, "OriginalInfluenza", recalPeriodName, sep = "_"
    )
    
    newModelPath <- file.path(destinationDir, newFolderName)
    message(sprintf("Saving recalibrated model from Analysis_%d to: %s", mapRow$analysisId, newModelPath))
    
    PatientLevelPrediction::savePlpModel(plpModel = promotedModel, dirPath = newModelPath)

    summaryLog[[analysisFolderName]] <- newFolderName
  }
  
  cat("\n====================================================\n")
  cat("Processing Complete. Summary:\n")
  cat("====================================================\n\n")
  
  if (length(summaryLog) > 0) {
    for (originalName in names(summaryLog)) {
      cat(sprintf("Processed '%s' -> '%s'\n", originalName, summaryLog[[originalName]]))
    }
    cat(sprintf("\nSuccessfully promoted %d models.\n", length(summaryLog)))
  } else {
    cat("No models were processed.\n")
  }
  
  return(invisible(NULL))
}

if (dir.exists(recalibratedModelsDir)) {
  message("Clearing out existing recalibrated models directory...")
  unlink(recalibratedModelsDir, recursive = TRUE)
}

masterMap <- createMasterMapFromJson(jsonPath = recalibrationJsonPath)

promoteRecalibratedModels(
  destinationDir = recalibratedModelsDir,
  masterMap = masterMap
)
