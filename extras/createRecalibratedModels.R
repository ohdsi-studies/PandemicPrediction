originalModelsDir <- "./inst/models/"
recalibrationResultsDir <- "./results/recalibration/"
recalibratedModelsDir <- "./inst/recalibratedModels/"

# Maps original outcome IDs to descriptive names for folder creation
getOutcomeName <- function(outcomeId) {
  dplyr::case_when(
    as.character(outcomeId) == "11" ~ "Death",
    as.character(outcomeId) == "13" ~ "Critical",
    as.character(outcomeId) == "14" ~ "Hospital",
    TRUE ~ "UnknownOutcome"
  )
}

# Determines the feature set from a model object for folder creation
getFeatureSet <- function(plpModel) {
  covariateSettingsLength <- length(plpModel$modelDesign$covariateSettings)
  if (covariateSettingsLength == 1) {
    return("Full")
  } else if (covariateSettingsLength > 1) {
    return("Parsimonious")
  } else {
    return("UnknownFeatures")
  }
}

# Determines the recalibration period name for folder creation
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

createOriginalModelMap <- function(modelsDir) {
  message("--- Stage 1: Creating a map of original models from: ", modelsDir, " ---")

  modelFolders <- list.dirs(modelsDir, full.names = TRUE, recursive = FALSE)

  modelMap <- purrr::map_dfr(modelFolders, function(modelPath) {
    plpModel <- PatientLevelPrediction::loadPlpModel(modelPath)

    # Create the unique "fingerprint" for this model
    fingerprint <- tibble::tibble(
      modelName = attr(plpModel$modelDesign$modelSettings$param, "settings")$modelType,
      outcomeId = plpModel$modelDesign$outcomeId,
      featureSet = getFeatureSet(plpModel),
      originalModelPath = modelPath
    )
    return(fingerprint)
  })

  message("Successfully created map for ", nrow(modelMap), " original models.")
  return(modelMap)
}

promoteRecalibratedModels <- function(sourceDir, destinationDir, modelMap) {
  if (!dir.exists(destinationDir)) {
    message("\n--- Stage 2: Promoting models to: ", destinationDir, " ---")
    dir.create(destinationDir, recursive = TRUE)
  }

  runPlpPaths <- list.files(
    path = sourceDir,
    pattern = "runPlp.rds",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(runPlpPaths) == 0) {
    warning("No 'runPlp.rds' files found in: ", sourceDir)
    return(invisible(NULL))
  }

  message(sprintf("Found %d recalibration results to process.", length(runPlpPaths)))
  summaryLog <- list()

  for (runPlpPath in runPlpPaths) {
    # find path of model next to runPlp.rds
    modelFolderPath <- file.path(dirname(runPlpPath), "model")
    recalModel <- PatientLevelPrediction::loadPlpModel(modelFolderPath)
    recalModelDesign <- recalModel$modelDesign

    fingerprintToFind <- tibble::tibble(
      modelName = attr(recalModelDesign$modelSettings$param, "settings")$modelType,
      outcomeId = recalModelDesign$outcomeId,
      featureSet = getFeatureSet(recalModel)
    )

    matchedModel <- dplyr::inner_join(fingerprintToFind, modelMap, by = c("modelName", "outcomeId", "featureSet"))

    if (nrow(matchedModel) == 0) {
      warning("Could not find a matching original model for recalibration result: ", runPlpPath)
      next
    }

    originalModel <- PatientLevelPrediction::loadPlpModel(matchedModel$originalModelPath)
    runPlpObject <- readRDS(runPlpPath)
    recalCoefficients <- attr(runPlpObject$prediction, "metaData")$weakRecalibration

    if (is.null(recalCoefficients) || is.null(originalModel$model$coefficients)) {
      warning("Skipping file: Recalibration coefficients or original coefficients not found for ", runPlpPath)
      next
    }

    originalCoefficients <- originalModel$model$coefficients$coefficient
    originalIntercept <- originalModel$model$coefficients$intercept
    newCoefficients <- c(
      recalCoefficients$adjustIntercept + (recalCoefficients$adjustGradient * originalIntercept),
      recalCoefficients$adjustGradient * originalCoefficients
    )

    promotedModel <- originalModel
    promotedModel$model$coefficients$coefficient <- newCoefficients
    promotedModel$model$intercept <- newCoefficients[1]
    promotedModel$trainDetails$RECALIBRATION_NOTE <- paste(
      "Recalibrated on",
      getRecalibrationPeriodName(recalModel$validationDetails$restrictPlpDataSettings)
    )

    outcomeName <- getOutcomeName(promotedModel$modelDesign$outcomeId)
    recalPeriodName <- getRecalibrationPeriodName(recalModel$validationDetails$restrictPlpDataSettings)

    newFolderName <- paste(
      outcomeName,
      matchedModel$featureSet,
      "OriginalInfluenza",
      recalPeriodName,
      sep = "_"
    )

    newModelPath <- file.path(destinationDir, newFolderName)
    message(sprintf("Saving recalibrated model to: %s", newModelPath))
    PatientLevelPrediction::savePlpModel(plpModel = promotedModel, dirPath = newModelPath)

    summaryLog[[basename(dirname(dirname(runPlpPath)))]] <- newFolderName
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

# Clear out the old directory for a clean run
if (dir.exists(recalibratedModelsDir)) {
  message("Clearing out existing recalibrated models directory...")
  unlink(recalibratedModelsDir, recursive = TRUE)
}

originalModelMap <- createOriginalModelMap(modelsDir = originalModelsDir)
print(originalModelMap)

promoteRecalibratedModels(
  sourceDir = recalibrationResultsDir,
  destinationDir = recalibratedModelsDir,
  modelMap = originalModelMap
)
