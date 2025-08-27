library(jsonlite)
library(PatientLevelPrediction)

sourceModelsDir <- "./results/development/models/"
destinationDir <- "./inst/newModels/"

# Map original outcome IDs to descriptive names
getOutcomeName <- function(outcomeId) {
  switch(as.character(outcomeId),
    "11" = "Death",
    "13" = "Critical",
    "14" = "Hospital",
    "UnknownOutcome"
  )
}

organizePlpModels <- function(sourceDir, destDir) {
  if (!dir.exists(destDir)) {
    message("Creating destination directory: ", destDir)
    dir.create(destDir, recursive = TRUE)
  }

  modelFolders <- list.dirs(sourceDir, full.names = TRUE, recursive = FALSE)

  if (length(modelFolders) == 0) {
    warning("No model folders found in: ", sourceDir)
    return(invisible(NULL))
  }

  message("Found ", length(modelFolders), " model folders to process.")

  summaryLog <- list()

  for (originalPath in modelFolders) {
    message("\nProcessing: ", basename(originalPath))

    modelDesignPath <- file.path(originalPath, "modelDesign.json")

    if (!file.exists(modelDesignPath)) {
      warning("Skipping folder: 'modelDesign.json' not found in ", originalPath)
      next
    }

    modelDesign <- fromJSON(modelDesignPath, simplifyVector = FALSE)

    outcomeId <- modelDesign$outcomeId
    startDate <- modelDesign$restrictPlpDataSettings$studyStartDate
    endDate <- modelDesign$restrictPlpDataSettings$studyEndDate

    if (is.null(outcomeId) || is.null(startDate) || is.null(endDate)) {
      warning("Skipping folder: Missing outcomeId or date information in ", originalPath)
      next
    }

    covariateSettingsLength <- length(modelDesign$covariateSettings)
    featureSet <- if (covariateSettingsLength == 1) {
      "Full"
    } else if (covariateSettingsLength > 1) {
      "Parsimonious"
    } else {
      "UnknownFeatures"
    }

    outcomeName <- getOutcomeName(outcomeId)

    newFolderName <- paste(
      outcomeName,
      featureSet,
      startDate,
      "to",
      endDate,
      sep = "_"
    )

    newModelPath <- file.path(destDir, newFolderName)

    if (!dir.exists(newModelPath)) {
      dir.create(newModelPath)
    }

    filesToCopy <- list.files(originalPath, full.names = TRUE)
    file.copy(from = filesToCopy, to = newModelPath, overwrite = TRUE)

    message("Successfully saved model to: ", newModelPath)

    summaryLog[[basename(originalPath)]] <- newFolderName
  }

  cat("\n====================================================\n")
  cat("Processing Complete. Summary:\n")
  cat("====================================================\n\n")
  if (length(summaryLog) > 0) {
    for (originalName in names(summaryLog)) {
      cat(sprintf("Moved '%s' -> '%s'\n", originalName, summaryLog[[originalName]]))
    }
  } else {
    cat("No models were processed.\n")
  }
  cat("\n")
}

organizePlpModels(sourceDir = sourceModelsDir, destDir = destinationDir)
