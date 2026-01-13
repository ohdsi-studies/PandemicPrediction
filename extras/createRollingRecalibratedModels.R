# Build quarter-shifted (Q-1 -> Q) recalibrated proxy models using weak-recalibration
# coefficients exported by extras/exportWeakRecalibration.R.

coeffPath <- "results/weakRecalibration_coeffs.csv"
baseModelsRoot <- "inst/models"
outDir <- "inst/rollingRecalibratedModels"

# Map outcome + feature set to base proxy model folder
baseModelFolder <- function(outcomeName, featureSet) {
  fs <- tolower(featureSet)
  on <- tolower(outcomeName)
  if (on == "fatality") {
    return(
      if (fs == "parsimonious") {
        file.path(baseModelsRoot, "dato")
      } else {
        file.path(baseModelsRoot, "dataDrivenF")
      }
    )
  }
  if (on == "hospitalization") {
    return(
      if (fs == "parsimonious") {
        file.path(baseModelsRoot, "sato")
      } else {
        file.path(baseModelsRoot, "dataDrivenH")
      }
    )
  }
  if (on == "respiratoryfailure") {
    return(
      if (fs == "parsimonious") {
        file.path(baseModelsRoot, "cato")
      } else {
        file.path(baseModelsRoot, "dataDrivenI")
      }
    )
  }
  stop("Unknown outcomeName: ", outcomeName)
}

if (!file.exists(coeffPath)) {
  stop(
    "Missing coefficients file: ",
    coeffPath,
    ". Run extras/exportWeakRecalibration.R first."
  )
}
coefs <- utils::read.csv(coeffPath, stringsAsFactors = FALSE)
if (!nrow(coefs)) {
  stop("No rows in coefficients file.")
}

# normalize types
coefs$quarterStartDate <- as.Date(as.character(coefs$quarterStart), "%Y%m%d")
coefs$quarterEndDate <- as.Date(as.character(coefs$quarterEnd), "%Y%m%d")
coefs$intercept <- as.numeric(coefs$intercept)
coefs$slope <- as.numeric(coefs$slope)

dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

manifest <- list()
groups <- split(
  coefs,
  interaction(coefs$outcomeName, coefs$featureSet, drop = TRUE)
)
for (g in groups) {
  # order by quarterStart
  g <- g[order(g$quarterStartDate), ]
  if (nrow(g) < 2) {
    next
  }
  # base model folder once per group
  baseFolder <- baseModelFolder(g$outcomeName[1], g$featureSet[1])
  if (!dir.exists(baseFolder)) {
    warning(
      "Base model folder not found: ",
      baseFolder,
      " for outcome=",
      g$outcomeName[1],
      " feature=",
      g$featureSet[1]
    )
    next
  }
  baseModel <- PatientLevelPrediction::loadPlpModel(baseFolder)
  for (i in 2:nrow(g)) {
    prev <- g[i - 1, ]
    cur <- g[i, ]
    rollKey <- sub("^proxy_frozen", "proxy_roll", cur$modelKey)
    # apply prev coefficients to base model for current quarter
    promoted <- baseModel
    st <- attr(baseModel, "saveType")
    if (is.null(st) || !nzchar(st)) {
      st <- "RtoJson"
    }
    attr(promoted, "saveType") <- st
    if (!is.null(promoted$modelDesign$modelSettings$settings)) {
      promoted$modelDesign$modelSettings$settings$saveType <- st
    }
    if (!is.null(promoted$modelDesign$modelSettings$param)) {
      attr(promoted$modelDesign$modelSettings$param, "saveType") <- st
    }
    attr(promoted, "originalPredictionFunction") <- attr(
      baseModel,
      "predictionFunction"
    )
    attr(
      promoted,
      "predictionFunction"
    ) <- "PandemicPrediction::predictWithRecalibration"
    attr(promoted, "recalibration") <- list(
      coefficients = list(
        adjustIntercept = setNames(prev$intercept, "(Intercept)"),
        adjustGradient = setNames(prev$slope, "lp")
      ),
      recalibratedOn = prev$quarterId
    )
    # encode in modelDesign too
    promoted$modelDesign$modelSettings$recalibrationPeriod <- prev$quarterId

    # Short folder name to avoid path-length issues
    fromTag <- paste0(substr(prev$quarterStart, 1, 6))
    toTag <- paste0(substr(cur$quarterStart, 1, 6))
    fsTag <- ifelse(tolower(cur$featureSet) == "parsimonious", "pars", "full")
    outTag <- substr(cur$outcomeName, 1, 4)
    folderName <- paste(outTag, fsTag, "R", fromTag, "A", toTag, sep = "_")
    dest <- file.path(outDir, folderName)
    if (!dir.exists(dest)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    PatientLevelPrediction::savePlpModel(promoted, dest)
    manifest[[length(manifest) + 1]] <- data.frame(
      outcomeName = cur$outcomeName,
      featureSet = cur$featureSet,
      modelKey = rollKey,
      applyQuarter = cur$quarterId,
      recalibratedOn = prev$quarterId,
      dest = dest,
      stringsAsFactors = FALSE
    )
  }
}

if (length(manifest)) {
  manifestDf <- do.call(rbind, manifest)
  utils::write.csv(
    manifestDf,
    file.path(outDir, "manifest.csv"),
    row.names = FALSE
  )
  message("Saved rolling recalibrated models to ", normalizePath(outDir))
  message("Manifest: ", normalizePath(file.path(outDir, "manifest.csv")))
} else {
  message("No rolling models generated.")
}
