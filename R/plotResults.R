# from each analysis, per target-outcome pair, extract performance (auc) and
# in which period the performance was calculated from restrictPlpDatasettings
# then plot performance as a line covering three months at a time
#' @export
plotResults <- function(
    resultFolder = "",
    filter = "cover") {
  analyses <- dir(resultFolder, pattern = "Analysis*")

  performance <- list()
  modelInfo <- list()
  for (i in seq_along(analyses)) {
    analysisPath <- file.path(resultFolder, analyses[[i]])
    performance[[i]] <- extractPerformance(analysisPath)
    modelInfo[[i]] <- extractModelInfo(analysisPath)
  }

  results <- data.frame(
    start_date = sapply(modelInfo, function(x) x$startDate),
    end_date = sapply(modelInfo, function(x) x$endDate),
    target = sapply(modelInfo, function(x) x$targetId),
    outcome = sapply(modelInfo, function(x) x$outcomeId),
    modelName = sapply(modelInfo, function(x) x$name)
  )
  results <- cbind(results, dplyr::bind_rows(performance))
  results$analysis <- analyses
  if (!is.null(filter)) {
    results <- results |>
      dplyr::filter(grepl(filter, .data$modelName, ignore.case = TRUE))
  }

  results <- results |>
    dplyr::mutate(
      name = paste(mapply(getName, .data$target, .data$outcome),
        .data$modelName, sep = "_"
      ),
      start_date = as.Date(.data$start_date),
      end_date = as.Date(.data$end_date)
    ) |>
    dplyr::arrange(.data$start_date)

  vplot <- ggplot2::ggplot(results, ggplot2::aes(
    x = .data$start_date,
    y = .data$AUROC,
    colour = .data$name
  )) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data$end_date,
        yend = .data$AUROC
      ),
      colour = "black"
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = .data$end_date), size = 3) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      minor_breaks = NULL
    ) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) + # Use "right" to include the legend
    ggplot2::labs(
      title = "Performance Over Time",
      x = "Time Period",
      y = "Performance"
    )

  return(vplot)
}


# HELPERS
extractPerformance <- function(analysisPath) {
  analysis <- readRDS(file.path(analysisPath, "validationResult", "runPlp.rds"))
  perfMetric <- analysis$performanceEvaluation$evaluationStatistics |>
    dplyr::filter(
      .data$metric %in% c(
        "AUROC", "populationSize", "outcomeCount",
        "Eavg"
      ),
      .data$evaluation == "Validation"
    ) |>
    dplyr::select("metric", "value") |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::mutate(
      analysis = basename(analysisPath),
      populationSize = as.integer(.data$populationSize),
      outcomeCount = as.integer(.data$outcomeCount),
      Eavg = as.numeric(.data$Eavg),
      AUROC = as.numeric(.data$AUROC)
    )
  return(perfMetric)
}

extractModelInfo <- function(analysisPath) {
  modelPath <- file.path(analysisPath, "validationResult", "model")
  validationDetails <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "validationDetails.json"))
  startDate <- validationDetails$restrictPlpDataSettings$studyStartDate
  endDate <- validationDetails$restrictPlpDataSettings$studyEndDate
  modelDesign <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "modelDesign.json"))
  name <- attributes(modelDesign$modelSettings$param)$settings$name
  modelInfo <- data.frame(
    name = getModelName(name, validationDetails$targetId, validationDetails$outcomeId),
    targetId = validationDetails$targetId,
    outcomeId = validationDetails$outcomeId,
    startDate = as.Date(startDate, format = "%Y%m%d"),
    endDate = as.Date(endDate, format = "%Y%m%d")
  )
  return(modelInfo)
}

getModelName <- function(name, targetId, outcomeId) {
  if (!grepl("cover", name, ignore.case = TRUE)) {
    if (targetId == 12) {
      if (outcomeId == 11) {
        name <- "dataDrivenF"
      } else if (outcomeId == 13) {
        name <- "dataDrivenI"
      } else if (outcomeId == 14) {
        name <- "dataDrivenH"
      }
    }
  }
  return(name)
}

getName <- function(target, outcome) {
  firstName <- switch(as.character(target),
    "12" = "Outpatient",
    "25" = "Inpatient",
    "UnknownTarget"
  )

  secondName <- switch(as.character(outcome),
    "11" = "Death",
    "13" = "Critical",
    "14" = "Hospital",
    "UnknownOutcome"
  )
  name <- paste0(firstName, secondName)
  return(name)
}
