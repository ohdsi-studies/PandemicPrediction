# from each analysis, per target-outcome pair, extract performance (auc) and
# in which period the performance was calculated from restrictPlpDatasettings
# then plot performance as a line covering three months at a time
#' @export
plotResults <- function(
    resultFolder = "") {
  analyses <- dir(resultFolder, pattern = "Analysis*")

  performance <- list()
  modelInfo <- list()
  for (i in seq_along(analyses)) {
    analysisPath <- file.path(resultFolder, analyses[[i]])
    performance[[i]] <- extractPerformance(analysisPath)
    modelInfo[[i]] <- extractModelInfo(analysisPath)
  }

  results <- data.frame(
    performance = unlist(performance),
    start_date = sapply(timePeriod, function(x) x$startDate),
    end_date = sapply(timePeriod, function(x) x$endDate),
    target = sapply(modelInfo, function(x) x$task$target),
    outcome = sapply(modelInfo, function(x) x$task$outcome)
  )

  results <- results |>
    dplyr::mutate(
      name = mapply(getName, .data$target, .data$outcome),
      start_date = as.Date(.data$start_date, "%Y%m%d"),
      end_date = as.Date(.data$end_date, "%Y%m%d")
    ) |>
    dplyr::arrange(.data$start_date)

  vplot <- ggplot2::ggplot(results, ggplot2::aes(x = .data$start_date, y = .data$performance, colour = .data$name)) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$end_date, yend = .data$performance), colour = "black") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_point(aes(x = .data$end_date), size = 3) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) + # Use "right" to include the legend
    ggplot2::labs(title = "Performance Over Time", x = "Time Period", y = "Performance")

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
  timePeriod <- list(
    startDate = as.Date(startDate, format = "%Y%m%d"),
    endDate = as.Date(endDate, format = "%Y%m%d")
  )
  task <- list(targetId = validationDetails$targetId, outcomeId = validationDetails$outcomeId)
  modelInfo <- data.frame(
    task = task,
    timePeriod = timePeriod
  )
  return(modelInfo)
}

getName <- function(target, outcome) {
  firstName <- switch(as.character(target),
      "12" = "Outpatient",
      "25" = "Inpatient",
      "UnknownTarget")

  secondName <- switch(as.character(outcome),
      "11" = "Death",
      "13" = "Critical",
      "14" = "Hospital",
      "UnknownOutcome")
  name <- paste0(firstName, secondName)
  return(name)
}
