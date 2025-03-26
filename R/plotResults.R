# from each analysis, per target-outcome pair, extract performance (auc) and
# in which period the performance was calculated from restrictPlpDatasettings
# then plot performance as a line covering three months at a time
#' @export
plotResults <- function(
    resultFolder = "/home/egill/ohdsi-studies/PandemicPrediction/results/new_inpatient_cohorts/strategusWork/PatientLevelPredictionValidationModule_3/ducky/") {
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
    target = sapply(modelInfo, function(x) x$task$targeti),
    outcome = sapply(modelInfo, function(x) x$task$outcome)
  )

  results <- results |>
    dplyr::mutate(
      name = mapply(getName, .data$target, .data$outcome),
      start_date = as.Date(.data$start_date, "%Y%m%d"),
      end_date = as.Date(.data$end_date, "%Y%m%d")
    ) |>
    dplyr::arrange(.data$start_date)

  vplot <- ggplot2::ggplot(results, ggplot2::aes(x = start_date, y = performance, colour = name)) +
    ggplot2::geom_segment(ggplot2::aes(xend = end_date, yend = performance), colour = "black") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_point(aes(x = end_date), size = 3) +
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

getModelName <- function(task) {
  if (task$targetId == 12) {
    if (task$outcomeId == 13) {
      modelName <- "dataDrivenI"
      return(modelName)
    } else if (task$outcomeId == 11) {
      modelName <- "dataDrivenF"
      return(modelName)
    } else if (task$outcomeId == 14) {
      modelName <- "dataDrivenH"
      return(modelName)
    } else {
      stop(paste0("Unknown outcome id: ", task$outcomeId))
    }
  }
}

extractModelInfo <- function(analysisPath) {
  modelPath <- file.path(analysisPath, "validationResult", "model")
  validationDetails <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "validationDetails.json"))
  modelDesign <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "modelDesign.json"))
  startDate <- validationDetails$restrictPlpDataSettings$studyStartDate
  endDate <- validationDetails$restrictPlpDataSettings$studyEndDate
  timePeriod <- list(
    startDate = as.Date(startDate, format = "%Y%m%d"),
    endDate = as.Date(endDate, format = "%Y%m%d")
  )
  task <- list(targetId = validationDetails$targetId, outcomeId = validationDetails$outcomeId)
  modelName <- attr(modelDesign$modelSettings$param, "settings")$name
  if (modelName == "Existing GLM") {
    modelName <- getModelName(task)
  }
  modelInfo <- data.frame(
    name = modelName,
    targetId = task$targetId,
    outcomeId = task$outcomeId,
    startDate = timePeriod$startDate,
    endDate = timePeriod$endDate
  )
  return(modelInfo)
}
resultFolder <- "./results/strategus_v1/strategusWork/PatientLevelPredictionValidationModule/OPTUM Extended DOD/"

analyses <- dir(resultFolder, pattern = "Analysis*")


allPerformance <- data.frame()
for (i in seq_along(analyses)) {
  analysisPath <- file.path(resultFolder, analyses[[i]])
  performance <- extractPerformance(analysisPath)
  modelInfo <- extractModelInfo(analysisPath)
  combined <- cbind(modelInfo, performance)
  allPerformance <- rbind(allPerformance, combined)
}

getName <- function(target, outcome) {
  firstName <- switch(as.character(target),
    "12" = "Outpatient",
    "25" = "Inpatient",
    "Unkown Target"
  )
  secondName <- switch(as.character(outcome),
    "11" = "Death",
    "13" = "Critical",
    "14" = "Hospital",
    "Unknown Outcome"
  )
  name <- paste0(firstName, secondName)
  return(name)
}

results <- results |>
  dplyr::mutate(
    name = mapply(getName, .data$target, .data$outcome),
    start_date = as.Date(.data$start_date, "%Y%m%d"),
    end_date = as.Date(.data$end_date, "%Y%m%d")
  ) |>
  dplyr::arrange(.data$start_date)


ggplot(results, aes(x = start_date, y = performance, colour = name)) +
  geom_segment(aes(xend = end_date, yend = performance), colour = "black") +
  geom_point(size = 3) +
  geom_point(aes(x = end_date), size = 3) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + # Use "right" to include the legend
  labs(title = "Performance Over Time", x = "Time Period", y = "Performance")



results <- allPerformance |>
  dplyr::mutate(taskName = mapply(getName, .data$targetId, .data$outcomeId)) |>
  dplyr::arrange(.data$startDate)

selectedModels <- c("coverH", "dataDrivenH")
metric <- "AUROC"
results <- results |>
  dplyr::filter(.data$name %in% selectedModels) |>
  dplyr::select(dplyr::all_of(metric), "startDate", "endDate", "name", "taskName")

ggplot(results, aes(x = .data$startDate, y = .data[[metric]], colour = .data$name)) +
  geom_segment(aes(xend = .data$endDate, yend = .data[[metric]]), colour = "black") +
  geom_point(size = 3) +
  geom_point(aes(x = .data$endDate), size = 3) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + # Use "right" to include the legend
  labs(title = "Performance Over Time", x = "Time Period", y = "Performance")
