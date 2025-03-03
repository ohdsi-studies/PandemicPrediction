# from each analysis, per target-outcome pair, extract performance (auc) and 
# in which period the performance was calculated from restrictPlpDatasettings
# then plot performance as a line covering three months at a time 
#' @export
plotResults <- function(
    resultFolder = '/home/egill/ohdsi-studies/PandemicPrediction/results/new_inpatient_cohorts/strategusWork/PatientLevelPredictionValidationModule_3/ducky/'
    ){
  
  analyses <- dir(resultFolder, pattern = "Analysis*")
  
performance <- list()
modelInfo <- list()
for (i in seq_along(analyses)) {
  analysisPath <- file.path(resultFolder, analyses[[i]])
  performance[[i]] <- extractPerformance(analysisPath)
  modelInfo[[i]] <- extractModelInfo(analysisPath)
}

results <- data.frame(performance = unlist(performance),
                      start_date = sapply(timePeriod, function(x) x$startDate),
                      end_date = sapply(timePeriod, function(x) x$endDate),
                      target = sapply(modelInfo, function(x) x$task$targeti),
                      outcome = sapply(modelInfo, function(x) x$task$outcome))

results <- results |>
  dplyr::mutate(name = mapply(getName, .data$target, .data$outcome),
                start_date = as.Date(.data$start_date, "%Y%m%d"),
                end_date = as.Date(.data$end_date, "%Y%m%d")) |>
  dplyr::arrange(.data$start_date)

vplot <- ggplot2::ggplot(results, ggplot2::aes(x = start_date, y = performance, colour = name)) +
  ggplot2::geom_segment(ggplot2::aes(xend = end_date, yend = performance), colour = "black") +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_point(aes(x = end_date), size = 3) +
  ggplot2::theme_bw() + 
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
  ggplot2::theme(legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +  # Use "right" to include the legend
  ggplot2::labs(title = "Performance Over Time", x = "Time Period", y = "Performance")

return(vplot)
}


# HELPERS
extractPerformance <- function(analysisPath) {
  analysis <- readRDS(file.path(analysisPath, "validationResult", "runPlp.rds"))
  perfMetric <- analysis$performanceEvaluation$evaluationStatistics |>
    dplyr::filter(.data$metric == "AUROC", .data$evaluation == "Validation") |>
    dplyr::pull(.data$value)
  if (inherits(perfMetric, "list")) {
    perfMetric <- perfMetric[[1]]
  }
  return(perfMetric)
}

extractModelInfo <- function(analysisPath) {
  modelPath <- file.path(analysisPath, "validationResult", "model")
  validationDetails <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "validationDetails.json"))
  startDate <- validationDetails$restrictPlpDataSettings$studyStartDate
  endDate <- validationDetails$restrictPlpDataSettings$studyEndDate
  timePeriod <- list(startDate = as.Date(startDate),
                     endDate = as.Date(endDate))
  task <- list(targeti = validationDetails$targetId, outcome = validationDetails$outcomeId)
  modelInfo <- list(task = task, timePeriod = timePeriod)
  return(modelInfo)
}
getName <- function(target, outcome) {
  firstName <- switch(as.character(target),
                      "12" = "Outpatient",
                      "25" = "Inpatient",
                      "Unkown Target")
  secondName <- switch(as.character(outcome),
                       "11" = "Death",
                       "13" = "Critical",
                       "14" = "Hospital",
                       "Unknown target")
  name <- paste0(firstName, secondName)
  return(name)
}