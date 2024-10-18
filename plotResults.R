# from each analysis, per target-outcome pair, extract performance (auc) and 
# in which period the performance was calculated from restrictPlpDatasettings
# then plot performance as a line covering three months at a time 

library(ggplot2)

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
  modelDesign <- ParallelLogger::loadSettingsFromJson(file.path(modelPath, "modelDesign.json"))
  startDate <- validationDetails$restrictPlpDataSettings$studyStartDate
  endDate <- validationDetails$restrictPlpDataSettings$studyEndDate
  timePeriod <- list(startDate = as.Date(startDate, format = "%Y%m%d"),
                     endDate = as.Date(endDate, format = "%Y%m%d"))
  task <- list(targetId = validationDetails$targetId, outcomeId = validationDetails$outcomeId)
  modelName <- attr(modelDesign$modelSettings$param, "settings")$name
  modelInfo <- list(name = modelName, task = task, timePeriod = timePeriod)
  return(modelInfo)
}

resultFolder <- './results/strategus_new/strategusWork/PatientLevelPredictionValidationModule/ducky/'

analyses <- dir(resultFolder, pattern = "Analysis*")


performance <- list()
modelInfo <- list()
for (i in seq_along(analyses)) {
  analysisPath <- file.path(resultFolder, analyses[[i]])
  performance[[i]] <- extractPerformance(analysisPath)
  modelInfo[[i]] <- extractModelInfo(analysisPath)
}

results <- data.frame(performance = unlist(performance),
                      name = sapply(modelInfo, function(x) x$name),
                      start_date = sapply(modelInfo, function(x) x$timePeriod$startDate),
                      end_date = sapply(modelInfo, function(x) x$timePeriod$endDate),
                      target = sapply(modelInfo, function(x) x$task$targetId),
                      outcome = sapply(modelInfo, function(x) x$task$outcomeId))

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

results <- results |>
  dplyr::mutate(
                start_date = as.Date(.data$start_date),
                end_date = as.Date(.data$end_date)) |>
  dplyr::arrange(.data$start_date)


ggplot(results, aes(x = start_date, y = performance, colour = name)) +
  geom_segment(aes(xend = end_date, yend = performance), colour = "black") +
  geom_point(size = 3) +
  geom_point(aes(x = end_date), size = 3) +
  theme_bw() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Use "right" to include the legend
  labs(title = "Performance Over Time", x = "Time Period", y = "Performance")


