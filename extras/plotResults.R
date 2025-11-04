library(PandemicPrediction)

allResults <- dplyr::bind_rows(
  getAnalysisResults(
    databasePath = "results/development/databaseFile.sqlite",
    evaluationType = "Test",
    analysisId = "dev"
  ),
  getAnalysisResults(
    databasePath = "results/validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_original"
  ),
  getAnalysisResults(
    databasePath = "results/covid-validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_new"
  ),
  getAnalysisResults(
    databasePath = "results/recalibrated-validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_new_recalibrated"
  )
)

val <- dplyr::filter(allResults, evaluationType == "val_original")
recal <- dplyr::filter(allResults, evaluationType == "val_new_recalibrated")

print("--- Master Results Data Frame ---")
print(dplyr::glimpse(allResults))

plot <- plotComparison(
  allResults = allResults,
  outcomes = c("Fatality", "Hospitalization", "RespiratoryFailure"),
  modelOriginsToCompare = c("Original Influenza"),
  featureSetsToCompare = c("Full", "Parsimonious"),
  devPeriodsToCompare = c("First 9 Months"),
  facetBy = c("outcomeName"),
  metric = "Eavg",
  title = "Calibration performance",
  showOutcomeRatePanel = TRUE,            # turn on Option C
  outcomeRatePanelStyle = "area"          # or "bars"
)

ggplot2::ggsave(
  filename = "results/figures/calibration_performance.svg",
  plot = plot,
  width = 380,
  height = 250,
  units = "mm",
  dpi = 300
)
