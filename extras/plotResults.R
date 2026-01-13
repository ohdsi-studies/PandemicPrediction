# library(PandemicPrediction)

allResults <- dplyr::bind_rows(
  getAnalysisResults(
    databasePath = "results/runs/development/databaseFile.sqlite",
    evaluationType = "Test",
    analysisId = "dev"
  ),
  getAnalysisResults(
    databasePath = "results/runs/validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_original"
  ),
  getAnalysisResults(
    databasePath = "results/runs/new_models_validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_new"
  ),
  getAnalysisResults(
    databasePath = "results/runs/recalibrated_models_validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_new_recalibrated"
  ),
  getAnalysisResults(
    databasePath = "results/runs/rolling_recalibrated_validation/databaseFile.sqlite",
    evaluationType = "Validation",
    analysisId = "val_rolling_recalibrated"
  )
)
plot <- plotComparison(
  allResults = allResults,
  outcomes = c("Fatality", "Hospitalization", "RespiratoryFailure"),
  modelOriginsToCompare = c("Original Influenza"),
  featureSetsToCompare = c("Full", "Parsimonious"),
  devPeriodsToCompare = c("First 9 Months"),
  facetBy = c("outcomeName"),
  metric = "AUROC",
  title = "performance",
  # showOutcomeRatePanel = TRUE,            # turn on Option C
  # outcomeRatePanelStyle = "area"          # or "bars"
)

# ggplot2::ggsave(
#   filename = "results/figures/calibration_performance.svg",
#   plot = plot,
#   width = 380,
#   height = 250,
#   units = "mm",
#   dpi = 300
# )
p <- plotComparison(
  allResults,
  outcomes = c("Fatality", "RespiratoryFailure", "Hospitalization"),
  modelOriginsToCompare = c("New Covid", "Original Influenza"),
  featureSetsToCompare = c("Parsimonious", "Full"),
  metric = "AUROC",
  facetBy = c("outcomeName"),
  legendCorner = "auto",
  title = "Original Influenza vs New Covid — AUROC over time"
)
