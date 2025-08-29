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
    databasePath = "results/new-model-validation/databaseFile.sqlite", 
    evaluationType = "Validation", 
    analysisId = "val_new"
  )
)

print("--- Master Results Data Frame ---")
print(dplyr::glimpse(allResults))


plotComparison(
  allResults = allResults,
  outcomes = c("Critical", "Hospital"),
  modelOriginsToCompare = c("Original Influenza", "New Covid"),
  featureSetsToCompare = c("Full", "Parsimonious"),
  devPeriodsToCompare = c("First 3 Months"),
  facetBy = c("featureSet", "outcomeName"),
)
