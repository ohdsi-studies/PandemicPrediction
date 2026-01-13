source("extras/postAnalysis/extractPredictions.R")
source("extras/postAnalysis/quarterwisePlots.R")
df <- collectQuarterwiseBootstrap(
  resultsDir = "results/full_proxy_v2",
  outcomes = "Hospitalization"
)
row <- subset(
  df,
  metric == "INB" &
    quarterLabel == "2021-Q3" &
    comparator == "covid_vs_proxy_recal" &
    featureSet == "Full" &
    W == "12m_150k"
)[1, ]

# Plot proxy-frozen model net benefit for that quarter
p <- plotQuarterNetBenefitCurve(row, role = "B", evalType = "Validation")
print(p)
