source("extras/postAnalysis/metrics.R")
source("extras/postAnalysis/pairedTests.R")
source("extras/postAnalysis/extractPredictions.R")
source("extras/postAnalysis/drivers.R")
comps <- buildComparisonsFromAllResults("results/allResults.csv")
proxy_comps <- subset(
  comps,
  comparator %in% c("proxy_frozen_vs_proxy_rolling_recal", "proxy_recal_vs_proxy_rolling_recal")
)
df <- runQuarterwise(
  comparisons = proxy_comps,
  outDir = "results/rolling_test2",
  B = 50,
  seedBase = 42,
  parallel = "mirai",
  threads = 1,
  workers = 20,
  showProgress = TRUE
)
