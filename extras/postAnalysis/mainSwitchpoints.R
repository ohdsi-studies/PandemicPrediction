source("extras/postAnalysis/switchPoints.R")

sp <- exportInbSwitchPoints(
  resultsDir = "results/rolling200/results/rolling_200",
  baselines = c("proxy_frozen", "proxy_recal"),
  estimate = "delta",
  minFromSwitch = 1
)

print(sp)
