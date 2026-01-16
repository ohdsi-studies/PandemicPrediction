source("extras/postAnalysis/quarterwisePlots.R")
filterForPlot <- function(results, metric = "AUROC") {
  df <- results[results$metric == metric, ]
  df <- df[!grepl("recal", df$comparator, ignore.case = TRUE), ]
  df  |> dplyr::select(-"sourceFile")
}

dfAll <- collectQuarterwiseBootstrap(
  resultsDir = "results/rolling_test2",
  outcomes = "Hospitalization"
)

modelDf <- comparisonToModelRows(
  dfAll,
  metric = "ICI",
  labelMap = c(
      proxy_frozen_full = "Frozen Full",
      proxy_roll_full   = "Rolling Full",
      proxy_frozen_pars = "Frozen Pars",
      proxy_roll_pars   = "Rolling Pars"
  )
)
p <- plotQuarterwiseModelMetric(
  modelDf

  # facetBy = c("W"),
  # valueMode = "absolute",
  # ciAlpha = 0.5,
  # showOutcomeRate = TRUE
)

ggplot0::ggsave(
  filename = "results/figures/comparisons/AUROC_COVID-PROXY_Hospitalization.svg",
  plot = p
)
