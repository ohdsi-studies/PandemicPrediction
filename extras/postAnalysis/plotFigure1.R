#!/usr/bin/env Rscript

# Figure 1 helper: quarter-wise utility (INB/ANBC) over time for all outcomes.
#
# Usage:
#   Rscript extras/postAnalysis/plotFigure1.R \
#     --resultsDir results/rolling200/results/rolling_200 \
#     --metric INB \
#     --outFile results/figures/Figure1_INB.svg
#
# Notes:
# - This script expects quarter-wise bootstrap CSVs in `resultsDir` (non-recursive).
# - If `--metric ANBC` is requested but not present, it falls back to INB.

args <- commandArgs(trailingOnly = TRUE)
getArg <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) return(args[[idx + 1]])
  default
}
getFlag <- function(flag, default = FALSE) {
  val <- getArg(flag, NULL)
  if (is.null(val)) return(default)
  tolower(val) %in% c("1", "true", "t", "yes", "y")
}

resultsDir <- getArg("--resultsDir", "results/rolling200/results/rolling_200")
metric <- getArg("--metric", "ANBC")
outFile <- getArg("--outFile", file.path("results", "figures", paste0("Figure1_", metric, ".svg")))
outDir <- getArg("--outDir", dirname(outFile))
splitOutcomes <- getFlag("--splitOutcomes", default = FALSE)
outcomeArg <- getArg("--outcome", NULL)
modelSet <- getArg("--modelSet", "all") # all | summary
dumpData <- getArg("--dumpData", "none") # none | head | all
dumpN <- as.integer(getArg("--dumpN", "50"))
dumpFile <- getArg("--dumpFile", NULL) # optional .csv/.tsv

source("extras/postAnalysis/quarterwisePlots.R")

autoResultsDir <- function(dir) {
  if (dir.exists(dir) && length(list.files(dir, pattern = "\\.csv$", full.names = TRUE))) {
    return(dir)
  }
  nested <- file.path(dir, "results", "rolling_200")
  if (dir.exists(nested) && length(list.files(nested, pattern = "\\.csv$", full.names = TRUE))) {
    message("No CSVs in ", dir, "; using nested folder: ", nested)
    return(nested)
  }
  dir
}
resultsDir <- autoResultsDir(resultsDir)

dfAll <- collectQuarterwiseBootstrap(
  resultsDir = resultsDir,
  metrics = NULL,
  outcomes = c("Fatality", "Hospitalization", "RespiratoryFailure")
)
if (!nrow(dfAll)) {
  stop("No data found under resultsDir=", resultsDir, ". Did you point to the folder with CSVs?")
}

if (!metric %in% unique(dfAll$metric)) {
  if (identical(metric, "ANBC") && "INB" %in% unique(dfAll$metric)) {
    message("Metric ANBC not found in results; falling back to INB.")
    metric <- "INB"
  } else {
    stop("Metric '", metric, "' not found in provided results.")
  }
}

labelMap <- c(
  # Use shared labels across Full/Pars to avoid legend duplication;
  # featureSet is shown via faceting.
  proxy_frozen_full = "Proxy frozen",
  proxy_frozen_pars = "Proxy frozen",
  proxy_roll_full = "Proxy rolling recal",
  proxy_roll_pars = "Proxy rolling recal",

  covid_full_3m = "COVID 3m",
  covid_full_6m = "COVID 6m",
  covid_full_9m = "COVID 9m",
  covid_full_12m_150k = "COVID 12m-150k",
  covid_full_12m_full = "COVID 12m-full",

  covid_pars_3m = "COVID 3m",
  covid_pars_6m = "COVID 6m",
  covid_pars_9m = "COVID 9m",
  covid_pars_12m_150k = "COVID 12m-150k",
  covid_pars_12m_full = "COVID 12m-full",

  proxy_recal_full_3m = "Proxy recal 3m",
  proxy_recal_full_6m = "Proxy recal 6m",
  proxy_recal_full_9m = "Proxy recal 9m",
  proxy_recal_full_12m_150k = "Proxy recal 12m-150k",
  proxy_recal_full_12m_full = "Proxy recal 12m-full",

  proxy_recal_pars_3m = "Proxy recal 3m",
  proxy_recal_pars_6m = "Proxy recal 6m",
  proxy_recal_pars_9m = "Proxy recal 9m",
  proxy_recal_pars_12m_150k = "Proxy recal 12m-150k",
  proxy_recal_pars_12m_full = "Proxy recal 12m-full"
)

palette <- c(
  "Proxy frozen" = "#000000",
  "Proxy rolling recal" = "#7f7f7f",
  "Proxy recal 3m" = "#c6dbef",
  "Proxy recal 6m" = "#9ecae1",
  "Proxy recal 9m" = "#6baed6",
  "Proxy recal 12m-150k" = "#3182bd",
  "Proxy recal 12m-full" = "#08519c",
  "COVID 3m" = "#fee0d2",
  "COVID 6m" = "#fcbba1",
  "COVID 9m" = "#fc9272",
  "COVID 12m-150k" = "#fb6a4a",
  "COVID 12m-full" = "#de2d26"
)

if (identical(modelSet, "summary")) {
  labelMap <- c(
    proxy_frozen_full = "Proxy frozen",
    proxy_frozen_pars = "Proxy frozen",
    proxy_roll_full = "Proxy rolling recal",
    proxy_roll_pars = "Proxy rolling recal",

    covid_full_3m = "COVID 3m",
    covid_pars_3m = "COVID 3m",
    covid_full_12m_full = "COVID 12m-full",
    covid_pars_12m_full = "COVID 12m-full",

    proxy_recal_full_12m_full = "Proxy recal 12m-full",
    proxy_recal_pars_12m_full = "Proxy recal 12m-full"
  )
  palette <- palette[names(palette) %in% unique(labelMap)]
}

yLabel <- if (metric == "ANBC") "A-NBC (trapezoidal area)" else metric
title <- if (metric == "ANBC") "Area under net benefit curve (A-NBC) over time" else "Integrated net benefit (INB) over time"

makeOutcomePanel <- function(outcomeName, showLegend = FALSE) {
  dfO <- dfAll[dfAll$outcomeName == outcomeName, , drop = FALSE]
  modelDf <- comparisonToModelRows(dfO, metric = metric, labelMap = labelMap)

  dumpMode <- tolower(dumpData %||% "none")
  if (!identical(dumpMode, "none")) {
    dumpDf <- modelDf
    if (identical(dumpMode, "head")) {
      dumpDf <- utils::head(dumpDf, n = dumpN)
    } else if (!identical(dumpMode, "all")) {
      stop("--dumpData must be one of: none, head, all")
    }
    message("---- plotFigure1 dump (", outcomeName, ", metric=", metric, ", rows=", nrow(modelDf), ") ----")
    if (!is.null(dumpFile) && nzchar(dumpFile)) {
      dir.create(dirname(dumpFile), recursive = TRUE, showWarnings = FALSE)
      if (grepl("\\.tsv$", dumpFile, ignore.case = TRUE)) {
        utils::write.table(dumpDf, file = dumpFile, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
      } else {
        utils::write.csv(dumpDf, file = dumpFile, row.names = FALSE)
      }
      message("Wrote dump: ", normalizePath(dumpFile))
    } else {
      print(dumpDf)
    }
  }

  plotQuarterwiseModelMetric(
    modelDf,
    title = outcomeName,
    yLabel = yLabel,
    showOutcomeRate = TRUE
  ) +
    ggplot2::scale_color_manual(values = palette, drop = TRUE) +
    ggplot2::scale_fill_manual(values = palette, guide = "none", drop = TRUE) +
    ggplot2::facet_wrap(~featureSet, nrow = 1, scales = "fixed") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = if (isTRUE(showLegend)) "bottom" else "none"
    )
}

outcomes <- c("Hospitalization", "RespiratoryFailure", "Fatality")
if (!is.null(outcomeArg)) {
  if (!outcomeArg %in% outcomes) {
    stop("--outcome must be one of: ", paste(outcomes, collapse = ", "))
  }
  outcomes <- outcomeArg
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required for Figure 1 layout.")
}

writeOne <- function(outcomeName, file) {
  p <- makeOutcomePanel(outcomeName, showLegend = TRUE) +
    ggplot2::labs(title = title, subtitle = outcomeName)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = file, plot = p, width = 13, height = 4.5, units = "in")
  message("Wrote: ", normalizePath(file))
}

if (isTRUE(splitOutcomes) || length(outcomes) == 1) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  for (o in outcomes) {
    file <- if (length(outcomes) == 1 && !isTRUE(splitOutcomes)) {
      outFile
    } else {
      file.path(outDir, paste0("Figure1_", metric, "_", o, ".svg"))
    }
    writeOne(o, file)
  }
  quit(save = "no", status = 0)
}

p1 <- makeOutcomePanel("Hospitalization", showLegend = TRUE)
p2 <- makeOutcomePanel("RespiratoryFailure", showLegend = FALSE)
p3 <- makeOutcomePanel("Fatality", showLegend = FALSE)

p <- (p1 / p2 / p3) + patchwork::plot_annotation(title = title)

dir.create(dirname(outFile), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(filename = outFile, plot = p, width = 13, height = 8, units = "in")
message("Wrote: ", normalizePath(outFile))
