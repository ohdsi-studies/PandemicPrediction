# Helpers to compute INB-based switch-points ("from which quarter onwards is
# COVID(W) better than proxy?") from quarter-wise bootstrap outputs.

parseQuarterIdLocal <- function(quarterId) {
  parts <- strsplit(as.character(quarterId), "_", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(list(
      start = NA,
      end = NA,
      mid = NA,
      label = quarterId,
      sortKey = NA_integer_
    ))
  }
  start <- as.Date(parts[1], "%Y%m%d")
  end <- as.Date(parts[2], "%Y%m%d")
  span <- if (is.na(start) || is.na(end)) NA_integer_ else as.integer(end - start)
  mid <- if (is.na(start) || is.na(span)) start else start + floor(span / 2)
  qNum <- if (is.na(start)) NA_integer_ else ((as.integer(format(start, "%m")) - 1L) %/% 3L) + 1L
  label <- if (is.na(qNum)) quarterId else sprintf("%s-Q%d", format(start, "%Y"), qNum)
  list(
    start = start,
    end = end,
    mid = mid,
    label = label,
    sortKey = if (is.na(start)) NA_integer_ else as.integer(start)
  )
}

ensureQuarterColumns <- function(df) {
  if (!nrow(df)) return(df)
  if (!"quarterStart" %in% names(df) || all(is.na(df$quarterStart))) {
    info <- lapply(df$quarterId, parseQuarterIdLocal)
    df$quarterStart <- as.Date(vapply(info, `[[`, as.Date(NA), "start"), origin = "1970-01-01")
    df$quarterEnd <- as.Date(vapply(info, `[[`, as.Date(NA), "end"), origin = "1970-01-01")
    df$quarterMid <- as.Date(vapply(info, `[[`, as.Date(NA), "mid"), origin = "1970-01-01")
    df$quarterLabel <- vapply(info, `[[`, character(1), "label")
  }
  df
}

computeEarliestSwitchQuarter <- function(
  quarters,
  deltas,
  minFromSwitch = 1L
) {
  stopifnot(length(quarters) == length(deltas))
  ok <- !is.na(quarters) & !is.na(deltas)
  quarters <- quarters[ok]
  deltas <- as.numeric(deltas[ok])
  if (!length(quarters)) {
    return(list(
      switchIndex = NA_integer_,
      switchQuarter = NA,
      nTotal = 0L
    ))
  }
  ord <- order(quarters)
  quarters <- quarters[ord]
  deltas <- deltas[ord]
  n <- length(quarters)
  minFromSwitch <- as.integer(minFromSwitch)
  if (is.na(minFromSwitch) || minFromSwitch < 1L) minFromSwitch <- 1L

  switchIndex <- NA_integer_
  for (i in seq_len(n)) {
    if ((n - i + 1L) < minFromSwitch) next
    med <- stats::median(deltas[i:n], na.rm = TRUE)
    if (is.finite(med) && med > 0) {
      switchIndex <- i
      break
    }
  }
  list(
    switchIndex = switchIndex,
    switchQuarter = if (is.na(switchIndex)) NA else quarters[switchIndex],
    nTotal = n
  )
}

computeInbSwitchPoints <- function(
  results,
  baselines = c("proxy_frozen", "proxy_recal"),
  outcomes = NULL,
  featureSets = NULL,
  wLevels = NULL,
  estimate = c("deltaFullSample", "delta"),
  minFromSwitch = 1L
) {
  if (missing(results) || !nrow(results)) stop("results is empty")
  estimate <- match.arg(estimate)
  baselines <- unique(match.arg(baselines, several.ok = TRUE))

  df <- ensureQuarterColumns(results)
  df <- df[df$metric == "INB", , drop = FALSE]
  if (!nrow(df)) stop("No INB rows found.")

  if (!is.null(outcomes)) df <- df[df$outcomeName %in% outcomes, , drop = FALSE]
  if (!is.null(featureSets)) df <- df[df$featureSet %in% featureSets, , drop = FALSE]
  if (!is.null(wLevels) && "W" %in% names(df)) df <- df[df$W %in% wLevels, , drop = FALSE]
  if (!nrow(df)) return(data.frame())

  baselineToComparator <- c(
    proxy_frozen = "covid_vs_proxy_frozen",
    proxy_recal = "covid_vs_proxy_recal"
  )

  df$deltaEstimate <- if (estimate %in% names(df)) df[[estimate]] else df$delta

  out <- list()
  k <- 1L
  for (baseline in baselines) {
    comp <- baselineToComparator[[baseline]]
    if (is.null(comp)) next
    d <- df[df$comparator == comp, , drop = FALSE]
    if (!nrow(d)) next

    keys <- c("outcomeName", "featureSet")
    if ("W" %in% names(d)) keys <- c(keys, "W")
    splitIdx <- split(seq_len(nrow(d)), interaction(d[keys], drop = TRUE, lex.order = TRUE))
    for (idx in splitIdx) {
      dd <- d[idx, , drop = FALSE]
      dd <- dd[order(dd$quarterStart), , drop = FALSE]
      sw <- computeEarliestSwitchQuarter(
        quarters = dd$quarterStart,
        deltas = dd$deltaEstimate,
        minFromSwitch = minFromSwitch
      )
      if (!is.na(sw$switchIndex)) {
        tail <- dd[sw$switchIndex:nrow(dd), , drop = FALSE]
        medDelta <- stats::median(tail$deltaEstimate, na.rm = TRUE)
        fracPos <- mean(tail$deltaEstimate > 0, na.rm = TRUE)
        minDelta <- suppressWarnings(min(tail$deltaEstimate, na.rm = TRUE))
        nFrom <- nrow(tail)
      } else {
        medDelta <- NA_real_
        fracPos <- NA_real_
        minDelta <- NA_real_
        nFrom <- 0L
      }

      row <- dd[1, , drop = FALSE]
      out[[k]] <- data.frame(
        baseline = baseline,
        comparator = comp,
        outcomeName = row$outcomeName,
        featureSet = row$featureSet,
        W = if ("W" %in% names(row)) as.character(row$W) else NA_character_,
        estimate = estimate,
        minFromSwitch = as.integer(minFromSwitch),
        nQuartersTotal = as.integer(sw$nTotal),
        switchQuarterStart = as.Date(sw$switchQuarter, origin = "1970-01-01"),
        switchQuarterLabel = if (is.na(sw$switchIndex)) NA_character_ else dd$quarterLabel[sw$switchIndex],
        nQuartersFromSwitch = as.integer(nFrom),
        medianDeltaFromSwitch = as.numeric(medDelta),
        fracPositiveFromSwitch = as.numeric(fracPos),
        minDeltaFromSwitch = as.numeric(minDelta),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  out <- do.call(rbind, out)
  if (is.null(out) || !nrow(out)) return(data.frame())
  out <- out[order(out$baseline, out$outcomeName, out$featureSet, out$W), ]
  rownames(out) <- NULL
  out
}

exportInbSwitchPoints <- function(
  resultsDir,
  outCsv = file.path(resultsDir, "switchpoints_inb.csv"),
  ...
) {
  source("extras/postAnalysis/quarterwisePlots.R")
  df <- collectQuarterwiseBootstrap(resultsDir = resultsDir)
  sp <- computeInbSwitchPoints(df, ...)
  utils::write.csv(sp, outCsv, row.names = FALSE)
  invisible(sp)
}

