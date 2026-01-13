getOutcome <- function(outcomeId) {
  outcome <- switch(
    as.character(outcomeId),
    "11" = "Fatality",
    "13" = "RespiratoryFailure",
    "14" = "Hospitalization",
    "UnknownOutcome"
  )
  return(outcome)
}

getTarget <- function(targetId) {
  target <- switch(
    as.character(targetId),
    "12" = "Outpatient",
    "25" = "Inpatient",
    "31" = "CovidNew",
    "UnknownTarget"
  )
  return(target)
}

getName <- function(target, outcome) {
  firstName <- getTarget(target)
  secondName <- getOutcome(outcome)
  name <- paste0(firstName, "-", secondName)
  return(name)
}

getModelName <- function(name, targetId, outcomeId) {
  if (!grepl("cover", name, ignore.case = TRUE)) {
    if (targetId == 12 || targetId == 31) {
      if (outcomeId == 11) {
        name <- "dataDrivenF"
      } else if (outcomeId == 13) {
        name <- "dataDrivenI"
      } else if (outcomeId == 14) {
        name <- "dataDrivenH"
      }
    }
  }
  return(name)
}

computeOutcomeRateSummary <- function(resultsToPlot, facetBy = NULL) {
  facetVars <- if (is.null(facetBy)) character(0) else facetBy
  groupVars <- c(facetVars, "periodMidpoint")

  resultsToPlot |>
    dplyr::filter(
      is.finite(.data$observedRisk),
      is.finite(.data$populationSize),
      .data$populationSize > 0
    ) |>
    dplyr::group_by(!!!rlang::syms(groupVars)) |>
    dplyr::summarise(
      risk = stats::weighted.mean(
        .data$observedRisk,
        w = .data$populationSize,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

featureShort <- function(x) ifelse(tolower(x) == "parsimonious", "pars", "full")

wFromDevPeriod <- function(devPeriod) {
  d <- tolower(trimws(devPeriod))
  d <- gsub("\\s+", " ", d)
  dplyr::case_when(
    is.na(d) ~ NA_character_,
    grepl("first 3", d) ~ "3m",
    grepl("first 6", d) ~ "6m",
    grepl("first 9", d) ~ "9m",
    grepl("sampled|150k", d) ~ "12m_150k",
    grepl("full year|full pop|full population", d) ~ "12m_full",
    TRUE ~ NA_character_
  )
}

wFromRecalPeriod <- function(recal) {
  r <- tolower(trimws(recal))
  r <- gsub("\\s+", " ", r)
  dplyr::case_when(
    is.na(r) | r == "" ~ NA_character_,
    grepl("first[_ ]?3", r) ~ "3m",
    grepl("first[_ ]?6", r) ~ "6m",
    grepl("first[_ ]?9", r) ~ "9m",
    grepl("sampled|150k", r) ~ "12m_150k",
    grepl("full|year|pop", r) ~ "12m_full",
    TRUE ~ NA_character_
  )
}

makeModelKey <- function(origin, featureSet, wDev, wRecal) {
  fs <- featureShort(featureSet)
  o <- tolower(origin)
  dplyr::case_when(
    o == "new covid" & is.na(wDev) ~ NA_character_,
    o == "new covid" ~ paste0("covid_", fs, "_", wDev),
    o == "original influenza" ~ paste0("proxy_frozen_", fs),
    o == "recalibrated influenza" & is.na(wRecal) ~ NA_character_,
    o == "recalibrated influenza" ~ paste0("proxy_recal_", fs, "_", wRecal),
    TRUE ~ NA_character_
  )
}

appendOutcomeSuffix <- function(modelKey, outcomeId, append = FALSE) {
  if (!append) {
    return(modelKey)
  }
  oc <- dplyr::case_when(
    outcomeId == 11 ~ "F",
    outcomeId == 13 ~ "I",
    outcomeId == 14 ~ "H",
    TRUE ~ "X"
  )
  paste0(modelKey, "_", oc)
}

#' Retrieve and process analysis results from the database
#' #' @param databasePath Path to the SQLite database file
#' @param evaluationType Type of evaluation (e.g., "Test", "Validation")
#' @param analysisId Identifier for the analysis (e.g., "dev", "val_original", "val_new")
#' @return A tibble containing the processed results
#' @export
getAnalysisResults <- function(databasePath, evaluationType, analysisId) {
  if (!file.exists(databasePath)) {
    stop("Database file not found at path: ", databasePath)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), databasePath)
  on.exit(DBI::dbDisconnect(con))

  cohortLookup <- DBI::dbGetQuery(
    con,
    "SELECT cohort_id, cohort_definition_id
    FROM cohorts;"
  )

  covariateSettings <- DBI::dbGetQuery(
    con,
    "SELECT covariate_setting_id, covariate_settings_json FROM covariate_settings;"
  )

  determineFeatureSet <- function(jsonText) {
    if (is.na(jsonText) || !nzchar(jsonText)) {
      return("Full")
    }
    parsed <- tryCatch(
      jsonlite::fromJSON(jsonText, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return("Full")
    }

    entries <- parsed
    if (!is.list(entries)) {
      entries <- list(entries)
    } else if (!length(entries)) {
      entries <- list()
    } else if (!is.null(entries$attr_fun)) {
      entries <- list(entries)
    }

    hasParsimoniousSignals <- any(vapply(
      entries,
      function(entry) {
        if (!is.list(entry)) {
          return(FALSE)
        }
        attrFun <- entry[["attr_fun"]]
        if (
          !is.null(attrFun) &&
            attrFun == "PatientLevelPrediction::getCohortCovariateData"
        ) {
          return(TRUE)
        }
        analysisId <- entry[["analysisId"]]
        if (!is.null(analysisId)) {
          analysisIdNumeric <- suppressWarnings(as.numeric(analysisId))
          if (!is.na(analysisIdNumeric) && analysisIdNumeric == 699) {
            return(TRUE)
          }
        }
        covariateName <- entry[["covariateName"]]
        if (
          !is.null(covariateName) &&
            grepl("^history of", covariateName, ignore.case = TRUE)
        ) {
          return(TRUE)
        }
        return(FALSE)
      },
      FUN.VALUE = logical(1)
    ))

    if (hasParsimoniousSignals) "Parsimonious" else "Full"
  }

  covariateLookup <- covariateSettings |>
    dplyr::mutate(
      featureSet = vapply(
        .data$covariate_settings_json,
        determineFeatureSet,
        FUN.VALUE = character(1)
      )
    ) |>
    dplyr::select("covariate_setting_id", "featureSet")
  devPeriodLookup <- DBI::dbGetQuery(
    con,
    "
    SELECT
      md.model_design_id,
      pds.plp_data_settings_json
    FROM model_designs md
    INNER JOIN plp_data_settings pds ON md.plp_data_setting_id = pds.plp_data_setting_id;
  "
  ) |>
    dplyr::mutate(
      parsedJson = lapply(
        .data$plp_data_settings_json,
        function(x) jsonlite::fromJSON(x)
      ),
      devStartDate = as.Date(
        sapply(
          .data$parsedJson,
          function(x) x$studyStartDate
        ),
        format = "%Y%m%d"
      ),
      devEndDate = as.Date(
        sapply(
          .data$parsedJson,
          function(x) x$studyEndDate
        ),
        format = "%Y%m%d"
      ),
      sampleSize = sapply(
        .data$parsedJson,
        function(x) ifelse(is.null(x$sampleSize), NA, x$sampleSize)
      ),
      devPeriod = dplyr::case_when(
        !is.na(sampleSize) &
          lubridate::interval(devStartDate, devEndDate) > lubridate::days(360) ~
          "Full Year 2020 (Sampled)",
        lubridate::interval(devStartDate, devEndDate) <= lubridate::days(92) ~
          "First 3 Months",
        lubridate::interval(devStartDate, devEndDate) <= lubridate::days(184) ~
          "First 6 Months",
        lubridate::interval(devStartDate, devEndDate) <= lubridate::days(275) ~
          "First 9 Months",
        TRUE ~ "Full Year 2020 (Full Pop.)"
      )
    ) |>
    dplyr::select("model_design_id", "devPeriod", "devStartDate", "devEndDate")

  sqlQuery <- "
    SELECT p.performance_id,
           md.model_design_id,
           p.target_id,
           p.outcome_id,
           pds.plp_data_settings_json,
           ms.model_settings_json,
           md.covariate_setting_id,
           es.metric, es.value
    FROM evaluation_statistics es
    INNER JOIN performances p ON es.performance_id = p.performance_id
    INNER JOIN model_designs md ON p.model_design_id = md.model_design_id
    INNER JOIN plp_data_settings pds ON p.plp_data_setting_id = pds.plp_data_setting_id
    LEFT JOIN model_settings ms ON md.model_setting_id = ms.model_setting_id
    WHERE es.evaluation = ? AND es.metric IN ('AUROC',
                                              'populationSize',
                                              'outcomeCount',
                                              'Eavg',
                                              'calibrationInLarge mean prediction',
                                              'calibrationInLarge observed risk');
  "
  resultsFromDb <- DBI::dbGetQuery(con, sqlQuery, params = list(evaluationType))

  if (nrow(resultsFromDb) == 0) {
    warning("Query returned no results for analysis: ", analysisId)
    return(dplyr::tibble())
  }

  safeExtract <- function(jsonObj, path) {
    if (!is.list(jsonObj)) {
      return(NA_character_)
    }
    val <- purrr::pluck(jsonObj, !!!path)
    return(if (is.null(val)) NA_character_ else as.character(val))
  }

  jsonPlpSettings <- lapply(resultsFromDb$plp_data_settings_json, function(x) {
    if (!is.na(x)) jsonlite::fromJSON(x) else NA
  })
  startDates <- sapply(
    jsonPlpSettings,
    safeExtract,
    path = list("studyStartDate")
  )
  endDates <- sapply(jsonPlpSettings, safeExtract, path = list("studyEndDate"))

  jsonModelSettings <- lapply(resultsFromDb$model_settings_json, function(x) {
    if (!is.na(x)) jsonlite::fromJSON(x) else NA
  })
  modelNames <- sapply(
    jsonModelSettings,
    safeExtract,
    path = list("param", "attr_settings", "name")
  )
  # Attempt to retrieve recalibrationPeriod from model settings if present
  recalibrationPeriods <- vapply(
    jsonModelSettings,
    function(js) {
      val <- safeExtract(js, path = list("param", "recalibrationPeriod"))
      if (is.na(val) || val == "") {
        val <- safeExtract(js, path = list("recalibrationPeriod"))
      }
      if (is.na(val) || val == "") {
        val <- safeExtract(
          js,
          path = list("param", "attr_settings", "recalibrationPeriod")
        )
      }
      if (is.na(val) || val == "") {
        val <- safeExtract(
          js,
          path = list("modelSettings", "recalibrationPeriod")
        )
      }
      if (is.na(val) || val == "") NA_character_ else as.character(val)
    },
    character(1)
  )

  resultsProcessed <- resultsFromDb |>
    dplyr::mutate(
      studyStartDate = startDates,
      studyEndDate = endDates,
      modelSettingName = ifelse(is.na(modelNames), "Existing GLM", modelNames),
      recalibrationPeriod = recalibrationPeriods
    ) |>
    dplyr::left_join(cohortLookup, by = c("target_id" = "cohort_id")) |>
    dplyr::rename(
      targetId = "cohort_definition_id"
    ) |>
    dplyr::mutate(
      targetName = dplyr::case_when(
        as.character(.data$targetId) == "12" ~ "Outpatient",
        as.character(.data$targetId) == "25" ~ "Inpatient",
        as.character(.data$targetId) == "31" ~ "CovidNew",
        TRUE ~ "UnknownTarget"
      )
    ) |>
    dplyr::left_join(cohortLookup, by = c("outcome_id" = "cohort_id")) |>
    dplyr::rename(
      outcomeId = "cohort_definition_id"
    ) |>
    dplyr::mutate(
      outcomeName = mapply(getOutcome, .data$outcomeId)
    ) |>
    dplyr::left_join(covariateLookup, by = "covariate_setting_id") |>
    dplyr::left_join(devPeriodLookup, by = "model_design_id")

  resultsWide <- resultsProcessed |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::mutate(
      AUROC = as.numeric(.data$AUROC),
      populationSize = as.integer(.data$populationSize),
      outcomeCount = as.integer(.data$outcomeCount),
      Eavg = as.numeric(.data$Eavg),
      meanPrediction = as.numeric(.data$`calibrationInLarge mean prediction`),
      observedRisk = as.numeric(.data$`calibrationInLarge observed risk`),
      observedMeanDiff = .data$observedRisk - .data$meanPrediction,
      startDate = as.Date(.data$studyStartDate, format = "%Y%m%d"),
      endDate = as.Date(.data$studyEndDate, format = "%Y%m%d"),
      periodMidpoint = .data$startDate + (.data$endDate - .data$startDate) / 2
    )
  resultsFinal <- resultsWide |>
    dplyr::mutate(
      analysisId = !!analysisId,
      modelOrigin = dplyr::case_when(
        analysisId == "dev" ~ "New Covid",
        analysisId == "val_new" ~ "New Covid",
        analysisId == "val_original" ~ "Original Influenza",
        analysisId == "val_new_recalibrated" ~ "Recalibrated Influenza",
        analysisId == "val_rolling_recalibrated" ~ "Rolling Recalibrated Influenza",
        TRUE ~ "Unknown"
      ),
      # Only retain development period for New Covid models; otherwise set NA for clarity
      devPeriod = dplyr::if_else(
        .data$modelOrigin == "New Covid",
        .data$devPeriod,
        NA_character_
      ),
      wDev = wFromDevPeriod(.data$devPeriod),
      wRecal = wFromRecalPeriod(.data$recalibrationPeriod),
      # canonical model key (no outcome suffix by default)
      modelKeyBase = makeModelKey(
        .data$modelOrigin,
        .data$featureSet,
        .data$wDev,
        .data$wRecal
      ),
      modelKey = appendOutcomeSuffix(
        .data$modelKeyBase,
        .data$outcomeId,
        append = FALSE
      ),
      # prediction scale tag
      predictionsScale = dplyr::case_when(
        .data$modelOrigin == "Recalibrated Influenza" ~ "interceptSlope",
        TRUE ~ "raw"
      ),
      fullName = paste(
        mapply(getName, .data$targetId, .data$outcomeId),
        .data$featureSet,
        .data$modelOrigin,
        .data$devPeriod,
        sep = " | "
      ) |>
        stringr::str_replace_all(" \\| NA", ""),
      outcomeCode = dplyr::case_when(
        .data$outcomeId == 11 ~ "F",
        .data$outcomeId == 13 ~ "I",
        .data$outcomeId == 14 ~ "H",
        TRUE ~ ""
      ),
      legendLabel = dplyr::case_when(
        .data$modelOrigin == "New Covid" & .data$featureSet == "Parsimonious" ~
          paste0("covid", .data$outcomeCode),
        .data$modelOrigin == "New Covid" & .data$featureSet == "Full" ~
          paste0("dataDrivenCovid", .data$outcomeCode),
        .data$modelOrigin == "Original Influenza" ~
          mapply(
            getModelName,
            .data$modelSettingName,
            .data$targetId,
            .data$outcomeId
          ),
        .data$modelOrigin == "Recalibrated Influenza" ~
          mapply(
            getModelName,
            .data$modelSettingName,
            .data$targetId,
            .data$outcomeId
          )
      )
    ) |>
    dplyr::select(
      "performance_id",
      "analysisId",
      "modelOrigin",
      "recalibrationPeriod",
      "featureSet",
      "devPeriod",
      "startDate",
      "endDate",
      "periodMidpoint",
      "devStartDate",
      "devEndDate",
      "wDev",
      "wRecal",
      "targetId",
      "outcomeId",
      "targetName",
      "outcomeName",
      "fullName",
      "legendLabel",
      "AUROC",
      "populationSize",
      "outcomeCount",
      "Eavg",
      "meanPrediction",
      "observedRisk",
      "observedMeanDiff"
    ) |>
    dplyr::arrange(.data$startDate, .data$fullName)
  return(resultsFinal)
}

#' Compare discrimination metrics across two analyses
#'
#' @param results Tibble of model performance rows (e.g., `allResults`).
#' @param analysisIdA Identifier of the reference analysis (e.g., `"val_original"`).
#' @param analysisIdB Identifier of the comparison analysis (e.g., `"val_new_recalibrated"`).
#' @param metrics Character vector of metric columns to compare. Defaults to `"AUROC"`.
#' @param joinVars Character vector of columns used to align the rows between analyses.
#'   Defaults to matching on `targetId`, `outcomeId`, `legendLabel`, `startDate`, and `endDate`.
#'
#' @return A tibble containing the matched rows with metric values from both analyses
#'   and a `_diff` column (analysis B minus analysis A) for each requested metric.
#' @export
compareDiscrimination <- function(
  results,
  analysisIdA,
  analysisIdB,
  metrics = "AUROC",
  joinVars = c("targetId", "outcomeId", "legendLabel", "startDate", "endDate")
) {
  missingMetrics <- setdiff(metrics, colnames(results))
  if (length(missingMetrics) > 0) {
    stop(
      "Metrics not found in results: ",
      paste(missingMetrics, collapse = ", ")
    )
  }

  missingJoinVars <- setdiff(joinVars, colnames(results))
  if (length(missingJoinVars) > 0) {
    stop(
      "Join variables not found in results: ",
      paste(missingJoinVars, collapse = ", ")
    )
  }

  dataA <- results |>
    dplyr::filter(.data$analysisId == analysisIdA) |>
    dplyr::select(dplyr::all_of(joinVars), dplyr::all_of(metrics)) |>
    dplyr::rename_with(
      ~ paste0(.x, "_", analysisIdA),
      dplyr::all_of(metrics)
    )

  dataB <- results |>
    dplyr::filter(.data$analysisId == analysisIdB) |>
    dplyr::select(dplyr::all_of(joinVars), dplyr::all_of(metrics)) |>
    dplyr::rename_with(
      ~ paste0(.x, "_", analysisIdB),
      dplyr::all_of(metrics)
    )

  joined <- dplyr::inner_join(dataA, dataB, by = joinVars)

  if (nrow(joined) == 0) {
    warning(
      "No overlapping rows found between analyses '",
      analysisIdA,
      "' and '",
      analysisIdB,
      "' using joinVars: ",
      paste(joinVars, collapse = ", ")
    )
    return(joined)
  }

  for (metric in metrics) {
    joined[[paste0(metric, "_diff")]] <-
      joined[[paste0(metric, "_", analysisIdB)]] -
      joined[[paste0(metric, "_", analysisIdA)]]
  }

  joined |>
    dplyr::arrange(dplyr::across(dplyr::all_of(joinVars)))
}

# helper for plotting
filterComparisonData <- function(
  allResults,
  outcomes,
  modelOriginsToCompare,
  featureSetsToCompare,
  devPeriodsToCompare
) {
  resultsToPlot <- allResults |>
    dplyr::filter(.data$analysisId != "dev")

  devPerformance <- allResults |>
    dplyr::filter(.data$analysisId == "dev")

  if (!is.null(outcomes)) {
    resultsToPlot <- resultsToPlot |>
      dplyr::filter(.data$outcomeName %in% outcomes)
    devPerformance <- devPerformance |>
      dplyr::filter(.data$outcomeName %in% outcomes)
  }
  resultsToPlot <- resultsToPlot |>
    dplyr::filter(
      .data$modelOrigin %in% modelOriginsToCompare,
      .data$featureSet %in% featureSetsToCompare
    )
  devPerformance <- devPerformance |>
    dplyr::filter(
      .data$modelOrigin %in% modelOriginsToCompare,
      .data$featureSet %in% featureSetsToCompare
    )
  if (!is.null(devPeriodsToCompare)) {
    resultsToPlot <- resultsToPlot |>
      dplyr::filter(
        .data$devPeriod %in%
          devPeriodsToCompare |
          .data$modelOrigin != "New Covid"
      )
    devPerformance <- devPerformance |>
      dplyr::filter(
        .data$devPeriod %in%
          devPeriodsToCompare |
          .data$modelOrigin != "New Covid"
      )
  }
  list(resultsToPlot = resultsToPlot, devPerformance = devPerformance)
}

# helper for plotting
computeDevPlotPerformance <- function(devPerformance, metric) {
  if (metric %in% names(devPerformance)) {
    devPerformance |>
      dplyr::filter(
        !is.na(.data$devStartDate),
        !is.na(.data$devEndDate),
        !is.na(.data[[metric]])
      )
  } else {
    devPerformance[0, ]
  }
}

# helper for plotting
computeYLimits <- function(resultsToPlot, devPerformance, metric) {
  allVals <- c(
    resultsToPlot[[metric]],
    if (metric %in% names(devPerformance)) {
      devPerformance[[metric]]
    } else {
      numeric(0)
    }
  )

  minVal <- min(allVals, na.rm = TRUE)
  maxVal <- max(allVals, na.rm = TRUE)
  padding <- (maxVal - minVal) * 0.05
  if (padding == 0) {
    padding <- 0.05
  }

  if (identical(toupper(metric), "AUROC")) {
    yLower <- max(0.5, minVal - padding)
    yUpper <- min(1.0, maxVal + padding)
  } else {
    yLower <- minVal - padding
    yUpper <- maxVal + padding
  }
  list(yLower = yLower, yUpper = yUpper)
}

# helper for plotting
computeGlobalXLimits <- function(resultsToPlot, devPerformance) {
  globalXmin <- min(
    c(resultsToPlot$startDate, devPerformance$devStartDate),
    na.rm = TRUE
  )
  globalXMax <- max(
    c(resultsToPlot$endDate, devPerformance$devEndDate),
    na.rm = TRUE
  )
  list(globalXmin = globalXmin, globalXMax = globalXMax)
}

# helper for plotting
buildFeatureSetPalette <- function(resultsToPlot, devPerformance) {
  allLevels <- sort(unique(c(resultsToPlot$fullName, devPerformance$fullName)))
  featureKey <- dplyr::bind_rows(
    resultsToPlot[, c("fullName", "featureSet")],
    devPerformance[, c("fullName", "featureSet")]
  ) |>
    dplyr::distinct(.data$fullName, .data$featureSet)

  colorMap <- tibble::tibble(fullName = allLevels) |>
    dplyr::left_join(featureKey, by = "fullName") |>
    dplyr::mutate(
      color = dplyr::case_when(
        tolower(.data$featureSet) == "parsimonious" ~ "#0072B2FF",
        TRUE ~ "#D55E00FF"
      )
    )

  colorPalette <- colorMap$color
  names(colorPalette) <- colorMap$fullName
  list(allLevels = allLevels, colorPalette = colorPalette)
}

# helper for plotting
balanceTwoColumns <- function(widthPt, nLabels) {
  leftTarget <- ceiling(nLabels / 2)
  rightTarget <- nLabels - leftTarget
  ord <- order(widthPt, decreasing = TRUE)

  leftIdx <- integer(0)
  rightIdx <- integer(0)
  leftMax <- 0
  rightMax <- 0

  for (i in ord) {
    canLeft <- length(leftIdx) < leftTarget
    canRight <- length(rightIdx) < rightTarget
    if (canLeft && canRight) {
      if (leftMax <= rightMax) {
        leftIdx <- c(leftIdx, i)
        leftMax <- max(leftMax, widthPt[i])
      } else {
        rightIdx <- c(rightIdx, i)
        rightMax <- max(rightMax, widthPt[i])
      }
    } else if (canLeft) {
      leftIdx <- c(leftIdx, i)
      leftMax <- max(leftMax, widthPt[i])
    } else {
      rightIdx <- c(rightIdx, i)
      rightMax <- max(rightMax, widthPt[i])
    }
  }

  list(
    leftIdx = leftIdx,
    rightIdx = rightIdx,
    leftMax = leftMax,
    rightMax = rightMax
  )
}

# helper for plotting
autoPlaceLegendNPC <- function(
  resultsFacet,
  metric,
  yLower,
  yUpper,
  vpHeight,
  legendCorner
) {
  # Padding constants (NPC)
  xPadRight <- 0.01
  yPadTop <- 0.02
  yPadBottom <- 0.05

  npcx <- 1 - xPadRight

  npcyFixed <- switch(
    tolower(legendCorner),
    "tr" = 1 - yPadTop - vpHeight,
    "tl" = 1 - yPadTop - vpHeight,
    "bl" = 0 + yPadBottom,
    "br" = 0 + yPadBottom,
    "auto" = NA_real_
  )
  if (!is.na(npcyFixed)) {
    npcy <- npcyFixed
    return(list(npcx = npcx, npcy = max(0, min(1 - vpHeight, npcy))))
  }

  xNum <- as.numeric(resultsFacet$periodMidpoint)
  yVec <- resultsFacet[[metric]]
  ok <- is.finite(xNum) & is.finite(yVec)
  xNum <- xNum[ok]
  yVec <- yVec[ok]

  if (length(yVec) == 0L) {
    npcy <- 0 + yPadBottom
  } else {
    xCut <- stats::quantile(xNum, probs = 0.8, na.rm = TRUE, type = 7)
    rightY <- yVec[xNum >= xCut]
    if (length(rightY) == 0L) {
      rightY <- yVec
    }

    rng <- max(yUpper - yLower, .Machine$double.eps)
    scaledY <- (rightY - yLower) / rng

    # Ensure the occupancy check accounts for both the legend height and
    # the vertical padding, so the legend box does not overlap lines.
    margin <- 0.02
    bottomCap <- min(yPadBottom + vpHeight + margin, 0.5)
    topFloor <- max(1 - (yPadTop + vpHeight + margin), 0.5)

    occupiedBottom <- any(scaledY <= bottomCap, na.rm = TRUE)
    occupiedTop <- any(scaledY >= topFloor, na.rm = TRUE)

    if (occupiedBottom && !occupiedTop) {
      npcy <- 1 - yPadTop - vpHeight
    } else if (!occupiedBottom && occupiedTop) {
      npcy <- 0 + yPadBottom
    } else if (occupiedBottom && occupiedTop) {
      nBottom <- sum(scaledY <= bottomCap, na.rm = TRUE)
      nTop <- sum(scaledY >= topFloor, na.rm = TRUE)
      npcy <- if (nBottom >= nTop) 1 - yPadTop - vpHeight else 0 + yPadBottom
    } else {
      npcy <- 0 + yPadBottom
    }
  }

  npcy <- max(0, min(1 - vpHeight, npcy))
  list(npcx = npcx, npcy = npcy)
}

# helper for plotting
buildFacetLegendGrobs <- function(
  resultsToPlot,
  facetBy,
  colorPalette,
  metric,
  yLower,
  yUpper,
  legendCorner
) {
  resultsToPlot |>
    dplyr::distinct(
      !!!rlang::syms(facetBy),
      .data$legendLabel,
      .data$fullName
    ) |>
    dplyr::group_by(!!!rlang::syms(facetBy)) |>
    dplyr::group_modify(
      ~ {
        df <- .x
        if (nrow(df) == 0L) {
          return(tibble::tibble(
            grob = list(grid::nullGrob()),
            vpWidth = 0.18,
            vpHeight = 0.12,
            npcx = 1 - 0.01,
            npcy = 0 + 0.05,
            side = "bottom",
            npcyAdj = 0 + 0.05
          ))
        }

        fontSize <- 16
        baseFamily <- ggplot2::theme_get()$text$family
        if (is.null(baseFamily) || baseFamily == "") {
          baseFamily <- "sans"
        }

        measureWidthPt <- function(s) {
          grid::convertWidth(
            grid::grobWidth(
              grid::textGrob(
                s,
                gp = grid::gpar(
                  fontsize = fontSize,
                  fontface = 2,
                  family = baseFamily
                )
              )
            ),
            "pt",
            valueOnly = TRUE
          )
        }
        widthPt <- vapply(df$legendLabel, measureWidthPt, numeric(1))
        nLabels <- length(widthPt)

        colorForIndex <- function(idx) {
          unname(vapply(
            idx,
            function(i) colorPalette[[df$fullName[i]]],
            character(1)
          ))
        }

        if (nLabels <= 2L) {
          cells <- Map(
            function(lbl, col) {
              grid::textGrob(
                label = lbl,
                x = 0,
                y = 1,
                hjust = 0,
                vjust = 1,
                gp = grid::gpar(
                  col = col,
                  fontsize = fontSize,
                  fontface = "bold",
                  family = baseFamily
                )
              )
            },
            df$legendLabel,
            colorForIndex(seq_len(nLabels))
          )
          gTable <- gridExtra::arrangeGrob(grobs = cells, ncol = 1)
          g <- grid::grobTree(gTable, vp = grid::viewport(clip = "on"))
          vpWidth <- 0.22
          vpHeight <- 0.12 + 0.10 * (nLabels - 1)
        } else {
          parts <- balanceTwoColumns(widthPt, nLabels)
          makeCell <- function(txt, col) {
            gridtext::textbox_grob(
              txt,
              x = 0,
              y = 1,
              hjust = 0,
              vjust = 1,
              width = grid::unit(1, "npc"),
              gp = grid::gpar(
                col = col,
                fontsize = fontSize,
                fontface = "bold",
                family = baseFamily
              ),
              box_gp = grid::gpar(col = NA, fill = NA),
              padding = grid::unit(c(0, 0, 0, 0), "pt"),
              r = grid::unit(0, "pt")
            )
          }
          leftGrobs <- Map(
            makeCell,
            df$legendLabel[parts$leftIdx],
            colorForIndex(parts$leftIdx)
          )
          rightGrobs <- Map(
            makeCell,
            df$legendLabel[parts$rightIdx],
            colorForIndex(parts$rightIdx)
          )

          nRows <- max(length(leftGrobs), length(rightGrobs))
          if (length(leftGrobs) < nRows) {
            leftGrobs <- c(
              leftGrobs,
              replicate(nRows - length(leftGrobs), grid::nullGrob(), FALSE)
            )
          }
          if (length(rightGrobs) < nRows) {
            rightGrobs <- c(
              rightGrobs,
              replicate(nRows - length(rightGrobs), grid::nullGrob(), FALSE)
            )
          }

          gTable <- gridExtra::arrangeGrob(
            grobs = as.list(c(rbind(leftGrobs, rightGrobs))),
            ncol = 2,
            widths = grid::unit.c(
              grid::unit(max(parts$leftMax, 1e-6), "null"),
              grid::unit(max(parts$rightMax, 1e-6), "null")
            )
          )
          g <- grid::grobTree(gTable, vp = grid::viewport(clip = "on"))
          vpWidth <- 0.40
          vpHeight <- min(0.30, 0.10 + 0.12 * nRows)
        }

        # Determine placement per facet
        facetKeys <- .y
        resultsFacet <- if (!is.null(facetBy) && length(facetBy) > 0) {
          dplyr::semi_join(resultsToPlot, facetKeys, by = facetBy)
        } else {
          resultsToPlot
        }

        pos <- autoPlaceLegendNPC(
          resultsFacet,
          metric,
          yLower,
          yUpper,
          vpHeight,
          legendCorner
        )
        side <- if (!is.na(pos$npcy) && pos$npcy >= 0.5) "top" else "bottom"
        # Anchor the top legend close to the top border (respect small padding),
        # using top alignment so the grob sits as high as possible without
        # forcing a larger clearance than necessary.
        topPad <- 0.02
        npcyAdj <- if (side == "top") {
          pmin(1 - 1e-3, 1 - topPad)
        } else {
          pos$npcy
        }
        tibble::tibble(
          grob = list(g),
          vpWidth = vpWidth,
          vpHeight = vpHeight,
          npcx = pos$npcx,
          npcy = pos$npcy,
          side = side,
          npcyAdj = npcyAdj
        )
      }
    ) |>
    dplyr::ungroup()
}

# helper for plotting
buildBasePlot <- function(
  resultsToPlot,
  devPlotPerformance,
  yVar,
  globalXmin,
  globalXMax,
  yLower,
  yUpper,
  metric,
  hasDevSegments,
  title,
  connectDevToVal = TRUE
) {
  ggplot2::ggplot(
    data = resultsToPlot,
    mapping = ggplot2::aes(
      x = .data$periodMidpoint,
      y = !!yVar,
      colour = .data$fullName
    )
  ) +
    ggplot2::geom_line(
      linewidth = 1.2,
      mapping = ggplot2::aes(linetype = "Validation Performance")
    ) +
    ggplot2::geom_point(size = 3.5) +
    {
      # Extend the development segment so it meets the first validation midpoint
      if (nrow(devPlotPerformance) > 0) {
        firstVal <- resultsToPlot |>
          dplyr::group_by(.data$fullName) |>
          dplyr::summarise(firstMid = suppressWarnings(min(.data$periodMidpoint, na.rm = TRUE)), .groups = "drop")
        devSegs <- devPlotPerformance |>
          dplyr::left_join(firstVal, by = "fullName") |>
          dplyr::mutate(
            x_start_ext = .data$devStartDate,
            x_end_ext = dplyr::coalesce(.data$firstMid, .data$devEndDate),
            x_end_ext = dplyr::if_else(!is.na(.data$devEndDate) & !is.na(.data$x_end_ext) & .data$x_end_ext < .data$devEndDate,
                                       .data$devEndDate, .data$x_end_ext)
          )
        ggplot2::geom_segment(
          data = devSegs,
          mapping = ggplot2::aes(
            x = .data$x_start_ext,
            xend = .data$x_end_ext,
            y = !!yVar,
            yend = !!yVar,
            colour = .data$fullName,
            linetype = "Development Performance"
          ),
          linewidth = 1.2,
          lineend = "butt"
        )
      }
    } +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y",
      minor_breaks = NULL,
      limits = c(globalXmin, globalXMax),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::coord_cartesian(ylim = c(yLower, yUpper), clip = "off") +
    ggplot2::labs(title = title, x = "Time", y = metric) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c(
        "Validation Performance" = "solid",
        "Development Performance" = "dashed"
      ),
      breaks = c("Validation Performance", "Development Performance"),
      guide = if (hasDevSegments) "legend" else "none"
    ) +
    ggplot2::theme(
      plot.margin = grid::unit(c(5.5, 10, 5.5, 5.5), "pt"),
      axis.text.x = ggplot2::element_text(
        size = 18,
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      axis.title.x = ggplot2::element_text(
        size = 24,
        margin = ggplot2::margin(t = 6)
      ),
      axis.title.y = ggplot2::element_text(
        size = 24,
        margin = ggplot2::margin(r = 6)
      ),
      axis.text.y = ggplot2::element_text(size = 18),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 20, face = "bold"),
      plot.title = ggplot2::element_text(size = 38, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = grid::unit(2, "cm"),
      legend.title = ggplot2::element_text(size = 20, face = "bold"),
      legend.text = ggplot2::element_text(size = 18),
      legend.spacing.x = grid::unit(6, "pt"),
      legend.spacing.y = grid::unit(2, "pt"),
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))
    )
}

.defaultUsMilestones <- function() {
  # Dates use the national week-ending dates cited by CDC MMWR/updates
  # type: "vaccine" (broad adult eligibility) or "variant" (national predominance or CDC-declared predominant)
  tibble::tibble(
    date = as.Date(c(
      "2020-12-14", # First U.S. vaccine administered (Pfizer-BioNTech)
      "2021-04-01", # Alpha >50% nationally (CDC MMWR)
      "2021-06-26", # Delta >50% nationally (CDC MMWR)
      "2021-12-25" # Omicron (BA.1) >50% nationally (CDC)
    )),
    type = c("vaccine", "variant", "variant", "variant"),
    label = c(
      "Vaccine",
      "Alpha",
      "Delta",
      "Omicron"
    )
  )
}

ptToMm <- function(pt) pt * 25.4 / 72

.pickMilestoneLabelYLikeLegend <- function(
  resultsData,
  metric,
  yLower,
  yUpper,
  padFrac = 0.06,
  legendCorner = "auto",
  legendSide = NULL,
  vpHeight = 0
) {
  yTop <- yUpper - padFrac * (yUpper - yLower)
  yBot <- yLower + padFrac * (yUpper - yLower)

  if (!is.null(legendSide)) {
    ls <- tolower(legendSide)
    return(
      if (ls == "top") {
        list(y = yTop, vjust = 1, side = "top")
      } else {
        list(y = yBot, vjust = 0, side = "bottom")
      }
    )
  }

  if (!identical(tolower(legendCorner), "auto")) {
    lc <- tolower(legendCorner)
    return(
      if (lc %in% c("tl", "tr")) {
        list(y = yTop, vjust = 1, side = "top")
      } else {
        list(y = yBot, vjust = 0, side = "bottom")
      }
    )
  }

  ok <- !is.null(resultsData) &&
    !is.null(metric) &&
    metric %in% names(resultsData)
  if (!ok) {
    return(list(y = yBot, vjust = 0, side = "bottom"))
  }

  pos <- autoPlaceLegendNPC(
    resultsFacet = resultsData,
    metric = metric,
    yLower = yLower,
    yUpper = yUpper,
    vpHeight = vpHeight,
    legendCorner = "auto"
  )
  if (is.finite(pos$npcy) && pos$npcy >= 0.5) {
    list(y = yTop, vjust = 1, side = "top")
  } else {
    list(y = yBot, vjust = 0, side = "bottom")
  }
}

# Decide milestone label side (top/bottom) per facet by
# inspecting headroom near milestone dates.
.pickMilestoneSide <- function(
  resultsFacet,
  metric,
  yLower,
  yUpper,
  milestoneDates = NULL,
  windowDays = 60
) {
  if (is.null(resultsFacet) || is.null(metric) || !(metric %in% names(resultsFacet))) {
    return("bottom")
  }
  x <- resultsFacet$periodMidpoint
  y <- resultsFacet[[metric]]
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (!length(y)) return("bottom")

  # Focus on data near milestone dates to decide where there is more free space
  if (!is.null(milestoneDates) && length(milestoneDates)) {
    wd <- as.numeric(windowDays)
    nearMask <- Reduce(
      `|`,
      lapply(as.Date(milestoneDates), function(d) abs(as.numeric(x - d)) <= wd),
      init = rep(FALSE, length(x))
    )
    yUse <- if (any(nearMask)) y[nearMask] else y
  } else {
    yUse <- y
  }

  # Compare available vertical room to top vs bottom using robust quantiles
  rng <- max(yUpper - yLower, .Machine$double.eps)
  topRoom <- yUpper - stats::quantile(yUse, probs = 0.9, na.rm = TRUE, type = 7)
  botRoom <- stats::quantile(yUse, probs = 0.1, na.rm = TRUE, type = 7) - yLower

  if (!is.finite(topRoom)) topRoom <- 0
  if (!is.finite(botRoom)) botRoom <- 0

  # Prefer the side with meaningful headroom; fall back to larger room
  minRoom <- 0.03 * rng
  if (topRoom < minRoom && botRoom >= minRoom) return("bottom")
  if (botRoom < minRoom && topRoom >= minRoom) return("top")
  if (botRoom >= topRoom) "bottom" else "top"
}

.addMilestoneLayers <- function(
  p,
  milestones,
  yLower,
  yUpper,
  xRange = NULL,
  vaccineLineColor = "#EF4444FF",
  variantLineColor = "#9CA3AFFF",
  showLabels = TRUE,
  labelPt = 16,
  vaccineLineMm = 1.0,
  variantLineMm = 0.8,
  topPadFrac = 0.06,
  labelAngle = 90,
  legendCorner = "auto",
  resultsData = NULL,
  metric = NULL,
  facetBy = NULL, # NEW
  legendPlacement = NULL # NEW: df with facet keys + side + vpHeight
) {
  if (is.null(milestones) || !nrow(milestones)) {
    return(p)
  }

  # Optional culling by x-range
  if (!is.null(xRange) && length(xRange) == 2L && all(is.finite(xRange))) {
    milestones <- milestones[
      milestones$date >= xRange[1] & milestones$date <= xRange[2],
      ,
      drop = FALSE
    ]
    if (!nrow(milestones)) return(p)
  }

  vaccines <- milestones[milestones$type == "vaccine", , drop = FALSE]
  variants <- milestones[milestones$type == "variant", , drop = FALSE]

  # Pre-compute NPC X for label alignment so labels stay centered on the line
  # and we can keep them inside the panel using NPC Y.
  panelMin <- as.numeric(xRange[1])
  panelMax <- as.numeric(xRange[2])
  spanNum <- max(panelMax - panelMin, .Machine$double.eps)
  if (nrow(vaccines)) {
    vaccines$npcx <- (as.numeric(vaccines$date) - panelMin) / spanNum
  }
  if (nrow(variants)) {
    variants$npcx <- (as.numeric(variants$date) - panelMin) / spanNum
  }

  labelMm <- ptToMm(labelPt)

  # Helper to build layers for a single facet side
  buildLayers <- function(side, vpH, facetKeys = NULL) {
    isTop <- identical(tolower(side), "top")
    rng <- yUpper - yLower
    # Anchor labels using NPC Y close to borders while keeping inside
    topPadNPC <- 0.01
    botPadNPC <- 0.01
    npcy <- if (isTop) (1 - topPadNPC) else botPadNPC
    vjustLB <- if (isTop) 1 else 0

    vG <- list()
    if (nrow(vaccines)) {
      vDf <- vaccines
      if (!is.null(facetKeys)) {
        vDf <- cbind(facetKeys[rep(1, nrow(vDf)), , drop = FALSE], vDf)
      }
      vDf$npcy <- npcy
      vG <- list(
        ggplot2::geom_vline(
          data = vDf,
          mapping = ggplot2::aes(xintercept = .data$date),
          colour = vaccineLineColor,
          linewidth = vaccineLineMm,
          lineend = "butt"
        ),
        if (showLabels) {
          ggpp::geom_text_npc(
            data = vDf,
            mapping = ggplot2::aes(
              npcx = .data$npcx,
              npcy = .data$npcy,
              label = .data$label
            ),
            colour = vaccineLineColor,
            angle = labelAngle,
            vjust = vjustLB,
            hjust = 0.5,
            size = labelMm,
            lineheight = 0.9,
            check_overlap = TRUE,
            na.rm = TRUE
          )
        }
      )
    }

    varG <- list()
    if (nrow(variants)) {
      aDf <- variants
      if (!is.null(facetKeys)) {
        aDf <- cbind(facetKeys[rep(1, nrow(aDf)), , drop = FALSE], aDf)
      }
      aDf$npcy <- npcy
      varG <- list(
        ggplot2::geom_vline(
          data = aDf,
          mapping = ggplot2::aes(xintercept = .data$date),
          colour = variantLineColor,
          linewidth = variantLineMm,
          linetype = "longdash",
          lineend = "butt"
        ),
        if (showLabels) {
          ggpp::geom_text_npc(
            data = aDf,
            mapping = ggplot2::aes(
              npcx = .data$npcx,
              npcy = .data$npcy,
              label = .data$label
            ),
            colour = variantLineColor,
            angle = labelAngle,
            vjust = vjustLB,
            hjust = 0.5,
            size = labelMm,
            lineheight = 0.9,
            check_overlap = TRUE,
            na.rm = TRUE
          )
        }
      )
    }

    c(vG, varG)
  }

  # If faceted and legendPlacement provided, add per-facet layers
  if (!is.null(facetBy) && length(facetBy) > 0 && !is.null(legendPlacement)) {
    # legendPlacement must have columns facetBy, side, vpHeight
    reqCols <- c(facetBy, "side", "vpHeight")
    if (!all(reqCols %in% names(legendPlacement))) {
      stop("legendPlacement must include: ", paste(reqCols, collapse = ", "))
    }

    # Build list of layers for each facet level
    for (i in seq_len(nrow(legendPlacement))) {
      fk <- legendPlacement[i, facetBy, drop = FALSE]
      # Determine results for this facet
      resFacet <- if (!is.null(resultsData)) {
        tryCatch({
          if (!is.null(facetBy) && length(facetBy) > 0) {
            dplyr::semi_join(resultsData, fk, by = facetBy)
          } else {
            resultsData
          }
        }, error = function(e) NULL)
      } else {
        NULL
      }

      sideI <- .pickMilestoneSide(
        resultsFacet = resFacet,
        metric = metric,
        yLower = yLower,
        yUpper = yUpper,
        milestoneDates = milestones$date,
        windowDays = 45
      )
      vpHI <- legendPlacement$vpHeight[i]
      layersI <- buildLayers(sideI, vpHI, facetKeys = fk)
      for (g in layersI) {
        if (!is.null(g)) p <- p + g
      }
    }
    return(p)
  }

  # Not faceted or no placement provided: fall back to automatic logic using vpHeight=median legend height if available
  vpH <- if (
    !is.null(legendPlacement) && "vpHeight" %in% names(legendPlacement)
  ) {
    stats::median(legendPlacement$vpHeight, na.rm = TRUE)
  } else {
    0
  }

  # Not faceted: decide side from data near milestones
  side0 <- .pickMilestoneSide(
    resultsFacet = resultsData,
    metric = metric,
    yLower = yLower,
    yUpper = yUpper,
    milestoneDates = milestones$date,
    windowDays = 45
  )
  layers <- buildLayers(side0, vpH, facetKeys = NULL)
  for (g in layers) {
    if (!is.null(g)) p <- p + g
  }
  p
}

computeOutcomeRateSummary <- function(resultsToPlot, facetBy = NULL) {
  facetVars <- if (is.null(facetBy)) character(0) else facetBy
  groupVars <- c(facetVars, "periodMidpoint")

  resultsToPlot |>
    dplyr::filter(
      is.finite(.data$observedRisk),
      is.finite(.data$populationSize),
      .data$populationSize > 0
    ) |>
    dplyr::group_by(!!!rlang::syms(groupVars)) |>
    dplyr::summarise(
      risk = stats::weighted.mean(
        .data$observedRisk,
        w = .data$populationSize,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

addOutcomeRateEdgeTicksLayer <- function(
  resultsToPlot,
  facetBy = NULL,
  yLower,
  yUpper,
  legendCorner = "auto",
  metric = "Eavg",
  color = "#6B7280FF",
  alpha = 0.9,
  linewidth = 0.8,
  padFrac = 0.06,
  bandFrac = 0.12,
  minLenFrac = 0.15,
  legendPlacement = NULL
) {
  rateDf <- computeOutcomeRateSummary(resultsToPlot, facetBy = facetBy)
  if (!nrow(rateDf)) {
    return(list())
  }

  rng <- yUpper - yLower
  if (!is.finite(rng) || rng <= 0) {
    return(list())
  }

  rMin <- min(rateDf$risk, na.rm = TRUE)
  rMax <- max(rateDf$risk, na.rm = TRUE)
  eps <- .Machine$double.eps
  norm <- if ((rMax - rMin) > eps) {
    (rateDf$risk - rMin) / (rMax - rMin)
  } else {
    rep(0.5, nrow(rateDf))
  }

  bandHeight <- bandFrac * rng
  len <- bandHeight * (minLenFrac + (1 - minLenFrac) * norm)

  # Faceted placement via legendPlacement
  if (!is.null(facetBy) && length(facetBy) > 0 && !is.null(legendPlacement)) {
    reqCols <- c(facetBy, "side")
    if (!all(reqCols %in% names(legendPlacement))) {
      stop("legendPlacement must include: ", paste(reqCols, collapse = ", "))
    }
    rateDf <- dplyr::left_join(
      rateDf,
      legendPlacement[, c(facetBy, "side"), drop = FALSE],
      by = facetBy
    )
    # Fallback: if side missing, compute from auto logic
    if (!("side" %in% names(rateDf)) || any(is.na(rateDf$side))) {
      pos <- .pickMilestoneLabelYLikeLegend(
        resultsToPlot,
        metric,
        yLower,
        yUpper,
        padFrac,
        legendCorner
      )
      rateDf$side <- ifelse(is.na(rateDf$side), pos$side, rateDf$side)
    }
    isTop <- tolower(rateDf$side) == "top"
    yBase <- ifelse(isTop, yUpper - padFrac * rng, yLower + padFrac * rng)
    rateDf$y <- yBase
    rateDf$yend <- ifelse(isTop, yBase - len, yBase + len)
  } else {
    # Non-faceted or no placement: use auto logic once
    pos <- .pickMilestoneLabelYLikeLegend(
      resultsToPlot,
      metric,
      yLower,
      yUpper,
      padFrac,
      legendCorner
    )
    isTop <- identical(pos$side, "top")
    yBase <- if (isTop) yUpper - padFrac * rng else yLower + padFrac * rng
    rateDf$y <- yBase
    rateDf$yend <- if (isTop) yBase - len else yBase + len
  }

  list(
    ggplot2::geom_segment(
      data = rateDf,
      mapping = ggplot2::aes(
        x = .data$periodMidpoint,
        xend = .data$periodMidpoint,
        y = .data$y,
        yend = .data$yend
      ),
      inherit.aes = FALSE,
      colour = color,
      alpha = alpha,
      linewidth = linewidth,
      show.legend = FALSE,
      lineend = "butt"
    )
  )
}

addOutcomeRateStripBarsLayer <- function(
  resultsToPlot,
  facetBy = NULL,
  yLower,
  yUpper,
  legendCorner = "auto",
  metric = "Eavg",
  fill = "#6B7280FF",
  alpha = 0.45,
  padFrac = 0.06,
  bandFrac = 0.12,
  barWidthDays = NULL,
  legendPlacement = NULL # optional: data.frame with facet keys + side
) {
  rateDf <- computeOutcomeRateSummary(resultsToPlot, facetBy = facetBy)
  if (!nrow(rateDf)) {
    return(list())
  }

  rng <- yUpper - yLower
  if (!is.finite(rng) || rng <= 0) {
    return(list())
  }

  # Decide side: use per-facet legendPlacement if provided; else auto logic once
  if (!is.null(facetBy) && length(facetBy) > 0 && !is.null(legendPlacement)) {
    req_cols <- c(facetBy, "side")
    if (!all(req_cols %in% names(legendPlacement))) {
      stop("legendPlacement must include: ", paste(req_cols, collapse = ", "))
    }
    rateDf <- dplyr::left_join(
      rateDf,
      legendPlacement[, req_cols, drop = FALSE],
      by = facetBy
    )
    if (!("side" %in% names(rateDf)) || any(is.na(rateDf$side))) {
      pos <- .pickMilestoneLabelYLikeLegend(
        resultsToPlot,
        metric,
        yLower,
        yUpper,
        padFrac,
        legendCorner
      )
      rateDf$side <- ifelse(is.na(rateDf$side), pos$side, rateDf$side)
    }
    isTop <- tolower(rateDf$side) == "top"
  } else {
    pos <- .pickMilestoneLabelYLikeLegend(
      resultsToPlot,
      metric,
      yLower,
      yUpper,
      padFrac,
      legendCorner
    )
    isTop <- rep(identical(pos$side, "top"), nrow(rateDf))
  }

  yBase <- ifelse(isTop, yUpper - padFrac * rng, yLower + padFrac * rng)
  bandHeight <- bandFrac * rng

  rMin <- min(rateDf$risk, na.rm = TRUE)
  rMax <- max(rateDf$risk, na.rm = TRUE)
  eps <- .Machine$double.eps
  norm <- if ((rMax - rMin) > eps) {
    (rateDf$risk - rMin) / (rMax - rMin)
  } else {
    rep(0.5, nrow(rateDf))
  }
  barHeight <- bandHeight * norm

  rateDf$ymin <- ifelse(isTop, yBase - bandHeight, yBase)
  rateDf$ymax <- ifelse(isTop, rateDf$ymin + barHeight, rateDf$ymin + barHeight)

  if (is.null(barWidthDays)) {
    ux <- sort(unique(rateDf$periodMidpoint))
    d <- diff(ux)
    barWidthDays <- if (length(d)) {
      max(7, round(as.numeric(stats::median(d, na.rm = TRUE))))
    } else {
      28
    }
  }
  halfW <- barWidthDays / 2
  rateDf$xmin <- rateDf$periodMidpoint - halfW
  rateDf$xmax <- rateDf$periodMidpoint + halfW

  list(
    ggplot2::geom_rect(
      data = rateDf,
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = fill,
      alpha = alpha,
      linewidth = 0,
      show.legend = FALSE
    )
  )
}

#' Plot comparison of model performance over time
#' @param allResults Data frame containing all results
#' @param outcome Outcome name to filter by ("Death", "Critical", "Hospital")
#' @param modelOriginsToCompare Vector of model origins to compare (e.g., c("New Covid", "Original Influenza"))
#' @param featureSetsToCompare Vector of feature sets to compare (e.g., c("Parsimonious", "Full"))
#' @param devPeriodsToCompare Optional vector of development periods to compare (e.g., c("First 3 Months", "Full Year 2020"))
#' @param facetBy Optional vector of column names to facet by (e.g., c("outcomeName"))
#' @param metric Metric to plot (default is "AUROC")
#' @param legendCorner Position of the legend ("tl", "tr", "bl", "br", or "auto")
#' where "auto" places the legend to avoid overlapping data points. Default is "auto".
#' @param showMilestones Whether to show milestone lines (default is TRUE)
#' @param vaccineLineColor Color for vaccine milestone lines (default is "#EF4444FF")
#' @param variantLineColor Color for variant milestone lines (default is "#9CA3AFFF
#' @param title Optional title for the plot
#' @return A ggplot object representing the comparison
#' @export
plotComparison <- function(
  allResults,
  outcomes = NULL,
  modelOriginsToCompare,
  featureSetsToCompare,
  devPeriodsToCompare = NULL,
  facetBy = NULL,
  facetCols = 1,
  metric = "AUROC",
  legendCorner = "auto",
  showMilestones = TRUE,
  vaccineLineColor = "#EF4444FF",
  variantLineColor = "#9CA3AFFF",
  title = NULL,
  showOutcomeRatePanel = FALSE,
  outcomeRatePanelStyle = c("area", "bars"),
  outcomeRatePanelFill = "#6B7280FF",
  outcomeRatePanelAlpha = 0.6,
  outcomeRatePanelHeights = c(0.78, 0.22)
) {
  outcomeRatePanelStyle <- match.arg(outcomeRatePanelStyle)

  dataList <- filterComparisonData(
    allResults = allResults,
    outcomes = outcomes,
    modelOriginsToCompare = modelOriginsToCompare,
    featureSetsToCompare = featureSetsToCompare,
    devPeriodsToCompare = devPeriodsToCompare
  )
  resultsToPlot <- dataList$resultsToPlot
  devPerformance <- dataList$devPerformance

  if (!(metric %in% names(resultsToPlot))) {
    stop("Column '", metric, "' not found in 'resultsToPlot'.")
  }
  if (nrow(resultsToPlot) == 0) {
    warning("No validation data found for the specified comparison criteria.")
    return(NULL)
  }
  yVar <- rlang::sym(metric)

  devPlotPerformance <- computeDevPlotPerformance(devPerformance, metric)
  hasDevSegments <- nrow(devPlotPerformance) > 0

  yLim <- computeYLimits(resultsToPlot, devPerformance, metric)
  yLower <- yLim$yLower
  yUpper <- yLim$yUpper
  milestones <- .defaultUsMilestones()
  if (is.null(title)) {
    title <- "Model Performance Over Time"
  }

  pal <- buildFeatureSetPalette(resultsToPlot, devPerformance)
  allLevels <- pal$allLevels
  colorPalette <- pal$colorPalette

  resultsToPlot$fullName <- factor(resultsToPlot$fullName, levels = allLevels)
  devPerformance$fullName <- factor(devPerformance$fullName, levels = allLevels)
  devPlotPerformance$fullName <- factor(
    devPlotPerformance$fullName,
    levels = allLevels
  )

  legendGrobs <- buildFacetLegendGrobs(
    resultsToPlot = resultsToPlot,
    facetBy = facetBy,
    colorPalette = colorPalette,
    metric = metric,
    yLower = yLower,
    yUpper = yUpper,
    legendCorner = legendCorner
  )

  xLim <- computeGlobalXLimits(resultsToPlot, devPerformance)
  globalXmin <- xLim$globalXmin
  globalXMax <- xLim$globalXMax
  # Panel limits including scale expansion to align NPC text with data vlines
  xSpanDays <- as.numeric(globalXMax - globalXmin)
  padFrac <- 0.01
  panelXmin <- globalXmin - padFrac * xSpanDays
  panelXmax <- globalXMax + padFrac * xSpanDays

  vplot <- buildBasePlot(
    resultsToPlot = resultsToPlot,
    devPlotPerformance = devPlotPerformance,
    yVar = yVar,
    globalXmin = globalXmin,
    globalXMax = globalXMax,
    yLower = yLower,
    yUpper = yUpper,
    metric = metric,
    hasDevSegments = hasDevSegments,
    title = title,
    connectDevToVal = TRUE
  )
  bottomLegend <- legendGrobs[legendGrobs$side == "bottom", , drop = FALSE]
  topLegend <- legendGrobs[legendGrobs$side == "top", , drop = FALSE]

  vplot <- vplot +
    ggpp::geom_grob_npc(
      data = bottomLegend,
      mapping = ggplot2::aes(
        label = .data$grob,
        `vp.width` = .data$vpWidth,
        `vp.height` = .data$vpHeight,
        npcx = .data$npcx,
        npcy = .data$npcyAdj
      ),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 0
    ) +
    ggpp::geom_grob_npc(
      data = topLegend,
      mapping = ggplot2::aes(
        label = .data$grob,
        `vp.width` = .data$vpWidth,
        `vp.height` = .data$vpHeight,
        npcx = .data$npcx,
        npcy = .data$npcyAdj
      ),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1
    ) +
    ggplot2::scale_color_manual(
      values = colorPalette,
      limits = allLevels,
      guide = "none"
    )

  if (!is.null(facetBy)) {
    if (!all(facetBy %in% names(resultsToPlot))) {
      stop(
        "One or more column names provided to 'facetBy' do not exist in the data."
      )
    }
    vplot <- vplot +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(!!!rlang::syms(facetBy)),
        ncol = facetCols,
        strip.position = "top"
      )
  }

  if (!hasDevSegments) {
    vplot <- vplot + ggplot2::theme(legend.position = "none")
  }

  xDateCol <- "periodMidpoint" # or "endDate"
  xRange <- range(resultsToPlot[[xDateCol]], na.rm = TRUE)

  if (showMilestones) {
    # Provide per-facet legend placement to anchor milestone labels correctly.
    vplot <- .addMilestoneLayers(
      p = vplot,
      milestones = milestones,
      yLower = yLower,
      yUpper = yUpper,
      xRange = c(panelXmin, panelXmax),
      vaccineLineColor = vaccineLineColor,
      variantLineColor = variantLineColor,
      showLabels = TRUE,
      legendCorner = legendCorner,
      resultsData = resultsToPlot,
      metric = metric,
      facetBy = facetBy,
      legendPlacement = legendGrobs
    )
  }

  # If not using panel approach, keep your original optional Eavg overlays (e.g., ticks)
  if (identical(tolower(metric), "eavg") && !isTRUE(showOutcomeRatePanel)) {
    # If you still have addOutcomeRateEdgeTicksLayer in your environment, you can keep it here.
    # vplot <- vplot + addOutcomeRateEdgeTicksLayer(...)
  }

  # NEW: Option C - add a compact, aligned outcome-rate panel below the main plot
  if (identical(tolower(metric), "eavg") && isTRUE(showOutcomeRatePanel)) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      warning(
        "showOutcomeRatePanel = TRUE requires the 'patchwork' package. Returning single-panel plot."
      )
      return(vplot)
    }

    rateDf <- computeOutcomeRateSummary(resultsToPlot, facetBy = facetBy)
    if (nrow(rateDf) == 0) {
      warning(
        "Outcome-rate panel requested but no 'observedRisk' data available. Returning single-panel plot."
      )
      return(vplot)
    }

    # Build rate panel (area or bars), aligned x-limits
    p_rate <- ggplot2::ggplot(
      rateDf,
      ggplot2::aes(x = .data$periodMidpoint, y = .data$risk)
    ) +
      {
        if (identical(outcomeRatePanelStyle, "area")) {
          ggplot2::geom_area(
            fill = outcomeRatePanelFill,
            alpha = outcomeRatePanelAlpha,
            linewidth = 0
          )
        } else {
          # pick a reasonable default bar width in days
          ux <- sort(unique(rateDf$periodMidpoint))
          d <- diff(ux)
          wDays <- if (length(d)) {
            max(7, round(as.numeric(stats::median(d, na.rm = TRUE))))
          } else {
            28
          }
          ggplot2::geom_col(
            fill = outcomeRatePanelFill,
            alpha = outcomeRatePanelAlpha,
            width = wDays
          )
        }
      } +
      ggplot2::scale_x_date(
        limits = c(globalXmin, globalXMax),
        date_breaks = "3 months",
        date_labels = "%b %Y",
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(accuracy = 1)
      ) +
      ggplot2::labs(x = "Time", y = "Outcome rate") +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = 0.25,
          colour = "grey85"
        ),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          size = 14,
          angle = 45,
          hjust = 1,
          vjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_text(
          size = 14,
          margin = ggplot2::margin(r = 4)
        ),
        axis.title.x = ggplot2::element_text(
          size = 16,
          margin = ggplot2::margin(t = 6)
        ),
        plot.margin = grid::unit(c(2, 6, 6, 6), "pt"),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 14, face = "bold")
      )

    if (!is.null(facetBy)) {
      if (!all(facetBy %in% names(rateDf))) {
        stop("One or more 'facetBy' columns are missing in outcome-rate data.")
      }
      p_rate <- p_rate +
        ggplot2::facet_wrap(
          facets = ggplot2::vars(!!!rlang::syms(facetBy)),
          ncol = 1,
          strip.position = "top"
        )
    }

    # Remove x labels from the main plot; keep them on the rate panel
    p_main <- vplot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        plot.margin = grid::unit(c(6, 6, 2, 6), "pt")
      )

    return(patchwork::wrap_plots(
      p_main,
      p_rate,
      ncol = 1,
      heights = outcomeRatePanelHeights
    ))
  }

  vplot
}
