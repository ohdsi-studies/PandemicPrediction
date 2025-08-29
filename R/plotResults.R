getOutcome <- function(outcomeId) {
  outcome <- switch(as.character(outcomeId),
    "11" = "Death",
    "13" = "Critical",
    "14" = "Hospital",
    "UnknownOutcome"
  )
  return(outcome)
}

getTarget <- function(targetId) {
  target <- switch(as.character(targetId),
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

  cohortLookup <- DBI::dbGetQuery(con, "SELECT cohort_id, cohort_definition_id
    FROM cohorts;")

  featureNameMap <- list(
    "1" = "Parsimonious",
    "2" = "Full"
  )
  covariateLookup <- dplyr::tibble(
    covariate_setting_id = as.integer(names(featureNameMap)),
    featureSet = unlist(featureNameMap)
  )
  devPeriodLookup <- DBI::dbGetQuery(con, "
    SELECT
      md.model_design_id,
      pds.plp_data_settings_json
    FROM model_designs md
    INNER JOIN plp_data_settings pds ON md.plp_data_setting_id = pds.plp_data_setting_id;
  ") |>
    dplyr::mutate(
      parsedJson = lapply(
        .data$plp_data_settings_json,
        function(x) jsonlite::fromJSON(x)
      ),
      devStartDate = as.Date(sapply(
        .data$parsedJson,
        function(x) x$studyStartDate
      ), format = "%Y%m%d"),
      devEndDate = as.Date(sapply(
        .data$parsedJson,
        function(x) x$studyEndDate
      ), format = "%Y%m%d"),
      sampleSize = sapply(
        .data$parsedJson,
        function(x) ifelse(is.null(x$sampleSize), NA, x$sampleSize)
      ),
      devPeriod = dplyr::case_when(
        !is.na(sampleSize) &
          lubridate::interval(devStartDate, devEndDate) >
            lubridate::days(360) ~ "Full Year 2020 (Sampled)",
        lubridate::interval(devStartDate, devEndDate) <=
          lubridate::days(92) ~ "First 3 Months",
        lubridate::interval(devStartDate, devEndDate) <=
          lubridate::days(184) ~ "First 6 Months",
        lubridate::interval(devStartDate, devEndDate) <=
          lubridate::days(275) ~ "First 9 Months",
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
    WHERE es.evaluation = ? AND es.metric IN ('AUROC', 'populationSize', 'outcomeCount', 'Eavg');
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

  jsonPlpSettings <- lapply(resultsFromDb$plp_data_settings_json, function(x) if (!is.na(x)) jsonlite::fromJSON(x) else NA)
  startDates <- sapply(jsonPlpSettings, safeExtract, path = list("studyStartDate"))
  endDates <- sapply(jsonPlpSettings, safeExtract, path = list("studyEndDate"))

  jsonModelSettings <- lapply(resultsFromDb$model_settings_json, function(x) if (!is.na(x)) jsonlite::fromJSON(x) else NA)
  modelNames <- sapply(jsonModelSettings, safeExtract, path = list("param", "attr_settings", "name"))

  resultsProcessed <- resultsFromDb |>
    dplyr::mutate(
      studyStartDate = startDates,
      studyEndDate = endDates,
      modelSettingName = ifelse(is.na(modelNames), "Existing GLM", modelNames)
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
      outcomeName = dplyr::case_when(
        as.character(.data$outcomeId) == "11" ~ "Death",
        as.character(.data$outcomeId) == "13" ~ "Critical",
        as.character(.data$outcomeId) == "14" ~ "Hospital",
        TRUE ~ "UnknownOutcome"
      )
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
      startDate = as.Date(.data$studyStartDate, format = "%Y%m%d"),
      endDate = as.Date(.data$studyEndDate, format = "%Y%m%d")
    )

  resultsFinal <- resultsWide |>
    dplyr::mutate(
      analysisId = !!analysisId,
      modelOrigin = dplyr::case_when(
        analysisId == "dev" ~ "New Covid",
        analysisId == "val_new" ~ "New Covid",
        analysisId == "val_original" ~ "Original Influenza",
        TRUE ~ "Unknown"
      ),
      devPeriod = ifelse(is.na(.data$devPeriod), NA_character_, .data$devPeriod),
      fullName = paste(
        mapply(getName, .data$targetId, .data$outcomeId),
        .data$featureSet,
        .data$modelOrigin,
        .data$devPeriod,
        sep = " | "
      ) |> stringr::str_replace_all(" \\| NA", ""),
      legendLabel = dplyr::case_when(
        modelOrigin == "Original Influenza" ~ "Original Influenza Model",
        modelOrigin == "New Covid" ~ paste("Covid Model - ", devPeriod),
        TRUE ~ "Unknown Model"
      )
    ) |>
    dplyr::select(
      "analysisId", "modelOrigin", "featureSet", "devPeriod",
      "startDate", "endDate",
      "devStartDate", "devEndDate",
      "targetId", "outcomeId", "targetName", "outcomeName",
      "fullName", "legendLabel", "AUROC", "populationSize", "outcomeCount", "Eavg"
    ) |>
    dplyr::arrange(.data$startDate, .data$fullName)
  return(resultsFinal)
}

#' Plot comparison of model performance over time
#' @param allResults Data frame containing all results
#' @param outcome Outcome name to filter by ("Death", "Critical", "Hospital")
#' @param modelOriginsToCompare Vector of model origins to compare (e.g., c("New Covid", "Original Influenza"))
#' @param featureSetsToCompare Vector of feature sets to compare (e.g., c("Parsimonious", "Full"))
#' @param devPeriodsToCompare Optional vector of development periods to compare (e.g., c("First 3 Months", "Full Year 2020"))
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
    title = NULL) {
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
      dplyr::filter(.data$devPeriod %in% devPeriodsToCompare | .data$modelOrigin != "New Covid")
    devPerformance <- devPerformance |>
      dplyr::filter(.data$devPeriod %in% devPeriodsToCompare | .data$modelOrigin != "New Covid")
  }

  if (nrow(resultsToPlot) == 0) {
    warning("No validation data found for the specified comparison criteria.")
    return(NULL)
  }

  if (is.null(title)) {
    title <- "Model Performance Over Time"
  }
  legendData <- resultsToPlot |>
    dplyr::group_by(!!!rlang::syms(facetBy), .data$fullName, .data$legendLabel) |>
    dplyr::summarise(labelX = max(.data$endDate, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(!!!rlang::syms(facetBy)) |>
    dplyr::mutate(labelY = 0.51 + (dplyr::row_number() - 1) * 0.04) |>
    dplyr::ungroup()

  vplot <- ggplot2::ggplot(
    data = resultsToPlot,
    mapping = ggplot2::aes(x = .data$startDate, y = .data$AUROC, colour = .data$fullName)
  ) +
    ggplot2::geom_segment(
      data = devPerformance,
      mapping = ggplot2::aes(
        x = .data$devStartDate,
        xend = .data$devEndDate,
        y = .data$AUROC,
        yend = .data$AUROC,
        colour = .data$fullName
      ),
      linetype = "dashed",
      linewidth = 1.2
    ) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$endDate, yend = .data$AUROC), linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = .data$endDate), size = 3) +
    ggplot2::geom_text(
      data = legendData,
      mapping = ggplot2::aes(
        x = .data$labelX, y = .data$labelY,
        label = .data$legendLabel, colour = .data$fullName
      ),
      hjust = 1, fontface = "bold", size = 3.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
    ggplot2::labs(title = title, x = "Validation Period", y = "AUROC") +
    ggplot2::ylim(0.5, 1) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  # Faceting logic now works with any valid column name
  if (!is.null(facetBy)) {
    if (!all(facetBy %in% names(resultsToPlot))) {
      stop("One or more column names provided to 'facetBy' do not exist in the data.")
    }
    vplot <- vplot + ggplot2::facet_grid(rows = ggplot2::vars(!!!rlang::syms(facetBy)))
  }

  return(vplot)
}
