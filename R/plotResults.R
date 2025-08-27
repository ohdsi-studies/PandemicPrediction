getName <- function(target, outcome) {
  firstName <- switch(as.character(target),
    "12" = "Outpatient",
    "25" = "Inpatient",
    "31" = "CovidNew",
    "UnknownTarget"
  )
  secondName <- switch(as.character(outcome),
    "11" = "Death",
    "13" = "Critical",
    "14" = "Hospital",
    "UnknownOutcome"
  )
  name <- paste0(firstName, secondName)
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

getResultsDb <- function(databasePath, evaluationType) {
  if (!file.exists(databasePath)) {
    stop("Database file not found at path: ", databasePath)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), databasePath)
  on.exit(DBI::dbDisconnect(con))

  sqlQuery <- "
    SELECT p.performance_id,
           md.target_id,
           md.outcome_id,
           pds.plp_data_settings_json,
           ms.model_settings_json, es.metric, es.value
    FROM evaluation_statistics es
    LEFT JOIN performances p ON es.performance_id = p.performance_id
    LEFT JOIN model_designs md ON p.model_design_id = md.model_design_id
    LEFT JOIN plp_data_settings pds ON md.plp_data_setting_id = pds.plp_data_setting_id
    LEFT JOIN model_settings ms ON md.model_setting_id = ms.model_setting_id
    WHERE es.evaluation = ? AND es.metric IN ('AUROC', 'populationSize', 'outcomeCount', 'Eavg');
  "
  resultsFromDb <- DBI::dbGetQuery(con, sqlQuery, params = list(evaluationType))

  if (nrow(resultsFromDb) == 0) {
    warning("Query returned no results for evaluation type: ", evaluationType)
    return(data.frame())
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
  startDates <- sapply(jsonPlpSettings, safeExtract,
    path = list("study_start_date")
  )
  endDates <- sapply(jsonPlpSettings, safeExtract,
    path = list("study_end_date")
  )

  jsonModelSettings <- lapply(
    resultsFromDb$model_settings_json,
    function(x) {
      if (!is.na(x)) jsonlite::fromJSON(x) else NA
    }
  )
  modelNames <- sapply(jsonModelSettings, safeExtract,
    path = list("param", "attr_settings", "name")
  )
  resultsLong <- resultsFromDb |>
    dplyr::mutate(
      studyStartDate = startDates,
      studyEndDate = endDates,
      modelSettingName = modelNames
    )

  resultsWide <- resultsLong |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::mutate(
      AUROC = as.numeric(.data$AUROC),
      populationSize = as.integer(.data$populationSize),
      outcomeCount = as.integer(.data$outcomeCount),
      Eavg = as.numeric(.data$Eavg),
      startDate = as.Date(.data$studyStartDate, format = "%Y%m%d"),
      endDate = as.Date(.data$studyEndDate, format = "%Y%m%d"),
      target = .data$targetId,
      outcome = .data$outcomeId,
      modelName = .data$modelSettingName
    )

  results <- resultsWide |>
    dplyr::mutate(
      name = paste(
        mapply(getName, .data$target, .data$outcome),
        mapply(getModelName, .data$modelName, .data$target, .data$outcome),
        sep = "_"
      )
    ) |>
    dplyr::select(
      "startDate",
      "endDate",
      "target",
      "outcome",
      "modelName",
      "name",
      "AUROC",
      "populationSize",
      "outcomeCount",
      "Eavg"
    ) |>
    dplyr::arrange(.data$startDate, .data$name)
  return(results)
}

plotResults <- function(results, filter = NULL) {
  if (!is.null(filter)) {
    results <- results |>
      dplyr::filter(grepl(filter, .data$name, ignore.case = TRUE))
  }
  if (nrow(results) == 0) {
    warning("No data left to plot after filtering.")
    return(NULL)
  }

  if (all(is.na(results$start_date))) {
    message("Cannot create time-series plot: Study date information is missing for these results.")
    message("Creating a bar chart of AUROC values instead.")

    altPlot <- ggplot2::ggplot(
      results,
      ggplot2::aes(
        x = stats::reorder(.data$name, -.data$AUROC),
        y = .data$AUROC, fill = .data$name
      )
    ) +
      ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = round(.data$AUROC, 3)), vjust = -0.3, size = 3.5) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = "Model Performance", x = "Model", y = "AUROC") +
      ggplot2::ylim(0, 1)
    return(altPlot)
  }

  # Original time-series plot code
  vplot <- ggplot2::ggplot(
    results,
    ggplot2::aes(x = .data$start_date, y = .data$AUROC, colour = .data$name)
  ) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$end_date, yend = .data$AUROC), linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    geom_point(aes(x = .data$end_date), size = 3) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
    ggplot2::theme(legend.position = "right", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Model Performance Over Time",
      subtitle = "Lines represent the time period used for model evaluation",
      x = "Time Period", y = "AUROC"
    ) +
    ggplot2::ylim(0, 1)
  return(vplot)
}
