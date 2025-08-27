# install.packages(c("DBI", "RSQLite", "dplyr", "tidyr", "ggplot2", "jsonlite"))

# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)

# ============================================================================
# HELPER FUNCTIONS (Unchanged)
# ============================================================================
getName <- function(target, outcome) {
  firstName <- switch(as.character(target), "12" = "Outpatient", "25" = "Inpatient", "31" = "CovidNew", "UnknownTarget")
  secondName <- switch(as.character(outcome), "11" = "Death", "13" = "Critical", "14" = "Hospital", "UnknownOutcome")
  name <- paste0(firstName, secondName)
  return(name)
}

getModelName <- function(name, targetId, outcomeId) {
  if (!grepl("cover", name, ignore.case = TRUE)) {
    if (targetId == 12 || targetId == 31) {
      if (outcomeId == 11) { name <- "dataDrivenF" } 
      else if (outcomeId == 13) { name <- "dataDrivenI" } 
      else if (outcomeId == 14) { name <- "dataDrivenH" }
    }
  }
  return(name)
}

# ============================================================================
# FINAL ROBUST DATABASE EXTRACTION FUNCTION
# ============================================================================
getResultsDb <- function(database_path, evaluation_type) {
  if (!file.exists(database_path)) { stop("Database file not found at path: ", database_path) }
  
  con <- dbConnect(RSQLite::SQLite(), database_path)
  on.exit(dbDisconnect(con))
  
  sql_query <- "
    SELECT p.performance_id, md.target_id, md.outcome_id, pds.plp_data_settings_json,
           ms.model_settings_json, es.metric, es.value
    FROM evaluation_statistics es
    LEFT JOIN performances p ON es.performance_id = p.performance_id
    LEFT JOIN model_designs md ON p.model_design_id = md.model_design_id
    LEFT JOIN plp_data_settings pds ON md.plp_data_setting_id = pds.plp_data_setting_id
    LEFT JOIN model_settings ms ON md.model_setting_id = ms.model_setting_id
    WHERE es.evaluation = ? AND es.metric IN ('AUROC', 'populationSize', 'outcomeCount', 'Eavg');
  "
  results_from_db <- dbGetQuery(con, sql_query, params = list(evaluation_type))
  
  if (nrow(results_from_db) == 0) {
    warning("Query returned no results for evaluation type: ", evaluation_type)
    return(data.frame())
  }
  
  safe_extract <- function(json_obj, path) {
    if (!is.list(json_obj)) return(NA_character_)
    val <- purrr::pluck(json_obj, !!!path)
    return(if (is.null(val)) NA_character_ else as.character(val))
  }

  json_plp_settings <- lapply(results_from_db$plp_data_settings_json, function(x) if(!is.na(x)) fromJSON(x) else NA)
  start_dates <- sapply(json_plp_settings, safe_extract, path = list("studyStartDate"))
  end_dates <- sapply(json_plp_settings, safe_extract, path = list("studyEndDate"))

  json_model_settings <- lapply(results_from_db$model_settings_json, function(x) if(!is.na(x)) fromJSON(x) else NA)
  model_names <- sapply(json_model_settings, safe_extract, path = list("param", "attr_settings", "name"))
  
  results_long <- results_from_db %>%
    mutate(
      study_start_date = start_dates,
      study_end_date = end_dates,
      model_setting_name = model_names
    )

  results_wide <- results_long %>%
    pivot_wider(names_from = "metric", values_from = "value") %>%
    mutate(
      AUROC = as.numeric(AUROC), populationSize = as.integer(populationSize),
      outcomeCount = as.integer(outcomeCount), Eavg = as.numeric(Eavg),
      start_date = as.Date(study_start_date, format = "%Y%m%d"),
      end_date = as.Date(study_end_date, format = "%Y%m%d"),
      target = target_id, outcome = outcome_id, modelName = model_setting_name
    )
  
  # MODIFIED: Removed the filter step. The function will now return results
  # even if their dates are NA.
  results <- results_wide %>%
    mutate(
      name = paste(
        mapply(getName, target, outcome),
        mapply(getModelName, modelName, target, outcome),
        sep = "_"
      )
    ) %>%
    select(
      start_date, end_date, target, outcome, modelName, name,
      AUROC, populationSize, outcomeCount, Eavg
    ) %>%
    arrange(start_date, name)
  
  return(results)
}

# ============================================================================
# FINAL ADAPTIVE PLOTTING FUNCTION
# ============================================================================
plotResults <- function(results, filter = NULL) {
  if (!is.null(filter)) {
    results <- results %>% filter(grepl(filter, .data$name, ignore.case = TRUE))
  }
  if (nrow(results) == 0) {
    warning("No data left to plot after filtering.")
    return(NULL)
  }

  # ADAPTIVE LOGIC: Check if date information is available
  if (all(is.na(results$start_date))) {
    message("Cannot create time-series plot: Study date information is missing for these results.")
    message("Creating a bar chart of AUROC values instead.")
    
    alt_plot <- ggplot(results, aes(x = reorder(name, -AUROC), y = AUROC, fill = name)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = round(AUROC, 3)), vjust = -0.3, size = 3.5) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Model Performance", x = "Model", y = "AUROC") +
      ylim(0, 1)
    return(alt_plot)
  }
  
  # Original time-series plot code
  vplot <- ggplot(results, aes(x = .data$start_date, y = .data$AUROC, colour = .data$name)) +
    geom_segment(aes(xend = .data$end_date, yend = .data$AUROC), linewidth = 1.2) +
    geom_point(size = 3) +
    geom_point(aes(x = .data$end_date), size = 3) +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", minor_breaks = NULL) +
    theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Model Performance Over Time",
      subtitle = "Lines represent the time period used for model evaluation",
      x = "Time Period", y = "AUROC"
    ) +
    ylim(0, 1)
  return(vplot)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# 1. Get results from the DEVELOPMENT database
dev_db_path <- "results/development/databaseFile.sqlite"
development_results <- getResultsDb(database_path = dev_db_path, evaluation_type = "Test")
print("--- Development Results ---")
print(head(development_results))

# 2. Get results from the VALIDATION database
val_db_path <- "results/validation/databaseFile.sqlite"
validation_results <- getResultsDb(database_path = val_db_path, evaluation_type = "Validation")
print("--- Validation Results ---")
print(head(validation_results))

# 3. Plot the development results (will create a time-series plot)
if (!is.null(development_results) && nrow(development_results) > -1) {
  my_plot <- plotResults(development_results)
  print(my_plot)
}

# 4. Plot the validation results (will create a bar chart and print a message)
if (!is.null(validation_results) && nrow(validation_results) > 0) {
  my_validation_plot <- plotResults(validation_results)
  print(my_validation_plot)
}
