# install.packages(c("DBI", "RSQLite", "dplyr", "tidyr", "ggplot2", "jsonlite", "purrr"))

# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(purrr)

# ============================================================================
# HELPER FUNCTIONS (Updated to handle potential NA names)
# ============================================================================
getName <- function(target, outcome) {
  # Added a check for NA to prevent errors
  if (is.na(target) || is.na(outcome)) return("UnknownTarget_UnknownOutcome")
  firstName <- switch(as.character(target), "12" = "Outpatient", "25" = "Inpatient", "31" = "CovidNew", "UnknownTarget")
  secondName <- switch(as.character(outcome), "11" = "Death", "13" = "Critical", "14" = "Hospital", "UnknownOutcome")
  return(paste0(firstName, secondName))
}

# This function is now less important as we derive the feature name, but we keep it
getModelName <- function(modelName, featureName) {
  # If we have a descriptive feature name, use it. Otherwise, use the model name.
  return(ifelse(!is.na(featureName) & featureName != "", featureName, modelName))
}


# ============================================================================
# FINAL UNIFIED DATABASE EXTRACTION FUNCTION
# ============================================================================
#' Extracts and processes all performance results from a PLP SQLite database.
getResults <- function(database_path, evaluation_type) {
  if (!file.exists(database_path)) { stop("Database file not found at path: ", database_path) }
  
  con <- dbConnect(RSQLite::SQLite(), database_path)
  on.exit(dbDisconnect(con))

  # --- Step 1: Create Lookup Tables from the Database ---
  
  # Cohort ID Lookup: Maps internal cohort_id to original cohort_definition_id
  cohort_lookup <- dbGetQuery(con, "SELECT cohort_id, cohort_definition_id FROM cohorts;")
  
  # Covariate Settings Lookup: We will manually create a mapping for feature sets
  # First, see what settings are in the database
  covariate_settings_db <- dbGetQuery(con, "SELECT covariate_setting_id, covariate_settings_json FROM covariate_settings;")
  
  # ==========================================================================
  # USER ACTION REQUIRED: Manually define your feature set names here.
  # Inspect `covariate_settings_db` to find the right IDs. For example:
  # If ID 1 is your full feature set and ID 2 is parsimonious, you would do:
  feature_name_map <- list(
    "1" = "Full",
    "2" = "Parsimonious" 
    # Add more mappings as needed based on your study
  )
  # ==========================================================================
  
  covariate_lookup <- data.frame(
    covariate_setting_id = as.integer(names(feature_name_map)),
    featureSetName = unlist(feature_name_map)
  )

  # --- Step 2: Run the Main SQL Query ---
  sql_query <- "
    SELECT
      p.performance_id, p.target_id, p.outcome_id,
      pds.plp_data_settings_json,
      ms.model_settings_json,
      md.covariate_setting_id, -- Get the link to the feature set
      es.metric, es.value
    FROM evaluation_statistics es
    INNER JOIN performances p ON es.performance_id = p.performance_id
    INNER JOIN model_designs md ON p.model_design_id = md.model_design_id
    INNER JOIN plp_data_settings pds ON p.plp_data_setting_id = pds.plp_data_setting_id
    LEFT JOIN model_settings ms ON md.model_setting_id = ms.model_setting_id
    WHERE es.evaluation = ? AND es.metric IN ('AUROC', 'populationSize', 'outcomeCount', 'Eavg');
  "
  results_from_db <- dbGetQuery(con, sql_query, params = list(evaluation_type))
  
  if (nrow(results_from_db) == 0) {
    warning("Query returned no results for evaluation type: ", evaluation_type)
    return(data.frame())
  }
  
  # --- Step 3: Parse JSON and Join Lookups ---
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
  
  results_processed <- results_from_db %>%
    mutate(
      study_start_date = start_dates, study_end_date = end_dates,
      modelName = ifelse(is.na(model_names), "Existing GLM", model_names) # Default for validation
    ) %>%
    # Join with lookups
    left_join(cohort_lookup, by = c("target_id" = "cohort_id")) %>% rename(original_target_id = cohort_definition_id) %>%
    left_join(cohort_lookup, by = c("outcome_id" = "cohort_id")) %>% rename(original_outcome_id = cohort_definition_id) %>%
    left_join(covariate_lookup, by = "covariate_setting_id")
  
  # --- Step 4: Pivot and Finalize ---
  results_wide <- results_processed %>%
    pivot_wider(names_from = "metric", values_from = "value") %>%
    mutate(
      across(c(AUROC, Eavg), as.numeric), across(c(populationSize, outcomeCount), as.integer),
      start_date = as.Date(study_start_date, format = "%Y%m%d"), end_date = as.Date(study_end_date, format = "%Y%m%d")
    )

  return(results_wide %>%
           filter(!is.na(start_date)) %>%
           mutate(
             # For validation, use custom names if available, otherwise use feature set name
             finalModelName = case_when(
               modelName %in% c("coverF", "coverH", "coverI") ~ modelName,
               !is.na(featureSetName) ~ featureSetName,
               TRUE ~ modelName
             ),
             name = paste(mapply(getName, original_target_id, original_outcome_id), finalModelName, sep = "_")
           ) %>%
           select(start_date, end_date, original_target_id, original_outcome_id, finalModelName, name, AUROC, populationSize, outcomeCount, Eavg) %>%
           rename(target = original_target_id, outcome = original_outcome_id, modelName = finalModelName) %>%
           arrange(start_date, name))
}

# ============================================================================
# PLOTTING FUNCTION (Unchanged)
# ============================================================================
plotResults <- function(results, filter = NULL) {
  if (!is.null(filter)) { results <- results %>% filter(grepl(filter, .data$name, ignore.case = TRUE)) }
  if (nrow(results) == 0) { warning("No data left to plot after filtering."); return(NULL) }
  
  ggplot(results, aes(x = .data$start_date, y = .data$AUROC, colour = .data$name)) +
    geom_segment(aes(xend = .data$end_date, yend = .data$AUROC), linewidth = 1.2) +
    geom_point(size = 3) + geom_point(aes(x = .data$end_date), size = 3) + theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Model Performance Over Time", x = "Time Period", y = "AUROC") + ylim(0, 1)
}

# ============================================================================
# FINAL EXAMPLE USAGE
# ============================================================================

# --- 1. Get and Plot DEVELOPMENT Results ---
dev_db_path <- "results/development/databaseFile.sqlite"
development_results <- getResults(database_path = dev_db_path, evaluation_type = "Test")
print("--- Development Results ---")
print(development_results) # Print the whole table to see new names

if (!is.null(development_results) && nrow(development_results) > 0) {
  my_dev_plot <- plotResults(development_results)
  print(my_dev_plot)
}

# --- 2. Get and Plot VALIDATION Results ---
val_db_path <- "results/validation/databaseFile.sqlite"
validation_results <- getResults(database_path = val_db_path, evaluation_type = "Validation")
print("--- Validation Results ---")
print(validation_results) # Print the whole table to see new names

if (!is.null(validation_results) && nrow(validation_results) > 0) {
  my_validation_plot <- plotResults(validation_results)
  print(my_validation_plot)
}
