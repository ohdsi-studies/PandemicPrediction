library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

df <- read.csv("./results/allResults.csv", stringsAsFactors = FALSE)

# allResults uses Date columns (YYYY-MM-DD) rather than YYYYMMDD strings
df <- df %>%
  mutate(
    start_date = as.Date(.data$startDate),
    end_date = as.Date(.data$endDate),
    Period = paste0(year(.data$start_date), "-Q", quarter(.data$start_date))
  )

# Use a single validation analysisId to avoid mixing development and validation
# runs (and to ensure populationSize/outcomeCount are taken from one consistent run).
analysisIdToUse <- "val_original"
df <- df %>% filter(.data$analysisId == analysisIdToUse)

# Keep the temporal validation windows (3-month windows; final window may be partial)
df <- df %>%
  mutate(windowDays = as.integer(.data$end_date - .data$start_date) + 1L) %>%
  filter(.data$windowDays >= 60L, .data$windowDays <= 100L)

# Derive sample size and outcome counts per period/outcome.
# populationSize/outcomeCount should be identical across models for the same outcome+period;
# use max() for robustness in case of duplicates/ordering differences.
by_outcome <- df %>%
  filter(.data$outcomeName %in% c("Hospitalization", "Fatality", "RespiratoryFailure")) %>%
  group_by(.data$Period, .data$start_date, .data$outcomeName) %>%
  summarise(
    N = suppressWarnings(max(as.integer(.data$populationSize), na.rm = TRUE)),
    Events = suppressWarnings(max(as.integer(.data$outcomeCount), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    EventsPct = sprintf(
      "%s (%.1f%%)",
      format(.data$Events, big.mark = ","),
      100 * .data$Events / .data$N
    )
  )

wide_tbl <- by_outcome %>%
  select("Period", "start_date", "outcomeName", "EventsPct") %>%
  pivot_wider(
    names_from = "outcomeName",
    values_from = "EventsPct"
  )

totalN <- by_outcome %>%
  group_by(.data$Period, .data$start_date) %>%
  summarise(
    SampleSize = format(max(.data$N, na.rm = TRUE), big.mark = ","),
    .groups = "drop"
  )

summary_tbl <- totalN %>%
  left_join(wide_tbl, by = c("Period", "start_date")) %>%
  arrange(.data$start_date) %>%
  select(
    "Period",
    "SampleSize",
    "RespiratoryFailure",
    "Fatality",
    "Hospitalization"
  )

kable(
  summary_tbl,
  format = "pipe",
  col.names = c(
    "Period",
    "Sample size",
    "Respiratory failure/insufficiency (%)",
    "Death (%)",
    "Hospitalization (%)"
  )
)
