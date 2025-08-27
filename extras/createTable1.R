library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

df <- readRDS("./results/aggregated/validationResults")
df2 <- df %>% 
  mutate(
    Period     = paste0(year(start_date), "-Q", quarter(start_date)),
    Outcome    = recode(outcome,
                        `14` = "Hosp",          # Hospitalisation
                        `13` = "Crit",          # Critical illness / ICU
                        `11` = "Death"),
    Model      = if_else(str_detect(modelName, "cover"),
                         "Cover", "DataDriven")         # 
  )

by_outcome <- df2 %>% 
  group_by(Period, Outcome) %>% 
  summarise(
    N      = first(populationSize),
    Events = first(outcomeCount),
    Cover_AUROC      = AUROC[Model == "Cover"][1],
    DataDriven_AUROC = AUROC[Model == "DataDriven"][1],
    .groups = "drop"
  ) %>% 
  mutate(
    EventsPct = sprintf("%s (%.1f%%)",
                        format(Events, big.mark = ","),
                        100 * Events / N)
  ) %>% 
  select(Period, Outcome,
         EventsPct, Cover_AUROC, DataDriven_AUROC)

# ── 4.  Pivot once: Outcomes become column prefixes --------------------------
wide_tbl <- by_outcome %>% 
  pivot_wider(
    names_from  = Outcome,
    values_from = c(EventsPct, Cover_AUROC, DataDriven_AUROC),
    names_glue  = "{Outcome}_{.value}"
  )

# ── 5.  Add total sample size for the quarter --------------------------------
totalN <- df2 %>% 
  group_by(Period) %>% 
  summarise(TotalN = format(max(populationSize), big.mark = ","), .groups = "drop")

summary_tbl <- totalN %>% 
  left_join(wide_tbl, by = "Period") %>% 
  arrange(Period)

# ── 6.  Console/Markdown table ----------------------------------------------
kable(summary_tbl, format = "pipe",
      col.names = c("Period", "Total N",
                    "Hosp events (%)",  "Hosp AUC Cover",  "Hosp AUC DD",
                    "Crit events (%)",  "Crit AUC Cover",  "Crit AUC DD",
                    "Death events (%)", "Death AUC Cover", "Death AUC DD"),
      digits = 3)
