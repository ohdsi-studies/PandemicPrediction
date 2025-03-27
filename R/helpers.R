#' @export
loadStudySpec <- function(type = c(
                            "validation.json",
                            "outpatient_critical_simple_validation.json",
                            "outpatient_death_simple_validation.json",
                            "outpatient_severe_simple_validation.json"
                          )[1]) {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("study_execution_jsons", type,
      package = "PandemicPrediction"
    )
  )
  return(analysisSpecifications)
}
