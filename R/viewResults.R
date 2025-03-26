#' @export
ViewResults <- function(
    server = "~/ohdsi-studies/PandemicPrediction/results/new_inpatient_cohorts/strategusWork/PatientLevelPredictionValidationModule_3/sqlite/databaseFile.sqlite"
    ){
connectionDetailsInput <- list(dbms = "sqlite", server=server)
databaseSettings <- list(connectionDetailSettings = connectionDetailsInput,
                         schema = "main")

PatientLevelPrediction:::viewPlps(databaseSettings)
}
