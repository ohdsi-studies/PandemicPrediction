#' @export
viewResults <- function(server = "") {
  connectionDetails <- list(dbms = "sqlite", server = server)
  databaseSettings <- list(
    connectionDetailSettings = connectionDetails,
    schema = "main"
  )

  PatientLevelPrediction:::viewPlps(databaseSettings)
}
