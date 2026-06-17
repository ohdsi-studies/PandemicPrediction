getPandemicOutcomeIds <- function() {
  c(
    Fatality = 11L,
    Hospitalization = 14L,
    Critical = 27L
  )
}

getPandemicOutcomeId <- function(outcomeName) {
  ids <- getPandemicOutcomeIds()
  key <- match.arg(outcomeName, names(ids))
  unname(ids[[key]])
}

getPandemicOutcomeLabel <- function(outcomeId) {
  switch(
    as.character(outcomeId),
    "11" = "Fatality",
    "14" = "Hospitalization",
    "27" = "RespiratoryFailure",
    "13" = "RespiratoryFailureLegacy",
    "UnknownOutcome"
  )
}
