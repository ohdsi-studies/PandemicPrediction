test_that("buildComparisonsFromAllResults adds proxy_frozen_only for frozen-only quarters", {
  driverPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "drivers.R")
  expect_true(file.exists(driverPath))
  source(driverPath, local = TRUE)

  tmp <- tempfile(fileext = ".csv")
  ar <- data.frame(
    outcomeName = c("Hospitalization", "Hospitalization", "Hospitalization", "Hospitalization"),
    featureSet = c("Full", "Full", "Full", "Full"),
    startDate = c("2020-04-01", "2020-04-01", "2020-07-01", "2020-07-01"),
    endDate = c("2020-06-30", "2020-06-30", "2020-09-30", "2020-09-30"),
    devEndDate = c("2020-03-31", NA, NA, NA),
    devPeriod = c("First 3 Months", NA, NA, NA),
    modelOrigin = c(
      "New Covid",
      "Original Influenza",
      "Original Influenza",
      "Rolling Recalibrated Influenza"
    ),
    recalibrationPeriod = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  utils::write.csv(ar, tmp, row.names = FALSE)

  comps <- buildComparisonsFromAllResults(tmp)
  expect_true("proxy_frozen_only" %in% comps$comparator)

  q1 <- computeQuarterId(as.Date("2020-04-01"), as.Date("2020-06-30"), "Hospitalization")
  q2 <- computeQuarterId(as.Date("2020-07-01"), as.Date("2020-09-30"), "Hospitalization")

  frozenOnly <- subset(comps, comparator == "proxy_frozen_only")
  expect_true(q1 %in% frozenOnly$quarterId)
  expect_false(q2 %in% frozenOnly$quarterId)
  expect_true(all(frozenOnly$modelAKey == frozenOnly$modelBKey))
})

