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

test_that("getPairedPredictions supports self-comparisons (modelAKey == modelBKey)", {
  driverPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "drivers.R")
  source(driverPath, local = TRUE)

  row <- data.frame(
    outcomeName = "Hospitalization",
    featureSet = "Full",
    W = "na",
    comparator = "proxy_frozen_only",
    modelAKey = "proxy_frozen_full",
    modelBKey = "proxy_frozen_full",
    quarterId = "20200101_20200331_Hospitalization",
    stringsAsFactors = FALSE
  )

  fakeLocate <- function(modelKey, row, runsRoot, allResultsPath) {
    list(runPlpPath = "fake.rds")
  }
  fakeLoad <- function(runPlpPath, evalType = "Validation") {
    data.frame(patientId = 1:4, y = c(0, 1, 0, 1), p = c(0.1, 0.9, 0.2, 0.8))
  }

  pr <- getPairedPredictions(
    row = row,
    locateQuarterRunFn = fakeLocate,
    loadRunPredictionsFn = fakeLoad,
    loadQuarterPairFromRunsFn = function(...) stop("should not be called")
  )
  expect_equal(pr$pA, pr$pB)
  expect_equal(length(pr$y), 4)
})

test_that("readExistingBootstrapIfCompatible returns cached results only when B matches", {
  driverPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "drivers.R")
  source(driverPath, local = TRUE)

  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    metric = c("AUROC", "AUPRC", "Brier", "ICI", "INB"),
    B = rep(25, 5),
    delta = rep(0, 5),
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, tmp, row.names = FALSE)

  ok <- readExistingBootstrapIfCompatible(tmp, B = 25)
  expect_true(!is.null(ok))
  expect_equal(nrow(ok), 5)

  bad <- readExistingBootstrapIfCompatible(tmp, B = 200)
  expect_true(is.null(bad))
})
