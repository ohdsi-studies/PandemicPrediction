test_that("parseModelKey keeps full W tokens (e.g., 12m_full)", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "extractPredictions.R")
  expect_true(file.exists(helperPath))
  source(helperPath, local = TRUE)

  expect_equal(parseModelKey("covid_full_3m")$W, "3m")
  expect_equal(parseModelKey("covid_full_12m_full")$W, "12m_full")
  expect_equal(parseModelKey("covid_pars_12m_150k")$W, "12m_150k")

  expect_equal(parseModelKey("proxy_recal_full_12m_full")$W, "12m_full")
  expect_equal(parseModelKey("proxy_recal_pars_12m_150k")$W, "12m_150k")
})

test_that("locateQuarterRun distinguishes covid models with different W", {
  skip_if_not_installed("pROC")

  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "extractPredictions.R")
  source(helperPath, local = TRUE)

  if (!dir.exists("results/runs") || !file.exists("results/allResults.csv")) {
    skip("No local Strategus outputs available under results/runs")
  }

  row <- data.frame(
    outcomeName = "Hospitalization",
    quarterId = "20221001_20221231_Hospitalization",
    stringsAsFactors = FALSE
  )

  a3 <- locateQuarterRun(
    modelKey = "covid_full_3m",
    row = row,
    runsRoot = "results/runs",
    allResultsPath = "results/allResults.csv"
  )
  a12 <- locateQuarterRun(
    modelKey = "covid_full_12m_full",
    row = row,
    runsRoot = "results/runs",
    allResultsPath = "results/allResults.csv"
  )

  expect_false(identical(a3$runPlpPath, a12$runPlpPath))
})

