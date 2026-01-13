test_that("comparisonToModelRows splits and filters to labelMap", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "quarterwisePlots.R")
  expect_true(file.exists(helperPath))
  source(helperPath, local = TRUE)

  df <- data.frame(
    metric = "INB",
    metricA = 0.01,
    metricA_lo = 0.005,
    metricA_hi = 0.015,
    metricB = 0.02,
    metricB_lo = 0.010,
    metricB_hi = 0.030,
    modelAKey = "proxy_frozen_full",
    modelBKey = "proxy_roll_full",
    featureSet = "Full",
    outcomeName = "Hospitalization",
    quarterId = "20200401_20200630_Hospitalization",
    quarterStart = as.Date("2020-04-01"),
    quarterEnd = as.Date("2020-06-30"),
    quarterMid = as.Date("2020-05-16"),
    comparator = "proxy_frozen_vs_proxy_rolling_recal",
    stringsAsFactors = FALSE
  )

  # Duplicate row to ensure de-duplication happens per (quarterId, modelKey)
  df2 <- df
  df2$comparator <- "some_other_comparator"
  results <- rbind(df, df2)

  labelMap <- c(
    proxy_frozen_full = "Frozen Full",
    proxy_roll_full = "Rolling Full"
  )
  modelDf <- comparisonToModelRows(results, metric = "INB", labelMap = labelMap)

  expect_true(all(modelDf$modelKey %in% names(labelMap)))
  expect_equal(sort(unique(modelDf$seriesLabel)), sort(unname(labelMap)))
  expect_equal(nrow(modelDf), 2)
})

test_that("plotQuarterwiseModelMetric returns a ggplot and only uses provided rows", {
  skip_if_not_installed("ggplot2")
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "quarterwisePlots.R")
  source(helperPath, local = TRUE)

  df <- data.frame(
    metric = "INB",
    metricValue = c(0.01, 0.02),
    metricLo = c(0.005, 0.010),
    metricHi = c(0.015, 0.030),
    modelKey = c("proxy_frozen_full", "proxy_roll_full"),
    modelRole = c("A", "B"),
    modelAKey = "proxy_frozen_full",
    modelBKey = "proxy_roll_full",
    featureSet = "Full",
    outcomeName = "Hospitalization",
    quarterId = "20200401_20200630_Hospitalization",
    quarterStart = as.Date("2020-04-01"),
    quarterEnd = as.Date("2020-06-30"),
    quarterMid = as.Date("2020-05-16"),
    comparator = "proxy_frozen_vs_proxy_rolling_recal",
    seriesLabel = c("Frozen Full", "Rolling Full"),
    stringsAsFactors = FALSE
  )

  p <- plotQuarterwiseModelMetric(df, title = "t", yLabel = "y")
  expect_s3_class(p, "ggplot")
  expect_equal(sort(unique(p$data$modelKey)), sort(unique(df$modelKey)))
  expect_equal(sort(unique(p$data$seriesLabel)), sort(unique(df$seriesLabel)))
})
