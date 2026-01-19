test_that("computeEarliestSwitchQuarter finds earliest quarter with positive median thereafter", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "switchPoints.R")
  source(helperPath, local = TRUE)

  q <- as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"))
  d <- c(-1, -1, -1, 2)
  sw <- computeEarliestSwitchQuarter(q, d, minFromSwitch = 1)
  expect_equal(sw$switchQuarter, as.Date("2020-07-01"))

  # Require at least 3 quarters from switch: still switches at 2020-07-01 (2 quarters only) -> no switch
  sw2 <- computeEarliestSwitchQuarter(q, d, minFromSwitch = 3)
  expect_true(is.na(sw2$switchQuarter))
})

test_that("computeInbSwitchPoints computes switchpoint per W and baseline", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "switchPoints.R")
  source(helperPath, local = TRUE)

  df <- data.frame(
    metric = "INB",
    comparator = rep("covid_vs_proxy_frozen", 4),
    outcomeName = "Hospitalization",
    featureSet = "Full",
    W = "3m",
    quarterId = c(
      "20200101_20200331_Hospitalization",
      "20200401_20200630_Hospitalization",
      "20200701_20200930_Hospitalization",
      "20201001_20201231_Hospitalization"
    ),
    deltaFullSample = c(-0.01, -0.02, -0.01, 0.02),
    stringsAsFactors = FALSE
  )
  df <- ensureQuarterColumns(df)

  sp <- computeInbSwitchPoints(
    df,
    baselines = "proxy_frozen",
    estimate = "deltaFullSample",
    minFromSwitch = 1
  )
  expect_equal(nrow(sp), 1)
  expect_equal(sp$switchQuarterLabel, "2020-Q3")
  expect_equal(sp$W, "3m")
  expect_equal(sp$baseline, "proxy_frozen")
})
