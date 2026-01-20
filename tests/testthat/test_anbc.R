test_that("areaNetBenefitCurve computes trapezoidal A-NBC", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "metrics.R")
  expect_true(file.exists(helperPath))
  source(helperPath, local = TRUE)

  y <- c(1, 0, 1, 0)
  p <- c(0.9, 0.8, 0.2, 0.1)
  th <- c(0.2, 0.6)

  nb1 <- netBenefit(y, p, th[1])
  nb2 <- netBenefit(y, p, th[2])
  expected <- (th[2] - th[1]) * (nb1 + nb2) / 2

  got <- areaNetBenefitCurve(y, p, th)
  expect_equal(got, expected, tolerance = 1e-12)
})

test_that("areaNetBenefitCurve is not the same as INB mean unless normalized", {
  helperPath <- testthat::test_path("..", "..", "extras", "postAnalysis", "metrics.R")
  source(helperPath, local = TRUE)

  # Construct a case where NB varies across thresholds (so trapezoid and rectangle differ).
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.8, 0.2, 0.1)
  th <- c(0.05, 0.25, 0.85)

  inb <- integratedNetBenefit(y, p, th)
  anbc <- areaNetBenefitCurve(y, p, th)
  rectArea <- inb * (max(th) - min(th))

  # Trapezoid and rectangle differ because endpoints are weighted differently.
  expect_gt(abs(anbc - rectArea), 1e-8)
})
