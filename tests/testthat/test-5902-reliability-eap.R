testthat::test_that("compute_reliability_EAP matches formula and handles edge cases", {
  draws <- matrix(
    c(
      0, 1, 2,
      1, 2, 3,
      2, 3, 4,
      3, 4, 5
    ),
    nrow = 4,
    byrow = TRUE
  )

  theta_mean <- colMeans(draws)
  theta_var <- apply(draws, 2, stats::var)
  expected <- stats::var(theta_mean) / (stats::var(theta_mean) + mean(theta_var))

  testthat::expect_equal(compute_reliability_EAP(draws), expected, tolerance = 1e-8)

  testthat::expect_true(is.na(compute_reliability_EAP(matrix(1, nrow = 1, ncol = 3))))
  testthat::expect_true(is.na(compute_reliability_EAP(matrix(1, nrow = 3, ncol = 1))))
  testthat::expect_true(is.na(compute_reliability_EAP(matrix(1, nrow = 3, ncol = 3))))
})
