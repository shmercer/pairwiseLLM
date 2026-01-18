testthat::test_that("compute_gini_degree matches expected cases", {
  testthat::expect_equal(compute_gini_degree(c(2, 2, 2, 2)), 0)
  testthat::expect_equal(compute_gini_degree(c(0, 0, 0, 10)), 0.75, tolerance = 1e-8)
  testthat::expect_equal(compute_gini_degree(c(0, 0, 0)), 0)

  testthat::expect_error(
    compute_gini_degree(c(1, -1, 2)),
    "`deg` must be non-negative."
  )
})
