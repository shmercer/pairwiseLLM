testthat::test_that("compute_gini_degree matches expected cases", {
  testthat::expect_equal(compute_gini_degree(c(2, 2, 2, 2)), 0)
  testthat::expect_equal(compute_gini_degree(c(0, 0, 0, 10)), 0.75, tolerance = 1e-8)
  testthat::expect_equal(compute_gini_degree(c(0, 0, 0)), 0)

  testthat::expect_error(
    compute_gini_degree(c(1, -1, 2)),
    "`deg` must be non-negative."
  )
})

testthat::test_that("compute_gini_posA handles degree adjustments and empty rates", {
  testthat::expect_equal(
    compute_gini_posA(c(1, 2), deg = c(2, 4)),
    0
  )

  testthat::expect_true(
    is.na(compute_gini_posA(c(1, 2), deg = c(0, 0)))
  )
})
