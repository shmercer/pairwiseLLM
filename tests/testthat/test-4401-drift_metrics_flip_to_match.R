test_that("bt_drift_metrics flips negative correlation when flip_to_match = TRUE", {
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 2, 3))
  base <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, -2, -3))

  m <- bt_drift_metrics(cur, base, prefix = "x_", flip_to_match = TRUE)

  expect_equal(m$x_n, 3L)
  expect_true(isTRUE(m$x_flip_applied))

  # After flipping the baseline, the two vectors match exactly.
  expect_true(is.finite(m$x_theta_cor))
  expect_true(m$x_theta_cor >= 0)
  expect_equal(m$x_mean_abs_shift, 0)
  expect_equal(m$x_mean_signed_shift, 0)
})

test_that("bt_drift_metrics does not flip when flip_to_match = FALSE", {
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 2, 3))
  base <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, -2, -3))

  m <- bt_drift_metrics(cur, base, prefix = "x_", flip_to_match = FALSE)

  expect_equal(m$x_n, 3L)
  expect_false(isTRUE(m$x_flip_applied))

  expect_true(is.finite(m$x_theta_cor))
  expect_true(m$x_theta_cor < 0)

  # Without flipping, shifts are computed against the unflipped baseline.
  expect_equal(m$x_mean_abs_shift, 4)
  expect_equal(m$x_mean_signed_shift, 4)
})

test_that("bt_drift_metrics returns schema-stable NA correlations when n < 2", {
  cur <- tibble::tibble(ID = "A", theta = 1)
  base <- tibble::tibble(ID = "A", theta = -1)

  m <- bt_drift_metrics(cur, base, prefix = "x_", flip_to_match = TRUE)

  expect_equal(m$x_n, 1L)
  expect_true("x_flip_applied" %in% names(m))
  expect_true("x_theta_cor" %in% names(m))
  expect_true("x_theta_spearman" %in% names(m))
  expect_true(is.na(m$x_theta_cor))
  expect_true(is.na(m$x_theta_spearman))
})
