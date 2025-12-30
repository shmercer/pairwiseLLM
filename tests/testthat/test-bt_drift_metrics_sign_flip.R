test_that("bt_drift_metrics flips baseline sign when correlation is negative", {
  skip_if_not_installed("tibble")
  theta_ids <- paste0("id", 1:10)
  base <- tibble::tibble(ID = theta_ids, theta = rnorm(10), se = rep(0.1, 10))
  cur <- tibble::tibble(ID = theta_ids, theta = -base$theta, se = rep(0.1, 10))

  m <- bt_drift_metrics(theta = cur, baseline_theta = base, core_ids = theta_ids)

  # After sign alignment, we should not report a negative correlation.
  expect_true(isTRUE(m$core_flip_applied))
  expect_true(is.finite(m$core_theta_cor))
  expect_gte(m$core_theta_cor, 0)
})
