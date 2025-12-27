test_that("bt_drift_metrics computes drift summaries on shared IDs", {
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2))
  prev <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0.5, 2.5))

  m <- bt_drift_metrics(cur, prev, prefix = "core_")
  expect_s3_class(m, "tbl_df")
  expect_equal(nrow(m), 1L)

  expect_true(all(c(
    "core_n",
    "core_theta_cor",
    "core_theta_spearman",
    "core_mean_abs_shift",
    "core_p90_abs_shift",
    "core_p95_abs_shift",
    "core_max_abs_shift",
    "core_mean_signed_shift"
  ) %in% names(m)))

  expect_true(is.finite(m$core_theta_cor))
  expect_true(m$core_theta_cor > 0.94)
  expect_equal(m$core_theta_spearman, 1)
})

test_that("bt_drift_metrics respects ids subset", {
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2))
  prev <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0.5, 2.5))

  m <- bt_drift_metrics(cur, prev, ids = c("A", "B"), prefix = "core_")
  expect_equal(m$core_n, 2L)
  expect_equal(m$core_mean_abs_shift, 0.25)
})

test_that("bt_drift_metrics accepts fits and named vectors", {
  cur_fit <- list(theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1)))
  prev_fit <- list(theta = tibble::tibble(ID = c("A", "B"), theta = c(0.2, 0.8)))

  m1 <- bt_drift_metrics(cur_fit, prev_fit, prefix = "x_")
  expect_equal(m1$x_n, 2L)
  expect_true(is.finite(m1$x_mean_abs_shift))

  cur_vec <- c(A = 0, B = 1)
  prev_vec <- c(A = 0.2, B = 0.8)
  m2 <- bt_drift_metrics(cur_vec, prev_vec, prefix = "")
  expect_equal(m2$n, 2L)
  expect_true(is.finite(m2$mean_abs_shift))
})

test_that("bt_drift_metrics returns NA metrics when there is no overlap", {
  cur <- tibble::tibble(ID = c("A", "B"), theta = c(0, 1))
  prev <- tibble::tibble(ID = c("C", "D"), theta = c(0, 1))

  m <- bt_drift_metrics(cur, prev, prefix = "p_")
  expect_equal(m$p_n, 0L)
  expect_true(is.na(m$p_mean_abs_shift))
  expect_true(is.na(m$p_theta_cor))
  expect_true(is.na(m$p_theta_spearman))
})

test_that("bt_drift_metrics validates inputs", {
  cur <- tibble::tibble(ID = c("A", "B"), theta = c(0, 1))
  prev <- tibble::tibble(ID = c("A", "B"), theta = c(0.2, 0.8))

  expect_error(bt_drift_metrics(cur, prev, prefix = c("a", "b")), "prefix")
  # Use NA/Inf to hit the function's abs_shift_probs validation (not quantile())
  expect_error(bt_drift_metrics(cur, prev, abs_shift_probs = c(0.9, NA_real_)), "abs_shift_probs")
  expect_error(bt_drift_metrics(cur, prev, abs_shift_probs = c(0.9, Inf)), "abs_shift_probs")
  expect_error(bt_drift_metrics(cur, prev, methods = c("pearson", "bad")), "methods")
  expect_error(bt_drift_metrics(cur, prev, ids = 1:2), "ids")

  expect_error(bt_drift_metrics(1, prev), "current")
})

