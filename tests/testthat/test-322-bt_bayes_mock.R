test_that("Bayesian BT path is exercised via mockable rstanarm wrappers", {
  # Use a small connected graph.
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "A", "B")
  )

  # Mock rstanarm availability and internal wrappers so we can cover the
  # Bayesian BT code path without installing suggested dependencies.
  testthat::local_mocked_bindings(
    .require_ns = function(pkg, quietly = TRUE) {
      identical(pkg, "rstanarm")
    },
    .rstanarm_normal = function(...) NULL,
    .rstanarm_stan_glm = function(...) {
      # Return a posterior draw matrix with coefficient names like those
      # produced by model.matrix(~ object1 - 1, ...) after dropping one column.
      set.seed(1)
      m <- matrix(rnorm(200), ncol = 2)
      colnames(m) <- c("object1B", "object1C")
      m
    },
    .package = "pairwiseLLM"
  )

  out <- compute_final_estimates(res, fit_engine_final = "bt_bayes", rc_damping = 0.05)

  expect_true(is.list(out))
  expect_s3_class(out$estimates, "tbl_df")
  expect_true(all(c("bt_engine_requested", "bt_engine_used", "bt_status") %in% names(out$estimates)))

  # With a successful mocked Bayesian fit, we should record success and produce
  # non-missing BT columns.
  expect_true(all(out$estimates$bt_engine_requested == "bt_bayes"))
  expect_true(all(out$estimates$bt_status == "succeeded"))
  expect_true(all(out$estimates$bt_engine_used == "bt_bayes"))
  expect_true(any(!is.na(out$estimates$theta_bt_firth)))
})


test_that("Bayesian BT failure falls back to Rank Centrality with provenance", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "A", "B")
  )

  testthat::local_mocked_bindings(
    .require_ns = function(pkg, quietly = TRUE) {
      identical(pkg, "rstanarm")
    },
    .rstanarm_normal = function(...) NULL,
    .rstanarm_stan_glm = function(...) {
      stop("simulated stan_glm failure")
    },
    .package = "pairwiseLLM"
  )

  out <- compute_final_estimates(res, fit_engine_final = "bt_bayes", rc_damping = 0.05)
  est <- out$estimates

  expect_true(all(est$bt_engine_requested == "bt_bayes"))
  expect_true(all(est$bt_status == "fallback"))
  expect_true(all(est$bt_engine_used == "rank_centrality"))
  expect_true(any(is.na(est$theta_bt_firth)))
  expect_true(any(!is.na(est$theta_rc)))
  expect_true(is.character(out$diagnostics$bt_failure_reason) || is.na(out$diagnostics$bt_failure_reason))
})
