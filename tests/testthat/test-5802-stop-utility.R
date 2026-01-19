testthat::test_that("utility top-K median controls U_pass", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E"),
    text = c("alpha", "bravo", "charlie", "delta", "echo")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 20L)
  )
  config_v3 <- adaptive_v3_config(
    state$N,
    list(S_subset = 3L, K_top = 3L, U_abs = 0.15)
  )
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  draws <- matrix(
    c(0, 0.1, 0.2, 0.3, 0.4,
      0.2, 0.3, 0.4, 0.5, 0.6),
    nrow = 2,
    byrow = TRUE
  )
  colnames(draws) <- state$ids
  fit <- list(
    theta_draws = draws,
    theta_mean = stats::setNames(colMeans(draws), state$ids)
  )

  utilities_pass <- tibble::tibble(utility = c(0.12, 0.1, 0.08, 0.05))
  metrics_pass <- compute_stop_metrics(state, fit, utilities_pass, config_v3)
  testthat::expect_true(metrics_pass$U_pass)
  testthat::expect_equal(metrics_pass$U0, metrics_pass$U_top_median)

  utilities_fail <- tibble::tibble(utility = c(0.3, 0.25, 0.2, 0.1))
  metrics_fail <- compute_stop_metrics(state, fit, utilities_fail, config_v3)
  testthat::expect_false(metrics_fail$U_pass)
})
