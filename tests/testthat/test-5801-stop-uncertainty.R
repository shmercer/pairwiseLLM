testthat::test_that("theta_sd subset median controls uncertainty pass", {
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
    list(S_subset = 3L, K_top = 2L, U_abs = 0.2)
  )
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  pass_draws <- matrix(
    c(0, 0, 0, 0, 0,
      0, 0.2, 0.4, 0.6, 0.8),
    nrow = 2,
    byrow = TRUE
  )
  colnames(pass_draws) <- state$ids
  pass_fit <- list(
    theta_draws = pass_draws,
    theta_mean = stats::setNames(colMeans(pass_draws), state$ids)
  )
  utilities <- tibble::tibble(utility = c(0.1, 0.05, 0.03))
  pass_metrics <- compute_stop_metrics(state, pass_fit, utilities, config_v3)
  testthat::expect_true(pass_metrics$theta_sd_pass)

  fail_draws <- matrix(
    c(0, 0, 0, 0, 0,
      0, 1, 2, 3, 4),
    nrow = 2,
    byrow = TRUE
  )
  colnames(fail_draws) <- state$ids
  fail_fit <- list(
    theta_draws = fail_draws,
    theta_mean = stats::setNames(colMeans(fail_draws), state$ids)
  )
  fail_metrics <- compute_stop_metrics(state, fail_fit, utilities, config_v3)
  testthat::expect_false(fail_metrics$theta_sd_pass)
})
