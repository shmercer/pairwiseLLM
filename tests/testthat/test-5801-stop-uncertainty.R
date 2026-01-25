testthat::test_that("lagged theta stability metrics compute when eligible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 20L)
  )
  config_v3 <- adaptive_v3_config(state$N)
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  history <- list(
    stats::setNames(c(0, 1, 2, 3), state$ids),
    stats::setNames(c(0.1, 1.1, 2.1, 3.1), state$ids)
  )
  state$posterior$theta_mean_history <- history

  draws <- matrix(rep(c(0, 1, 2, 3), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- compute_stop_metrics(state, fit, tibble::tibble(), config_v3)
  testthat::expect_equal(metrics$rho_theta_lag, 1)
  testthat::expect_equal(metrics$delta_sd_theta_lag, 0)
  testthat::expect_equal(metrics$rho_rank_lag, 1)
  testthat::expect_true(metrics$rank_stability_pass)
})

testthat::test_that("lagged stability metrics are NA before eligibility", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 20L)
  )
  config_v3 <- adaptive_v3_config(state$N)
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  history <- list(stats::setNames(c(0, 1, 2), state$ids))
  state$posterior$theta_mean_history <- history

  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- compute_stop_metrics(state, fit, tibble::tibble(), config_v3)
  testthat::expect_true(is.na(metrics$rho_theta_lag))
  testthat::expect_true(is.na(metrics$delta_sd_theta_lag))
  testthat::expect_true(is.na(metrics$rho_rank_lag))
  testthat::expect_true(is.na(metrics$rank_stability_pass))
})
