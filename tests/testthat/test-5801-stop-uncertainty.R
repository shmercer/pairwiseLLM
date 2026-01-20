testthat::test_that("theta_sd subset median controls uncertainty pass", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E", "F"),
    text = c("alpha", "bravo", "charlie", "delta", "echo", "foxtrot")
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

  pass_draws <- matrix(c(
    -5, -20, -20, -20, -5, 5,
    -4.8, 12, 14, 16, 3, 5.2
  ), nrow = 2, byrow = TRUE)
  colnames(pass_draws) <- state$ids
  pass_fit <- list(
    theta_draws = pass_draws,
    theta_mean = stats::setNames(colMeans(pass_draws), state$ids)
  )
  utilities <- tibble::tibble(utility = c(0.1, 0.05, 0.03))
  pass_metrics <- compute_stop_metrics(state, pass_fit, utilities, config_v3)
  testthat::expect_true(pass_metrics$theta_sd_pass)

  fail_draws <- matrix(c(
    -5, -20, -20, -20, -5, 5,
    -2, 12, 14, 16, -2, 8
  ), nrow = 2, byrow = TRUE)
  colnames(fail_draws) <- state$ids
  fail_fit <- list(
    theta_draws = fail_draws,
    theta_mean = stats::setNames(colMeans(fail_draws), state$ids)
  )
  fail_metrics <- compute_stop_metrics(state, fail_fit, utilities, config_v3)
  testthat::expect_false(fail_metrics$theta_sd_pass)
})

testthat::test_that("stopping subset tie-breaking is deterministic by item id", {
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

  draws <- matrix(c(
    0, 0.9, -12, 9.9, 0,
    0.2, 1.1, 16, 10.1, 20
  ), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- list(
    theta_draws = draws,
    theta_mean = stats::setNames(colMeans(draws), state$ids)
  )
  metrics <- compute_stop_metrics(state, fit, tibble::tibble(utility = 0.1), config_v3)
  testthat::expect_true(metrics$theta_sd_pass)
})
