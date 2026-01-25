testthat::test_that("compute_stop_metrics validates lagged theta history", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(c(0, 1, 2))

  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L
  ))

  testthat::expect_error(
    pairwiseLLM:::compute_stop_metrics(state, fit, tibble::tibble(), config_v3),
    "theta history"
  )
})

testthat::test_that("should_stop validates stability thresholds", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 6L)
  )
  state$posterior$theta_mean_history <- list(stats::setNames(c(0, 1, 2), state$ids))
  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  state$fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  config_bad <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L
  ))
  config_bad$theta_corr_min <- NA_real_

  metrics <- list(
    hard_cap_reached = FALSE,
    refit_performed = TRUE,
    diagnostics_pass = TRUE,
    reliability_EAP = 0.99,
    rho_theta_lag = 0.99,
    delta_sd_theta_lag = 0.01,
    rank_stability_pass = TRUE
  )

  testthat::expect_error(
    pairwiseLLM:::should_stop(metrics, state, config_bad),
    "theta_corr_min"
  )
})
