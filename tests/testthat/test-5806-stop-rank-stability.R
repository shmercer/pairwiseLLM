testthat::test_that("rank stability uses lagged Spearman correlation", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L,
    rank_spearman_min = 0.99
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  history <- list(stats::setNames(c(0, 1, 2, 3), state$ids))
  state$posterior$theta_mean_history <- history

  draws <- matrix(rep(c(3, 2, 1, 0), times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  testthat::expect_equal(metrics$rho_rank_lag, -1)
  testthat::expect_false(metrics$rank_stability_pass)
})

testthat::test_that("stop passes only update at refits and reset on diagnostics failure", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L,
    stability_consecutive = 2L,
    eap_reliability_min = 0.5
  ))
  state$config$v3 <- config_v3
  state$posterior$theta_mean_history <- list(stats::setNames(c(0, 1, 2), state$ids))

  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  state$fit <- make_v3_fit_contract(state$ids, theta_draws = draws)
  state$checks_passed_in_row <- 1L

  no_refit <- pairwiseLLM:::should_stop(
    metrics = list(hard_cap_reached = FALSE, refit_performed = FALSE),
    state = state,
    config = config_v3
  )
  testthat::expect_equal(no_refit$state$checks_passed_in_row, 1L)

  diag_fail <- pairwiseLLM:::should_stop(
    metrics = list(
      hard_cap_reached = FALSE,
      refit_performed = TRUE,
      diagnostics_pass = FALSE,
      reliability_EAP = 0.9,
      rho_theta_lag = 0.99,
      delta_sd_theta_lag = 0.01,
      rank_stability_pass = TRUE
    ),
    state = no_refit$state,
    config = config_v3
  )
  testthat::expect_equal(diag_fail$state$checks_passed_in_row, 0L)
})
