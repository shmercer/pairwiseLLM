testthat::test_that("consecutive passing refits trigger v3 convergence", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- adaptive_v3_config(state$N, list(
    stability_consecutive = 2L,
    min_refits_for_stability = 2L,
    stability_lag = 1L,
    eap_reliability_min = 0.9
  ))
  state$config$v3 <- config_v3
  state$posterior$theta_mean_history <- list(stats::setNames(c(0, 1, 2, 3), state$ids))

  draws <- matrix(rep(c(0, 1, 2, 3), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  state$fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- list(
    hard_cap_reached = FALSE,
    diagnostics_pass = TRUE,
    reliability_EAP = 0.95,
    rho_theta_lag = 0.995,
    delta_sd_theta_lag = 0.001,
    rank_stability_pass = TRUE,
    refit_performed = TRUE
  )

  out1 <- should_stop(metrics, state, config_v3)
  testthat::expect_false(out1$stop_decision)
  testthat::expect_equal(out1$state$checks_passed_in_row, 1L)

  out2 <- should_stop(metrics, out1$state, config_v3)
  testthat::expect_true(out2$stop_decision)
  testthat::expect_identical(out2$state$stop_reason, "v3_converged")

  state_fail <- out1$state
  metrics_fail <- metrics
  metrics_fail$reliability_EAP <- 0.5
  out_fail <- should_stop(metrics_fail, state_fail, config_v3)
  testthat::expect_false(out_fail$stop_decision)
  testthat::expect_equal(out_fail$state$checks_passed_in_row, 0L)
})
