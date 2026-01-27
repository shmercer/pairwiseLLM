testthat::test_that("stop triggers immediately when all gates pass", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- adaptive_v3_config(state$N, list(
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
    eap_pass = TRUE,
    theta_corr_pass = TRUE,
    delta_sd_theta_pass = TRUE,
    rho_rank_pass = TRUE,
    refit_performed = TRUE
  )

  out1 <- should_stop(metrics, state, config_v3)
  testthat::expect_true(out1$stop_decision)
  testthat::expect_identical(out1$state$stop_reason, "v3_converged")

  metrics_fail <- metrics
  metrics_fail$eap_pass <- FALSE
  out_fail <- should_stop(metrics_fail, state, config_v3)
  testthat::expect_false(out_fail$stop_decision)
})
