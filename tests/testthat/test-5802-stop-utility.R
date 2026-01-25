testthat::test_that("EAP reliability gate controls stop passes", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L, M1_target = 0L))
  config_v3 <- adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L,
    stability_consecutive = 2L
  ))
  state$config$v3 <- config_v3
  state$posterior$theta_mean_history <- list(stats::setNames(c(0, 1, 2), state$ids))

  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  state$fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- list(
    hard_cap_reached = FALSE,
    refit_performed = TRUE,
    diagnostics_pass = TRUE,
    reliability_EAP = 0.5,
    rho_theta_lag = 1,
    delta_sd_theta_lag = 0,
    rank_stability_pass = TRUE
  )
  out <- should_stop(metrics, state, config_v3)

  testthat::expect_false(out$stop_decision)
  testthat::expect_equal(out$state$checks_passed_in_row, 0L)
})
