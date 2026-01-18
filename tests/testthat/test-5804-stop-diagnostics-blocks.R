testthat::test_that("diagnostics failure blocks uncertainty and utility stopping", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 10L)
  )
  config_v3 <- adaptive_v3_config(state$N, list(checks_passed_target = 2L))
  state$config$v3 <- config_v3
  state$checks_passed_in_row <- 1L

  metrics <- list(
    hard_cap_reached = FALSE,
    diagnostics_pass = FALSE,
    theta_sd_pass = TRUE,
    U_pass = TRUE,
    U0 = 0.1,
    refit_performed = TRUE
  )

  out <- should_stop_v3(metrics, state, config_v3)
  testthat::expect_false(out$stop_decision)
  testthat::expect_equal(out$state$checks_passed_in_row, 0L)
})
