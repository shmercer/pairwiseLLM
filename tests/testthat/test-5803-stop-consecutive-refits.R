testthat::test_that("consecutive passing refits trigger v3 convergence", {
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

  metrics <- list(
    hard_cap_reached = FALSE,
    diagnostics_pass = TRUE,
    theta_sd_pass = TRUE,
    U_pass = TRUE,
    U0 = 0.1,
    refit_performed = TRUE
  )

  out1 <- should_stop_v3(metrics, state, config_v3)
  testthat::expect_false(out1$stop_decision)
  testthat::expect_equal(out1$state$checks_passed_in_row, 1L)

  out2 <- should_stop_v3(metrics, out1$state, config_v3)
  testthat::expect_true(out2$stop_decision)
  testthat::expect_identical(out2$state$stop_reason, "v3_converged")

  state_fail <- out1$state
  metrics_fail <- metrics
  metrics_fail$U_pass <- FALSE
  out_fail <- should_stop_v3(metrics_fail, state_fail, config_v3)
  testthat::expect_false(out_fail$stop_decision)
  testthat::expect_equal(out_fail$state$checks_passed_in_row, 0L)
})
