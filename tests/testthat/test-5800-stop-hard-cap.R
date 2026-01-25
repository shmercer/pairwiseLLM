testthat::test_that("hard cap stop triggers immediately", {
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

  pair_keys <- names(state$pair_count)[seq_len(4L)]
  state$pair_count[pair_keys] <- 1L

  draws <- matrix(
    c(0, 0.1, 0.2, 0.3, 0.4,
      0.2, 0.3, 0.4, 0.5, 0.6),
    nrow = 2,
    byrow = TRUE
  )
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  utilities <- tibble::tibble(utility = c(0.1, 0.05, 0.03))
  metrics <- compute_stop_metrics(state, fit, utilities, config_v3)
  metrics$refit_performed <- TRUE
  stop_out <- should_stop(metrics, state, config_v3)

  testthat::expect_true(stop_out$stop_decision)
  testthat::expect_identical(stop_out$state$stop_reason, "hard_cap_40pct")
  testthat::expect_identical(stop_out$state$mode, "stopped")
})
