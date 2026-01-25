testthat::test_that("compute_stop_metrics reports theta_sd_eap and counts", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 12L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$comparisons_scheduled <- 5L
  state$comparisons_observed <- 3L

  draws <- matrix(
    c(1, 0, -1,
      1.2, 0.2, -0.8),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit,
    tibble::tibble(),
    config_v3
  )

  testthat::expect_true(is.finite(metrics$theta_sd_eap))
  testthat::expect_equal(metrics$scheduled_pairs, 5L)
  testthat::expect_equal(metrics$completed_pairs, 3L)
})
