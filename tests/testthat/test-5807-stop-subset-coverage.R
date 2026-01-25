testthat::test_that("stop subset selection covers top, bottom, and uncertainty ids", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E", "F"),
    text = c("alpha", "bravo", "charlie", "delta", "echo", "foxtrot")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 12L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(S_subset = 3L, K_top = 1L, U_abs = 0.5)
  )
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  draws <- matrix(
    c(
      3, 2, 1, 0, -1, -2,
      3.2, 2.1, 0.9, 0.1, -0.8, -2.2
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit,
    tibble::tibble(utility = 0.1),
    config_v3
  )

  testthat::expect_true(is.finite(metrics$theta_sd_median_S))
  testthat::expect_true(is.logical(metrics$theta_sd_pass))
  testthat::expect_true(!is.na(metrics$rank_stability_pass))
  testthat::expect_equal(metrics$scheduled_pairs, 0L)
  testthat::expect_equal(metrics$completed_pairs, 0L)
})
