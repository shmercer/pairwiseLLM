testthat::test_that("hard cap fraction controls the threshold", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = LETTERS[1:10],
    text = paste0("text-", seq_len(10))
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 20L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(hard_cap_frac = 0.10)
  )
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  pair_keys <- names(state$pair_count)[seq_len(5L)]
  state$pair_count[pair_keys] <- 1L

  draws <- matrix(seq(0, 0.9, length.out = 20), nrow = 2L, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  utilities <- tibble::tibble(utility = c(0.1, 0.05, 0.03))
  metrics <- pairwiseLLM:::compute_stop_metrics(state, fit, utilities, config_v3)
  metrics$refit_performed <- TRUE
  stop_out <- pairwiseLLM:::should_stop(metrics, state, config_v3)

  testthat::expect_equal(metrics$hard_cap_threshold, 5)
  testthat::expect_true(metrics$hard_cap_reached)
  testthat::expect_true(stop_out$stop_decision)
  testthat::expect_identical(stop_out$state$stop_reason, "hard_cap_40pct")
})

testthat::test_that("hard cap fraction validation rejects invalid values", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste0("text-", seq_len(5))
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 4L, budget_max = 20L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N
  )
  config_v3$hard_cap_frac <- 1.2
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  draws <- matrix(seq(0, 0.4, length.out = 10), nrow = 2L, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  utilities <- tibble::tibble(utility = c(0.1, 0.05, 0.03))

  testthat::expect_error(
    pairwiseLLM:::compute_stop_metrics(state, fit, utilities, config_v3),
    "hard_cap_frac"
  )
})
