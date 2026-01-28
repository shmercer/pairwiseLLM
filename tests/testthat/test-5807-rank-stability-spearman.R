testthat::test_that("rank stability uses true Spearman without ties", {
  withr::local_seed(123)
  theta_t <- c(0.1, 0.3, 0.2, 0.4)
  theta_t_lag <- c(0.4, 0.2, 0.3, 0.1)

  samples <- tibble::tibble(
    ID = letters[seq_along(theta_t)],
    text = letters[seq_along(theta_t)]
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    stability_lag = 1L,
    rank_spearman_min = 0.5
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(stats::setNames(theta_t_lag, state$ids))

  draws <- matrix(rep(theta_t, times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  expected <- stats::cor(theta_t, theta_t_lag, method = "spearman", use = "complete.obs")
  testthat::expect_equal(metrics$rho_rank_lag, expected)
})

testthat::test_that("rank stability uses true Spearman with ties", {
  withr::local_seed(123)
  theta_t <- c(0.0, 0.0, 1.0, 2.0, 2.0)
  theta_t_lag <- c(0.0, 1.0, 1.0, 2.0, 0.0)

  samples <- tibble::tibble(
    ID = letters[seq_along(theta_t)],
    text = letters[seq_along(theta_t)]
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    stability_lag = 1L,
    rank_spearman_min = 0.5
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(stats::setNames(theta_t_lag, state$ids))

  draws <- matrix(rep(theta_t, times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  expected <- stats::cor(theta_t, theta_t_lag, method = "spearman", use = "complete.obs")
  testthat::expect_equal(metrics$rho_rank_lag, expected)
})

testthat::test_that("rank stability remains NA when ineligible", {
  withr::local_seed(123)
  theta_t <- c(0.1, 0.2, 0.3)
  theta_t_lag <- c(0.3, 0.2, 0.1)

  samples <- tibble::tibble(
    ID = letters[seq_along(theta_t)],
    text = letters[seq_along(theta_t)]
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    stability_lag = 2L,
    rank_spearman_min = 0.5
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(stats::setNames(theta_t_lag, state$ids))

  draws <- matrix(rep(theta_t, times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  testthat::expect_true(is.na(metrics$rho_rank_lag))
  testthat::expect_true(is.na(metrics$rank_stability_pass))
})
