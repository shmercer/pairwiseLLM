testthat::test_that("rank stability uses lagged Spearman correlation", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L,
    rank_spearman_min = 0.99
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  history <- list(stats::setNames(c(1, 2, 4, 8), state$ids))
  state$posterior$theta_mean_history <- history

  draws <- matrix(rep(c(1, 2, 3, 4), times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  expected_spearman <- stats::cor(c(1, 2, 3, 4), c(1, 2, 4, 8), method = "spearman")
  expected_pearson <- stats::cor(c(1, 2, 3, 4), c(1, 2, 4, 8), method = "pearson")
  testthat::expect_equal(metrics$rho_rank_lag, expected_spearman)
  testthat::expect_false(isTRUE(all.equal(metrics$rho_rank_lag, expected_pearson)))
  testthat::expect_true(metrics$rho_rank_pass)
})

testthat::test_that("theta stability uses Pearson correlation", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L, M1_target = 2L))
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 1L
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(stats::setNames(c(1, 2, 4, 8), state$ids))

  draws <- matrix(rep(c(1, 2, 3, 4), times = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  expected_pearson <- stats::cor(c(1, 2, 3, 4), c(1, 2, 4, 8), method = "pearson")
  expected_spearman <- stats::cor(c(1, 2, 3, 4), c(1, 2, 4, 8), method = "spearman")
  testthat::expect_equal(metrics$rho_theta_lag, expected_pearson)
  testthat::expect_false(isTRUE(all.equal(metrics$rho_theta_lag, expected_spearman)))
})

testthat::test_that("lagged flags are NA when ineligible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L, M1_target = 2L))
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    min_refits_for_stability = 2L,
    stability_lag = 2L
  ))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list(stats::setNames(c(0, 1, 2), state$ids))

  draws <- matrix(rep(c(0, 1, 2), each = 2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )

  testthat::expect_true(is.na(metrics$rho_theta_lag))
  testthat::expect_true(is.na(metrics$theta_corr_pass))
  testthat::expect_true(is.na(metrics$delta_sd_theta_lag))
  testthat::expect_true(is.na(metrics$delta_sd_theta_pass))
  testthat::expect_true(is.na(metrics$rho_rank_lag))
  testthat::expect_true(is.na(metrics$rho_rank_pass))
})
