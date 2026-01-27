testthat::test_that("compute_stop_metrics uses Spearman rho without ties", {
  withr::local_seed(1)
  ids <- c("A", "B", "C")
  samples <- tibble::tibble(ID = ids, text = ids)
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(min_refits_for_stability = 2L, stability_lag = 1L, rank_spearman_min = 0, hard_cap_frac = 1)
  )

  lag_theta <- stats::setNames(c(1, 2, 3), ids)
  state$posterior$theta_mean_history <- list(lag_theta)

  theta_draws <- matrix(
    c(3, 2, 1, 3, 2, 1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, ids)
  )
  fit <- pairwiseLLM:::build_v3_fit_contract(theta_draws = theta_draws, model_variant = "btl")

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = state$config$v3
  )

  expected <- stats::cor(c(3, 2, 1), c(1, 2, 3), method = "spearman", use = "complete.obs")
  testthat::expect_equal(metrics$rho_rank_lag, expected, tolerance = 1e-8)
})

testthat::test_that("compute_stop_metrics uses Spearman rho with ties", {
  withr::local_seed(1)
  ids <- c("A", "B", "C")
  samples <- tibble::tibble(ID = ids, text = ids)
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(min_refits_for_stability = 2L, stability_lag = 1L, rank_spearman_min = 0, hard_cap_frac = 1)
  )

  lag_theta <- stats::setNames(c(0, 1, 1), ids)
  state$posterior$theta_mean_history <- list(lag_theta)

  theta_draws <- matrix(
    c(1, 1, 0, 1, 1, 0),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, ids)
  )
  fit <- pairwiseLLM:::build_v3_fit_contract(theta_draws = theta_draws, model_variant = "btl")

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = state$config$v3
  )

  expected <- stats::cor(c(1, 1, 0), c(0, 1, 1), method = "spearman", use = "complete.obs")
  testthat::expect_equal(metrics$rho_rank_lag, expected, tolerance = 1e-8)
})

testthat::test_that("compute_stop_metrics reports NA rho when ineligible", {
  withr::local_seed(1)
  ids <- c("A", "B", "C")
  samples <- tibble::tibble(ID = ids, text = ids)
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(min_refits_for_stability = 3L, stability_lag = 2L, rank_spearman_min = 0, hard_cap_frac = 1)
  )

  lag_theta <- stats::setNames(c(1, 2, 3), ids)
  state$posterior$theta_mean_history <- list(lag_theta)

  theta_draws <- matrix(
    c(3, 2, 1, 3, 2, 1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, ids)
  )
  fit <- pairwiseLLM:::build_v3_fit_contract(theta_draws = theta_draws, model_variant = "btl")

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = state$config$v3
  )

  testthat::expect_true(is.na(metrics$rho_rank_lag))
})
