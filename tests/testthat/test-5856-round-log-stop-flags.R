testthat::test_that("round_log records stop flag values from metrics", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L, M1_target = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  metrics <- list(
    diagnostics_pass = TRUE,
    eap_pass = TRUE,
    theta_corr_pass = FALSE,
    delta_sd_theta_pass = TRUE,
    rho_rank_pass = FALSE,
    rank_stability_pass = FALSE,
    stop_eligible = TRUE
  )
  fit <- make_v3_fit_contract(state$ids)

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    config = state$config$v3,
    new_pairs = 0L
  )

  testthat::expect_identical(row$eap_pass[[1L]], TRUE)
  testthat::expect_identical(row$theta_corr_pass[[1L]], FALSE)
  testthat::expect_identical(row$delta_sd_theta_pass[[1L]], TRUE)
  testthat::expect_identical(row$rho_rank_pass[[1L]], FALSE)
})

testthat::test_that("round_log keeps lagged stop flags NA when ineligible", {
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

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    config = config_v3,
    new_pairs = 0L
  )

  testthat::expect_true(is.na(row$rho_theta_lag[[1L]]))
  testthat::expect_true(is.na(row$theta_corr_pass[[1L]]))
  testthat::expect_true(is.na(row$delta_sd_theta_lag[[1L]]))
  testthat::expect_true(is.na(row$delta_sd_theta_pass[[1L]]))
  testthat::expect_true(is.na(row$rho_rank_lag[[1L]]))
  testthat::expect_true(is.na(row$rho_rank_pass[[1L]]))
})
