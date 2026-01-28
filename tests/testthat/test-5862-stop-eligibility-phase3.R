testthat::test_that("stop ignores lagged gates when lag is ineligible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 6L)
  )
  state$phase <- "phase2"
  state$mode <- "adaptive"
  state$comparisons_observed <- 1L
  state$comparisons_scheduled <- 1L
  state$last_refit_at <- 0L
  state$new_since_refit <- 1L
  A_id <- state$ids[[1L]]
  B_id <- state$ids[[2L]]
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- paste0(unordered_key, "#1")
  created_at <- as.POSIXct("2024-01-01", tz = "UTC")
  state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = "phase2",
    iter = 1L,
    created_at = created_at
  )
  state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    better_id = A_id,
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = created_at,
    backend = "test",
    model = "test"
  )

  draws <- matrix(
    c(0, 1, 2, 0.1, 1.1, 2.1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)
  state$fit <- fit
  state$posterior$diagnostics_pass <- TRUE
  state$posterior$theta_mean_history <- list()

  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    stability_lag = 3L,
    eap_reliability_min = 0,
    theta_corr_min = 0,
    theta_sd_rel_change_max = 1,
    rank_spearman_min = 0
  ))

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = tibble::tibble(),
    config = config_v3
  )
  metrics$refit_performed <- TRUE

  out <- pairwiseLLM:::should_stop(metrics, state, config_v3, fit = fit)

  testthat::expect_true(out$stop_decision)
  testthat::expect_true(is.na(metrics$rho_theta_lag))
  testthat::expect_true(is.na(metrics$theta_corr_pass))
  testthat::expect_true(is.na(metrics$delta_sd_theta_lag))
  testthat::expect_true(is.na(metrics$delta_sd_theta_pass))
  testthat::expect_true(is.na(metrics$rho_rank_lag))
  testthat::expect_true(is.na(metrics$rho_rank_pass))
})

testthat::test_that("lagged gates are required once eligible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  base_state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 6L)
  )
  base_state$phase <- "phase2"
  base_state$mode <- "adaptive"
  base_state$comparisons_observed <- 1L
  base_state$comparisons_scheduled <- 1L
  base_state$last_refit_at <- 0L
  base_state$new_since_refit <- 1L
  A_id <- base_state$ids[[1L]]
  B_id <- base_state$ids[[2L]]
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- paste0(unordered_key, "#1")
  created_at <- as.POSIXct("2024-01-01", tz = "UTC")
  base_state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = base_state$texts[[A_id]],
    B_text = base_state$texts[[B_id]],
    phase = "phase2",
    iter = 1L,
    created_at = created_at
  )
  base_state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    better_id = A_id,
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = created_at,
    backend = "test",
    model = "test"
  )

  draws <- matrix(
    c(0, 1, 2, 0.1, 1.1, 2.1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, base_state$ids)
  )
  base_state$fit <- make_v3_fit_contract(base_state$ids, theta_draws = draws)
  base_state$posterior$theta_mean_history <- list(
    stats::setNames(c(0, 1, 2), base_state$ids)
  )

  config_v3 <- pairwiseLLM:::adaptive_v3_config(base_state$N, list(
    stability_lag = 1L,
    eap_reliability_min = 0,
    theta_corr_min = 0.9,
    theta_sd_rel_change_max = 0.1,
    rank_spearman_min = 0.9
  ))

  metrics_fail <- list(
    refit_performed = TRUE,
    diagnostics_pass = TRUE,
    eap_pass = TRUE,
    theta_corr_pass = FALSE,
    delta_sd_theta_pass = TRUE,
    rho_rank_pass = TRUE
  )
  out_fail <- pairwiseLLM:::should_stop(metrics_fail, base_state, config_v3, fit = base_state$fit)
  testthat::expect_false(out_fail$stop_decision)

  metrics_pass <- metrics_fail
  metrics_pass$theta_corr_pass <- TRUE
  out_pass <- pairwiseLLM:::should_stop(metrics_pass, base_state, config_v3, fit = base_state$fit)
  testthat::expect_true(out_pass$stop_decision)
})

testthat::test_that("phase3 entry is restricted to phase2", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  make_state <- function(phase) {
    state <- pairwiseLLM:::adaptive_state_new(
      samples = samples,
      config = list(d1 = 2L, M1_target = 0L, budget_max = 6L)
    )
    state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
      eap_reliability_min = 0.95,
      stability_lag = 2L,
      model_variant = "btl"
    ))
    state$phase <- phase
    state$mode <- "adaptive"
    state$iter <- 1L

    A_id <- state$ids[[1L]]
    B_id <- state$ids[[2L]]
    unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
    ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
    pair_uid <- paste0(unordered_key, "#1")
    created_at <- as.POSIXct("2024-01-01", tz = "UTC")

    state$history_pairs <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = state$texts[[A_id]],
      B_text = state$texts[[B_id]],
      phase = phase,
      iter = 1L,
      created_at = created_at
    )
    state$history_results <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      better_id = A_id,
      winner_pos = 1L,
      phase = phase,
      iter = 1L,
      received_at = created_at,
      backend = "test",
      model = "test"
    )
    state$comparisons_scheduled <- 1L
    state$comparisons_observed <- 1L
    state$last_refit_at <- 0L
    state$new_since_refit <- 1L
    state
  }

  mock_fit <- function(bt_data, config, seed = NULL) {
    draws <- matrix(
      c(0, 1, 2, 0.1, 1.1, 2.1),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(NULL, samples$ID)
    )
    list(
      model_variant = "btl",
      draws = list(theta = draws),
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
      mcmc_config_used = list(chains = 2L, parallel_chains = 2L, core_fraction = 1)
    )
  }

  out_phase1 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_stopping_checks(
      make_state("phase1"),
      adaptive = list(),
      seed = 1L
    ),
    .fit_bayes_btl_mcmc_adaptive = mock_fit,
    .package = "pairwiseLLM"
  )
  testthat::expect_identical(out_phase1$state$phase, "phase1")

  out_phase2 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_stopping_checks(
      make_state("phase2"),
      adaptive = list(),
      seed = 1L
    ),
    .fit_bayes_btl_mcmc_adaptive = mock_fit,
    .package = "pairwiseLLM"
  )
  testthat::expect_identical(out_phase2$state$phase, "phase3")
})
