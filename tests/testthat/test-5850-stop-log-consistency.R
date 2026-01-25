testthat::test_that("stop_metrics defaults are complete and non-null", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  metrics <- state$posterior$stop_metrics
  required <- c(
    "hard_cap_reached",
    "hard_cap_threshold",
    "n_unique_pairs_seen",
    "scheduled_pairs",
    "proposed_pairs",
    "completed_pairs",
    "diagnostics_pass",
    "divergences",
    "min_ess_bulk",
    "max_rhat",
    "reliability_EAP",
    "theta_sd_eap",
    "rho_theta_lag",
    "delta_sd_theta_lag",
    "rho_rank_lag",
    "rank_stability_pass",
    "refit_performed",
    "candidate_starved",
    "reason_short_batch"
  )

  testthat::expect_true(is.list(metrics))
  testthat::expect_true(all(required %in% names(metrics)))
  testthat::expect_false(any(vapply(metrics[required], is.null, logical(1L))))
})

testthat::test_that("round_log uses the same rank_stability_pass as stop_metrics", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$posterior$diagnostics_pass <- TRUE
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = theta_draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )

  utilities <- tibble::tibble()
  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = utilities,
    config = config
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)
  round_row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    config = config
  )

  testthat::expect_identical(
    as.logical(round_row$rank_stability_pass),
    as.logical(metrics$rank_stability_pass)
  )
  testthat::expect_identical(as.integer(metrics$divergences), 0L)
})
