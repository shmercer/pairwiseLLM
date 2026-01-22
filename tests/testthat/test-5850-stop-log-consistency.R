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
    "theta_sd_median_S",
    "theta_sd_pass",
    "tau",
    "U0",
    "U_top_median",
    "U_abs",
    "U_pass",
    "rank_stability_pass",
    "frac_weak_adj",
    "min_adj_prob",
    "weak_adj_threshold",
    "weak_adj_frac_max",
    "min_adj_prob_threshold",
    "min_new_pairs_for_check",
    "refit_performed",
    "candidate_starved",
    "reason_short_batch"
  )

  testthat::expect_true(is.list(metrics))
  testthat::expect_true(all(required %in% names(metrics)))
  testthat::expect_false(any(vapply(metrics[required], is.null, logical(1L))))
})

testthat::test_that("round_log uses the same U_pass as stop_metrics", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$posterior$diagnostics_pass <- TRUE
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )

  utilities <- tibble::tibble(utility = c(0.02, 0.01, 0.03))
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

  testthat::expect_identical(as.logical(round_row$U_pass), as.logical(metrics$U_pass))
  testthat::expect_identical(as.integer(metrics$divergences), 0L)
})
