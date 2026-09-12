# Deterministic completed-result fixtures for rubric contract tests.
rubric_test_fit <- function(variant = "btl_e_b", shift = 0) {
  draws <- outer(c(-0.15, -0.05, 0.05, 0.15), c(-1.5, -0.5, 0.5, 1.5) + shift, `+`)
  colnames(draws) <- letters[1:4]
  pairwiseLLM:::build_btl_fit_contract(draws,
    epsilon_draws = if (pairwiseLLM:::model_has_e(variant)) c(0.03, 0.04, 0.05, 0.06) else NULL,
    beta_draws = if (pairwiseLLM:::model_has_b(variant)) c(-0.1, 0, 0.1, 0.2) else NULL,
    model_variant = variant,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
    diagnostics_pass = TRUE)
}

rubric_test_fixed <- function(variant = "btl_e_b", shift = 0) {
  fit <- rubric_test_fit(variant, shift)
  list(fit = fit, fits = list(fit),
    item_log_list = list(tibble::tibble(refit_id = 1L, ID = names(fit$theta_mean),
      theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
    round_log = tibble::tibble(round_id = 1L, model_variant = variant, reliability_EAP = 0.95))
}

rubric_test_adaptive <- function(variant = "btl_e_b") {
  state <- pairwiseLLM::adaptive_rank_start(
    tibble::tibble(item_id = letters[1:4], global_item_id = letters[1:4]),
    seed = 13L, now_fn = function() as.POSIXct("2026-09-12", tz = "UTC"))
  state$btl_fit <- pairwiseLLM:::.adaptive_btl_adapt_fit(rubric_test_fit(variant))
  state$config$btl_config$model_variant <- variant
  state$item_log <- list(pairwiseLLM:::.adaptive_build_item_log_refit(state, 1L))
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log,
    list(refit_id = 1L, step_id_at_refit = 3L, total_pairs_done = 3L,
      model_variant = variant, stop_decision = TRUE, stop_reason = "btl_converged", reliability_EAP = 0.95))
  state$refit_meta$last_refit_round_id <- 1L
  state$refit_meta$last_refit_step <- 3L
  state$refit_meta$last_refit_M_done <- 3L
  state$meta$stop_decision <- TRUE
  state$meta$stop_reason <- "btl_converged"
  state$step_log <- tibble::tibble(pair_id = 1:3)
  state$history_pairs <- tibble::tibble(A_id = letters[1:3], B_id = letters[2:4])
  state
}

rubric_test_link_state <- function(n_sets = 3L) {
  state <- task10_link_state(n_sets)
  state$btl_fit$beta_mean <- 0
  state$btl_fit$epsilon_mean <- 0
  for (k in seq_len(n_sets)) {
    key <- as.character(k)
    artifact <- state$linking$phase_a$artifacts[[key]]
    artifact$n_items <- nrow(artifact$items)
    artifact$items$rank_mu_raw <- rank(-artifact$items$theta_raw_mean)
    artifact$fit_model_id <- "btl_e_b"
    artifact$fit_config_surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(state, k)
    artifact$fit_config_hash <- pairwiseLLM:::.adaptive_phase_a_hash_object(artifact$fit_config_surface)
    artifact$diagnostics <- list(diagnostics_pass = TRUE, reliability_EAP_within = 0.9)
    state$linking$phase_a$artifacts[[key]] <- artifact
  }
  state
}

rubric_test_linked <- function(n_sets = 3L) {
  state <- rubric_test_link_state(n_sets)
  for (k in seq.int(2L, n_sets)) {
    i <- match(state$items$item_id[state$items$set_id == k][1L], state$item_ids)
    step <- as.integer(k - 1L)
    state$step_log <- pairwiseLLM:::append_step_log(state$step_log, list(
      step_id = step, pair_id = step, timestamp = as.POSIXct("2026-09-12", tz = "UTC"),
      i = i, j = 1L, A = i, B = 1L, Y = 1L, set_i = k, set_j = 1L,
      is_cross_set = TRUE, link_spoke_id = k, run_mode = state$controller$run_mode,
      is_probe_step = FALSE))
  }
  state$refit_meta$link_cross_edges_cache_built <- FALSE
  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  state$item_log <- list(pairwiseLLM:::.adaptive_build_item_log_refit(state, 1L))
  # Simulated terminal budget exhaustion, not a claim that one edge meets stopping precision.
  state$meta$stop_decision <- TRUE
  state$meta$stop_reason <- "all_spokes_exhausted"
  state
}
