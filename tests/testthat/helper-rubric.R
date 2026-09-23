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

# Synthetic completed CJ locations with deterministic category frequencies.
rubric_linear_fixed <- function(theta, ids = paste0("item", seq_along(theta)), variant = "btl") {
  draws <- outer(c(-0.125, 0.125), theta, `+`)
  colnames(draws) <- ids
  fit <- pairwiseLLM:::build_btl_fit_contract(draws, model_variant = variant,
    epsilon_draws = if (pairwiseLLM:::model_has_e(variant)) c(0.03, 0.05) else NULL,
    beta_draws = if (pairwiseLLM:::model_has_b(variant)) c(-0.1, 0.1) else NULL,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000), diagnostics_pass = TRUE)
  list(fit = fit, fits = list(fit),
    item_log_list = list(tibble::tibble(refit_id = 1L, ID = ids,
      theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
    round_log = tibble::tibble(round_id = 1L, model_variant = variant, reliability_EAP = 0.95))
}

rubric_linear_data <- function(K = 3L, slope = 1.2, variant = "btl") {
  theta <- seq(-2, 2, length.out = 9)
  thresholds <- seq(-1.5, 1.5, length.out = K - 1L)
  cumulative <- stats::plogis(outer(theta, thresholds, function(x, tau) tau - slope * x))
  probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
  counts <- round(50 * probabilities)
  grid <- expand.grid(category = seq_len(K), theta = theta)
  data <- grid[rep(seq_len(nrow(grid)), as.vector(t(counts))), ]
  ids <- paste0("item", seq_len(nrow(data)))
  list(cj = rubric_linear_fixed(data$theta, ids, variant),
    rubric = data.frame(item_id = ids, rubric_score = data$category),
    theta = data$theta, thresholds = thresholds)
}

rubric_linear_fit <- function(data) {
  pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric, trait = "organization")
}


# Deterministic latent logistic quantiles, without random rubric labels.
rubric_monotone_data <- function(K = 3L, n_unique = 31L, variant = "btl", reverse_items = FALSE) {
  theta <- rep(seq(-2, 2, length.out = n_unique), each = 12L)
  u <- rep((seq_len(12L) - 0.5) / 12, n_unique)
  eta <- theta + 0.25 * theta^3
  cumulative <- stats::plogis(outer(eta, seq(-1.5, 1.5, length.out = K - 1L), function(e, t) t - e))
  category <- as.integer(1 + rowSums(cumulative < u))
  ids <- paste0("item", seq_along(theta))
  if (reverse_items) {
    theta <- rev(theta)
    category <- rev(category)
    ids <- rev(ids)
  }
  draws <- outer(c(-0.125, 0.125), theta, `+`)
  colnames(draws) <- ids
  fit <- pairwiseLLM:::build_btl_fit_contract(draws, model_variant = variant,
    epsilon_draws = if (pairwiseLLM:::model_has_e(variant)) c(0.03, 0.05) else NULL,
    beta_draws = if (pairwiseLLM:::model_has_b(variant)) c(-0.1, 0.1) else NULL,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000), diagnostics_pass = TRUE)
  cj <- list(fit = fit, fits = list(fit),
    item_log_list = list(tibble::tibble(refit_id = 1L, ID = ids,
      theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
    round_log = tibble::tibble(round_id = 1L, model_variant = variant, reliability_EAP = 0.95))
  list(cj = cj, rubric = data.frame(item_id = ids, rubric_score = category))
}

rubric_monotone_fit <- function(data, ...) {
  pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric, method = "ordinal_monotone",
    trait = "organization", ...)
}


# A larger reference is necessary for identifiable ordinal calibration. Spokes
# remain three-item problems; Phase B uses real deterministic MAP refits.
rubric_linked_fixture <- function(n_sets = 2L, variant = "btl", target_shift = 0, reference_shift = 0,
                                  estimator = "fixed_shape_offset") {
  theta_hub <- rep(seq(-2, 2, length.out = 9L), each = 6L) + reference_shift
  theta <- c(theta_hub, rep(c(-3, 0, 3) + target_shift, n_sets - 1L))
  ids <- sprintf("item%03d", seq_along(theta))
  sets <- c(rep(1L, length(theta_hub)), rep(seq.int(2L, n_sets), each = 3L))
  items <- tibble::tibble(item_id = ids, global_item_id = paste0("g_", ids), set_id = sets)
  now_fn <- function() as.POSIXct("2026-09-12", tz = "UTC")
  environment(now_fn) <- baseenv()
  state <- pairwiseLLM::adaptive_rank_start(items, seed = 9200L, now_fn = now_fn,
    adaptive_config = list(run_mode = if (n_sets == 2L) "link_one_spoke" else "link_multi_spoke", hub_id = 1L))
  state$config$btl_config$model_variant <- variant
  state$trait <- "organization"
  draws <- outer(c(-0.3, -0.1, 0.1, 0.3), theta, `+`)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws, model_variant = variant)
  state$btl_fit$beta_mean <- if (pairwiseLLM:::model_has_b(variant)) 0.1 else 0
  state$btl_fit$epsilon_mean <- if (pairwiseLLM:::model_has_e(variant)) 0.04 else 0
  artifacts <- lapply(seq_len(n_sets), function(k) {
    keep <- sets == k
    surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(state, k)
    artifact <- list(set_id = k, n_items = sum(keep), posterior_draws = draws[, keep, drop = FALSE],
      fit_model_id = variant, fit_config_surface = surface,
      fit_config_hash = pairwiseLLM:::.adaptive_phase_a_hash_object(surface),
      diagnostics = list(diagnostics_pass = TRUE, reliability_EAP_within = 0.9), trait = "organization",
      items = tibble::tibble(item_id = ids[keep], global_item_id = items$global_item_id[keep],
        theta_raw_mean = theta[keep], theta_raw_sd = rep(0.2, sum(keep)), rank_mu_raw = rank(-theta[keep])))
    artifact <- add_test_phase_a_evidence(artifact, state, k)
    artifact$phase_a_within_set_evidence <- pairwiseLLM:::.adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact, state, k, state$controller)
    artifact$phase_a_within_set_evidence_hash <-
      pairwiseLLM:::.adaptive_phase_a_hash_object(artifact$phase_a_within_set_evidence)
    artifact
  })
  names(artifacts) <- as.character(seq_len(n_sets))
  state$linking$phase_a$artifacts <- artifacts
  state$linking$phase_a$set_status$status[] <- "ready"
  state$linking$phase_a$required_sets <- seq_len(n_sets)
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- seq.int(2L, n_sets)
  state$linking$phase_a$active_spokes <- seq.int(2L, n_sets)
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$active_phase_a_set <- NA_integer_
  state$warm_start_done <- TRUE
  # Explicit-evidence replacement; no legacy Phase B optimizer or providers.
  hub <- list(set_id = "1", items = artifacts[["1"]]$items[c("item_id", "global_item_id")])
  inputs <- lapply(seq.int(2L, n_sets), function(k) {
    a <- artifacts[[as.character(k)]]
    spoke <- list(set_id = as.character(k), items = a$items[c("item_id", "global_item_id")])
    cross <- data.frame(observation_id = paste0("cross-", k, "-", 1:6),
      A_set = "1", A_item = rep(hub$items$item_id[c(1L, nrow(hub$items))], 3L),
      B_set = as.character(k), B_item = rep(spoke$items$item_id, each = 2L), y_A = rep(c(0L, 1L), 3L))
    pairwiseLLM::prepare_link_input(estimator, hub, spoke,
      list(hub = list(artifact = artifacts[["1"]]), spoke = list(artifact = a)), cross,
      list(beta = state$btl_fit$beta_mean, epsilon = state$btl_fit$epsilon_mean,
        model_variant = variant, link = "logit", source = "synthetic frozen Phase A"))
  })
  state <- pairwiseLLM::start_link_session(inputs)
  u <- rep((seq_len(6L) - 0.5) / 6, 9L)
  eta <- theta_hub + 0.25 * theta_hub^3
  cumulative <- stats::plogis(outer(eta, c(-1, 1), function(e, t) t - e))
  labels <- c("developing", "proficient", "advanced")
  rubric <- tibble::tibble(item_id = items$global_item_id[sets == 1L],
    rubric_score = labels[1L + rowSums(cumulative < u)])
  list(state = state, reference = artifacts[["1"]], rubric = rubric, levels = labels)
}

rubric_linked_fit <- function(data, method = "ordinal_linear", ...) {
  pairwiseLLM::fit_rubric_calibration(data$reference, data$rubric, method = method,
    calibration_design = "linked_anchors", levels = data$levels, ...)
}
