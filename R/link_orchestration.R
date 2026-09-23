# Estimator-neutral orchestration. These hooks do not fit, select, or promote
# evidence. Posterior covariance is not a validated adaptive information rule.
.link_selector_unvalidated <- function() {
  rlang::abort(paste0("Adaptive Phase B D-optimal selection is unavailable for all linking ",
    "estimators, including E1, E2, E3 and E3-MCMC, pending the separate selector ",
    "validation study. Legacy D-optimal modes are also unavailable. ",
    "Use prepare_link_input(), fit_link(), and start_link_session()/resume_link_session() ",
    "with explicit cross-set evidence. No selector fallback is provided."),
    class = "pairwiseLLM_link_selector_unvalidated")
}

.link_guard_adaptive_selection <- function(state, controller = NULL) {
  if (inherits(state, c("pairwiseLLM_link_session", "pairwiseLLM_link_result"))) {
    .link_selector_unvalidated()
  }
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (as.character(controller$run_mode %||% "within_set") %in%
      c("link_one_spoke", "link_multi_spoke") &&
      identical(.adaptive_link_phase_context(state, controller)$phase, "phase_b")) {
    .link_selector_unvalidated()
  }
  invisible(TRUE)
}

.link_orchestration_result <- function(x, spoke_id = NULL) {
  if (inherits(x, "pairwiseLLM_link_result")) {
    .link_validate_result(x)
    result <- x
  } else {
    if (inherits(x, "pairwiseLLM_link_session")) .link_session_validate(x)
    results <- .link_session_results(x)
    .link_check(length(results) > 0L,
      "Orchestration requires common E1--E3 results; restart from explicit Phase A/cross evidence.")
    if (is.null(spoke_id)) {
      .link_check(length(results) == 1L, "A multi-spoke operation requires an explicit spoke_id.")
      spoke_id <- names(results)[1L]
    }
    key <- .link_ids(spoke_id, "spoke_id")
    .link_check(length(key) == 1L && key %in% names(results), "Unknown orchestration spoke_id.")
    result <- results[[key]]
    .link_validate_result(result)
  }
  if (!is.null(spoke_id)) {
    .link_check(identical(as.character(spoke_id), result$continuation$input$spoke$set_id),
      "Orchestration result/spoke identity mismatch.")
  }
  result
}

.link_orchestration_view <- function(result) {
  .link_validate_result(result)
  input <- result$continuation$input
  list(estimator_id = result$estimator_id, estimator_version = result$estimator_version,
    hub_set_id = input$hub$set_id, spoke_set_id = input$spoke$set_id,
    items = result$items, covariance = result$uncertainty$covariance,
    coordinates = colnames(result$uncertainty$item_transform),
    item_transform = result$uncertainty$item_transform,
    uncertainty_scope = result$diagnostics$uncertainty_scope,
    fit_valid = result$diagnostics$fit_valid, identification = result$offset$identification,
    evidence_counts = result$provenance$counts, evidence_hashes = result$provenance$hashes,
    adaptive_d_optimal_supported = FALSE)
}

.link_candidate_gradient <- function(result, pairs) {
  .link_validate_result(result)
  input <- result$continuation$input
  pairs <- .link_observations(pairs, input$hub, input$spoke, outcomes = FALSE)
  # Gradient of theta_A - theta_B, not of a posterior-average probability.
  .link_pair_surface(pairs, input)
}

.link_global_item_index <- function(result, ids) {
  ids <- .link_ids(ids, "global item IDs", unique = FALSE, empty = TRUE)
  globals <- result$items$global_item_id
  .link_check(!anyNA(globals) && !anyDuplicated(globals),
    "Adaptive item mapping requires explicit unique global_item_id values.")
  idx <- match(ids, globals)
  .link_check(!anyNA(idx), "Unknown global item ID for the selected spoke result.")
  idx
}

.link_routing_scores <- function(result, item_ids) {
  .link_validate_result(result)
  .link_check(isTRUE(result$diagnostics$fit_valid), "Cannot route from an invalid linking fit.")
  idx <- .link_global_item_index(result, item_ids)
  stats::setNames(result$items$theta_link_mean[idx], as.character(item_ids))
}

.link_global_pairs <- function(result, A_id, B_id) {
  .link_check(length(A_id) == length(B_id), "Candidate endpoints must have equal lengths.")
  a <- .link_global_item_index(result, A_id)
  b <- .link_global_item_index(result, B_id)
  tibble::tibble(observation_id = paste0("candidate-", seq_along(a)),
    A_set = result$items$set_id[a], A_item = result$items$item_id[a],
    B_set = result$items$set_id[b], B_item = result$items$item_id[b])
}

.link_probe_partition <- function(result, probes) {
  input <- result$continuation$input
  probes <- .link_observations(probes, input$hub, input$spoke)
  phase_ids <- unlist(lapply(input$phase_a, function(x) {
    if (x$kind == "observations") x$value$observation_id else character()
  }), use.names = FALSE)
  .link_check(!any(probes$observation_id %in% c(input$cross$observation_id, phase_ids)),
    "Held-out probe observation IDs overlap active or Phase A evidence.")
  # Keep both orientations and repeated judgments of a base pair in one role.
  pair_keys <- function(rows) {
    h <- ifelse(rows$A_set == input$hub$set_id, rows$A_item, rows$B_item)
    s <- ifelse(rows$A_set == input$spoke$set_id, rows$A_item, rows$B_item)
    vapply(seq_len(nrow(rows)), function(i) .link_hash(list(h[i], s[i])), character(1))
  }
  .link_check(!any(pair_keys(probes) %in% pair_keys(input$cross)),
    "Held-out probe base pairs overlap active cross-set evidence.")
  probes
}

.link_probe_metrics <- function(result, probes, previous = NULL, panel = NULL, controller = list()) {
  .link_validate_result(result)
  probes <- .link_probe_partition(result, probes)
  pairs <- probes[, setdiff(names(probes), "y_A"), drop = FALSE]
  p <- rep(NA_real_, nrow(probes))
  if (result$diagnostics$fit_valid && nrow(pairs) > 0L) p <- predict_link(result, pairs)
  input <- result$continuation$input
  hub <- ifelse(probes$A_set == input$hub$set_id, probes$A_item, probes$B_item)
  spoke <- ifelse(probes$A_set == input$spoke$set_id, probes$A_item, probes$B_item)
  hub_bins <- spoke_bins <- integer()
  if (!is.null(panel)) {
    .link_fields(panel, c("observation_id", "hub_bin", "spoke_bin"),
      c("observation_id", "hub_bin", "spoke_bin"), "probe panel")
    ids <- .link_ids(panel$observation_id, "probe panel observation IDs", empty = TRUE)
    for (k in c("hub_bin", "spoke_bin")) {
      .link_check(is.numeric(panel[[k]]) && length(panel[[k]]) == length(ids) &&
        all(is.na(panel[[k]]) | (is.finite(panel[[k]]) & panel[[k]] >= 1 &
          panel[[k]] == floor(panel[[k]]))), "Invalid probe panel rank bins.")
    }
    at <- match(probes$observation_id, ids)
    hub_bins <- unique(stats::na.omit(panel$hub_bin[at]))
    spoke_bins <- unique(stats::na.omit(panel$spoke_bin[at]))
  }
  valid <- all(is.finite(p))
  quality <- .link_probe_quality(if (valid) p else numeric(), probes$y_A,
    hub, spoke, hub_bins, spoke_bins, controller)
  lag_rmse <- NA_real_
  if (!is.null(previous)) {
    .link_validate_comparison(result, previous)
    .link_probe_partition(previous, probes)
    if (valid && nrow(probes) > 0L && previous$diagnostics$fit_valid) {
      lag_rmse <- sqrt(mean((p - predict_link(previous, pairs))^2))
    }
  }
  c(list(estimator_id = result$estimator_id, estimator_version = result$estimator_version,
    hub_set_id = input$hub$set_id, spoke_set_id = input$spoke$set_id,
    uncertainty_scope = result$diagnostics$uncertainty_scope,
    input_hash = input$hashes$input, cross_evidence_hash = input$hashes$cross,
    probe_evidence_hash = .link_hash(probes), probe_edges_realized = nrow(probes),
    probe_brier = if (valid && nrow(probes)) mean((p - probes$y_A)^2) else NA_real_,
    probe_pred_rmse_lagged = lag_rmse), quality)
}

.link_validate_comparison <- function(result, previous) {
  .link_validate_result(previous)
  current <- result$continuation$input
  old <- previous$continuation$input
  for (k in c("estimator", "hub", "spoke", "phase_a", "judge", "control")) {
    .link_check(identical(current[[k]], old[[k]]),
      paste0("Lagged comparison changed ", k, "; reset the spoke history."))
  }
  .link_previous_mode(current, previous)
  invisible(TRUE)
}

.link_stop_metrics <- function(result, probes, previous = NULL, panel = NULL,
                               controller = list(), refits_in_epoch = 1L,
                               lag_eligible = FALSE) {
  .link_validate_result(result)
  .link_check(length(refits_in_epoch) == 1L && is.numeric(refits_in_epoch) &&
    is.finite(refits_in_epoch) && refits_in_epoch >= 1 &&
    refits_in_epoch == floor(refits_in_epoch), "Invalid refits_in_epoch.")
  .link_check(is.logical(lag_eligible) && length(lag_eligible) == 1L && !is.na(lag_eligible),
    "lag_eligible must be TRUE or FALSE.")
  metrics <- .link_probe_metrics(result, probes, previous, panel, controller)
  usable_lag <- lag_eligible && !is.null(previous) &&
    isTRUE(result$diagnostics$fit_valid) && isTRUE(previous$diagnostics$fit_valid)
  input <- result$continuation$input
  cross <- input$cross
  means <- result$items$theta_link_mean
  variances <- result$items$theta_link_sd^2
  # Preserve active-item reliability: all spoke items and directly evidenced hub items.
  active_hub <- unique(ifelse(cross$A_set == input$hub$set_id, cross$A_item, cross$B_item))
  active <- result$items$set_id == input$spoke$set_id |
    (result$items$set_id == input$hub$set_id & result$items$item_id %in% active_hub)
  reliability <- NA_real_
  full_uncertainty <- identical(result$diagnostics$uncertainty_scope, "joint_shapes_and_offset")
  if (full_uncertainty && result$diagnostics$fit_valid &&
      all(is.finite(variances[active])) && all(is.finite(means[active]))) {
    reliability <- .adaptive_link_reliability_decomposition(means[active], variances[active],
      controller$reliability_var_mu_epsilon %||% 1e-6,
      controller$reliability_total_var_epsilon %||% 1e-6)$reliability
  }
  # Preserve the existing score-stability domain: directly evidenced spoke items.
  ids <- unique(ifelse(cross$A_set == input$spoke$set_id, cross$A_item, cross$B_item))
  scope <- controller$theta_global_rmse_scope %||% "direct_evidence_spoke"
  .link_check(scope %in% c("direct_evidence_spoke", "all_spoke_items", "min_cross_set_edges_k"),
    "Unsupported score-stability scope.")
  if (scope == "min_cross_set_edges_k") {
    counts <- table(ifelse(cross$A_set == input$spoke$set_id, cross$A_item, cross$B_item))
    ids <- names(counts)[counts >= (controller$min_cross_set_edges_k %||% 1L)]
  }
  use <- result$items$set_id == input$spoke$set_id &
    (scope == "all_spoke_items" | result$items$item_id %in% ids)
  theta_rmse <- if (usable_lag && any(use)) {
    sqrt(mean((means[use] - previous$items$theta_link_mean[use])^2))
  } else {
    NA_real_
  }
  if (!usable_lag) metrics$probe_pred_rmse_lagged <- NA_real_
  identified <- identical(result$offset$identification, "cross_set")
  blockers <- .adaptive_link_stop_blockers(
    link_diagnostics_pass = result$diagnostics$fit_valid && identified,
    link_lag_eligible = usable_lag,
    link_min_refit_eligible = refits_in_epoch >= (controller$min_refits_in_phase_b %||% 3L),
    probe_edges_realized = metrics$probe_edges_realized,
    probe_edges_min_for_stop = controller$probe_edges_min_for_stop %||% 80L,
    link_stop_reliability_min = controller$link_stop_reliability_min %||% .90,
    reliability_active = reliability, probe_brier = metrics$probe_brier,
    probe_brier_max = controller$probe_brier_max %||% .19,
    probe_pred_rmse_lagged = metrics$probe_pred_rmse_lagged,
    probe_pred_rmse_max = controller$probe_pred_rmse_max %||% .015,
    theta_global_rmse_lagged = theta_rmse,
    theta_global_rmse_max = controller$theta_global_rmse_max %||% .05,
    probe_quality_pass = metrics$probe_quality_pass,
    hub_anchored = TRUE)
  # The centered hub is the coordinate reference, not a hard-locked posterior.
  # Expose an assessment only: callers cannot trigger adaptive stopping here.
  c(metrics, list(fit_valid = result$diagnostics$fit_valid,
    identification = result$offset$identification,
    active_edges = input$counts$cross, reliability_link_global = reliability,
    theta_global_rmse_lagged = theta_rmse, lag_eligible = usable_lag,
    refits_in_epoch = as.integer(refits_in_epoch),
    link_stop_pass_now = !any(blockers$blockers), link_stop_blocker_codes = blockers$codes,
    adaptive_d_optimal_supported = FALSE))
}

.link_adaptive_global_ids <- function(state, item_ids) {
  if (inherits(state, c("pairwiseLLM_link_result", "pairwiseLLM_link_session"))) {
    return(as.character(item_ids))
  }
  at <- match(as.character(item_ids), as.character(state$items$item_id))
  .link_check(!anyNA(at), "Unknown adaptive item ID.")
  ids <- state$items[["global_item_id"]][at]
  .link_check(length(ids) == length(item_ids) && !anyNA(ids),
    "Adaptive item mapping requires explicit global_item_id values.")
  as.character(ids)
}

.link_attach_probe_predictions <- function(candidates, state, controller, spoke_id) {
  result <- .link_orchestration_result(state, spoke_id)
  pairs <- .link_global_pairs(result, .link_adaptive_global_ids(state, candidates$i),
    .link_adaptive_global_ids(state, candidates$j))
  candidates$link_p <- predict_link(result, pairs)
  candidates$link_u <- candidates$link_p * (1 - candidates$link_p)
  candidates
}

.link_adaptive_probe_observations <- function(state, result, realized) {
  at <- match(realized$step_id, state$step_log$step_id)
  .link_check(!anyNA(at), "Probe prediction requires the original ordered step log.")
  steps <- state$step_log[at, , drop = FALSE]
  .link_check(all(.adaptive_link_is_holdout_probe_rows(steps)),
    "Probe prediction matched an active step instead of held-out evidence.")
  A <- state$item_ids[steps$A]
  B <- state$item_ids[steps$B]
  pairs <- .link_global_pairs(result, .link_adaptive_global_ids(state, A),
    .link_adaptive_global_ids(state, B))
  pairs$observation_id <- paste0("probe-step-", realized$step_id)
  pairs$y_A <- as.integer(steps$Y)
  .link_probe_partition(result, pairs)
}

.link_orchestration_history_hash <- function(result) {
  input <- result$continuation$input
  .link_hash(input[c("estimator", "hub", "spoke", "phase_a", "judge", "control")])
}
