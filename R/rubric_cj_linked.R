# Read-only adapters for existing Phase A artifacts and Phase B accepted states.

.rubric_phase_a_metadata <- function(artifact) {
  if (!is.list(artifact) || !is.list(artifact$fit_config_surface)) {
    rlang::abort("Phase A reference requires explicit fit-contract metadata.")
  }
  surface <- artifact$fit_config_surface
  .rubric_choice(surface$model_variant, c("btl", "btl_e", "btl_b", "btl_e_b"), "model_variant")
  .rubric_choice(surface$judge_param_mode, "global_shared", "judge_param_mode")
  .rubric_choice(artifact$fit_model_id, surface$model_variant, "fit_model_id")
  if (!is.character(artifact$fit_config_hash) || length(artifact$fit_config_hash) != 1L ||
    is.na(artifact$fit_config_hash) || !nzchar(artifact$fit_config_hash)) {
    rlang::abort("Phase A reference requires its fit_config_hash.")
  }
  .rubric_orientation(artifact$orientation, artifact$items[["orientation"]])
  .adaptive_phase_a_artifact_fit_contract_surface(artifact)
}

.rubric_phase_a_context <- function(artifact, surface) {
  if (!is.data.frame(artifact$items) || !all(c("item_id", "global_item_id") %in% names(artifact$items))) {
    rlang::abort("Phase A reference requires source and global item IDs.")
  }
  .rubric_ids(artifact$items$item_id)
  .rubric_ids(artifact$items$global_item_id, "global_item_id")
  set_id <- artifact$set_id
  if (!is.numeric(set_id) || length(set_id) != 1L || !is.finite(set_id) ||
    set_id != as.integer(set_id)) rlang::abort("Phase A reference requires a single integer set_id.")
  items <- artifact$items
  items$set_id <- as.integer(set_id)
  structure(list(
    items = items, item_ids = as.character(items$item_id), n_items = nrow(items),
    controller = list(judge_param_mode = surface$judge_param_mode, link_estimation_mode = "anchored_joint"),
    config = list(btl_config = list(model_variant = surface$model_variant))
  ), class = "adaptive_state")
}

.rubric_phase_a_validate <- function(artifact, state = NULL, controller = NULL) {
  surface <- .rubric_phase_a_metadata(artifact)
  state <- state %||% .rubric_phase_a_context(artifact, surface)
  controller <- controller %||% .adaptive_controller_resolve(state)
  .adaptive_phase_a_validate_imported_artifact(artifact, state, artifact$set_id, controller, source = "import")
  if (!.adaptive_phase_a_set_stop_passed(artifact, "import", controller)) {
    rlang::abort("Phase A reference is not import-ready under the existing diagnostics/reliability gate.")
  }
  # Require exact reference evidence; the existing validator checks IDs/counts.
  evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
    artifact, state, artifact$set_id, controller)
  evidence_hash <- .adaptive_phase_a_hash_object(evidence)
  if (!is.null(artifact$phase_a_within_set_evidence_hash) &&
    !identical(artifact$phase_a_within_set_evidence_hash, evidence_hash)) {
    rlang::abort("Phase A reference evidence hash does not match its evidence.")
  }
  list(surface = surface, evidence = evidence, evidence_hash = evidence_hash)
}

.rubric_reference <- function(artifact, validated) {
  items <- artifact$items
  idx <- order(as.character(items$global_item_id))
  list(set_id = artifact$set_id,
    items = tibble::tibble(item_id = as.character(items$global_item_id[idx]),
      theta = as.double(items$theta_raw_mean[idx]), theta_sd = as.double(items$theta_raw_sd[idx])),
    fit_contract = validated$surface, fit_contract_hash = artifact$fit_config_hash,
    evidence = validated$evidence, evidence_hash = validated$evidence_hash)
}

.rubric_cj_phase_a <- function(artifact, trait) {
  validated <- .rubric_phase_a_validate(artifact)
  raw <- artifact$items
  items <- .rubric_items(raw$item_id, raw$theta_raw_mean, raw$theta_raw_sd,
    raw$global_item_id, rep(artifact$set_id, nrow(raw)))
  .rubric_new_cj(
    items, validated$surface$model_variant, "phase_a", "phase_a_reference",
    .rubric_trait(trait, artifact$trait, raw[["trait"]]),
    fit_contract = validated$surface, fit_contract_hash = artifact$fit_config_hash,
    provenance = list(finalization = "validated_import", set_id = artifact$set_id,
      refit_id = artifact$refit_id, round_id_at_refit = artifact$round_id_at_refit,
      step_id_at_refit = artifact$step_id_at_refit, n_pairs_committed = artifact$n_pairs_committed,
      quality_gate_accepted = isTRUE(artifact$quality_gate_accepted)),
    diagnostics = artifact$diagnostics, reliability = .adaptive_phase_a_extract_reliability(artifact),
    posterior_draws = .rubric_draws(artifact$posterior_draws, raw$item_id, items$item_id),
    reference = .rubric_reference(artifact, validated)
  )
}

.rubric_cj_linked <- function(state, trait, controller) {
  .rubric_choice(controller$run_mode, c("link_one_spoke", "link_multi_spoke"), "run_mode")
  .rubric_choice(controller$link_estimation_mode, "anchored_joint", "link_estimation_mode")
  .rubric_adaptive_terminal(state, linked = TRUE)
  .rubric_orientation(state$orientation, state$meta$orientation, state$items[["orientation"]])
  ids <- .rubric_ids(state$items$item_id)
  global_ids <- .rubric_ids(state$items$global_item_id, "global_item_id")
  hub_id <- controller$hub_id
  sets <- unique(state$items$set_id)
  if (length(hub_id) != 1L || is.na(hub_id) || !hub_id %in% sets || length(sets) < 2L) {
    rlang::abort("Linked CJ requires a hub and at least one spoke.")
  }
  artifacts <- state$linking$phase_a$artifacts
  hub <- artifacts[[as.character(hub_id)]]
  hub_validated <- .rubric_phase_a_validate(hub, state, controller)
  if (!is.null(state$btl_fit$model_variant)) {
    .rubric_choice(state$btl_fit$model_variant, hub_validated$surface$model_variant, "model_variant")
  }
  trait <- .rubric_trait(trait, hub$trait, hub$items[["trait"]])
  summary <- summarize_items(state)
  if (!all(c("item_id", "theta_link_eap", "theta_link_sd") %in% names(summary)) ||
    !setequal(.rubric_ids(summary$item_id), ids)) {
    rlang::abort("Linked CJ requires an aligned accepted common-scale item summary.")
  }
  summary <- summary[match(ids, summary$item_id), , drop = FALSE]
  theta <- sd <- stats::setNames(rep(NA_real_, length(ids)), ids)
  hub_items <- hub$items
  theta[hub_items$item_id] <- hub_items$theta_raw_mean
  sd[hub_items$item_id] <- 0
  spokes <- setdiff(sets, hub_id)
  edges <- .adaptive_link_cross_edges_rebuild(state)
  stats_by_spoke <- contracts <- list()
  for (spoke in spokes) {
    key <- as.character(spoke)
    artifact <- artifacts[[key]]
    validated <- .rubric_phase_a_validate(artifact, state, controller)
    if (!identical(validated$surface$model_variant, hub_validated$surface$model_variant)) {
      rlang::abort("Linked CJ model variants must match the reference.")
    }
    trait <- .rubric_trait(trait, artifact$trait, artifact$items[["trait"]])
    accepted <- state$linking$anchored_joint$accepted_state_by_spoke[[key]]
    if (!identical(accepted$anchored_joint_init_state_method, "phase_b_refit")) {
      rlang::abort("Linked CJ requires an accepted Phase B refit; initialization is not a linked score.")
    }
    accepted <- .adaptive_anchored_joint_validate_current_state(accepted, state, spoke, controller)
    cross <- edges[[key]]
    if (is.null(cross) || nrow(cross) == 0L || !any(!(cross$is_probe_step %in% TRUE))) {
      rlang::abort("Linked CJ requires committed active hub-spoke evidence for every spoke.")
    }
    active <- cross[!(cross$is_probe_step %in% TRUE), , drop = FALSE]
    if (any(!active$y_spoke %in% c(0L, 1L)) ||
      any(!active$spoke_item %in% artifact$items$item_id) || any(!active$hub_item %in% hub_items$item_id)) {
      rlang::abort("Linked CJ contains malformed committed active hub-spoke evidence.")
    }
    stats <- controller$link_refit_stats_by_spoke[[key]]
    if (!is.list(stats$fit_contract)) rlang::abort("Linked CJ is missing its Phase B fit contract.")
    .rubric_choice(stats$fit_contract$estimation_method,
      c("map_laplace", "accepted_state_reuse"), "Phase B estimation_method")
    uncertainty <- if (stats$fit_contract$estimation_method == "map_laplace") "laplace_hessian" else "accepted_state"
    .rubric_choice(stats$fit_contract$uncertainty_approximation, uncertainty, "Phase B uncertainty_approximation")
    if (stats$fit_contract$estimation_method == "map_laplace" &&
      !isTRUE(stats$fit_contract$anchored_joint$cross_active_edges > 0L)) {
      rlang::abort("Phase B fit contract must record fitted active cross-set evidence.")
    }
    .rubric_equal_scores(accepted$theta_hub_fixed[hub_items$item_id], hub_items$theta_raw_mean, "locked hub")
    spoke_ids <- state$items$item_id[state$items$set_id == spoke]
    theta[spoke_ids] <- accepted$theta_spoke_global_mean[spoke_ids]
    sd[spoke_ids] <- accepted$theta_spoke_global_sd[spoke_ids]
    stats_by_spoke[[key]] <- stats
    contracts[[key]] <- list(phase_a = validated$surface, phase_a_hash = artifact$fit_config_hash,
      phase_b = stats$fit_contract, evidence_hash = validated$evidence_hash)
  }
  .rubric_equal_scores(summary$theta_link_eap, theta, "linked item summary")
  .rubric_equal_scores(summary$theta_link_sd, sd, "linked uncertainty summary")
  passes <- vapply(stats_by_spoke, function(x) isTRUE(x$link_diagnostics_pass), logical(1L))
  known_failure <- vapply(stats_by_spoke, function(x) isFALSE(x$link_diagnostics_pass), logical(1L))
  diagnostic_pass <- if (any(known_failure)) FALSE else if (all(passes)) TRUE else NA
  .rubric_new_cj(
    .rubric_items(ids, theta, sd, global_ids, state$items$set_id),
    hub_validated$surface$model_variant, "phase_b", "phase_b_linked", trait,
    fit_contract = list(reference = hub_validated$surface, spokes = contracts),
    fit_contract_hash = hub$fit_config_hash,
    provenance = list(finalization = state$meta$stop_reason, hub_id = hub_id, spoke_ids = spokes,
      estimation_method = "map_laplace", uncertainty_approximation = "laplace_hessian",
      link_stage_log = .adaptive_latest_link_stage_rows(state)),
    diagnostics = list(diagnostics_pass = diagnostic_pass, spokes = stats_by_spoke),
    reliability = lapply(stats_by_spoke, function(x) x$reliability_link_global),
    reference = .rubric_reference(hub, hub_validated)
  )
}
