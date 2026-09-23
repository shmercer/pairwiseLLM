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
  if (!is.numeric(set_id) || !is.null(dim(set_id)) || length(set_id) != 1L || !is.finite(set_id) ||
    abs(set_id) > .Machine$integer.max ||
    set_id != as.integer(set_id)) rlang::abort("Phase A reference requires a single integer set_id.")
  items <- artifact$items
  items$set_id <- as.integer(set_id)
  structure(list(
    items = items, item_ids = as.character(items$item_id), n_items = nrow(items),
    controller = list(judge_param_mode = surface$judge_param_mode, link_estimation_mode = "transform"),
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
  list(set_id = artifact$set_id, artifact_hash = .link_hash(artifact),
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
  .link_reject_legacy(state)
  rlang::abort("Linked rubric prediction requires an E1--E3 common result or linking session.")
}

# A calibration is attached to the centered Phase A hub scale, not to the
# estimator's potentially updated hub posterior. Exact artifact identity binds
# all three estimators to that origin; raw Phase A means are never used as priors.
.rubric_cj_estimator <- function(state, reference, trait) {
  results <- .link_reporting_results(state)
  ref <- .rubric_reference_identity(reference)
  .link_check(is.character(reference$artifact_hash) && length(reference$artifact_hash) == 1L,
    "Linked E1--E3 calibration requires reference artifact identity; refit the hub calibration.")
  summaries <- list()
  for (key in names(results)) {
    r <- results[[key]]
    input <- r$continuation$input
    for (a in input$phase_a) {
      if (!is.na(a$source$trait %||% NA_character_)) .rubric_trait(trait, a$source$trait)
    }
    .link_check(isTRUE(r$diagnostics$fit_valid) && identical(r$offset$identification, "cross_set"),
      "Linked rubric prediction requires a valid, cross-set identified E1--E3 result.")
    .link_check(identical(input$hub$set_id, as.character(ref$set_id)) &&
      identical(input$judge$model_variant, ref$fit_contract$model_variant) &&
      identical(input$phase_a$hub$source$artifact_hash, reference$artifact_hash),
      "Linked prediction requires the stored rubric reference hub artifact, identities, and model.")
    .link_check(setequal(input$hub$items$global_item_id, ref$items$item_id),
      "Linked prediction hub global identities do not match the stored reference.")
    items <- r$items[r$items$set_id == input$spoke$set_id, ]
    .link_check(!anyNA(items$global_item_id), "Linked rubric prediction requires global item IDs.")
    # Historical calibrations used raw Phase A locations. The common link scale
    # centers each hub; translate back by the frozen reference mean only.
    summaries[[key]] <- .rubric_items(items$item_id,
      items$theta_link_mean + mean(ref$items$theta), items$theta_link_sd,
      items$global_item_id, items$set_id)
  }
  .rubric_new_cj(dplyr::bind_rows(summaries), ref$fit_contract$model_variant,
    "phase_b", "phase_b_linked", .rubric_trait(trait),
    fit_contract = list(reference = ref$fit_contract,
      estimators = lapply(results, function(r) list(id = r$estimator_id, version = r$estimator_version,
        uncertainty_scope = r$diagnostics$uncertainty_scope, hashes = r$provenance$hashes))),
    fit_contract_hash = reference$fit_contract_hash,
    provenance = list(estimator_id = unique(vapply(results, `[[`, character(1), "estimator_id")),
      uncertainty_scope = unique(vapply(results, function(r) r$diagnostics$uncertainty_scope, character(1))),
      spokes = lapply(results, `[[`, "provenance")),
    diagnostics = list(diagnostics_pass = TRUE, spokes = lapply(results, `[[`, "diagnostics")),
    reference = reference)
}
