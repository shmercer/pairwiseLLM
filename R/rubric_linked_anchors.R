# Historical ordinal calibration consumes validated Phase B scores read-only.

.rubric_reference_identity <- function(reference) {
  required <- c("set_id", "items", "fit_contract", "fit_contract_hash", "evidence", "evidence_hash")
  scalar_id <- function(x) {
    is.numeric(x) && is.null(dim(x)) && length(x) == 1L &&
      is.finite(x) && abs(x) <= .Machine$integer.max && x == as.integer(x)
  }
  hash <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!is.list(reference) || !all(required %in% names(reference)) ||
    !scalar_id(reference$set_id) || !is.data.frame(reference$items) ||
    !all(c("item_id", "theta", "theta_sd") %in% names(reference$items)) ||
    !is.list(reference$fit_contract) || !hash(reference$fit_contract_hash) ||
    !is.data.frame(reference$evidence) || !hash(reference$evidence_hash)) {
    rlang::abort("Linked calibration requires complete reference identity, scores, contract, and evidence.")
  }
  items <- .rubric_items(reference$items$item_id, reference$items$theta, reference$items$theta_sd)
  .rubric_choice(reference$fit_contract$model_variant, c("btl", "btl_e", "btl_b", "btl_e_b"), "model_variant")
  .rubric_choice(reference$fit_contract$judge_param_mode, "global_shared", "judge_param_mode")
  if (!identical(reference$evidence_hash, .adaptive_phase_a_hash_object(reference$evidence))) {
    rlang::abort("Linked reference evidence hash does not match its evidence.")
  }
  # Original hashes may predate the current canonical surface. The existing
  # import validator permits that case; compare the surface and exact metric.
  list(set_id = as.integer(reference$set_id), items = items[order(items$item_id), ],
    fit_contract = reference$fit_contract, evidence = reference$evidence,
    evidence_hash = reference$evidence_hash)
}

.rubric_validate_linked_calibration <- function(object) {
  cj <- object$cj
  .rubric_choice(cj$scale_status, "phase_a_reference", "linked reference scale")
  .rubric_choice(cj$estimation_mode, "phase_a", "linked reference estimation mode")
  reference <- .rubric_reference_identity(object$reference)
  items <- cj$items[order(cj$items$item_id), c("item_id", "theta", "theta_sd")]
  if (!identical(object$reference, cj$reference) || !identical(reference$items, items) ||
    !identical(reference$fit_contract, cj$fit_contract) ||
    !identical(reference$fit_contract$model_variant, cj$model_variant) ||
    !identical(object$reference$fit_contract_hash, cj$fit_contract_hash) ||
    !"set_id" %in% names(cj$items) || !all(cj$items$set_id %in% reference$set_id)) {
    rlang::abort("Stored linked reference does not match the calibration CJ metric and fit contract.")
  }
  invisible(object)
}

.rubric_predict_linked <- function(object, newdata, hard_score) {
  cj <- if (inherits(newdata, "pairwiseLLM_link_result") || inherits(newdata, "pairwiseLLM_link_session")) {
    .rubric_cj_estimator(newdata, object$reference, object$trait)
  } else {
    .rubric_normalize_cj(newdata, object$trait, scale_status = "phase_b_linked", include_draws = FALSE)
  }
  if (!identical(cj$model_variant, object$cj$model_variant) ||
    !identical(cj$orientation, object$orientation) ||
    !identical(.rubric_reference_identity(cj$reference), .rubric_reference_identity(object$reference))) {
    rlang::abort(paste0("Linked prediction requires the stored rubric reference hub with unchanged ",
      "item identities, reference scores, uncertainty, evidence, and compatible fit contract."))
  }
  items <- cj$items[cj$items$set_id != cj$reference$set_id, ]
  probabilities <- if (object$method == "ordinal_linear") {
    .rubric_ordinal_probabilities(items$theta, object)
  } else {
    .rubric_monotone_dependencies()
    .rubric_monotone_probabilities(items$theta, object)
  }
  out <- .rubric_ordinal_prediction_table(object, items, probabilities, hard_score)
  out[c("set_id", "source_item_id", "global_item_id", "theta_sd")] <-
    items[c("set_id", "source_item_id", "global_item_id", "theta_sd")]
  attr(out, "linking") <- list(reference = cj$reference, fit_contract = cj$fit_contract,
    provenance = cj$provenance, diagnostics = cj$diagnostics, reliability = cj$reliability)
  out
}
