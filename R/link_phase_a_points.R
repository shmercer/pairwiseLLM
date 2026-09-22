# Canonical Phase A artifacts enter E1 only through their item EAP means.
.link_e1_artifact <- function(x, identity, judge) {
  if (!is.list(x) || !"artifact" %in% names(x)) return(x)
  .link_fields(x, c("artifact", "source"), "artifact", "E1 Phase A (single-use evidence)")
  a <- x$artifact
  .link_check(is.list(a) && all(c("set_id", "items", "n_items", "n_pairs_committed",
    "fit_model_id") %in% names(a)), "E1 requires an in-memory canonical Phase A artifact.")
  .link_check(identical(.link_ids(a$set_id, "artifact set_id"), identity$set_id),
    "Phase A artifact set_id mismatch.")
  .link_check(identical(normalize_model_variant(a$fit_model_id), judge$model_variant),
    "Phase A artifact model is incompatible with the frozen judge.")
  if (!is.null(a$phase_scope) && !identical(a$phase_scope, NA_character_)) {
    .link_check(identical(a$phase_scope, "phase_a_set"), "Artifact must contain Phase A, not Phase B summaries.")
  }
  if (!is.null(a$phase_scope_set_id) && !all(is.na(a$phase_scope_set_id))) {
    .link_check(identical(.link_ids(a$phase_scope_set_id, "artifact scope set_id"), identity$set_id),
      "Phase A artifact scope set_id mismatch.")
  }
  items <- a$items
  .link_check(is.data.frame(items) && all(c("item_id", "theta_raw_mean") %in% names(items)),
    "Artifact items require item_id and theta_raw_mean EAP estimates.")
  ids <- .link_ids(items$item_id, "artifact item_id")
  .link_check(setequal(ids, identity$items$item_id) &&
    .link_scalar(a$n_items, "artifact n_items", 1) == length(ids), "Phase A artifact item identities/count mismatch.")
  index <- match(identity$items$item_id, ids)
  if ("global_item_id" %in% names(items)) {
    global <- .link_ids(items$global_item_id, "artifact global_item_id")
    supplied <- !is.na(identity$items$global_item_id)
    .link_check(all(global[index][supplied] == identity$items$global_item_id[supplied]),
      "Phase A artifact global_item_id mapping mismatch.")
  } else {
    .link_check(all(is.na(identity$items$global_item_id)), "Artifact is missing required global_item_id identities.")
  }
  means <- items$theta_raw_mean
  .link_check(is.numeric(means) && is.null(dim(means)) && all(is.finite(means)),
    "Phase A artifact EAP means must be finite numeric values.")
  # Hashing preserves artifact identity; neither raw outcomes nor draws are
  # retained in the normalized input or passed to a likelihood evaluator.
  source <- .link_source(list(artifact_hash = .link_hash(a),
    evidence_hash = a$phase_a_within_set_evidence_hash %||% NA_character_,
    n_observations = a$n_pairs_committed))
  supplied <- x$source %||% list()
  .link_source(supplied)
  for (k in names(supplied)) {
    .link_check(isTRUE(all.equal(supplied[[k]], source[[k]])), paste0("Artifact source mismatch: ", k, "."))
  }
  list(points = stats::setNames(as.double(means[index]), identity$items$item_id), source = source)
}
