# Extract only canonical item draws and provenance, never the Phase A likelihood.
.link_e2_artifact <- function(x, identity, judge) {
  if (!is.list(x) || !"artifact" %in% names(x)) return(x)
  .link_fields(x, c("artifact", "source"), "artifact", "E2 Phase A (single-use evidence)")
  a <- x$artifact
  .link_check(is.list(a) && all(c("set_id", "items", "n_items", "n_pairs_committed",
    "fit_model_id", "posterior_draws") %in% names(a)),
    "E2 requires a canonical Phase A artifact with posterior item draws.")
  .link_check(identical(.link_ids(a$set_id, "artifact set_id"), identity$set_id),
    "Phase A artifact set_id mismatch.")
  .link_check(identical(normalize_model_variant(a$fit_model_id), judge$model_variant),
    "Phase A artifact model is incompatible with the frozen judge.")
  if (!is.null(a[["phase_scope"]]) && !identical(a[["phase_scope"]], NA_character_)) {
    .link_check(identical(a[["phase_scope"]], "phase_a_set"), "Artifact must contain Phase A, not Phase B draws.")
  }
  if (!is.null(a[["phase_scope_set_id"]]) && !all(is.na(a[["phase_scope_set_id"]]))) {
    .link_check(identical(.link_ids(a[["phase_scope_set_id"]], "artifact scope set_id"), identity$set_id),
      "Phase A artifact scope set_id mismatch.")
  }
  items <- a$items
  .link_check(is.data.frame(items) && "item_id" %in% names(items), "Artifact items require item_id.")
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
  # The common draw validator binds columns by item ID and centers every draw.
  # Artifact summaries/SDs are deliberately not inspected or used as substitutes.
  .link_check(is.matrix(a$posterior_draws), "E2 requires posterior item draws; summaries alone are insufficient.")
  source <- .link_source(list(artifact_hash = .link_hash(a),
    evidence_hash = a$phase_a_within_set_evidence_hash %||% NA_character_,
    n_observations = a$n_pairs_committed,
    trait = .link_artifact_trait(a), orientation = .link_artifact_orientation(a)))
  supplied <- x$source %||% list()
  .link_source(supplied)
  for (k in names(supplied)) {
    .link_check(isTRUE(all.equal(supplied[[k]], source[[k]])), paste0("Artifact source mismatch: ", k, "."))
  }
  list(draws = a$posterior_draws, source = source)
}
