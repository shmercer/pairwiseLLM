# E3 imports raw Phase A rows once; no posterior summaries become priors.
.link_e3_artifact <- function(x, identity, judge) {
  if (!is.list(x) || !"artifact" %in% names(x)) {
    .link_check(is.list(x) && "observations" %in% names(x),
      "E3 requires exact raw within-set observations; point summaries/draws are insufficient.")
    return(x)
  }
  .link_fields(x, c("artifact", "source"), "artifact", "E3 Phase A (single-use evidence)")
  a <- x$artifact
  .link_check(is.list(a) && all(c("set_id", "items", "n_items", "n_pairs_committed", "fit_model_id") %in% names(a)),
    "E3 requires a canonical Phase A artifact with exact raw within-set rows.")
  .link_check(identical(.link_ids(a$set_id, "artifact set_id"), identity$set_id), "Phase A artifact set_id mismatch.")
  .link_check(identical(normalize_model_variant(a$fit_model_id), judge$model_variant),
    "Phase A artifact model is incompatible with the frozen judge.")
  if (!is.null(a[["phase_scope"]]) && !identical(a[["phase_scope"]], NA_character_)) {
    .link_check(identical(a[["phase_scope"]], "phase_a_set"), "Artifact must contain Phase A, not Phase B evidence.")
  }
  if (!is.null(a[["phase_scope_set_id"]]) && !all(is.na(a[["phase_scope_set_id"]]))) {
    .link_check(identical(.link_ids(a[["phase_scope_set_id"]], "artifact scope set_id"), identity$set_id),
      "Phase A artifact scope set_id mismatch.")
  }
  .link_check(is.data.frame(a$items) && "item_id" %in% names(a$items), "Artifact items require item_id.")
  ids <- .link_ids(a$items$item_id, "artifact item_id")
  .link_check(setequal(ids, identity$items$item_id) &&
    .link_scalar(a$n_items, "artifact n_items", 1) == length(ids), "Phase A artifact item identities/count mismatch.")
  supplied_global <- !is.na(identity$items$global_item_id)
  if (any(supplied_global)) {
    global <- .link_ids(a$items$global_item_id, "artifact global_item_id")
    .link_check(all(global[match(identity$items$item_id, ids)][supplied_global] ==
      identity$items$global_item_id[supplied_global]), "Phase A artifact global_item_id mapping mismatch.")
  }
  rows <- a$phase_a_within_set_evidence %||% a$within_set_evidence
  .link_check(is.data.frame(rows), "E3 requires exact raw within-set rows; summaries/draws alone are insufficient.")
  hash <- .adaptive_phase_a_hash_object(tibble::as_tibble(rows))
  if (!is.null(a$phase_a_within_set_evidence_hash) && !is.na(a$phase_a_within_set_evidence_hash)) {
    .link_check(identical(a$phase_a_within_set_evidence_hash, hash), "Phase A artifact raw evidence hash mismatch.")
  }
  source <- .link_source(list(artifact_hash = .link_hash(a), evidence_hash = hash,
    n_observations = a$n_pairs_committed,
    trait = .link_artifact_trait(a), orientation = .link_artifact_orientation(a)))
  if (!all(c("observation_id", "A_set", "B_set") %in% names(rows))) {
    .link_check(all(c("pair_id", "step_id", "A_item", "B_item", "y_A") %in% names(rows)),
      "E3 artifact raw rows require pair_id, step_id, A_item, B_item, and y_A.")
    for (key in c("pair_id", "step_id")) {
      v <- rows[[key]]
      .link_check(is.numeric(v) && all(is.finite(v) & v >= 1 & v == floor(v)) && !anyDuplicated(v),
        "E3 artifact raw row IDs must be unique positive integers.")
    }
    # Length-prefixing makes IDs unambiguous even when set IDs contain separators.
    rows <- tibble::tibble(observation_id = paste0("phase-a:", nchar(identity$set_id, type = "bytes"),
      ":", identity$set_id, ":", rows$pair_id), A_set = rep(identity$set_id, nrow(rows)),
      A_item = rows$A_item, B_set = rep(identity$set_id, nrow(rows)), B_item = rows$B_item, y_A = rows$y_A)
  }
  rows <- .link_observations(rows, identity)
  .link_check(identical(nrow(rows), source$n_observations), "Phase A artifact raw evidence count mismatch.")
  supplied <- x$source %||% list()
  .link_source(supplied)
  for (k in names(supplied)) {
    .link_check(isTRUE(all.equal(supplied[[k]], source[[k]])), paste0("Artifact source mismatch: ", k, "."))
  }
  list(observations = rows, source = source)
}
