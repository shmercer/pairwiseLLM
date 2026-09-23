# Explicit-evidence sessions. Estimator-private data live only in common results.

.link_reject_legacy <- function(state) {
  if (!is.list(state)) return(invisible(NULL))
  get <- function(x, key) if (is.list(x)) x[[key]] else NULL
  controller <- get(state, "controller")
  linking <- get(state, "linking")
  mode <- get(controller, "link_estimation_mode") %||%
    get(get(get(state, "config"), "adaptive_config"), "link_estimation_mode")
  phase <- get(get(linking, "phase_a"), "phase")
  legacy <- get(linking, "anchored_joint")
  accepted <- get(legacy, "accepted_state_by_spoke")
  used <- is.list(accepted) && length(accepted) > 0L && any(vapply(accepted, function(x)
    identical(get(x, "anchored_joint_init_state_method"), "phase_b_refit"), logical(1)))
  legacy_mode <- identical(mode, "anchored_joint") || !is.null(legacy) ||
    any(get(get(state, "link_stage_log"), "link_estimation_mode") == "anchored_joint", na.rm = TRUE)
  if (isTRUE(used) || (legacy_mode &&
      (identical(phase, "phase_b") || NROW(state$link_stage_log) > 0L))) {
    rlang::abort(paste0("Unsupported legacy anchored-joint Phase B session. Restart linking from ",
      "compatible Phase A artifacts/evidence with an explicit E1, E2, or E3 estimator; ",
      "the Phase B posterior cannot be migrated."), class = "pairwiseLLM_unsupported_legacy_link_state")
  }
  invisible(NULL)
}

.link_session_results <- function(state) {
  state$linking$estimator$accepted_state_by_spoke
}

.link_session_hash <- function(state) {
  state$identity_hash <- NULL
  .link_hash(state)
}

.link_session_validate <- function(state) {
  .link_reject_legacy(state)
  .link_check(inherits(state, "pairwiseLLM_link_session") && identical(state$schema_version, 1L),
    "Unsupported linking session schema.")
  .link_check(identical(state$identity_hash, .link_session_hash(state)),
    "Link session evidence/configuration/continuation identity hash mismatch.")
  .link_fields(state, c("schema_version", "refit_id", "linking", "status_by_spoke",
    "link_stage_log", "identity_hash"), label = "link session")
  .link_check(.link_data_only(state), "Link session must contain serializable data only.")
  e <- state$linking$estimator
  backend <- .link_resolve(e$id)
  .link_check(identical(e$version, backend$version), "Link session estimator version mismatch.")
  results <- .link_session_results(state)
  .link_check(length(results) > 0L && identical(names(results), names(state$status_by_spoke)),
    "Link session spoke identity mismatch.")
  for (key in names(results)) {
    r <- results[[key]]
    .link_validate_result(r)
    .link_check(identical(r$estimator_id, e$id) && identical(r$estimator_version, e$version) &&
      identical(r$continuation$input$spoke$set_id, key), "Link session estimator/spoke mismatch.")
    .link_check(identical(e$diagnostics_by_spoke[[key]], r$diagnostics) &&
      identical(e$identity_by_spoke[[key]], r$provenance), "Link session provenance/diagnostics mismatch.")
    .link_check(state$status_by_spoke[[key]] %in% c("active", "probe", "frozen"), "Invalid link status.")
  }
  .link_session_common_inputs(lapply(results, function(x) x$continuation$input))
  .adaptive_validate_log_schema(state$link_stage_log, schema_link_stage_log, "link_stage_log")
  invisible(TRUE)
}

.link_session_common_inputs <- function(inputs) {
  first <- inputs[[1L]]
  for (input in inputs) {
    .link_validate_input(input)
    for (field in c("estimator", "hub", "judge")) {
      .link_check(identical(input[[field]], first[[field]]), paste0("Session inputs differ in ", field, "."))
    }
    .link_check(identical(input$phase_a$hub, first$phase_a$hub), "Session hub Phase A evidence differs.")
  }
  keys <- vapply(inputs, function(x) x$spoke$set_id, character(1))
  .link_check(!anyDuplicated(keys), "Session inputs require unique spoke IDs.")
  globals <- unlist(lapply(c(list(first$hub), lapply(inputs, `[[`, "spoke")),
    function(x) x$items$global_item_id), use.names = FALSE)
  .link_check(!anyDuplicated(globals[!is.na(globals)]), "Global item IDs repeat across session sets.")
  invisible(keys)
}

.link_session_record <- function(state, result, status, fit_reused = FALSE) {
  key <- result$continuation$input$spoke$set_id
  state$linking$estimator$accepted_state_by_spoke[[key]] <- result
  state$linking$estimator$diagnostics_by_spoke[[key]] <- result$diagnostics
  state$linking$estimator$identity_by_spoke[[key]] <- result$provenance
  state$status_by_spoke[[key]] <- status
  state$refit_id <- state$refit_id + 1L
  row <- .link_stage_row(result, state$refit_id, status, fit_reused)
  state$link_stage_log <- append_canonical_row(state$link_stage_log, row,
    schema_link_stage_log, allow_multirow = FALSE)
  state$identity_hash <- .link_session_hash(state)
  state
}

#' Run and resume explicit-evidence linking sessions
#'
#' Sessions retain common E1--E3 results under `linking$estimator`, including
#' estimator ID/version, per-spoke continuation, diagnostics and exact provenance.
#' Pair selection and stopping remain external. No estimator is selected by default.
#' Adaptive Phase B D-optimal execution is unavailable for all estimators pending
#' a separate selector validation study. Posterior covariance does not establish
#' selector validity. Use these explicit-evidence sessions for E1--E3 comparisons;
#' E3-MCMC remains an explicitly requested audit/reference engine.
#' @param input A [prepare_link_input()] object, or (for `start_link_session`)
#'   a list of such objects with distinct spokes and identical hub evidence/judge.
#' @param state A linking session from `start_link_session()` or `load_link_session()`.
#' @param status Explicit reporting status: `active`, `probe`, or `frozen`.
#'   This labels controller status only: all observations in `input$cross` remain
#'   active estimation evidence. Held-out probe outcomes must not enter that table.
#' @details Resume requires the same estimator, version, item order, Phase A,
#'   judge, numerical configuration and unchanged old cross-evidence prefix.
#'   New rows may only be appended. Passing the identical input is an exact no-op
#'   (including MCMC); extended evidence refits using the saved numerical mode,
#'   never the previous posterior as a new prior. A frozen spoke cannot append
#'   evidence until the caller explicitly sets `status = "active"`.
#'
#'   With multiple spokes, each estimator fits its own hub/spoke pair. E2/E3 hub
#'   posteriors can differ across spokes. Item summaries retain `link_spoke_id`
#'   and do not average or silently select one hub posterior. Ranks are per fit.
#' @return A `pairwiseLLM_link_session` with exact data-only results and a stage log.
#' @seealso [prepare_link_input()], [fit_link()], [start_link_session()], [save_link_session()]
#' @family linking
#' @export
start_link_session <- function(input, status = "active") {
  .rubric_choice(status, c("active", "probe", "frozen"), "status")
  inputs <- if (inherits(input, "pairwiseLLM_link_input")) list(input) else input
  .link_check(is.list(inputs) && length(inputs) > 0L, "Supply prepared linking inputs.")
  .link_session_common_inputs(inputs)
  backend <- .link_resolve(inputs[[1L]]$estimator)
  state <- structure(list(schema_version = 1L, refit_id = 0L,
    linking = list(estimator = list(id = backend$id, version = backend$version,
      accepted_state_by_spoke = list(), diagnostics_by_spoke = list(), identity_by_spoke = list())),
    status_by_spoke = list(), link_stage_log = new_link_stage_log()), class = "pairwiseLLM_link_session")
  for (x in inputs) state <- .link_session_record(state, fit_link(x), status)
  .link_session_validate(state)
  state
}

#' @rdname start_link_session
#' @export
resume_link_session <- function(state, input, status = NULL) {
  .link_session_validate(state)
  .link_validate_input(input)
  previous <- .link_session_results(state)[[input$spoke$set_id]]
  .link_check(!is.null(previous), "Resume spoke is absent from the saved session.")
  old <- previous$continuation$input
  .link_check(identical(input$estimator, state$linking$estimator$id), "Resume estimator mismatch.")
  for (k in c("hub", "spoke", "phase_a", "judge", "control", "provenance")) {
    .link_check(identical(input[[k]], old[[k]]), paste0("Resume changed frozen ", k, "/configuration."))
  }
  .link_previous_mode(input, previous)
  current_status <- state$status_by_spoke[[input$spoke$set_id]]
  status <- status %||% current_status
  .rubric_choice(status, c("active", "probe", "frozen"), "status")
  same <- identical(input, old)
  .link_check(same || status != "frozen", "A frozen spoke cannot append evidence; explicitly reactivate it.")
  if (same && identical(status, current_status)) return(state)
  result <- if (same) previous else fit_link(input, previous = previous)
  out <- .link_session_record(state, result, status, fit_reused = same)
  .link_session_validate(out)
  out
}

#' Save and load exact linking sessions
#'
#' RDS preserves exact types, item/evidence order, hashes, covariance, numerical
#' modes and prediction data, including E3-MCMC draws. Reading never refits or
#' migrates a posterior. Only open trusted RDS files. Unknown external Phase A
#' provenance remains missing; computed payload hashes are always present.
#' @param state A [start_link_session()] result.
#' @param path File path for an RDS session.
#' @param overwrite Whether to replace an existing file.
#' @param input Optional expected prepared input (or list of inputs) on load.
#'   It must match the saved input exactly. Use [resume_link_session()] to append
#'   new evidence after this identity check.
#' @return Saving invisibly returns `path`; loading returns the identical session.
#' @seealso [prepare_link_input()], [fit_link()], [start_link_session()], [save_link_session()]
#' @family linking
#' @export
save_link_session <- function(state, path, overwrite = FALSE) {
  .link_session_validate(state)
  .link_check(is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path), "Invalid session path.")
  .link_check(is.logical(overwrite) && length(overwrite) == 1L && !is.na(overwrite), "Invalid overwrite flag.")
  .link_check(overwrite || !file.exists(path), "Session file exists; use overwrite = TRUE.")
  tmp <- tempfile(".link-session-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(state, tmp, version = 3)
  .link_check(file.rename(tmp, path), "Could not replace linking session file.")
  invisible(path)
}

#' @rdname save_link_session
#' @export
load_link_session <- function(path, input = NULL) {
  state <- readRDS(path)
  .link_session_validate(state)
  if (!is.null(input)) {
    inputs <- if (inherits(input, "pairwiseLLM_link_input")) list(input) else input
    .link_session_common_inputs(inputs)
    results <- .link_session_results(state)
    keys <- vapply(inputs, function(x) x$spoke$set_id, character(1))
    .link_check(setequal(keys, names(results)), "Expected session spoke identities differ.")
    for (x in inputs) {
      .link_check(identical(x, results[[x$spoke$set_id]]$continuation$input),
        "Saved estimator/item order/evidence/configuration does not match expected input.")
    }
  }
  state
}
