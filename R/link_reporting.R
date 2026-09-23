# Common reporting consumes validated results, never an estimator's private state.
.link_stage_row <- function(result, refit_id = 1L, status = "active", fit_reused = FALSE) {
  input <- result$continuation$input
  p <- result$provenance
  d <- result$diagnostics
  o <- result$offset
  list(refit_id = as.integer(refit_id), spoke_set_id = input$spoke$set_id,
    hub_set_id = input$hub$set_id, estimator_id = result$estimator_id,
    estimator_version = result$estimator_version, config_hash = p$hashes$config,
    input_hash = p$hashes$input, phase_a_hub_hash = p$hashes$phase_a_hub,
    phase_a_spoke_hash = p$hashes$phase_a_spoke,
    phase_a_hub_artifact_hash = p$phase_a_sources$hub$artifact_hash,
    phase_a_spoke_artifact_hash = p$phase_a_sources$spoke$artifact_hash,
    phase_a_hub_source_evidence_hash = p$phase_a_sources$hub$evidence_hash,
    phase_a_spoke_source_evidence_hash = p$phase_a_sources$spoke$evidence_hash,
    phase_a_hub_kind = input$phase_a$hub$kind, phase_a_spoke_kind = input$phase_a$spoke$kind,
    phase_a_within_edges_hub_used = p$counts$phase_a_hub,
    phase_a_within_edges_spoke_used = p$counts$phase_a_spoke,
    phase_a_hub_source_count = p$counts$source_hub,
    phase_a_spoke_source_count = p$counts$source_spoke,
    phase_a_hub_payload_count = as.integer(if (input$phase_a$hub$kind == "points")
      length(input$phase_a$hub$value) else nrow(input$phase_a$hub$value)),
    phase_a_spoke_payload_count = as.integer(if (input$phase_a$spoke$kind == "points")
      length(input$phase_a$spoke$value) else nrow(input$phase_a$spoke$value)),
    phase_b_active_edges_used = p$counts$cross, cross_evidence_hash = p$hashes$cross,
    delta_spoke_mean = o$delta_mean, delta_spoke_sd = o$delta_sd,
    delta_spoke_lower = o$delta_lower, delta_spoke_upper = o$delta_upper,
    fit_reused = fit_reused, fit_attempted = d$fit_attempted, fit_valid = d$fit_valid, convergence_code = d$convergence_code,
    hessian_pd = d$hessian_pd, covariance_valid = d$covariance_valid,
    covariance_jitter = d$covariance_jitter, warning_code = d$warning_code, failure_code = d$failure_code,
    uncertainty_scope = d$uncertainty_scope, identification = o$identification,
    linking_identified = identical(o$identification, "cross_set"),
    status = status, link_state_frozen = identical(status, "frozen"))
}

.link_reporting_results <- function(x) {
  if (inherits(x, "pairwiseLLM_link_session")) {
    .link_session_validate(x)
    .link_session_results(x)
  } else {
    .link_validate_result(x)
    stats::setNames(list(x), x$continuation$input$spoke$set_id)
  }
}

.link_item_summary <- function(x, top_n = NULL, sort_by = NULL) {
  results <- .link_reporting_results(x)
  out <- dplyr::bind_rows(lapply(results, function(r) {
    items <- r$items
    items$theta_link_eap <- items$theta_link_mean
    items$estimator_id <- rep(r$estimator_id, nrow(items))
    items$uncertainty_scope <- rep(r$diagnostics$uncertainty_scope, nrow(items))
    items$link_spoke_id <- r$continuation$input$spoke$set_id
    items
  }))
  if (!is.null(sort_by)) {
    .link_check(is.character(sort_by) && length(sort_by) == 1L && sort_by %in% names(out),
      "Unknown linked item sort column.")
    out <- out[order(out[[sort_by]], na.last = TRUE), , drop = FALSE]
  }
  if (!is.null(top_n)) out <- utils::head(out, top_n)
  out
}

# Keep historical session log schemas unchanged; expose new identity in views.
.link_reference_reporting <- function(result) {
  source <- result$provenance$phase_a_sources
  if (is.null(source$hub$reference_hash) && is.null(source$spoke$reference_hash)) return(list())
  list(phase_a_hub_reference_hash = source$hub$reference_hash %||% NA_character_,
    phase_a_spoke_reference_hash = source$spoke$reference_hash %||% NA_character_)
}

#' Inspect explicit-evidence linking results and sessions
#'
#' @param object,x A common linking result or linking session.
#' @param ... Unused.
#' @return `summary()` returns one row per spoke, including offset, uncertainty
#'   scope, evidence counts, validity and controller status. `print()` returns
#'   its input invisibly. [summarize_items()] supplies linked item means, SDs,
#'   intervals, ranks, estimator ID and uncertainty scope.
#' @details `theta_link_eap` is a compatibility alias for `theta_link_mean`:
#'   a posterior mean for E1 quadrature and E3-MCMC, and the MAP location for
#'   E2/E3 Laplace. Unavailable uncertainty remains `NA`. E1 uncertainty is
#'   conditional on fixed Phase A shapes; E2/E3 include shapes and offset.
#' @seealso [fit_link()], [start_link_session()], [summarize_items()], [summarize_refits()]
#' @family linking
#' @export
summary.pairwiseLLM_link_result <- function(object, ...) {
  .link_validate_result(object)
  tibble::as_tibble(c(.link_stage_row(object), .link_reference_reporting(object)))
}

#' @rdname summary.pairwiseLLM_link_result
#' @export
summary.pairwiseLLM_link_session <- function(object, ...) {
  results <- .link_reporting_results(object)
  dplyr::bind_rows(lapply(names(results), function(key) {
    ids <- object$link_stage_log$refit_id[object$link_stage_log$spoke_set_id == key]
    c(.link_stage_row(results[[key]], max(ids), object$status_by_spoke[[key]],
      object$link_stage_log$fit_reused[match(max(ids), object$link_stage_log$refit_id)]),
      .link_reference_reporting(results[[key]]))
  }))
}

.link_print <- function(x) {
  rows <- summary(x)
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    cat(sprintf("Link %s -> %s: %s v%s [%s]\n", r$hub_set_id, r$spoke_set_id,
      r$estimator_id, r$estimator_version, r$status))
    cat(sprintf("  Offset: %.6g (SD %.6g); %s; valid: %s\n", r$delta_spoke_mean,
      r$delta_spoke_sd, r$identification, r$fit_valid))
    cat(sprintf("  Uncertainty: %s\n", r$uncertainty_scope))
    cat(sprintf("  Phase A: %s/%s payloads %d/%d; raw rows used %d/%d; cross rows %d\n",
      r$phase_a_hub_kind, r$phase_a_spoke_kind, r$phase_a_hub_payload_count,
      r$phase_a_spoke_payload_count, r$phase_a_within_edges_hub_used,
      r$phase_a_within_edges_spoke_used, r$phase_b_active_edges_used))
  }
  invisible(x)
}

#' @rdname summary.pairwiseLLM_link_result
#' @export
print.pairwiseLLM_link_result <- function(x, ...) .link_print(x)

#' @rdname summary.pairwiseLLM_link_result
#' @export
print.pairwiseLLM_link_session <- function(x, ...) .link_print(x)
