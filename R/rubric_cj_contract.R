# Completed CJ inputs for downstream rubric calibration. No inference occurs here.

.rubric_choice <- function(x, choices, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    rlang::abort(paste0("`", name, "` must be one of: ", paste(choices, collapse = ", "), "."))
  }
  x
}

.rubric_ids <- function(x, name = "item_id") {
  if (!(is.character(x) || is.numeric(x)) || !is.null(dim(x)) ||
    length(x) == 0L || anyNA(x) || any(!nzchar(trimws(as.character(x)))) ||
    (is.numeric(x) && any(!is.finite(x)))) {
    rlang::abort(paste0("`", name, "` must contain nonmissing, nonempty item IDs."))
  }
  x <- as.character(x)
  if (anyDuplicated(x)) rlang::abort(paste0("`", name, "` must contain unique IDs."))
  x
}

.rubric_trait <- function(trait, ...) {
  if (!is.null(trait) && length(trait) != 1L) rlang::abort("`trait` must be a single trait identifier.")
  sources <- c(list(trait), list(...))
  values <- lapply(sources, function(x) {
    if (is.null(x)) return(character())
    if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
      rlang::abort("Trait identity must be a nonmissing, nonempty character identifier.")
    }
    unique(x)
  })
  values <- unique(unlist(values, use.names = FALSE))
  if (length(values) == 0L) rlang::abort("Supply `trait`: this CJ result does not record trait identity.")
  if (length(values) != 1L) rlang::abort("Trait mismatch: one calibration must contain exactly one trait.")
  values
}

.rubric_orientation <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  if (length(values) && (anyNA(values) || any(values != "higher_is_better"))) {
    rlang::abort("CJ orientation must be `higher_is_better`; scores are never silently reversed.")
  }
  "higher_is_better"
}

.rubric_items <- function(ids, theta, theta_sd = NULL, global_ids = NULL, set_id = NULL) {
  ids <- .rubric_ids(ids)
  if (!is.numeric(theta) || !is.null(dim(theta)) || length(theta) != length(ids) ||
    any(!is.finite(theta))) rlang::abort("Accepted `theta` must be finite and aligned with item IDs.")
  theta_sd <- theta_sd %||% rep(NA_real_, length(ids))
  if (!is.numeric(theta_sd) || !is.null(dim(theta_sd)) || length(theta_sd) != length(ids) ||
    any(is.nan(theta_sd) | is.infinite(theta_sd) | theta_sd < 0, na.rm = TRUE)) {
    rlang::abort("`theta_sd` must be nonnegative and finite, or NA when unavailable.")
  }
  canonical <- if (is.null(global_ids)) ids else .rubric_ids(global_ids, "global_item_id")
  if (length(canonical) != length(ids)) rlang::abort("Global and source item IDs must align.")
  out <- tibble::tibble(item_id = canonical, theta = as.double(theta), theta_sd = as.double(theta_sd))
  if (!is.null(global_ids)) {
    out$source_item_id <- ids
    out$global_item_id <- canonical
  }
  if (!is.null(set_id)) {
    if (length(set_id) != length(ids) || anyNA(set_id)) rlang::abort("Set IDs must align with item IDs.")
    out$set_id <- set_id
  }
  out
}

.rubric_draws <- function(draws, source_ids, ids) {
  if (is.null(draws)) return(NULL)
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 2L || any(!is.finite(draws))) {
    rlang::abort("Posterior draws must be a finite numeric matrix with at least two draws.")
  }
  .rubric_ids(colnames(draws), "posterior draw column names")
  draws <- reorder_theta_draws(draws, source_ids)
  colnames(draws) <- ids
  draws
}

.rubric_equal_scores <- function(x, y, name) {
  if (!is.numeric(x) || !is.numeric(y) ||
    !isTRUE(all.equal(as.double(x), as.double(y), tolerance = 1e-8))) {
    rlang::abort(paste0("CJ ", name, " does not match the accepted fit by item ID."))
  }
  invisible(TRUE)
}

.rubric_summary_match <- function(summary, ids, theta, theta_sd, adaptive = FALSE) {
  fields <- if (adaptive) c("item_id", "theta_raw_eap", "theta_raw_sd") else c("ID", "theta_mean", "theta_sd")
  if (!is.data.frame(summary) || !all(fields %in% names(summary))) {
    rlang::abort("Completed CJ result is missing its authoritative item summary.")
  }
  summary_ids <- .rubric_ids(summary[[fields[[1L]]]], "summary IDs")
  if (!setequal(ids, summary_ids)) rlang::abort("CJ summary IDs do not match the accepted fit.")
  idx <- match(ids, summary_ids)
  .rubric_equal_scores(summary[[fields[[2L]]]][idx], theta, "item summary")
  .rubric_equal_scores(summary[[fields[[3L]]]][idx], theta_sd, "uncertainty summary")
  invisible(TRUE)
}

.rubric_new_cj <- function(items, model_variant, estimation_mode, scale_status, trait,
                           fit_contract = list(), fit_contract_hash = NULL, provenance = list(),
                           diagnostics = list(), reliability = NULL, posterior_draws = NULL,
                           reference = NULL) {
  out <- structure(list(
    items = items, model_variant = model_variant, fit_contract = fit_contract,
    fit_contract_hash = fit_contract_hash, estimation_mode = estimation_mode,
    provenance = provenance, scale_status = scale_status, trait = trait,
    orientation = "higher_is_better", diagnostics = diagnostics, reliability = reliability,
    posterior_draws = posterior_draws, reference = reference
  ), class = "pairwiseLLM_rubric_cj")
  .rubric_validate_cj(out)
  out
}

.rubric_validate_cj <- function(x) {
  if (!inherits(x, "pairwiseLLM_rubric_cj") || !is.list(x) || !is.data.frame(x$items) ||
    !all(c("item_id", "theta", "theta_sd") %in% names(x$items))) {
    rlang::abort("Invalid normalized rubric CJ contract.")
  }
  .rubric_items(x$items$item_id, x$items$theta, x$items$theta_sd)
  .rubric_choice(x$model_variant, c("btl", "btl_e", "btl_b", "btl_e_b"), "model_variant")
  .rubric_choice(x$scale_status, c("within_set", "phase_a_reference", "phase_b_linked"), "scale_status")
  .rubric_choice(x$estimation_mode, c("fixed", "adaptive", "phase_a", "phase_b"), "estimation_mode")
  .rubric_trait(x$trait)
  .rubric_choice(x$orientation, "higher_is_better", "orientation")
  if (!is.list(x$fit_contract) || !is.list(x$diagnostics) || !is.list(x$provenance)) {
    rlang::abort("Normalized CJ metadata must retain fit, diagnostic, and provenance lists.")
  }
  .rubric_draws(x$posterior_draws, x$items$item_id, x$items$item_id)
  if (x$scale_status == "phase_b_linked" && !is.null(x$posterior_draws)) {
    rlang::abort("Phase A posterior draws are not Phase B linked draws.")
  }
  invisible(x)
}

.rubric_cj_fixed <- function(cj, trait) {
  fits <- cj$fits
  if (!is.list(fits) || length(fits) == 0L || !is.list(cj$item_log_list) ||
    length(cj$item_log_list) != length(fits) || !is.data.frame(cj$round_log) ||
    nrow(cj$round_log) != length(fits)) {
    rlang::abort("Supply a completed `fit_bayes_btl_mcmc()` result with aligned refits and logs.")
  }
  refit <- length(fits)
  fit <- fits[[refit]]
  if (!is.list(fit)) rlang::abort("Completed fixed CJ requires a Bayesian fit contract.")
  ids <- .rubric_ids(names(fit$theta_mean))
  validate_btl_fit_contract(fit, ids)
  .rubric_choice(cj$round_log[["model_variant"]][refit], fit$model_variant, "round-log model_variant")
  .rubric_summary_match(summarize_items(cj), ids, fit$theta_mean, fit$theta_sd)
  if (!"refit_id" %in% names(cj$item_log_list[[refit]]) ||
    anyNA(cj$item_log_list[[refit]]$refit_id) || any(cj$item_log_list[[refit]]$refit_id != refit) ||
    !identical(as.integer(cj$round_log[["round_id"]]), seq_len(refit))) {
    rlang::abort("Standalone CJ refit IDs do not align with the completed fits.")
  }
  .rubric_orientation(cj$orientation, fit$orientation)
  .rubric_new_cj(
    .rubric_items(ids, fit$theta_mean, fit$theta_sd), fit$model_variant, "fixed", "within_set",
    .rubric_trait(trait, cj$trait, fit$trait), fit_contract = fit[setdiff(names(fit), c(
      "theta_draws", "epsilon_draws", "beta_draws"
    ))], fit_contract_hash = fit$fit_config_hash,
    provenance = list(refit_id = refit, refit = cj$round_log[refit, , drop = FALSE],
      collection = cj$provenance %||% list(), finalization = "completed_fixed_fit"),
    diagnostics = list(fit = fit$diagnostics, diagnostics_pass = fit$diagnostics_pass),
    reliability = cj$round_log[["reliability_EAP"]][refit],
    posterior_draws = .rubric_draws(fit$theta_draws, ids, ids)
  )
}

.rubric_adaptive_terminal <- function(state, linked = FALSE) {
  if (!isTRUE(state$meta$stop_decision)) {
    rlang::abort("CJ input is incomplete: an adaptive return or step limit is not a terminal stop.")
  }
  reasons <- if (linked) {
    c("all_spokes_stopped", "all_spokes_exhausted")
  } else {
    c("btl_converged", "max_pairs_after_stop_exhausted")
  }
  .rubric_choice(state$meta$stop_reason, reasons, "adaptive stop_reason")
  if (!linked) {
    log <- state$round_log
    if (!is.data.frame(log) || nrow(log) == 0L ||
      !all(c("refit_id", "step_id_at_refit", "stop_decision") %in% names(log))) {
      rlang::abort("Completed adaptive CJ requires a recorded Bayesian refit and stop boundary.")
    }
    last <- nrow(log)
    if (!identical(as.integer(state$refit_meta$last_refit_round_id), as.integer(log$refit_id[[last]])) ||
      !identical(as.integer(state$refit_meta$last_refit_step), as.integer(log$step_id_at_refit[[last]]))) {
      rlang::abort("Adaptive accepted fit and last refit log are inconsistent.")
    }
    boundary <- if (state$meta$stop_reason == "btl_converged") {
      log$refit_id[[last]]
    } else {
      state$meta$stop_boundary_refit_id
    }
    idx <- match(boundary, log$refit_id)
    if (length(idx) != 1L || is.na(idx) || !isTRUE(log$stop_decision[[idx]])) {
      rlang::abort("Adaptive completion requires a passing recorded stop boundary.")
    }
  }
  invisible(TRUE)
}

.rubric_cj_adaptive <- function(state, trait) {
  .rubric_adaptive_terminal(state)
  if (length(unique(state$items$set_id)) != 1L) {
    rlang::abort("Independent Phase A sets cannot be combined as one within-set CJ scale.")
  }
  ids <- .rubric_ids(state$item_ids)
  fit <- state$btl_fit
  if (!is.list(fit)) rlang::abort("Completed adaptive CJ requires a Bayesian fit.")
  fit$theta_draws <- fit$btl_posterior_draws
  validate_btl_fit_contract(fit, ids)
  if (!is.null(state$config$btl_config$model_variant)) {
    .rubric_choice(state$config$btl_config$model_variant, fit$model_variant, "configured model_variant")
  }
  .rubric_choice(utils::tail(state$round_log[["model_variant"]], 1L), fit$model_variant, "round-log model_variant")
  summary <- summarize_items(state)
  .rubric_summary_match(summary, ids, fit$theta_mean, fit$theta_sd, adaptive = TRUE)
  if (!"refit_id" %in% names(summary) || anyNA(summary$refit_id) ||
    !all(summary$refit_id == state$refit_meta$last_refit_round_id)) {
    rlang::abort("Adaptive item summary is not from the accepted refit.")
  }
  source_ids <- .rubric_ids(state$items$item_id)
  if (!setequal(ids, source_ids)) rlang::abort("Adaptive fit IDs do not match the complete item domain.")
  item_idx <- match(ids, source_ids)
  items <- .rubric_items(ids, fit$theta_mean, fit$theta_sd,
    state$items$global_item_id[item_idx], state$items$set_id[item_idx])
  .rubric_orientation(state$orientation, state$meta$orientation, state$items[["orientation"]], fit$orientation)
  .rubric_new_cj(
    items, fit$model_variant, "adaptive", "within_set",
    .rubric_trait(trait, state$trait, state$meta$trait, state$items[["trait"]], fit$trait),
    fit_contract = fit[setdiff(names(fit), c("theta_draws", "btl_posterior_draws", "epsilon_draws", "beta_draws"))],
    fit_contract_hash = fit$fit_config_hash,
    provenance = list(finalization = state$meta$stop_reason,
      refit = utils::tail(state$round_log, 1L), stop_boundary_refit_id = state$meta$stop_boundary_refit_id,
      fitted_comparisons = state$refit_meta$last_refit_M_done,
      collected_comparisons = sum(!is.na(state$step_log$pair_id)),
      collection = unique(state$step_log[intersect(
        c("judge_backend", "judge_model", "judge_endpoint"), names(state$step_log))])),
    diagnostics = list(fit = fit$diagnostics, diagnostics_pass = fit$diagnostics_pass,
      stop_metrics = state$stop_metrics),
    reliability = utils::tail(state$round_log$reliability_EAP, 1L),
    posterior_draws = .rubric_draws(fit$theta_draws, ids, items$item_id)
  )
}

.rubric_normalize_cj <- function(cj, trait = NULL, scale_status = NULL, include_draws = TRUE) {
  if (!is.logical(include_draws) || length(include_draws) != 1L || is.na(include_draws)) {
    rlang::abort("`include_draws` must be TRUE or FALSE.")
  }
  if (!is.list(cj) || is.data.frame(cj)) rlang::abort("Supply a supported completed CJ result.")
  wrapper_trait <- if (inherits(cj$state, "adaptive_state")) cj$trait else NULL
  wrapper_orientation <- if (inherits(cj$state, "adaptive_state")) cj$orientation else NULL
  .rubric_orientation(wrapper_orientation)
  if (inherits(cj$state, "adaptive_state")) cj <- cj$state
  if (inherits(cj, "adaptive_state")) {
    trait <- .rubric_trait(trait, wrapper_trait, cj$trait, cj$meta$trait, cj$items[["trait"]])
    controller <- .adaptive_controller_resolve(cj)
    out <- if (identical(controller$run_mode, "within_set")) {
      .rubric_cj_adaptive(cj, trait)
    } else {
      .rubric_cj_linked(cj, trait, controller)
    }
  } else if (!is.null(cj$fits)) {
    out <- .rubric_cj_fixed(cj, trait)
  } else if (!is.null(cj$fit_config_surface) && !is.null(cj$items)) {
    out <- .rubric_cj_phase_a(cj, trait)
  } else {
    rlang::abort("Unsupported CJ input: supply a completed fixed/adaptive result or a Phase A artifact.")
  }
  if (!is.null(scale_status) && !out$scale_status %in% scale_status) {
    rlang::abort(paste0("CJ scale `", out$scale_status, "` is inappropriate; required: ",
      paste(scale_status, collapse = ", "), "."))
  }
  if (!include_draws) out$posterior_draws <- NULL
  if (isFALSE(out$diagnostics$diagnostics_pass)) {
    rlang::warn("Upstream CJ diagnostics failed; retained scores do not imply adequate precision.",
      class = "pairwiseLLM_rubric_cj_diagnostics")
  }
  out
}
