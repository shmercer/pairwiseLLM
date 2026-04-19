# -------------------------------------------------------------------------
# Adaptive linking Phase A artifact helpers.
# -------------------------------------------------------------------------

.adaptive_phase_a_empty_state <- function(set_ids = integer()) {
  set_ids <- as.integer(sort(unique(set_ids)))
  tibble::tibble(
    set_id = set_ids,
    source = rep(NA_character_, length(set_ids)),
    status = rep(NA_character_, length(set_ids)),
    validation_message = rep(NA_character_, length(set_ids)),
    artifact_path = rep(NA_character_, length(set_ids))
  )
}

.adaptive_phase_a_status_tbl <- function(state) {
  phase_a <- state$linking$phase_a %||% list()
  tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
}

.adaptive_phase_a_ready_sets <- function(state) {
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  ready_sets <- integer()
  if (nrow(status_tbl) > 0L) {
    ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
    ready_sets <- ready_sets[!is.na(ready_sets)]
  }
  as.integer(sort(unique(ready_sets)))
}

.adaptive_phase_a_pending_run_sets <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")) {
    return(integer())
  }
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  if (nrow(status_tbl) < 1L) {
    return(integer())
  }
  keep <- !is.na(status_tbl$set_id) &
    status_tbl$source %in% "run" &
    status_tbl$status != "ready"
  as.integer(status_tbl$set_id[keep])
}

.adaptive_phase_a_ready_spokes <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  if (nrow(status_tbl) < 1L) {
    return(integer())
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
  if (!hub_id %in% ready_sets) {
    return(integer())
  }
  spokes <- setdiff(unique(as.integer(state$items$set_id)), hub_id)
  as.integer(sort(intersect(spokes, ready_sets)))
}

.adaptive_phase_a_required_sets <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(integer())
  }
  set_ids <- as.integer(sort(unique(state$items$set_id)))
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- setdiff(set_ids, hub_id)
  as.integer(sort(unique(c(hub_id, spoke_ids))))
}

.adaptive_phase_a_set_stop_passed <- function(artifact, source, controller) {
  source <- as.character(source %||% NA_character_)
  if (is.null(artifact) || !is.list(artifact)) {
    return(FALSE)
  }
  if (identical(source, "run")) {
    return(isTRUE(.adaptive_phase_a_run_stop_passed(artifact, controller = controller)))
  }
  if (identical(source, "import")) {
    return(isTRUE(.adaptive_phase_a_run_stop_passed(artifact, controller = controller)) ||
      isTRUE(artifact$quality_gate_accepted %||% FALSE))
  }
  FALSE
}

.adaptive_link_phase_context <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  mode <- as.character(controller$run_mode %||% "within_set")
  if (!mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(list(
      phase = "phase_a",
      pending_run_sets = integer(),
      ready_spokes = integer(),
      active_spokes = integer(),
      stopped_spokes = integer(),
      active_phase_a_set = NA_integer_
    ))
  }

  phase_a <- state$linking$phase_a %||% list()
  explicit_phase <- as.character(phase_a$phase %||% NA_character_)
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  required_sets <- as.integer(
    phase_a$required_sets %||% .adaptive_phase_a_required_sets(state, controller = controller)
  )
  stop_pass_map <- phase_a$set_stop_pass_by_set %||% list()
  ready_spokes <- as.integer(phase_a$ready_spokes %||% .adaptive_phase_a_ready_spokes(state, controller = controller))
  ready_spokes <- ready_spokes[!is.na(ready_spokes)]
  pending_run_sets <- integer()
  if (nrow(status_tbl) > 0L) {
    pending <- status_tbl$source %in% "run" & status_tbl$status != "ready"
    pending_run_sets <- as.integer(sort(unique(as.integer(status_tbl$set_id[pending]))))
    pending_run_sets <- pending_run_sets[!is.na(pending_run_sets)]
  }
  if (identical(explicit_phase, "phase_b") && length(ready_spokes) < 1L) {
    hub_id <- as.integer(controller$hub_id %||% 1L)
    if (nrow(status_tbl) > 0L) {
      ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
      ready_spokes <- as.integer(sort(unique(setdiff(ready_sets, hub_id))))
    } else {
      ready_spokes <- as.integer(sort(unique(setdiff(as.integer(state$items$set_id), hub_id))))
    }
  }
  active_set <- if (length(pending_run_sets) > 0L) {
    as.integer(sort(unique(pending_run_sets))[[1L]])
  } else {
    NA_integer_
  }
  stopped_map <- controller$link_stopped_by_spoke %||% list()
  stopped_spokes <- integer()
  if (length(ready_spokes) > 0L) {
    stopped_spokes <- as.integer(ready_spokes[vapply(as.character(ready_spokes), function(key) {
      isTRUE(stopped_map[[key]])
    }, logical(1L))])
  }
  # In linking Variant 2, freeze ends all remaining cross-set work for the
  # spoke in the current run, including held-out probe collection.
  active_spokes <- as.integer(ready_spokes)
  strict_ready <- isTRUE(phase_a$strict_ready_for_phase_b %||% phase_a$ready_for_phase_b %||% FALSE)
  has_stop_map <- length(stop_pass_map) > 0L
  has_effective_stop_map <- isTRUE(has_stop_map) && any(vapply(stop_pass_map, isTRUE, logical(1L)))
  if (isTRUE(has_effective_stop_map) && length(required_sets) > 0L && length(ready_spokes) > 0L) {
    strict_ready <- strict_ready &&
      all(vapply(as.character(required_sets), function(key) isTRUE(stop_pass_map[[key]]), logical(1L)))
  }
  phase <- if (identical(explicit_phase, "phase_b") && length(pending_run_sets) == 0L) {
    "phase_b"
  } else if (length(pending_run_sets) > 0L) {
    "phase_a"
  } else if (length(ready_spokes) > 0L && isTRUE(strict_ready)) {
    "phase_b"
  } else {
    "phase_a"
  }
  list(
    phase = as.character(phase),
    pending_run_sets = as.integer(pending_run_sets),
    ready_spokes = as.integer(ready_spokes),
    active_spokes = as.integer(sort(unique(active_spokes))),
    stopped_spokes = as.integer(sort(unique(stopped_spokes))),
    active_phase_a_set = as.integer(active_set)
  )
}

.adaptive_phase_a_hash_object <- function(x) {
  tmp <- tempfile("phase_a_hash_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  as.character(unname(tools::md5sum(tmp)))
}

.adaptive_phase_a_artifact_memo_surface <- function(artifact) {
  if (!is.list(artifact)) {
    return(NULL)
  }

  evidence_hash <- as.character(artifact$phase_a_within_set_evidence_hash %||% NA_character_)
  evidence <- NULL
  if (is.na(evidence_hash) || !nzchar(evidence_hash)) {
    evidence <- tibble::as_tibble(
      artifact$phase_a_within_set_evidence %||%
        artifact$within_set_evidence %||%
        .adaptive_phase_a_empty_within_set_evidence()
    )
    evidence_hash <- .adaptive_phase_a_hash_object(evidence)
  }

  list(
    set_id = as.integer(artifact$set_id %||% NA_integer_),
    fit_model_id = as.character(artifact$fit_model_id %||% NA_character_),
    fit_config_surface = artifact$fit_config_surface %||% list(),
    fit_config_hash = as.character(artifact$fit_config_hash %||% NA_character_),
    n_items = as.integer(artifact$n_items %||% NA_integer_),
    n_pairs_committed = as.integer(artifact$n_pairs_committed %||% NA_integer_),
    refit_id = as.integer(artifact$refit_id %||% NA_integer_),
    round_id_at_refit = as.integer(artifact$round_id_at_refit %||% NA_integer_),
    step_id_at_refit = as.integer(artifact$step_id_at_refit %||% NA_integer_),
    phase_scope = as.character(artifact$phase_scope %||% NA_character_),
    phase_scope_set_id = as.integer(artifact$phase_scope_set_id %||% NA_integer_),
    items = tibble::as_tibble(artifact$items %||% tibble::tibble()),
    diagnostics = artifact$diagnostics %||% list(),
    quality_gate_accepted = as.logical(artifact$quality_gate_accepted %||% FALSE),
    phase_a_within_set_evidence_hash = evidence_hash,
    phase_a_within_set_evidence_source = as.character(
      artifact$phase_a_within_set_evidence_source %||% NA_character_
    ),
    phase_a_within_set_evidence = evidence,
    judge_param_mode = as.character(artifact$judge_param_mode %||% NA_character_)
  )
}

.adaptive_phase_a_artifact_memo_hash <- function(artifact) {
  surface <- .adaptive_phase_a_artifact_memo_surface(artifact)
  if (is.null(surface)) {
    return(NA_character_)
  }
  .adaptive_phase_a_hash_object(surface)
}

.adaptive_phase_a_fit_contract_surface <- function(judge_param_mode,
                                                   model_variant) {
  judge_param_mode <- as.character(judge_param_mode %||% "global_shared")
  model_variant <- as.character(model_variant %||% "btl_e_b")

  if (length(judge_param_mode) != 1L || is.na(judge_param_mode) || !nzchar(judge_param_mode)) {
    rlang::abort("Phase A fit contract surface requires a single non-empty `judge_param_mode`.")
  }
  if (length(model_variant) != 1L || is.na(model_variant) || !nzchar(model_variant)) {
    rlang::abort("Phase A fit contract surface requires a single non-empty `model_variant`.")
  }

  list(
    judge_param_mode = judge_param_mode,
    model_variant = model_variant
  )
}

.adaptive_phase_a_required_config_surface <- function(state, set_id) {
  controller <- .adaptive_controller_resolve(state)
  fit <- state$btl_fit %||% list()
  .adaptive_phase_a_fit_contract_surface(
    judge_param_mode = controller$judge_param_mode %||% "global_shared",
    model_variant = fit$model_variant %||% "btl_e_b"
  )
}

.adaptive_phase_a_required_config_hash <- function(state, set_id) {
  .adaptive_phase_a_hash_object(.adaptive_phase_a_required_config_surface(state, set_id = set_id))
}

.adaptive_phase_a_round_diagnostics_surface <- function(state) {
  round_log <- tibble::as_tibble(state$round_log %||% tibble::tibble())
  if (nrow(round_log) < 1L) {
    return(list(
      has_round_log = FALSE,
      diagnostics_pass = NA,
      ts_btl_rank_spearman = NA_real_
    ))
  }

  list(
    has_round_log = TRUE,
    diagnostics_pass = if ("diagnostics_pass" %in% names(round_log)) {
      as.logical(round_log$diagnostics_pass[[nrow(round_log)]])
    } else {
      NA
    },
    ts_btl_rank_spearman = if ("ts_btl_rank_spearman" %in% names(round_log)) {
      as.double(round_log$ts_btl_rank_spearman[[nrow(round_log)]])
    } else {
      NA_real_
    }
  )
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_empty <- function(set_ids = integer()) {
  set_ids <- as.integer(sort(unique(set_ids[!is.na(set_ids)])))
  stats::setNames(rep.int(0L, length(set_ids)), as.character(set_ids))
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_validate <- function(cache,
                                                       set_ids,
                                                       context = "runtime") {
  expected_names <- as.character(sort(unique(as.integer(set_ids[!is.na(set_ids)]))))
  cache <- cache %||% integer()

  if (!is.integer(cache)) {
    rlang::abort(
      paste0(
        "Adaptive Phase A committed-pair cache invariant failed during ",
        context,
        ": `refit_meta$phase_a_committed_pairs_by_set` must be an integer vector."
      )
    )
  }

  cache_names <- names(cache) %||% character()
  if (!identical(cache_names, expected_names)) {
    rlang::abort(
      paste0(
        "Adaptive Phase A committed-pair cache invariant failed during ",
        context,
        ": set-id names must exactly match `state$items$set_id`."
      )
    )
  }

  if (any(is.na(cache)) || any(cache < 0L)) {
    rlang::abort(
      paste0(
        "Adaptive Phase A committed-pair cache invariant failed during ",
        context,
        ": cached counts must be non-missing and non-negative."
      )
    )
  }

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_rebuild <- function(state) {
  set_ids <- as.integer(sort(unique(state$items$set_id)))
  cache <- .adaptive_phase_a_committed_pairs_empty(set_ids)
  history <- .adaptive_history_tbl(state)

  if (nrow(history) < 1L) {
    return(cache)
  }

  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  a_set <- as.integer(set_map[as.character(history$A_id)])
  b_set <- as.integer(set_map[as.character(history$B_id)])
  within_set <- !is.na(a_set) & !is.na(b_set) & a_set == b_set
  if (!any(within_set)) {
    return(cache)
  }

  within_counts <- table(a_set[within_set])
  for (set_key in names(within_counts)) {
    cache[[set_key]] <- as.integer(within_counts[[set_key]])
  }

  cache
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_equivalent <- function(cache, rebuilt) {
  cache_names <- names(cache) %||% character()
  rebuilt_names <- names(rebuilt) %||% character()

  identical(cache_names, rebuilt_names) &&
    identical(as.integer(cache), as.integer(rebuilt))
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_reconcile <- function(cache,
                                                        state,
                                                        context = "runtime") {
  set_ids <- as.integer(sort(unique(state$items$set_id)))
  .adaptive_phase_a_committed_pairs_validate(cache, set_ids = set_ids, context = context)
  rebuilt <- .adaptive_phase_a_committed_pairs_rebuild(state)
  if (!isTRUE(.adaptive_phase_a_committed_pairs_equivalent(cache, rebuilt))) {
    rlang::abort(
      paste0(
        "Adaptive Phase A committed-pair cache invariant failed during ",
        context,
        ": cached set-local counts diverged from canonical committed history."
      )
    )
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_resolve <- function(state,
                                                      validate_existing = FALSE,
                                                      context = "runtime") {
  set_ids <- as.integer(sort(unique(state$items$set_id)))
  cache <- (state$refit_meta %||% list())$phase_a_committed_pairs_by_set %||% NULL
  if (is.null(cache)) {
    return(.adaptive_phase_a_committed_pairs_rebuild(state))
  }

  cache_ok <- tryCatch(
    {
      .adaptive_phase_a_committed_pairs_validate(cache, set_ids = set_ids, context = context)
      TRUE
    },
    error = function(e) {
      if (isTRUE(validate_existing)) {
        stop(e)
      }
      FALSE
    }
  )

  if (!isTRUE(cache_ok)) {
    return(.adaptive_phase_a_committed_pairs_rebuild(state))
  }

  expected_history_n <- as.integer(nrow(.adaptive_history_tbl(state)))
  cache_history_n <- as.integer(
    (state$refit_meta %||% list())$phase_a_committed_pairs_history_n %||% NA_integer_
  )
  if (length(cache_history_n) != 1L ||
    is.na(cache_history_n) ||
    !identical(cache_history_n, expected_history_n)) {
    return(.adaptive_phase_a_committed_pairs_rebuild(state))
  }

  if (isTRUE(validate_existing)) {
    .adaptive_phase_a_committed_pairs_reconcile(cache, state = state, context = context)
  }

  cache
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_update <- function(cache, state, A_id, B_id) {
  set_ids <- as.integer(sort(unique(state$items$set_id)))
  cache <- cache %||% .adaptive_phase_a_committed_pairs_empty(set_ids)
  .adaptive_phase_a_committed_pairs_validate(cache, set_ids = set_ids, context = "commit update")

  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  A_id <- as.character(A_id %||% NA_character_)
  B_id <- as.character(B_id %||% NA_character_)
  a_set <- as.integer(set_map[[A_id]] %||% NA_integer_)
  b_set <- as.integer(set_map[[B_id]] %||% NA_integer_)
  if (is.na(a_set) || is.na(b_set) || !identical(a_set, b_set)) {
    return(cache)
  }

  key <- as.character(a_set)
  cache[[key]] <- as.integer(cache[[key]] %||% 0L) + 1L
  cache
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_committed_pairs_rebuild_state <- function(state,
                                                            validate_existing = FALSE,
                                                            context = "runtime") {
  state$refit_meta <- state$refit_meta %||% list()
  state$refit_meta$phase_a_committed_pairs_by_set <- .adaptive_phase_a_committed_pairs_resolve(
    state,
    validate_existing = validate_existing,
    context = context
  )
  state$refit_meta$phase_a_committed_pairs_history_n <- as.integer(nrow(.adaptive_history_tbl(state)))
  state
}

.adaptive_phase_a_within_set_pair_count <- function(state, set_id) {
  cache <- .adaptive_phase_a_committed_pairs_resolve(state)
  as.integer(cache[[as.character(set_id)]] %||% 0L)
}

.adaptive_phase_a_summary_surface_stamp <- function(state, set_id) {
  items_set <- state$items[state$items$set_id == as.integer(set_id), , drop = FALSE]
  ids <- as.character(items_set$item_id)

  latest_item_log <- NULL
  if (is.list(state$item_log) && length(state$item_log) > 0L) {
    latest_item_log <- tibble::as_tibble(state$item_log[[length(state$item_log)]])
  }

  if (!is.null(latest_item_log) &&
    nrow(latest_item_log) > 0L &&
    all(c("item_id", "theta_raw_eap", "theta_raw_sd") %in% names(latest_item_log))) {
    idx <- match(ids, as.character(latest_item_log$item_id))
    if (all(!is.na(idx))) {
      theta_mean <- as.double(latest_item_log$theta_raw_eap[idx])
      theta_sd <- as.double(latest_item_log$theta_raw_sd[idx])
      rank_mu_raw <- if ("rank_raw" %in% names(latest_item_log)) {
        as.double(latest_item_log$rank_raw[idx])
      } else {
        rep(NA_real_, length(idx))
      }
      if (all(is.finite(theta_mean)) &&
        all(is.finite(theta_sd)) &&
        all(theta_sd >= 0)) {
        surface <- tibble::tibble(
          item_id = ids,
          theta_raw_eap = theta_mean,
          theta_raw_sd = theta_sd,
          rank_raw = rank_mu_raw
        )
        return(list(
          source = "item_log",
          hash = .adaptive_phase_a_hash_object(surface)
        ))
      }
    }
  }

  draws <- .adaptive_phase_a_extract_set_draws(state, set_id = set_id)
  if (!is.null(draws)) {
    return(list(
      source = "posterior_draws",
      hash = .adaptive_phase_a_hash_object(list(
        colnames = colnames(draws),
        draws = unclass(draws)
      ))
    ))
  }

  list(source = "missing", hash = NA_character_)
}

.adaptive_phase_a_prepare_context_hash <- function(state,
                                                   set_id,
                                                   requested_source,
                                                   controller,
                                                   prior_status = NA_character_,
                                                   persisted_artifact = NULL,
                                                   import_artifact = NULL) {
  requested_source <- as.character(requested_source %||% NA_character_)
  policy <- as.character(controller$phase_a_import_failure_policy %||% "fail_fast")
  reliability_min <- as.double(controller$phase_a_required_reliability_min %||% 0.80)

  context <- list(
    set_id = as.integer(set_id),
    requested_source = requested_source,
    link_estimation_mode = as.character(controller$link_estimation_mode %||% "transform"),
    phase_a_import_failure_policy = policy,
    phase_a_required_reliability_min = reliability_min,
    phase_a_compatible_model_ids = sort(unique(as.character(
      controller$phase_a_compatible_model_ids %||% "btl_e_b"
    ))),
    phase_a_compatible_config_hashes = sort(unique(as.character(
      controller$phase_a_compatible_config_hashes %||% character()
    ))),
    required_config_hash = .adaptive_phase_a_required_config_hash(state, set_id = set_id)
  )
  if (identical(requested_source, "import")) {
    context$import_artifact_hash <- .adaptive_phase_a_artifact_memo_hash(import_artifact)
  }

  include_run_surface <- identical(requested_source, "run") ||
    identical(policy, "fallback_to_run")
  if (isTRUE(include_run_surface)) {
    within_set_evidence <- .adaptive_phase_a_within_set_evidence_from_state(state, set_id = set_id)
    context$run_surface <- list(
      latest_refit_row = tibble::as_tibble(
        .adaptive_phase_a_latest_refit_row(state, set_id = set_id) %||% tibble::tibble()
      ),
      round_diagnostics = .adaptive_phase_a_round_diagnostics_surface(state),
      n_pairs_committed = .adaptive_phase_a_within_set_pair_count(state, set_id = set_id),
      within_set_evidence_hash = .adaptive_phase_a_within_set_evidence_hash(within_set_evidence),
      summary_surface = .adaptive_phase_a_summary_surface_stamp(state, set_id = set_id)
    )
  }

  .adaptive_phase_a_hash_object(context)
}

.adaptive_phase_a_strip_runtime_prepare_memo <- function(state) {
  out <- state
  phase_a <- out$linking$phase_a %||% NULL
  if (is.list(phase_a)) {
    phase_a$prepare_context_by_set <- NULL
    out$linking$phase_a <- phase_a
  }
  out
}

.adaptive_phase_a_artifact_fit_contract_surface <- function(artifact) {
  artifact_surface <- artifact$fit_config_surface %||% list()
  .adaptive_phase_a_fit_contract_surface(
    judge_param_mode = artifact_surface$judge_param_mode %||%
      artifact$judge_param_mode %||%
      "global_shared",
    model_variant = artifact_surface$model_variant %||%
      artifact$fit_model_id %||%
      artifact$model_variant %||%
      "btl_e_b"
  )
}

.adaptive_phase_a_latest_refit_row <- function(state, set_id) {
  round_log <- tibble::as_tibble(state$round_log %||% tibble::tibble())
  if (nrow(round_log) < 1L) {
    return(NULL)
  }

  if (all(c("phase_scope", "phase_scope_set_id") %in% names(round_log))) {
    scoped <- round_log[
      round_log$phase_scope %in% "phase_a_set" &
        as.integer(round_log$phase_scope_set_id) == as.integer(set_id),
      ,
      drop = FALSE
    ]
    if (nrow(scoped) > 0L) {
      return(scoped[nrow(scoped), , drop = FALSE])
    }
  }

  round_log[nrow(round_log), , drop = FALSE]
}

.adaptive_phase_a_run_stop_passed <- function(artifact, controller) {
  diagnostics <- artifact$diagnostics %||% list()
  diagnostics_pass <- isTRUE(diagnostics$diagnostics_pass %||% FALSE)
  reliability <- .adaptive_phase_a_extract_reliability(artifact)
  reliability_min <- as.double(controller$phase_a_required_reliability_min %||% 0.80)
  n_pairs_committed <- as.integer(artifact$n_pairs_committed %||% 0L)

  isTRUE(diagnostics_pass) &&
    is.finite(reliability) &&
    reliability >= reliability_min &&
    n_pairs_committed > 0L
}

.adaptive_phase_a_extract_set_draws <- function(state, set_id) {
  draws <- state$btl_fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 1L || ncol(draws) < 1L) {
    return(NULL)
  }

  ids <- as.character(state$items$item_id[state$items$set_id == as.integer(set_id)])
  if (length(ids) == 0L) {
    return(NULL)
  }

  if (is.null(colnames(draws))) {
    all_ids <- as.character(state$item_ids)
    if (ncol(draws) != length(all_ids)) {
      return(NULL)
    }
    colnames(draws) <- all_ids
  }
  if (!all(ids %in% colnames(draws))) {
    return(NULL)
  }

  draws <- draws[, ids, drop = FALSE]
  .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")
}

.adaptive_phase_a_empty_within_set_evidence <- function() {
  tibble::tibble(
    pair_id = integer(),
    step_id = integer(),
    A_item = character(),
    B_item = character(),
    y_A = integer()
  )
}

.adaptive_phase_a_within_set_evidence_from_state <- function(state, set_id) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L) {
    return(.adaptive_phase_a_empty_within_set_evidence())
  }
  required <- c("pair_id", "step_id", "A", "B", "Y", "set_i", "set_j")
  if (!all(required %in% names(step_log))) {
    return(.adaptive_phase_a_empty_within_set_evidence())
  }
  rows <- step_log[
    !is.na(step_log$pair_id) &
      as.integer(step_log$set_i) == as.integer(set_id) &
      as.integer(step_log$set_j) == as.integer(set_id),
    ,
    drop = FALSE
  ]
  if (nrow(rows) < 1L) {
    return(.adaptive_phase_a_empty_within_set_evidence())
  }
  ids <- as.character(state$item_ids %||% character())
  A_item <- ids[as.integer(rows$A)]
  B_item <- ids[as.integer(rows$B)]
  y_A <- as.integer(rows$Y)
  keep <- !is.na(A_item) & !is.na(B_item) & y_A %in% c(0L, 1L)
  if (!any(keep)) {
    return(.adaptive_phase_a_empty_within_set_evidence())
  }
  out <- tibble::tibble(
    pair_id = as.integer(rows$pair_id[keep]),
    step_id = as.integer(rows$step_id[keep]),
    A_item = as.character(A_item[keep]),
    B_item = as.character(B_item[keep]),
    y_A = as.integer(y_A[keep])
  )
  out[order(out$step_id, out$pair_id), , drop = FALSE]
}

.adaptive_phase_a_validate_within_set_evidence <- function(evidence,
                                                           state,
                                                           set_id,
                                                           expected_n_pairs = NULL,
                                                           label = "Phase A within-set evidence") {
  evidence <- tibble::as_tibble(evidence %||% .adaptive_phase_a_empty_within_set_evidence())
  required <- c("pair_id", "step_id", "A_item", "B_item", "y_A")
  missing <- setdiff(required, names(evidence))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      label,
      " for set ",
      as.integer(set_id),
      " is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }

  evidence <- evidence[, required, drop = FALSE]
  evidence$pair_id <- as.integer(evidence$pair_id)
  evidence$step_id <- as.integer(evidence$step_id)
  evidence$A_item <- as.character(evidence$A_item)
  evidence$B_item <- as.character(evidence$B_item)
  evidence$y_A <- as.integer(evidence$y_A)

  if (nrow(evidence) > 0L) {
    if (any(!is.finite(evidence$pair_id) | evidence$pair_id < 1L)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must contain positive integer `pair_id` values."
      ))
    }
    if (any(!is.finite(evidence$step_id) | evidence$step_id < 1L)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must contain positive integer `step_id` values."
      ))
    }
    if (anyDuplicated(evidence$pair_id)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must not contain duplicate `pair_id` values."
      ))
    }
    if (anyDuplicated(evidence$step_id)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must not contain duplicate `step_id` values."
      ))
    }
    if (any(is.na(evidence$A_item) | !nzchar(evidence$A_item) |
      is.na(evidence$B_item) | !nzchar(evidence$B_item))) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must contain non-empty `A_item`/`B_item` values."
      ))
    }
    if (any(evidence$y_A %in% c(0L, 1L) == FALSE)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " must encode `y_A` in {0, 1}."
      ))
    }
  }

  set_items <- as.character(
    state$items$item_id[as.integer(state$items$set_id) == as.integer(set_id)]
  )
  if (length(set_items) < 1L) {
    rlang::abort(paste0("No items found for set_id ", as.integer(set_id), "."))
  }
  if (nrow(evidence) > 0L) {
    bad_items <- !(evidence$A_item %in% set_items) | !(evidence$B_item %in% set_items)
    if (any(bad_items)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " contains items outside the set domain."
      ))
    }
  }

  if (!is.null(expected_n_pairs)) {
    expected_n_pairs <- as.integer(expected_n_pairs %||% NA_integer_)
    if (is.finite(expected_n_pairs) && !identical(nrow(evidence), expected_n_pairs)) {
      rlang::abort(paste0(
        label,
        " for set ",
        as.integer(set_id),
        " did not reconcile to `n_pairs_committed`."
      ))
    }
  }

  evidence[order(evidence$step_id, evidence$pair_id), , drop = FALSE]
}

.adaptive_phase_a_within_set_evidence_hash <- function(evidence) {
  .adaptive_phase_a_hash_object(tibble::as_tibble(evidence %||% .adaptive_phase_a_empty_within_set_evidence()))
}

.adaptive_phase_a_artifact_resolve_within_set_evidence <- function(artifact,
                                                                   state,
                                                                   set_id,
                                                                   controller) {
  artifact_has_evidence <- !is.null(artifact$phase_a_within_set_evidence) ||
    !is.null(artifact$within_set_evidence)
  evidence <- artifact$phase_a_within_set_evidence %||% artifact$within_set_evidence %||% NULL
  if (is.null(evidence)) {
    evidence <- .adaptive_phase_a_within_set_evidence_from_state(state, set_id = set_id)
  }
  evidence <- .adaptive_phase_a_validate_within_set_evidence(
    evidence = evidence,
    state = state,
    set_id = set_id,
    expected_n_pairs = NULL,
    label = "Phase A within-set evidence"
  )
  expected_n_pairs <- as.integer(artifact$n_pairs_committed %||% NA_integer_)
  if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint") &&
    !isTRUE(artifact_has_evidence) &&
    (!is.finite(expected_n_pairs) || nrow(evidence) != expected_n_pairs)) {
    rlang::abort(paste0(
      "Phase A artifact evidence-domain availability failure for set ",
      as.integer(set_id),
      ": exact within-set committed-edge history is unavailable for anchored_joint."
    ))
  }
  evidence <- .adaptive_phase_a_validate_within_set_evidence(
    evidence = evidence,
    state = state,
    set_id = set_id,
    expected_n_pairs = expected_n_pairs,
    label = "Phase A within-set evidence"
  )
  evidence
}

.adaptive_anchored_joint_init_state_method_levels <- function() {
  c("artifact_copy_init", "phase_a_only_init_refit", "phase_b_refit")
}

.adaptive_normalize_anchored_joint_init_state_method <- function(method) {
  value <- as.character(method %||% NA_character_)
  if (length(value) != 1L || is.na(value) || !value %in% .adaptive_anchored_joint_init_state_method_levels()) {
    rlang::abort(
      paste0(
        "`anchored_joint_init_state_method` must be one of: ",
        paste(.adaptive_anchored_joint_init_state_method_levels(), collapse = ", "),
        "."
      )
    )
  }
  value
}

.adaptive_anchored_joint_validate_named_numeric <- function(x, ids, field, allow_na = FALSE) {
  ids <- as.character(ids)
  vals <- x %||% NULL
  if (!is.numeric(vals) || is.null(names(vals))) {
    rlang::abort(paste0("Anchored-joint accepted state `", field, "` must be a named numeric vector."))
  }
  vals <- as.double(vals)
  names(vals) <- as.character(names(x))
  if (!setequal(names(vals), ids)) {
    rlang::abort(paste0(
      "Anchored-joint accepted state `",
      field,
      "` item domain mismatch."
    ))
  }
  vals <- vals[ids]
  ok <- is.finite(vals)
  if (isTRUE(allow_na)) {
    ok <- ok | is.na(vals)
  }
  if (any(!ok)) {
    rlang::abort(paste0(
      "Anchored-joint accepted state `",
      field,
      "` must be finite",
      if (isTRUE(allow_na)) " or NA" else "",
      "."
    ))
  }
  vals
}

.adaptive_anchored_joint_new_accepted_state <- function(state,
                                                        hub_id,
                                                        spoke_id,
                                                        theta_hub_fixed,
                                                        theta_spoke_global_mean,
                                                        theta_spoke_global_sd,
                                                        judge_params,
                                                        anchored_joint_init_state_method,
                                                        phase_a_evidence_hash_hub,
                                                        phase_a_evidence_hash_spoke) {
  hub_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(hub_id)])
  spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(spoke_id)])
  if (length(hub_items) < 1L || length(spoke_items) < 1L) {
    rlang::abort("Anchored-joint accepted state requires non-empty hub and spoke item domains.")
  }
  theta_hub_fixed <- .adaptive_anchored_joint_validate_named_numeric(
    theta_hub_fixed,
    ids = hub_items,
    field = "theta_hub_fixed"
  )
  theta_spoke_global_mean <- .adaptive_anchored_joint_validate_named_numeric(
    theta_spoke_global_mean,
    ids = spoke_items,
    field = "theta_spoke_global_mean"
  )
  theta_spoke_global_sd <- .adaptive_anchored_joint_validate_named_numeric(
    theta_spoke_global_sd,
    ids = spoke_items,
    field = "theta_spoke_global_sd",
    allow_na = TRUE
  )
  if (any(theta_spoke_global_sd < 0, na.rm = TRUE)) {
    rlang::abort("Anchored-joint accepted state `theta_spoke_global_sd` must be non-negative.")
  }
  if (!is.list(judge_params)) {
    rlang::abort("Anchored-joint accepted state `judge_params` must be a list.")
  }
  beta <- as.double(judge_params$beta %||% NA_real_)
  epsilon <- as.double(judge_params$epsilon %||% NA_real_)
  if (!is.finite(beta) || !is.finite(epsilon)) {
    rlang::abort("Anchored-joint accepted state requires finite fixed judge parameters.")
  }
  list(
    hub_id = as.integer(hub_id),
    spoke_id = as.integer(spoke_id),
    theta_hub_fixed = theta_hub_fixed,
    theta_spoke_global_mean = theta_spoke_global_mean,
    theta_spoke_global_sd = theta_spoke_global_sd,
    judge_params = list(
      mode = as.character(judge_params$mode %||% NA_character_),
      scope = as.character(judge_params$scope %||% "link"),
      beta = beta,
      epsilon = epsilon,
      cold_start_fallback_used = as.logical(judge_params$cold_start_fallback_used %||% FALSE)
    ),
    anchored_joint_init_state_method = .adaptive_normalize_anchored_joint_init_state_method(
      anchored_joint_init_state_method
    ),
    phase_a_evidence_hash_hub = as.character(phase_a_evidence_hash_hub %||% NA_character_),
    phase_a_evidence_hash_spoke = as.character(phase_a_evidence_hash_spoke %||% NA_character_)
  )
}

.adaptive_anchored_joint_artifact_copy_init <- function(state, spoke_id, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    rlang::abort("Anchored-joint artifact-copy initialization requires `link_estimation_mode = anchored_joint`.")
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  phase_a <- state$linking$phase_a %||% list()
  artifacts <- phase_a$artifacts %||% list()
  hub_artifact <- artifacts[[as.character(hub_id)]] %||% NULL
  spoke_artifact <- artifacts[[as.character(spoke_id)]] %||% NULL
  if (!is.list(hub_artifact) || !is.list(spoke_artifact)) {
    rlang::abort("Anchored-joint artifact-copy initialization requires hub and spoke Phase A artifacts.")
  }

  hub_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
    artifact = hub_artifact,
    state = state,
    set_id = hub_id,
    controller = controller
  )
  spoke_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
    artifact = spoke_artifact,
    state = state,
    set_id = as.integer(spoke_id),
    controller = controller
  )
  judge_params <- .adaptive_link_judge_params(
    state = state,
    controller = controller,
    scope = "link",
    allow_cold_start_fallback = TRUE,
    expected_link_params = FALSE
  )
  .adaptive_anchored_joint_new_accepted_state(
    state = state,
    hub_id = hub_id,
    spoke_id = as.integer(spoke_id),
    theta_hub_fixed = .adaptive_link_phase_a_theta_map(state, hub_id, "theta_raw_mean"),
    theta_spoke_global_mean = .adaptive_link_phase_a_theta_map(state, as.integer(spoke_id), "theta_raw_mean"),
    theta_spoke_global_sd = .adaptive_phase_a_artifact_item_field_map(
      state = state,
      set_id = as.integer(spoke_id),
      field = "theta_raw_sd"
    ),
    judge_params = judge_params,
    anchored_joint_init_state_method = "artifact_copy_init",
    phase_a_evidence_hash_hub = .adaptive_phase_a_hash_object(hub_evidence),
    phase_a_evidence_hash_spoke = .adaptive_phase_a_hash_object(spoke_evidence)
  )
}

.adaptive_anchored_joint_validate_current_state <- function(state_obj, state, spoke_id, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  hub_id <- as.integer(controller$hub_id %||% 1L)
  if (!is.list(state_obj)) {
    rlang::abort("Anchored-joint accepted state must be a list.")
  }
  if (!identical(as.integer(state_obj$hub_id %||% NA_integer_), hub_id) ||
    !identical(as.integer(state_obj$spoke_id %||% NA_integer_), as.integer(spoke_id))) {
    rlang::abort("Anchored-joint accepted state spoke/hub identifiers do not match current state.")
  }
  hub_artifact <- (state$linking$phase_a$artifacts %||% list())[[as.character(hub_id)]] %||% NULL
  spoke_artifact <- (state$linking$phase_a$artifacts %||% list())[[as.character(spoke_id)]] %||% NULL
  if (!is.list(hub_artifact) || !is.list(spoke_artifact)) {
    rlang::abort("Anchored-joint accepted state validation requires current hub and spoke Phase A artifacts.")
  }
  hub_evidence_hash_current <- .adaptive_phase_a_hash_object(
    .adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact = hub_artifact,
      state = state,
      set_id = hub_id,
      controller = controller
    )
  )
  spoke_evidence_hash_current <- .adaptive_phase_a_hash_object(
    .adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact = spoke_artifact,
      state = state,
      set_id = as.integer(spoke_id),
      controller = controller
    )
  )
  stored_hub_hash <- as.character(state_obj$phase_a_evidence_hash_hub %||% NA_character_)
  stored_spoke_hash <- as.character(state_obj$phase_a_evidence_hash_spoke %||% NA_character_)
  if (!is.na(stored_hub_hash) && nzchar(stored_hub_hash) &&
    !identical(stored_hub_hash, hub_evidence_hash_current)) {
    rlang::abort("Anchored-joint accepted state hub evidence hash does not match the current Phase A artifact.")
  }
  if (!is.na(stored_spoke_hash) && nzchar(stored_spoke_hash) &&
    !identical(stored_spoke_hash, spoke_evidence_hash_current)) {
    rlang::abort("Anchored-joint accepted state spoke evidence hash does not match the current Phase A artifact.")
  }
  .adaptive_anchored_joint_new_accepted_state(
    state = state,
    hub_id = hub_id,
    spoke_id = as.integer(spoke_id),
    theta_hub_fixed = state_obj$theta_hub_fixed,
    theta_spoke_global_mean = state_obj$theta_spoke_global_mean,
    theta_spoke_global_sd = state_obj$theta_spoke_global_sd,
    judge_params = state_obj$judge_params,
    anchored_joint_init_state_method = state_obj$anchored_joint_init_state_method,
    phase_a_evidence_hash_hub = hub_evidence_hash_current,
    phase_a_evidence_hash_spoke = spoke_evidence_hash_current
  )
}

.adaptive_anchored_joint_sync_scaffolding <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  out$linking <- out$linking %||% list()
  anchored <- out$linking$anchored_joint %||% .adaptive_anchored_joint_empty_state()
  if (!identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    out$linking$anchored_joint <- anchored
    return(out)
  }

  phase_a <- out$linking$phase_a %||% list()
  artifacts <- phase_a$artifacts %||% list()
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- as.integer(
    phase_a$ready_spokes %||% .adaptive_phase_a_ready_spokes(out, controller = controller)
  )
  spoke_ids <- sort(unique(spoke_ids[!is.na(spoke_ids)]))
  if (length(spoke_ids) < 1L) {
    out$linking$anchored_joint <- anchored
    return(out)
  }
  if (is.null(artifacts[[as.character(hub_id)]])) {
    rlang::abort("Anchored-joint initialization requires a hub Phase A artifact.")
  }

  accepted_map <- anchored$accepted_state_by_spoke %||% list()
  fisher_map <- anchored$fisher_t0_by_spoke %||% list()
  for (spoke_id in spoke_ids) {
    key <- as.character(spoke_id)
    if (is.null(artifacts[[key]])) {
      rlang::abort(paste0(
        "Anchored-joint initialization requires a Phase A artifact for spoke set_id=",
        as.integer(spoke_id),
        "."
      ))
    }
    hub_artifact <- artifacts[[as.character(hub_id)]]
    spoke_artifact <- artifacts[[key]]
    hub_artifact$phase_a_within_set_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact = hub_artifact,
      state = out,
      set_id = hub_id,
      controller = controller
    )
    hub_artifact$phase_a_within_set_evidence_hash <- .adaptive_phase_a_hash_object(
      hub_artifact$phase_a_within_set_evidence
    )
    hub_artifact$phase_a_within_set_evidence_source <- as.character(
      hub_artifact$phase_a_within_set_evidence_source %||% "canonical_committed_step_log"
    )
    spoke_artifact$phase_a_within_set_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact = spoke_artifact,
      state = out,
      set_id = as.integer(spoke_id),
      controller = controller
    )
    spoke_artifact$phase_a_within_set_evidence_hash <- .adaptive_phase_a_hash_object(
      spoke_artifact$phase_a_within_set_evidence
    )
    spoke_artifact$phase_a_within_set_evidence_source <- as.character(
      spoke_artifact$phase_a_within_set_evidence_source %||% "canonical_committed_step_log"
    )
    artifacts[[as.character(hub_id)]] <- hub_artifact
    artifacts[[key]] <- spoke_artifact

    accepted_state <- accepted_map[[key]] %||% NULL
    if (is.null(accepted_state)) {
      accepted_state <- .adaptive_anchored_joint_artifact_copy_init(
        out,
        spoke_id = as.integer(spoke_id),
        controller = controller
      )
    } else {
      accepted_state <- tryCatch(
        {
          .adaptive_anchored_joint_validate_current_state(
            state_obj = accepted_state,
            state = out,
            spoke_id = as.integer(spoke_id),
            controller = controller
          )
        },
        error = function(e) {
          if (isTRUE(.adaptive_is_resumed_session(out))) {
            rlang::abort(paste0(
              "Adaptive resume anchored-joint invariant failed for spoke_id=",
              as.integer(spoke_id),
              ": persisted accepted-state scaffolding could not be preserved: ",
              conditionMessage(e),
              "."
            ))
          }
          .adaptive_anchored_joint_artifact_copy_init(
            out,
            spoke_id = as.integer(spoke_id),
            controller = controller
          )
        }
      )
    }
    accepted_map[[key]] <- accepted_state
    prior_fisher <- fisher_map[[key]] %||% list()
    expected_dim <- as.integer(length(accepted_state$theta_spoke_global_mean))
    prior_dim <- as.integer(prior_fisher$free_block_dim %||% NA_integer_)
    if (is.finite(prior_dim) && !identical(prior_dim, expected_dim)) {
      if (isTRUE(.adaptive_is_resumed_session(out))) {
        rlang::abort(paste0(
          "Adaptive resume anchored-joint invariant failed for spoke_id=",
          as.integer(spoke_id),
          ": persisted fisher free-block dimension does not match the accepted-state domain."
        ))
      }
      prior_fisher <- list()
    }
    prior_pairs <- as.integer(prior_fisher$n_link_active_pairs %||% 0L)
    if (!is.finite(prior_pairs) || prior_pairs < 0L) {
      if (isTRUE(.adaptive_is_resumed_session(out))) {
        rlang::abort(paste0(
          "Adaptive resume anchored-joint invariant failed for spoke_id=",
          as.integer(spoke_id),
          ": persisted fisher active-edge count must be a non-negative integer."
        ))
      }
      prior_pairs <- 0L
    }
    prior_init_method <- prior_fisher$anchored_joint_init_state_method %||%
      accepted_state$anchored_joint_init_state_method
    prior_init_method <- tryCatch(
      .adaptive_normalize_anchored_joint_init_state_method(prior_init_method),
      error = function(e) {
        if (isTRUE(.adaptive_is_resumed_session(out))) {
          rlang::abort(paste0(
            "Adaptive resume anchored-joint invariant failed for spoke_id=",
            as.integer(spoke_id),
            ": persisted fisher init-state method is invalid."
          ))
        }
        .adaptive_normalize_anchored_joint_init_state_method(
          accepted_state$anchored_joint_init_state_method
        )
      }
    )
    fisher_map[[key]] <- list(
      free_block_dim = expected_dim,
      I_s_t0_zero = as.logical(prior_fisher$I_s_t0_zero %||% TRUE),
      n_link_active_pairs = as.integer(prior_pairs),
      anchored_joint_init_state_method = as.character(prior_init_method)
    )
  }

  anchored$accepted_state_by_spoke <- accepted_map
  anchored$fisher_t0_by_spoke <- fisher_map
  out$linking$anchored_joint <- anchored
  out$linking$phase_a$artifacts <- artifacts
  out
}

.adaptive_phase_a_build_artifact <- function(state, set_id) {
  set_id <- as.integer(set_id)
  items_set <- state$items[state$items$set_id == set_id, , drop = FALSE]
  if (nrow(items_set) == 0L) {
    rlang::abort(paste0("No items found for set_id ", set_id, "."))
  }

  ids <- as.character(items_set$item_id)
  global_ids <- as.character(items_set$global_item_id)

  latest_item_log <- NULL
  if (is.list(state$item_log) && length(state$item_log) > 0L) {
    latest_item_log <- tibble::as_tibble(state$item_log[[length(state$item_log)]])
  }

  theta_mean <- NULL
  theta_sd <- NULL
  rank_mu_raw <- NULL

  if (!is.null(latest_item_log) && nrow(latest_item_log) > 0L &&
    all(c("item_id", "theta_raw_eap", "theta_raw_sd") %in% names(latest_item_log))) {
    idx <- match(ids, as.character(latest_item_log$item_id))
    if (all(!is.na(idx))) {
      theta_mean <- as.double(latest_item_log$theta_raw_eap[idx])
      theta_sd <- as.double(latest_item_log$theta_raw_sd[idx])
      if ("rank_raw" %in% names(latest_item_log)) {
        rank_mu_raw <- as.double(latest_item_log$rank_raw[idx])
      }
    }
  }

  # Ignore stale/non-finite summaries and fall back to set-scoped draws.
  if (!is.null(theta_mean) && !is.null(theta_sd) &&
    (any(!is.finite(theta_mean)) || any(!is.finite(theta_sd)) || any(theta_sd < 0))) {
    theta_mean <- NULL
    theta_sd <- NULL
    rank_mu_raw <- NULL
  }

  draws <- .adaptive_phase_a_extract_set_draws(state, set_id = set_id)
  if ((is.null(theta_mean) || is.null(theta_sd)) && !is.null(draws)) {
    theta_mean <- as.double(colMeans(draws))
    theta_sd <- as.double(apply(draws, 2, stats::sd))
  }

  if (is.null(theta_mean) || is.null(theta_sd)) {
    rlang::abort(paste0(
      "Within-set summaries are unavailable for set_id ",
      set_id,
      "; run additional within-set comparisons/refits before linking."
    ))
  }

  if (is.null(rank_mu_raw)) {
    rank_mu_raw <- as.double(rank(-theta_mean, ties.method = "average"))
  }

  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  history <- .adaptive_history_tbl(state)
  n_pairs_committed <- 0L
  if (nrow(history) > 0L) {
    a_set <- set_map[as.character(history$A_id)]
    b_set <- set_map[as.character(history$B_id)]
    n_pairs_committed <- as.integer(sum(a_set == set_id & b_set == set_id, na.rm = TRUE))
  }

  controller <- .adaptive_controller_resolve(state)
  round_log <- tibble::as_tibble(state$round_log %||% tibble::tibble())
  diagnostics_pass <- if (nrow(round_log) > 0L && "diagnostics_pass" %in% names(round_log)) {
    as.logical(round_log$diagnostics_pass[[nrow(round_log)]])
  } else {
    NA
  }
  ts_rank <- if (nrow(round_log) > 0L && "ts_btl_rank_spearman" %in% names(round_log)) {
    as.double(round_log$ts_btl_rank_spearman[[nrow(round_log)]])
  } else {
    NA_real_
  }
  reliability <- if (!is.null(draws)) {
    as.double(compute_reliability_EAP(draws))
  } else {
    NA_real_
  }

  fit <- state$btl_fit %||% list()
  fit_model_id <- as.character(fit$model_variant %||% "btl_e_b")
  config_surface <- .adaptive_phase_a_required_config_surface(state, set_id = set_id)
  fit_config_hash <- .adaptive_phase_a_required_config_hash(state, set_id = set_id)
  refit_row <- .adaptive_phase_a_latest_refit_row(state, set_id = set_id)
  artifact_refit_id <- if (!is.null(refit_row) && "refit_id" %in% names(refit_row)) {
    as.integer(refit_row$refit_id[[1L]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  artifact_round_id <- if (!is.null(refit_row) && "round_id_at_refit" %in% names(refit_row)) {
    as.integer(refit_row$round_id_at_refit[[1L]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  artifact_step_id <- if (!is.null(refit_row) && "step_id_at_refit" %in% names(refit_row)) {
    as.integer(refit_row$step_id_at_refit[[1L]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  artifact_phase_scope <- if (!is.null(refit_row) && "phase_scope" %in% names(refit_row)) {
    as.character(refit_row$phase_scope[[1L]] %||% NA_character_)
  } else {
    NA_character_
  }
  artifact_phase_scope_set_id <- if (!is.null(refit_row) && "phase_scope_set_id" %in% names(refit_row)) {
    as.integer(refit_row$phase_scope_set_id[[1L]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  within_set_evidence <- .adaptive_phase_a_within_set_evidence_from_state(state, set_id = set_id)

  list(
    set_id = set_id,
    fit_model_id = fit_model_id,
    fit_config_surface = config_surface,
    fit_config_hash = fit_config_hash,
    n_items = as.integer(length(ids)),
    n_pairs_committed = as.integer(n_pairs_committed),
    refit_id = artifact_refit_id,
    round_id_at_refit = artifact_round_id,
    step_id_at_refit = artifact_step_id,
    phase_scope = artifact_phase_scope,
    phase_scope_set_id = artifact_phase_scope_set_id,
    items = tibble::tibble(
      item_id = as.character(ids),
      global_item_id = as.character(global_ids),
      theta_raw_mean = as.double(theta_mean),
      theta_raw_sd = as.double(theta_sd),
      rank_mu_raw = as.double(rank_mu_raw)
    ),
    diagnostics = list(
      reliability_EAP_within = reliability,
      ts_btl_rank_spearman = ts_rank,
      diagnostics_pass = diagnostics_pass
    ),
    phase_a_within_set_evidence = within_set_evidence,
    phase_a_within_set_evidence_hash = .adaptive_phase_a_hash_object(within_set_evidence),
    phase_a_within_set_evidence_source = "canonical_committed_step_log",
    posterior_draws = draws,
    judge_param_mode = as.character(controller$judge_param_mode %||% NA_character_),
    created_at = Sys.time()
  )
}

.adaptive_phase_a_read_import_artifact <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    if (!file.exists(x)) {
      rlang::abort(paste0("Phase A artifact path does not exist: ", x))
    }
    return(readRDS(x))
  }
  if (is.list(x)) {
    return(x)
  }
  rlang::abort("Imported Phase A artifact entries must be a list or .rds path.")
}

.adaptive_phase_a_extract_reliability <- function(artifact) {
  diagnostics <- artifact$diagnostics %||% list()
  as.double(diagnostics$reliability_EAP_within %||% artifact$reliability_EAP_within %||% NA_real_)
}

.adaptive_phase_a_validate_imported_artifact <- function(artifact, state, set_id, controller) {
  if (!is.list(artifact)) {
    rlang::abort("Imported Phase A artifact must be a list.")
  }

  set_id <- as.integer(set_id)
  artifact_set <- as.integer(artifact$set_id %||% NA_integer_)
  if (is.na(artifact_set) || !identical(artifact_set, set_id)) {
    rlang::abort(paste0("Phase A artifact set_id mismatch for set ", set_id, "."))
  }

  fit_model_id <- as.character(artifact$fit_model_id %||% NA_character_)
  allowed_model_ids <- as.character(controller$phase_a_compatible_model_ids %||% "btl_e_b")
  if (!is.character(fit_model_id) || length(fit_model_id) != 1L || is.na(fit_model_id) ||
    !fit_model_id %in% allowed_model_ids) {
    rlang::abort(paste0(
      "Phase A artifact likelihood/model incompatibility for set ",
      set_id,
      "."
    ))
  }

  fit_config_hash <- as.character(artifact$fit_config_hash %||% NA_character_)
  required_hash <- .adaptive_phase_a_required_config_hash(state, set_id = set_id)
  required_surface <- .adaptive_phase_a_required_config_surface(state, set_id = set_id)
  artifact_surface <- .adaptive_phase_a_artifact_fit_contract_surface(artifact)
  artifact_contract_hash <- .adaptive_phase_a_hash_object(artifact_surface)
  compatible_hashes <- as.character(controller$phase_a_compatible_config_hashes %||% character())
  if (is.na(fit_config_hash) || fit_config_hash == "") {
    rlang::abort(paste0("Phase A artifact missing fit_config_hash for set ", set_id, "."))
  }
  if (!identical(fit_config_hash, required_hash) &&
    !identical(artifact_contract_hash, required_hash) &&
    !fit_config_hash %in% compatible_hashes &&
    !artifact_contract_hash %in% compatible_hashes) {
    mismatch_fields <- character()
    if (is.list(artifact_surface)) {
      common_fields <- intersect(names(required_surface), names(artifact_surface))
      mismatch_fields <- common_fields[vapply(common_fields, function(field) {
        !identical(artifact_surface[[field]], required_surface[[field]])
      }, logical(1L))]
    }
    mismatch_msg <- if (length(mismatch_fields) > 0L) {
      paste0(" Incompatible settings: ", paste(mismatch_fields, collapse = ", "), ".")
    } else {
      ""
    }
    rlang::abort(paste0(
      "Phase A artifact within-set fit incompatibility for set ",
      set_id,
      ": artifact hash `",
      fit_config_hash,
      "` did not match required hash `",
      required_hash,
      "`, reconstructed fit-contract hash `",
      artifact_contract_hash,
      "` did not match the current within-set fit contract, and neither hash was found in ",
      "`adaptive_config$phase_a_compatible_config_hashes`.",
      mismatch_msg
    ))
  }

  items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
  required_cols <- c("global_item_id", "theta_raw_mean", "theta_raw_sd", "rank_mu_raw")
  missing_cols <- setdiff(required_cols, names(items_tbl))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0(
      "Phase A artifact completeness failure for set ",
      set_id,
      ": missing ",
      paste(missing_cols, collapse = ", "),
      "."
    ))
  }

  n_items <- artifact$n_items %||% NA_integer_
  if (!.adaptive_is_integerish(n_items) || length(n_items) != 1L || is.na(n_items)) {
    rlang::abort(paste0("Phase A artifact completeness failure for set ", set_id, ": missing `n_items`."))
  }
  n_items <- as.integer(n_items)
  if (n_items < 1L) {
    rlang::abort(paste0("Phase A artifact field validation failure for set ", set_id, ": `n_items` must be >= 1."))
  }
  n_pairs_committed <- artifact$n_pairs_committed %||% NA_integer_
  if (!.adaptive_is_integerish(n_pairs_committed) || length(n_pairs_committed) != 1L || is.na(n_pairs_committed)) {
    rlang::abort(paste0(
      "Phase A artifact completeness failure for set ",
      set_id,
      ": missing `n_pairs_committed`."
    ))
  }
  n_pairs_committed <- as.integer(n_pairs_committed)
  if (n_pairs_committed < 0L) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `n_pairs_committed` must be >= 0."
    ))
  }

  if (!is.character(items_tbl$global_item_id) ||
    any(is.na(items_tbl$global_item_id) | items_tbl$global_item_id == "")) {
    rlang::abort(paste0("Phase A artifact item identity failure for set ", set_id, "."))
  }
  if (anyDuplicated(items_tbl$global_item_id)) {
    rlang::abort(paste0("Phase A artifact item identity duplicates for set ", set_id, "."))
  }

  state_items <- tibble::as_tibble(state$items)
  state_set_items <- state_items[state_items$set_id == set_id, c("item_id", "global_item_id"), drop = FALSE]
  expected_global <- as.character(state_set_items$global_item_id)
  expected_n_items <- as.integer(length(expected_global))
  if (!identical(n_items, expected_n_items) || !identical(nrow(items_tbl), expected_n_items)) {
    rlang::abort(paste0(
      "Phase A artifact completeness failure for set ",
      set_id,
      ": item-count metadata mismatch."
    ))
  }
  if (!setequal(expected_global, as.character(items_tbl$global_item_id))) {
    rlang::abort(paste0("Phase A artifact global_item_id mapping mismatch for set ", set_id, "."))
  }

  by_global <- match(expected_global, as.character(items_tbl$global_item_id))
  theta_mean <- as.double(items_tbl$theta_raw_mean[by_global])
  theta_sd <- as.double(items_tbl$theta_raw_sd[by_global])
  rank_mu_raw <- as.double(items_tbl$rank_mu_raw[by_global])
  if (any(is.na(theta_mean)) || any(is.na(theta_sd)) || any(is.na(rank_mu_raw))) {
    rlang::abort(paste0("Phase A artifact completeness failure for set ", set_id, "."))
  }
  if (any(!is.finite(theta_mean))) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `theta_raw_mean` must be finite for all items."
    ))
  }
  if (any(!is.finite(theta_sd))) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `theta_raw_sd` must be finite for all items."
    ))
  }
  if (any(theta_sd < 0)) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `theta_raw_sd` must be non-negative for all items."
    ))
  }
  if (any(!is.finite(rank_mu_raw))) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `rank_mu_raw` must be finite for all items."
    ))
  }

  if ("item_id" %in% names(items_tbl)) {
    imported_item_ids <- as.character(items_tbl$item_id[by_global])
    if (any(is.na(imported_item_ids)) || !identical(imported_item_ids, as.character(state_set_items$item_id))) {
      rlang::abort(paste0("Phase A artifact item_id mapping mismatch for set ", set_id, "."))
    }
  }

  diagnostics <- artifact$diagnostics %||% list()
  if (!is.list(diagnostics) || !"diagnostics_pass" %in% names(diagnostics)) {
    rlang::abort(paste0(
      "Phase A artifact completeness failure for set ",
      set_id,
      ": missing diagnostics metadata."
    ))
  }
  diagnostics_pass <- diagnostics$diagnostics_pass
  if (!is.logical(diagnostics_pass) || length(diagnostics_pass) != 1L) {
    rlang::abort(paste0(
      "Phase A artifact field validation failure for set ",
      set_id,
      ": `diagnostics$diagnostics_pass` must be TRUE/FALSE/NA."
    ))
  }

  reliability <- .adaptive_phase_a_extract_reliability(artifact)
  reliability_min <- as.double(controller$phase_a_required_reliability_min %||% 0.80)
  quality_gate_accepted <- isTRUE(artifact$quality_gate_accepted %||% FALSE)
  if ((!is.finite(reliability) || is.na(reliability)) && !quality_gate_accepted) {
    rlang::abort(paste0("Phase A artifact missing reliability_EAP_within for set ", set_id, "."))
  }
  if (is.finite(reliability) && reliability < reliability_min && !quality_gate_accepted) {
    rlang::abort(paste0(
      "Phase A artifact reliability gate failed for set ",
      set_id,
      ": reliability_EAP_within below threshold."
    ))
  }

  if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    .adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact = artifact,
      state = state,
      set_id = set_id,
      controller = controller
    )
  }

  invisible(artifact)
}

.adaptive_phase_a_collect_import_map <- function(controller) {
  raw <- controller$phase_a_artifacts %||% list()
  if (is.null(raw)) {
    raw <- list()
  }
  if (!is.list(raw)) {
    rlang::abort("`adaptive_config$phase_a_artifacts` must be a named list.")
  }

  out <- list()
  nms <- names(raw)
  if (is.null(nms)) {
    nms <- rep("", length(raw))
  }
  for (idx in seq_along(raw)) {
    entry <- .adaptive_phase_a_read_import_artifact(raw[[idx]])
    set_key <- nms[[idx]]
    if (is.na(set_key) || set_key == "") {
      set_key <- as.character(as.integer(entry$set_id %||% NA_integer_))
    }
    set_id <- suppressWarnings(as.integer(set_key))
    if (is.na(set_id)) {
      rlang::abort("Unable to resolve set_id for an imported Phase A artifact.")
    }
    out[[as.character(set_id)]] <- entry
  }

  out
}

.adaptive_phase_a_resolve_set_sources <- function(controller, set_ids, import_map) {
  set_ids <- as.integer(sort(unique(set_ids)))
  mode <- as.character(controller$phase_a_mode %||% "run")
  source <- stats::setNames(rep("run", length(set_ids)), as.character(set_ids))

  if (identical(mode, "import")) {
    source[] <- "import"
  }

  if (identical(mode, "mixed")) {
    import_sets <- intersect(names(source), names(import_map))
    source[import_sets] <- "import"
  }

  explicit <- controller$phase_a_set_source %||% NULL
  if (!is.null(explicit)) {
    if (length(explicit) == 0L) {
      return(source)
    }
    if (!is.character(explicit) || is.null(names(explicit)) || any(names(explicit) == "")) {
      rlang::abort("`adaptive_config$phase_a_set_source` must be a named character vector.")
    }
    bad <- setdiff(unique(as.character(explicit)), c("run", "import"))
    if (length(bad) > 0L) {
      rlang::abort("`adaptive_config$phase_a_set_source` values must be `run` or `import`.")
    }
    overlap <- intersect(names(source), names(explicit))
    source[overlap] <- explicit[overlap]
  }

  source
}

.adaptive_phase_a_prepare <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  set_ids <- as.integer(sort(unique(out$items$set_id)))
  persisted_status_tbl <- tibble::as_tibble(out$linking$phase_a$set_status %||% tibble::tibble())
  prior_phase_a <- out$linking$phase_a %||% list()
  prior_prepare_context_by_set <- prior_phase_a$prepare_context_by_set %||% list()
  prior_stop_pass_map <- prior_phase_a$set_stop_pass_by_set %||% list()
  prior_warm_start_scope_set <- as.integer(prior_phase_a$warm_start_scope_set %||% NA_integer_)
  status_cols <- c("set_id", "source", "status", "validation_message")
  if (!all(status_cols %in% names(persisted_status_tbl))) {
    persisted_status_tbl <- tibble::tibble()
  }
  persisted_map <- list()
  persisted_raw <- out$linking$phase_a$artifacts %||% list()
  if (is.list(persisted_raw) && length(persisted_raw) > 0L) {
    for (nm in names(persisted_raw)) {
      art <- persisted_raw[[nm]]
      set_id <- as.integer(art$set_id %||% suppressWarnings(as.integer(nm)))
      if (!is.na(set_id)) {
        persisted_map[[as.character(set_id)]] <- art
      }
    }
  }

  import_map <- .adaptive_phase_a_collect_import_map(controller)
  for (set_key in names(persisted_map)) {
    if (is.null(import_map[[set_key]])) {
      import_map[[set_key]] <- persisted_map[[set_key]]
    }
  }
  sources <- .adaptive_phase_a_resolve_set_sources(controller, set_ids = set_ids, import_map = import_map)
  policy <- as.character(controller$phase_a_import_failure_policy %||% "fail_fast")

  statuses <- .adaptive_phase_a_empty_state(set_ids = set_ids)
  artifacts <- list()
  set_stop_pass_map <- list()
  prepare_context_by_set <- list()

  for (idx in seq_along(set_ids)) {
    set_id <- as.integer(set_ids[[idx]])
    set_key <- as.character(set_id)
    requested_source <- as.character(sources[[set_key]] %||% "run")
    source <- requested_source
    persisted_row <- persisted_status_tbl[persisted_status_tbl$set_id == set_id, , drop = FALSE]
    prior_status <- if (nrow(persisted_row) > 0L) {
      as.character(persisted_row$status[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }

    status <- "pending_finalization"
    message <- NA_character_
    persisted <- persisted_map[[set_key]] %||% NULL
    import_artifact <- import_map[[set_key]] %||% NULL
    current_context_hash <- .adaptive_phase_a_prepare_context_hash(
      state = out,
      set_id = set_id,
      requested_source = requested_source,
      controller = controller,
      prior_status = prior_status,
      persisted_artifact = persisted,
      import_artifact = import_artifact
    )
    prepare_context_by_set[[set_key]] <- current_context_hash

    can_reuse <- nrow(persisted_row) > 0L &&
      identical(as.character(prior_prepare_context_by_set[[set_key]] %||% NA_character_), current_context_hash)
    if (isTRUE(can_reuse)) {
      reused_source <- as.character(persisted_row$source[[1L]] %||% source)
      statuses$source[[idx]] <- reused_source
      statuses$status[[idx]] <- as.character(persisted_row$status[[1L]] %||% NA_character_)
      statuses$validation_message[[idx]] <- as.character(
        persisted_row$validation_message[[1L]] %||% NA_character_
      )
      if (!is.null(persisted)) {
        artifacts[[set_key]] <- persisted
      }
      if (!is.null(prior_stop_pass_map[[set_key]])) {
        set_stop_pass_map[[set_key]] <- isTRUE(prior_stop_pass_map[[set_key]])
      } else {
        set_stop_pass_map[[set_key]] <- isTRUE(.adaptive_phase_a_set_stop_passed(
          artifact = artifacts[[set_key]] %||% NULL,
          source = reused_source,
          controller = controller
        ))
      }
      next
    }

    if (!is.null(persisted)) {
      persisted_ok <- tryCatch(
        {
          normalized <- .adaptive_phase_a_validate_imported_artifact(
            persisted,
            out,
            set_id = set_id,
            controller = controller
          )
          if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
            normalized$phase_a_within_set_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
              artifact = normalized,
              state = out,
              set_id = set_id,
              controller = controller
            )
            normalized$phase_a_within_set_evidence_hash <- .adaptive_phase_a_hash_object(
              normalized$phase_a_within_set_evidence
            )
            normalized$phase_a_within_set_evidence_source <- as.character(
              normalized$phase_a_within_set_evidence_source %||% "canonical_committed_step_log"
            )
          }
          artifacts[[set_key]] <<- normalized
          TRUE
        },
        error = function(e) {
          message <<- paste0("persisted_invalid: ", conditionMessage(e))
          FALSE
        }
      )
      if (isTRUE(persisted_ok)) {
        if (identical(source, "run") && identical(prior_status, "pending_finalization")) {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        } else {
          status <- "ready"
          message <- "persisted"
        }
      }
    }

    if (identical(source, "import") && !identical(status, "ready")) {
      artifact <- import_map[[set_key]] %||% NULL
      if (is.null(artifact)) {
        status <- "failed"
        message <- "configured for import but no artifact was provided"
      } else {
        validated <- tryCatch(
          {
            normalized <- .adaptive_phase_a_validate_imported_artifact(
              artifact,
              out,
              set_id = set_id,
              controller = controller
            )
            if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
              normalized$phase_a_within_set_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
                artifact = normalized,
                state = out,
                set_id = set_id,
                controller = controller
              )
              normalized$phase_a_within_set_evidence_hash <- .adaptive_phase_a_hash_object(
                normalized$phase_a_within_set_evidence
              )
              normalized$phase_a_within_set_evidence_source <- as.character(
                normalized$phase_a_within_set_evidence_source %||% "canonical_committed_step_log"
              )
            }
            artifact <<- normalized
            TRUE
          },
          error = function(e) {
            message <<- conditionMessage(e)
            FALSE
          }
        )

        if (isTRUE(validated)) {
          artifacts[[set_key]] <- artifact
          status <- "ready"
          message <- "imported"
        } else if (identical(policy, "fallback_to_run")) {
          source <- "run"
          message <- paste0("import_failed_fallback_to_run: ", message)
        } else {
          status <- "failed"
        }
      }
    }

    if (identical(source, "run")) {
      built <- tryCatch(
        {
          .adaptive_phase_a_build_artifact(out, set_id = set_id)
        },
        error = function(e) {
          message <<- conditionMessage(e)
          NULL
        }
      )

      if (is.null(built)) {
        if (identical(status, "ready")) {
          message <- message %||% "persisted"
        } else if (is.character(message) &&
          grepl("Within-set summaries are unavailable", message, fixed = TRUE)) {
          status <- "pending_finalization"
          message <- "pending_finalization: awaiting_within_set_finalization"
        } else {
          status <- "failed"
        }
      } else {
        built_refit_id <- as.integer(built$refit_id %||% NA_integer_)
        persisted_refit_id <- as.integer(persisted$refit_id %||% NA_integer_)
        built_stop_pass <- isTRUE(.adaptive_phase_a_run_stop_passed(built, controller = controller))
        promote_ready <- built_stop_pass && (
          is.null(artifacts[[set_key]]) ||
            !is.finite(persisted_refit_id) ||
            (is.finite(built_refit_id) && built_refit_id >= persisted_refit_id)
        )
        store_built <- is.null(artifacts[[set_key]]) ||
          !identical(status, "ready") ||
          isTRUE(promote_ready)
        if (isTRUE(store_built)) {
          artifacts[[set_key]] <- built
        }
        prior_pairs <- as.integer(persisted$n_pairs_committed %||% NA_integer_)
        built_pairs <- as.integer(built$n_pairs_committed %||% NA_integer_)
        hold_pending <- identical(prior_status, "pending_finalization") &&
          is.finite(prior_pairs) &&
          is.finite(built_pairs) &&
          built_pairs <= prior_pairs
        if (isTRUE(hold_pending)) {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        } else if (isTRUE(promote_ready)) {
          status <- "ready"
          message <- if (is.finite(built_refit_id)) {
            paste0("built_in_run_refit_", built_refit_id)
          } else {
            "built_in_run"
          }
        } else if (identical(status, "ready")) {
          message <- message %||% "persisted"
        } else {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        }
      }
    }

    statuses$source[[idx]] <- source
    statuses$status[[idx]] <- status
    statuses$validation_message[[idx]] <- message
    set_stop_pass_map[[set_key]] <- isTRUE(.adaptive_phase_a_set_stop_passed(
      artifact = artifacts[[set_key]] %||% NULL,
      source = source,
      controller = controller
    ))
  }

  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  ready_for_phase_b <- isTRUE(all(statuses$status == "ready"))
  required_sets <- .adaptive_phase_a_required_sets(out, controller = controller)
  strict_ready_for_phase_b <- length(required_sets) > 0L &&
    all(vapply(as.character(required_sets), function(key) isTRUE(set_stop_pass_map[[key]]), logical(1L)))
  ready_spokes <- integer()
  active_phase_a_set <- NA_integer_
  pending_run <- integer()
  if (isTRUE(is_link_mode)) {
    status_tbl <- tibble::as_tibble(statuses)
    hub_id <- as.integer(controller$hub_id %||% 1L)
    ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
    spokes <- setdiff(unique(as.integer(out$items$set_id)), hub_id)
    ready_spokes <- if (hub_id %in% ready_sets) {
      as.integer(sort(intersect(spokes, ready_sets)))
    } else {
      integer()
    }
    pending_run <- as.integer(status_tbl$set_id[status_tbl$source == "run" & status_tbl$status != "ready"])
    pending_run <- pending_run[!is.na(pending_run)]
    if (length(pending_run) > 0L) {
      active_phase_a_set <- as.integer(sort(pending_run)[[1L]])
    }
  }
  phase <- if (isTRUE(is_link_mode) && length(pending_run) == 0L &&
    isTRUE(strict_ready_for_phase_b) && length(ready_spokes) > 0L) {
    "phase_b"
  } else {
    "phase_a"
  }
  prior_phase <- as.character((out$linking$phase_a %||% list())$phase %||% "phase_a")
  prior_phase_b_start <- as.integer((out$linking$phase_a %||% list())$phase_b_started_at_step %||% NA_integer_)
  phase_b_start <- prior_phase_b_start
  if (!identical(prior_phase, "phase_b") && identical(phase, "phase_b") && !is.finite(phase_b_start)) {
    phase_b_start <- as.integer(nrow(out$step_log %||% tibble::tibble()) + 1L)
  }

  out$linking <- out$linking %||% list()
  out$linking$phase_a <- list(
    set_status = statuses,
    artifacts = artifacts,
    ready_for_phase_b = ready_for_phase_b,
    strict_ready_for_phase_b = as.logical(strict_ready_for_phase_b),
    required_sets = as.integer(required_sets),
    set_stop_pass_by_set = set_stop_pass_map,
    phase = phase,
    ready_spokes = as.integer(ready_spokes),
    active_phase_a_set = as.integer(active_phase_a_set),
    phase_b_started_at_step = as.integer(phase_b_start),
    warm_start_scope_set = prior_warm_start_scope_set,
    prepare_context_by_set = prepare_context_by_set
  )

  .adaptive_anchored_joint_sync_scaffolding(out)
}

.adaptive_phase_a_finalize_if_ready <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  if (!as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")) {
    return(out)
  }
  phase_a <- out$linking$phase_a %||% list()
  status_tbl <- tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
  artifacts <- phase_a$artifacts %||% list()
  required_sets <- as.integer(phase_a$required_sets %||% .adaptive_phase_a_required_sets(out, controller = controller))
  if (length(required_sets) < 1L) {
    return(out)
  }

  set_stop_pass_map <- phase_a$set_stop_pass_by_set %||% list()
  missing_stop_sets <- required_sets[!vapply(as.character(required_sets), function(key) {
    !is.null(set_stop_pass_map[[key]]) && !is.na(set_stop_pass_map[[key]])
  }, logical(1L))]
  for (set_id in missing_stop_sets) {
    set_key <- as.character(set_id)
    source <- NA_character_
    if (nrow(status_tbl) > 0L && set_id %in% as.integer(status_tbl$set_id)) {
      source <- as.character(status_tbl$source[match(set_id, as.integer(status_tbl$set_id))] %||% NA_character_)
    }
    set_stop_pass_map[[set_key]] <- isTRUE(.adaptive_phase_a_set_stop_passed(
      artifact = artifacts[[set_key]] %||% NULL,
      source = source,
      controller = controller
    ))
  }

  strict_ready <- all(vapply(as.character(required_sets), function(key) {
    isTRUE(set_stop_pass_map[[key]])
  }, logical(1L)))
  pending_run_sets <- integer()
  if (nrow(status_tbl) > 0L) {
    pending_ids <- as.integer(status_tbl$set_id[status_tbl$source == "run" & status_tbl$status != "ready"])
    pending_ids <- pending_ids[!is.na(pending_ids)]
    pending_run_sets <- as.integer(sort(unique(pending_ids)))
  }

  hub_id <- as.integer(controller$hub_id %||% 1L)
  required_spokes <- setdiff(required_sets, hub_id)
  ready_spokes <- as.integer(required_spokes[vapply(as.character(required_spokes), function(key) {
    isTRUE(set_stop_pass_map[[key]])
  }, logical(1L))])
  phase <- if (length(pending_run_sets) == 0L &&
    isTRUE(strict_ready) &&
    length(ready_spokes) > 0L) {
    "phase_b"
  } else {
    "phase_a"
  }

  prior_phase <- as.character(phase_a$phase %||% "phase_a")
  phase_b_start <- as.integer(phase_a$phase_b_started_at_step %||% NA_integer_)
  if (!identical(prior_phase, "phase_b") &&
    identical(phase, "phase_b") &&
    !is.finite(phase_b_start)) {
    phase_b_start <- as.integer(nrow(out$step_log %||% tibble::tibble()) + 1L)
  }

  out$linking <- out$linking %||% list()
  out$linking$phase_a <- utils::modifyList(
    phase_a,
    list(
      strict_ready_for_phase_b = as.logical(strict_ready),
      required_sets = as.integer(required_sets),
      set_stop_pass_by_set = set_stop_pass_map,
      ready_spokes = as.integer(sort(unique(ready_spokes))),
      active_phase_a_set = if (length(pending_run_sets) > 0L) as.integer(pending_run_sets[[1L]]) else NA_integer_,
      phase = as.character(phase),
      phase_b_started_at_step = as.integer(phase_b_start)
    )
  )
  out
}

.adaptive_phase_a_gate_or_abort <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  if (!isTRUE(is_link_mode)) {
    return(invisible(state))
  }

  phase_a <- state$linking$phase_a %||% list()
  required_sets <- as.integer(
    phase_a$required_sets %||% .adaptive_phase_a_required_sets(state, controller = controller)
  )
  stop_pass_map <- phase_a$set_stop_pass_by_set %||% list()
  status_tbl <- tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
  if (nrow(status_tbl) > 0L && any(status_tbl$status %in% "failed")) {
    blocked <- status_tbl[status_tbl$status %in% "failed", , drop = FALSE]
    blocked_msg <- paste0(
      "set ",
      blocked$set_id,
      " [",
      blocked$source,
      "]: ",
      blocked$validation_message
    )
    blocked_msg <- paste(blocked_msg, collapse = "; ")
    rlang::abort(paste0(
      "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
      blocked_msg
    ))
  }

  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  artifacts <- phase_a$artifacts %||% list()
  missing_stop <- required_sets[!vapply(as.character(required_sets), function(key) {
    if (!is.null(stop_pass_map[[key]])) {
      return(isTRUE(stop_pass_map[[key]]))
    }
    set_id <- as.integer(key)
    source <- NA_character_
    if (nrow(status_tbl) > 0L && set_id %in% as.integer(status_tbl$set_id)) {
      source <- as.character(status_tbl$source[match(set_id, as.integer(status_tbl$set_id))] %||% NA_character_)
    }
    isTRUE(.adaptive_phase_a_set_stop_passed(
      artifact = artifacts[[key]] %||% NULL,
      source = source,
      controller = controller
    ))
  }, logical(1L))]
  if (length(missing_stop) > 0L && identical(phase_ctx$phase, "phase_b")) {
    rlang::abort(
      paste0(
        "Phase B linking cannot start until required Phase A stop-pass completion exists for required sets. ",
        "missing stop-pass set_id: ",
        paste(sort(unique(missing_stop)), collapse = ", "),
        "."
      )
    )
  }
  if (length(phase_ctx$pending_run_sets) > 0L) {
    return(invisible(state))
  }
  if (identical(phase_ctx$phase, "phase_b")) {
    if (isTRUE(controller$within_phase_b_within_set_steps_allowed %||% FALSE)) {
      rlang::abort(
        paste0(
          "Phase B runtime does not support ",
          "`adaptive_config$within_phase_b_within_set_steps_allowed = TRUE`; ",
          "current Phase B execution remains cross-set only."
        )
      )
    }
    if (length(phase_ctx$ready_spokes) < 1L) {
      rlang::abort(
        paste0(
          "Phase metadata and routing mode disagree: phase marked phase_b but no ready spokes are available."
        )
      )
    }
    required_sets <- as.integer(unique(c(as.integer(controller$hub_id %||% 1L), phase_ctx$ready_spokes)))
    missing_sets <- required_sets[!as.character(required_sets) %in% names(artifacts)]
    if (length(missing_sets) > 0L) {
      rlang::abort(
        paste0(
          "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
          "missing artifacts for set_id: ",
          paste(missing_sets, collapse = ", "),
          "."
        )
      )
    }
    for (set_id in required_sets) {
      artifact <- artifacts[[as.character(set_id)]] %||% NULL
      if (is.null(artifact)) {
        rlang::abort(
          paste0(
            "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
            "missing artifact for set_id: ",
            as.integer(set_id),
            "."
          )
        )
      }
      .adaptive_phase_a_validate_imported_artifact(
        artifact = artifact,
        state = state,
        set_id = as.integer(set_id),
        controller = controller
      )
      source <- NA_character_
      if (nrow(status_tbl) > 0L && set_id %in% as.integer(status_tbl$set_id)) {
        source <- as.character(status_tbl$source[match(set_id, as.integer(status_tbl$set_id))] %||% NA_character_)
      }
      if (!isTRUE(.adaptive_phase_a_set_stop_passed(artifact = artifact, source = source, controller = controller))) {
        rlang::abort(
          paste0(
            "Phase B linking cannot start until required Phase A stop-pass completion exists for required sets. ",
            "set_id ",
            as.integer(set_id),
            " did not satisfy strict stop-pass criteria."
          )
        )
      }
    }
    return(invisible(state))
  }

  blocked <- status_tbl
  if (nrow(blocked) > 0L) {
    blocked <- blocked[blocked$status != "ready", , drop = FALSE]
  }
  if (nrow(blocked) == 0L) {
    blocked_msg <- "phase_a_artifacts_missing"
  } else {
    blocked_msg <- paste0(
      "set ",
      blocked$set_id,
      " [",
      blocked$source,
      "]: ",
      blocked$validation_message
    )
    blocked_msg <- paste(blocked_msg, collapse = "; ")
  }

  rlang::abort(paste0(
    "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
    blocked_msg
  ))
}

.adaptive_phase_a_artifact_filename <- function(set_id) {
  paste0("set_", formatC(as.integer(set_id), width = 4, flag = "0"), ".rds")
}

.adaptive_write_phase_a_artifacts <- function(artifacts,
                                             artifact_dir,
                                             overwrite_existing = TRUE,
                                             trim_stale = FALSE) {
  if (is.null(artifacts) || !is.list(artifacts) || length(artifacts) == 0L) {
    if (isTRUE(trim_stale) && dir.exists(artifact_dir)) {
      unlink(artifact_dir, recursive = TRUE, force = TRUE)
    }
    return(invisible(NULL))
  }
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  expected_paths <- character(0)

  for (name in names(artifacts)) {
    art <- artifacts[[name]]
    set_id <- as.integer(art$set_id %||% suppressWarnings(as.integer(name)))
    if (is.na(set_id)) {
      next
    }
    path <- file.path(artifact_dir, .adaptive_phase_a_artifact_filename(set_id))
    expected_paths <- c(expected_paths, path)
    if (!isTRUE(overwrite_existing) && file.exists(path)) {
      next
    }
    write_log(art, path)
  }

  if (isTRUE(trim_stale) && dir.exists(artifact_dir)) {
    existing_paths <- list.files(
      artifact_dir,
      pattern = "^set_\\d+\\.rds$",
      full.names = TRUE
    )
    stale_paths <- setdiff(existing_paths, unique(expected_paths))
    if (length(stale_paths) > 0L) {
      unlink(stale_paths, force = TRUE)
    }
  }

  invisible(NULL)
}

.adaptive_read_phase_a_artifacts <- function(artifact_dir) {
  if (!dir.exists(artifact_dir)) {
    return(list())
  }
  files <- list.files(artifact_dir, pattern = "^set_\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0L) {
    return(list())
  }

  out <- list()
  for (path in files) {
    art <- read_log(path)
    set_id <- as.integer(art$set_id %||% NA_integer_)
    if (!is.na(set_id)) {
      out[[as.character(set_id)]] <- art
    }
  }

  out
}
