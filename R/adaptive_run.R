# -------------------------------------------------------------------------
# Adaptive entrypoints.
# -------------------------------------------------------------------------

.adaptive_build_warm_start_pairs <- function(item_ids, seed) {
  item_ids <- as.character(item_ids)
  if (length(item_ids) < 2L) {
    return(tibble::tibble(i_id = character(), j_id = character()))
  }
  seed <- .adaptive_validate_seed(seed)
  shuffled <- withr::with_seed(seed, sample(item_ids, size = length(item_ids)))
  i_ids <- shuffled[-length(shuffled)]
  j_ids <- shuffled[-1L]
  tibble::tibble(i_id = i_ids, j_id = j_ids)
}

#' @keywords internal
#' @noRd
.adaptive_link_phase_a_scope <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(list(
      active_set_id = NA_integer_,
      active_set_n = NA_integer_
    ))
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(list(
      active_set_id = NA_integer_,
      active_set_n = NA_integer_
    ))
  }
  active_set_id <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
  if (is.na(active_set_id)) {
    return(list(
      active_set_id = NA_integer_,
      active_set_n = NA_integer_
    ))
  }
  active_set_n <- as.integer(sum(as.integer(state$items$set_id) == active_set_id, na.rm = TRUE))
  if (!is.finite(active_set_n) || active_set_n < 1L) {
    active_set_n <- NA_integer_
  }
  list(
    active_set_id = as.integer(active_set_id),
    active_set_n = as.integer(active_set_n)
  )
}

#' @keywords internal
#' @noRd
.adaptive_controller_with_phase_scope <- function(state, controller = NULL) {
  out <- controller %||% .adaptive_controller_resolve(state)
  scope <- .adaptive_link_phase_a_scope(state, controller = out)
  out$phase_a_active_set_id <- as.integer(scope$active_set_id %||% NA_integer_)
  out$phase_a_active_n <- as.integer(scope$active_set_n %||% NA_integer_)
  out
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_mark_unresolved <- function(state, set_id, message) {
  out <- state
  set_id <- as.integer(set_id %||% NA_integer_)
  phase_a <- out$linking$phase_a %||% list()
  status_tbl <- tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
  if (nrow(status_tbl) > 0L && !is.na(set_id) && set_id %in% as.integer(status_tbl$set_id)) {
    idx <- which(as.integer(status_tbl$set_id) == set_id)[[1L]]
    status_tbl$status[[idx]] <- "failed"
    status_tbl$validation_message[[idx]] <- as.character(message %||% "phase_a_set_unresolved")
    out$linking$phase_a$set_status <- status_tbl
  }
  out$meta$stop_reason_detail <- as.character(message %||% "phase_a_set_unresolved")
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_sync_warm_start <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(out)
  }

  phase_ctx <- .adaptive_link_phase_context(out, controller = controller)
  out$linking <- out$linking %||% list()
  out$linking$phase_a <- out$linking$phase_a %||% list()

  if (identical(phase_ctx$phase, "phase_b")) {
    if (!isTRUE(out$warm_start_done)) {
      out$warm_start_done <- TRUE
      out$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
      out$warm_start_idx <- 1L
    }
    out$linking$phase_a$warm_start_scope_set <- NA_integer_
    return(out)
  }

  active_set <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
  if (is.na(active_set)) {
    return(out)
  }

  current_scope <- as.integer(out$linking$phase_a$warm_start_scope_set %||% NA_integer_)
  if (!identical(current_scope, active_set)) {
    ids <- as.character(out$items$item_id[as.integer(out$items$set_id) == active_set])
    seed <- as.integer(out$meta$seed %||% 1L)
    out$warm_start_pairs <- .adaptive_build_warm_start_pairs(
      item_ids = ids,
      seed = as.integer(seed + (active_set * 1009L))
    )
    out$warm_start_idx <- 1L
    out$warm_start_done <- nrow(out$warm_start_pairs) == 0L
    out$linking$phase_a$warm_start_scope_set <- as.integer(active_set)
    return(out)
  }

  pairs <- out$warm_start_pairs %||% tibble::tibble(i_id = character(), j_id = character())
  if (nrow(pairs) > 0L) {
    set_map <- stats::setNames(as.integer(out$items$set_id), as.character(out$items$item_id))
    set_i <- as.integer(set_map[as.character(pairs$i_id)])
    set_j <- as.integer(set_map[as.character(pairs$j_id)])
    keep <- !is.na(set_i) & !is.na(set_j) & set_i == active_set & set_j == active_set
    if (any(!keep)) {
      pairs <- pairs[keep, , drop = FALSE]
      out$warm_start_pairs <- pairs
      if (nrow(pairs) < 1L) {
        out$warm_start_done <- TRUE
        out$warm_start_idx <- 1L
      } else {
        out$warm_start_idx <- min(as.integer(out$warm_start_idx %||% 1L), nrow(pairs))
        out$warm_start_done <- as.integer(out$warm_start_idx) > nrow(pairs)
      }
    }
  }

  out
}

#' @keywords internal
#' @noRd
.adaptive_round_activate_if_ready <- function(state) {
  out <- state
  out$controller <- .adaptive_controller_with_phase_scope(out, controller = .adaptive_controller_resolve(out))
  if (is.null(out$round) || !is.list(out$round)) {
    out$round <- .adaptive_new_round_state(
      out$item_ids,
      round_id = 1L,
      staged_active = FALSE,
      controller = out$controller
    )
  }
  if (isTRUE(out$warm_start_done)) {
    out$round$staged_active <- TRUE
    if ((out$round$round_committed %||% 0L) >= (out$round$round_pairs_target %||% 0L)) {
      out <- .adaptive_round_start_next(out)
    }
  }
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_refit_window_id <- function(state) {
  as.integer(nrow(state$round_log %||% tibble::tibble()) + 1L)
}

#' @keywords internal
#' @noRd
.adaptive_link_refit_spoke_key <- function(refit_id, spoke_id) {
  paste0(as.integer(refit_id), "::", as.integer(spoke_id))
}

#' @keywords internal
#' @noRd
.adaptive_link_refit_shortfalls_map <- function(state) {
  primary <- state$refit_meta$link_stage_shortfalls_by_refit_spoke %||% NULL
  if (is.list(primary)) {
    return(primary)
  }
  legacy <- state$round$link_stage_shortfalls_by_refit_spoke %||% NULL
  if (is.list(legacy)) {
    return(legacy)
  }
  list()
}

#' @keywords internal
#' @noRd
.adaptive_link_refit_exhausted_map <- function(state) {
  primary <- state$refit_meta$link_stage_exhausted_by_refit_spoke %||% NULL
  if (is.list(primary)) {
    return(primary)
  }
  legacy <- state$round$link_stage_exhausted_by_refit_spoke %||% NULL
  if (is.list(legacy)) {
    return(legacy)
  }
  list()
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_state <- function(state) {
  probe <- state$linking$probe %||% NULL
  if (!is.list(probe)) {
    probe <- .adaptive_link_probe_empty_state()
  }
  probe$panels_by_spoke <- probe$panels_by_spoke %||% list()
  probe$prediction_cache <- tibble::as_tibble(
    probe$prediction_cache %||% .adaptive_link_probe_empty_cache()
  )
  probe$realized_edges <- tibble::as_tibble(
    probe$realized_edges %||% .adaptive_link_probe_empty_realized_log()
  )
  probe$collect_holdout_now_by_spoke <- probe$collect_holdout_now_by_spoke %||% list()
  probe
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_panel_size <- function(n_spoke_items,
                                            n_available_pairs = NA_integer_,
                                            probe_edges_min_for_stop = 30L) {
  n_spoke_items <- as.integer(n_spoke_items)
  n_available_pairs <- as.integer(n_available_pairs %||% NA_integer_)
  probe_edges_min_for_stop <- max(1L, as.integer(probe_edges_min_for_stop %||% 30L))
  base_target <- max(1L, as.integer(ceiling(0.25 * n_spoke_items)))
  target <- max(base_target, probe_edges_min_for_stop)
  if (!is.na(n_available_pairs)) {
    target <- min(target, max(1L, as.integer(floor(0.5 * n_available_pairs))))
  }
  max(1L, as.integer(target))
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_epoch_for_spoke <- function(state, spoke_id) {
  controller <- .adaptive_controller_resolve(state)
  epoch_map <- controller$link_epoch_id_by_spoke %||% list()
  if (!is.null(epoch_map[[as.character(spoke_id)]])) {
    return(as.integer(epoch_map[[as.character(spoke_id)]] %||% 1L))
  }
  stats <- controller$link_refit_stats_by_spoke %||% list()
  as.integer(stats[[as.character(spoke_id)]]$link_epoch_id %||% 1L)
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_panel_id <- function(panel_tbl) {
  panel_tbl <- tibble::as_tibble(panel_tbl)
  keys <- if (nrow(panel_tbl) > 0L) {
    sort(unique(as.character(panel_tbl$pair_key %||%
      make_unordered_key(panel_tbl$hub_item_id, panel_tbl$spoke_item_id))))
  } else {
    character()
  }
  tmp <- tempfile("probe_panel_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(keys, tmp)
  unname(tools::md5sum(tmp))[[1L]]
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_panel_for_spoke <- function(state, spoke_id, epoch_id = NULL) {
  probe <- .adaptive_link_probe_state(state)
  panel <- probe$panels_by_spoke[[as.character(as.integer(spoke_id))]] %||% .adaptive_link_probe_empty_panel()
  panel <- tibble::as_tibble(panel)
  if (!is.null(epoch_id)) {
    panel <- panel[as.integer(panel$link_epoch_id) == as.integer(epoch_id), , drop = FALSE]
  }
  if (nrow(panel) < 1L) {
    return(panel)
  }
  realized_edges <- tibble::as_tibble(probe$realized_edges %||% .adaptive_link_probe_empty_realized_log())
  if (nrow(realized_edges) < 1L) {
    return(panel)
  }
  realized_edges <- realized_edges[
    as.integer(realized_edges$spoke_id) == as.integer(spoke_id) &
      as.integer(realized_edges$link_epoch_id) == as.integer(panel$link_epoch_id[[1L]] %||% NA_integer_),
    ,
    drop = FALSE
  ]
  panel_ids <- unique(as.character(panel$probe_panel_id))
  panel_ids <- panel_ids[!is.na(panel_ids) & nzchar(panel_ids)]
  if (length(panel_ids) > 1L) {
    rlang::abort(
      paste0(
        "Phase B probe-panel invariant failed: current panel has multiple `probe_panel_id` values ",
        "for spoke_id=", as.integer(spoke_id),
        " in link_epoch_id=", as.integer(panel$link_epoch_id[[1L]] %||% NA_integer_),
        "."
      )
    )
  }
  panel_id <- if (length(panel_ids) == 1L) panel_ids[[1L]] else NA_character_
  if (nrow(realized_edges) > 0L) {
    realized_panel_ids <- unique(as.character(realized_edges$probe_panel_id))
    realized_panel_ids <- realized_panel_ids[!is.na(realized_panel_ids) & nzchar(realized_panel_ids)]
    if (length(realized_panel_ids) > 1L ||
      (length(realized_panel_ids) == 1L && !identical(realized_panel_ids[[1L]], panel_id))) {
      rlang::abort(
        paste0(
          "Phase B probe-panel invariant failed: canonical `realized_edges$probe_panel_id` does not ",
          "match the current panel for spoke_id=", as.integer(spoke_id),
          " in link_epoch_id=", as.integer(panel$link_epoch_id[[1L]] %||% NA_integer_),
          "."
        )
      )
    }
    if (!all(as.character(realized_edges$pair_key) %in% as.character(panel$pair_key))) {
      rlang::abort(
        paste0(
          "Phase B probe-panel invariant failed: canonical realized probe edges are not contained in ",
          "the current panel for spoke_id=", as.integer(spoke_id),
          " in link_epoch_id=", as.integer(panel$link_epoch_id[[1L]] %||% NA_integer_),
          "."
        )
      )
    }
  }
  if (nrow(realized_edges) < 1L) {
    return(panel)
  }
  realized_edges <- realized_edges[
    !duplicated(as.character(realized_edges$pair_key), fromLast = TRUE),
    ,
    drop = FALSE
  ]
  realized_idx <- match(as.character(panel$pair_key), as.character(realized_edges$pair_key))
  hit <- !is.na(realized_idx)
  if (!any(hit)) {
    return(panel)
  }
  panel$realized[hit] <- TRUE
  panel$realized_step_id[hit] <- as.integer(realized_edges$step_id[realized_idx[hit]])
  panel$realized_pair_id[hit] <- as.integer(realized_edges$pair_id[realized_idx[hit]])
  panel$realized_run_mode[hit] <- as.character(realized_edges$run_mode[realized_idx[hit]])
  panel
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_reserved_keys <- function(state, spoke_id, epoch_id = NULL) {
  panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
  unique(as.character(panel$pair_key))
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_realized_count <- function(state, spoke_id, epoch_id = NULL) {
  panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
  if (nrow(panel) < 1L) {
    return(0L)
  }
  realized_edges <- .adaptive_link_probe_realized_log_for_panel(
    state = state,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(panel$link_epoch_id[[1L]] %||% epoch_id %||% NA_integer_),
    panel = panel
  )
  as.integer(nrow(realized_edges))
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_holdout_since_last_refit <- function(state, spoke_id) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (
    nrow(step_log) < 1L ||
      !all(c("pair_id", "step_id", "link_spoke_id", "run_mode") %in% names(step_log))
  ) {
    return(0L)
  }
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
  as.integer(sum(
    !is.na(step_log$pair_id) &
      as.integer(step_log$step_id) > last_refit_step &
      as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
      as.character(step_log$run_mode) == "link_probe_holdout",
    na.rm = TRUE
  ))
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_holdout_total_since_last_refit <- function(state) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (
    nrow(step_log) < 1L ||
      !all(c("pair_id", "step_id", "run_mode") %in% names(step_log))
  ) {
    return(0L)
  }
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
  as.integer(sum(
    !is.na(step_log$pair_id) &
      as.integer(step_log$step_id) > last_refit_step &
      as.character(step_log$run_mode) == "link_probe_holdout",
    na.rm = TRUE
  ))
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_last_stage_row <- function(state, spoke_id) {
  link_stage_log <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  if (
    nrow(link_stage_log) < 1L ||
      !all(c("spoke_id", "refit_id") %in% names(link_stage_log))
  ) {
    return(tibble::as_tibble(new_link_stage_log()))
  }
  rows <- link_stage_log[as.integer(link_stage_log$spoke_id) == as.integer(spoke_id), , drop = FALSE]
  if (nrow(rows) < 1L) {
    return(tibble::as_tibble(new_link_stage_log()))
  }
  rows <- rows[order(as.integer(rows$refit_id), seq_len(nrow(rows))), , drop = FALSE]
  rows[nrow(rows), , drop = FALSE]
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_effort_plan <- function(state, controller, spoke_id) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  spoke_id <- as.integer(spoke_id %||% NA_integer_)
  base_cap <- max(0L, as.integer(controller$probe_pairs_per_refit_per_spoke %||% 2L))
  realized_min <- max(1L, as.integer(controller$probe_edges_min_for_stop %||% 30L))
  epoch_id <- .adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)
  panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
  panel_n <- as.integer(nrow(panel))
  realized_total <- max(0L, .adaptive_link_probe_realized_count(state, spoke_id = spoke_id, epoch_id = epoch_id))
  realized_refit <- max(0L, .adaptive_link_probe_holdout_since_last_refit(state, spoke_id = spoke_id))
  realized_before_refit <- max(0L, as.integer(realized_total - realized_refit))
  remaining_to_min_start <- max(0L, as.integer(realized_min - realized_before_refit))
  panel_shortfall_start <- max(0L, as.integer(panel_n - realized_before_refit))

  stats_row <- (controller$link_refit_stats_by_spoke %||% list())[[as.character(spoke_id)]] %||% list()
  last_stage_row <- .adaptive_link_probe_last_stage_row(state, spoke_id = spoke_id)
  linking_identified <- isTRUE(
    stats_row$link_identified %||%
      if (nrow(last_stage_row) > 0L) last_stage_row$linking_identified[[1L]] else FALSE
  )
  link_stop_eligible <- isTRUE(
    stats_row$link_stop_eligible %||%
      if (nrow(last_stage_row) > 0L) last_stage_row$link_stop_eligible[[1L]] else FALSE
  )

  acceleration_used <- isTRUE(linking_identified) &&
    !isTRUE(link_stop_eligible) &&
    remaining_to_min_start > base_cap &&
    panel_shortfall_start > base_cap
  effective_cap <- if (isTRUE(acceleration_used)) {
    min(panel_shortfall_start, remaining_to_min_start)
  } else {
    base_cap
  }

  list(
    spoke_id = as.integer(spoke_id),
    base_cap = as.integer(base_cap),
    effective_cap = as.integer(max(base_cap, effective_cap)),
    realized_min = as.integer(realized_min),
    realized_total = as.integer(realized_total),
    realized_refit = as.integer(realized_refit),
    realized_before_refit = as.integer(realized_before_refit),
    remaining_to_min_start = as.integer(remaining_to_min_start),
    panel_shortfall_start = as.integer(panel_shortfall_start),
    linking_identified = as.logical(linking_identified),
    link_stop_eligible = as.logical(link_stop_eligible),
    acceleration_used = as.logical(acceleration_used)
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_active_progress_guard <- function(state,
                                                       controller,
                                                       eligible_spoke_ids = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  concurrent_mode <- identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
  if (!isTRUE(concurrent_mode)) {
    return(list(
      block_probes = FALSE,
      pending_spokes = integer(),
      budgeted_spokes = integer()
    ))
  }

  refit_id <- .adaptive_link_refit_window_id(state)
  effective_spokes <- .adaptive_link_effective_active_spokes(
    state = state,
    controller = controller,
    refit_id = refit_id,
    exclude_exhausted = TRUE
  )
  if (!is.null(eligible_spoke_ids)) {
    effective_spokes <- intersect(as.integer(effective_spokes), as.integer(eligible_spoke_ids))
  }
  effective_spokes <- sort(unique(as.integer(effective_spokes[!is.na(effective_spokes)])))
  if (length(effective_spokes) < 1L) {
    return(list(
      block_probes = FALSE,
      pending_spokes = integer(),
      budgeted_spokes = integer()
    ))
  }

  cached_refit_id <- as.integer(controller$link_budget_refit_id %||% NA_integer_)
  cached_budget_map <- controller$link_budget_map %||% list()
  budget_map <- if (identical(cached_refit_id, refit_id) && length(cached_budget_map) > 0L) {
    cached_budget_map
  } else {
    .adaptive_link_budget_map_for_refit(
      state = state,
      controller = controller,
      eligible_spoke_ids = effective_spokes
    )
  }
  frozen_map <- controller$link_transform_frozen_by_spoke %||% list()
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)

  budgeted_spokes <- integer()
  pending_spokes <- integer()
  for (spoke_id in effective_spokes) {
    key <- as.character(spoke_id)
    if (isTRUE(frozen_map[[key]])) {
      next
    }
    budget <- as.integer(budget_map[[key]]$B_spoke_refit_budget %||% 0L)
    if (!is.finite(budget) || budget < 1L) {
      next
    }
    budgeted_spokes <- c(budgeted_spokes, as.integer(spoke_id))
    cross_since <- .adaptive_link_cross_edges(
      state = state,
      spoke_id = as.integer(spoke_id),
      last_refit_step = last_refit_step
    )
    active_edges_since_refit <- if (nrow(cross_since) > 0L) {
      as.integer(sum(!as.logical(cross_since$is_probe_step %||% FALSE), na.rm = TRUE))
    } else {
      0L
    }
    if (active_edges_since_refit < 1L) {
      pending_spokes <- c(pending_spokes, as.integer(spoke_id))
    }
  }

  list(
    block_probes = length(pending_spokes) > 0L,
    pending_spokes = as.integer(sort(unique(pending_spokes))),
    budgeted_spokes = as.integer(sort(unique(budgeted_spokes)))
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_next_holdout_spoke <- function(state,
                                                    controller,
                                                    eligible_spoke_ids = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  spoke_ids <- as.integer(eligible_spoke_ids %||% phase_ctx$active_spokes %||% integer())
  spoke_ids <- sort(unique(spoke_ids[!is.na(spoke_ids)]))
  if (length(spoke_ids) < 1L) {
    return(NA_integer_)
  }
  link_stage_log <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  if (nrow(link_stage_log) < 1L) {
    return(NA_integer_)
  }

  realized_min <- as.integer(controller$probe_edges_min_for_stop %||% 30L)
  probe_cap <- max(0L, as.integer(controller$probe_pairs_per_refit_per_spoke %||% 2L))
  refit_target <- max(0L, as.integer(
    state$refit_meta$refit_pairs_target_current %||%
      controller$refit_pairs_target %||%
      0L
  ))
  global_probe_cap <- max(0L, refit_target - 1L)
  if (global_probe_cap <= 0L) {
    return(NA_integer_)
  }
  holdout_total_since_refit <- .adaptive_link_probe_holdout_total_since_last_refit(state)
  if (holdout_total_since_refit >= global_probe_cap) {
    return(NA_integer_)
  }
  fairness_guard <- .adaptive_link_probe_active_progress_guard(
    state = state,
    controller = controller,
    eligible_spoke_ids = spoke_ids
  )
  if (isTRUE(fairness_guard$block_probes)) {
    return(NA_integer_)
  }
  frozen_map <- controller$link_transform_frozen_by_spoke %||% list()
  ranked_spokes <- .adaptive_link_ranked_spokes(
    state = state,
    controller = controller,
    eligible_spoke_ids = spoke_ids
  )
  if (length(ranked_spokes) < 1L) {
    ranked_spokes <- spoke_ids
  }
  rank_map <- stats::setNames(seq_along(ranked_spokes), as.character(ranked_spokes))

  pending <- lapply(spoke_ids, function(spoke_id) {
    key <- as.character(spoke_id)
    if (isTRUE(frozen_map[[key]])) {
      return(NULL)
    }
    plan <- .adaptive_link_probe_effort_plan(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id)
    )
    epoch_id <- .adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)
    panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
    if (nrow(panel) < 1L) {
      return(list(
        spoke_id = as.integer(spoke_id),
        realized_total = 0L,
        realized_refit = 0L,
        effective_cap = as.integer(plan$effective_cap %||% probe_cap),
        remaining_to_min_start = as.integer(plan$remaining_to_min_start %||% realized_min),
        acceleration_used = as.logical(plan$acceleration_used %||% FALSE),
        rank = as.integer(rank_map[key] %||% (length(spoke_ids) + as.integer(spoke_id)))
      ))
    }
    realized_total <- as.integer(plan$realized_total %||% 0L)
    if (realized_total >= realized_min) {
      return(NULL)
    }
    realized_refit <- as.integer(plan$realized_refit %||% 0L)
    effective_cap <- as.integer(plan$effective_cap %||% probe_cap)
    if (realized_refit >= effective_cap) {
      return(NULL)
    }
    list(
      spoke_id = as.integer(spoke_id),
      realized_total = as.integer(realized_total),
      realized_refit = as.integer(realized_refit),
      effective_cap = as.integer(effective_cap),
      remaining_to_min_start = as.integer(plan$remaining_to_min_start %||% realized_min),
      acceleration_used = as.logical(plan$acceleration_used %||% FALSE),
      rank = as.integer(rank_map[key] %||% (length(spoke_ids) + as.integer(spoke_id)))
    )
  })
  pending <- Filter(Negate(is.null), pending)
  if (length(pending) < 1L) {
    return(NA_integer_)
  }
  pending_tbl <- tibble::as_tibble(do.call(rbind, lapply(pending, as.data.frame)))
  pending_tbl <- pending_tbl[
    order(
      -as.integer(pending_tbl$acceleration_used %in% TRUE),
      as.integer(pending_tbl$remaining_to_min_start),
      as.integer(pending_tbl$realized_refit),
      as.integer(pending_tbl$realized_total),
      as.integer(pending_tbl$rank),
      as.integer(pending_tbl$spoke_id)
    ),
    ,
    drop = FALSE
  ]
  as.integer(pending_tbl$spoke_id[[1L]] %||% NA_integer_)
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_next_pair <- function(state, spoke_id, epoch_id = NULL) {
  panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
  pending <- panel[!panel$realized %in% TRUE, , drop = FALSE]
  if (nrow(pending) < 1L) {
    return(NULL)
  }
  pending <- pending[
    order(as.integer(pending$planned_rank), pending$hub_item_id, pending$spoke_item_id),
    ,
    drop = FALSE
  ]
  pending[1L, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_select_holdout <- function(state, step_id, spoke_id) {
  probe_row <- .adaptive_link_probe_next_pair(
    state,
    spoke_id = as.integer(spoke_id),
    epoch_id = .adaptive_link_probe_epoch_for_spoke(state, spoke_id)
  )
  if (is.null(probe_row) || nrow(probe_row) != 1L) {
    return(NULL)
  }

  i_id <- as.character(probe_row$hub_item_id[[1L]])
  j_id <- as.character(probe_row$spoke_item_id[[1L]])
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, state$item_ids)
  recent_deg <- .adaptive_recent_deg(history, state$item_ids, adaptive_defaults(length(state$item_ids))$W_cap)
  order_vals <- .adaptive_assign_order(
    tibble::tibble(i = i_id, j = j_id),
    counts$posA,
    counts$posB,
    counts$pair_last_order,
    seed_base = as.integer(state$meta$seed %||% 1L)
  )
  idx_map <- state$item_index %||% stats::setNames(seq_along(state$item_ids), state$item_ids)
  trueskill_items <- state$trueskill_state$items
  mu_vals <- stats::setNames(as.double(trueskill_items$mu), as.character(trueskill_items$item_id))
  sigma_vals <- stats::setNames(as.double(trueskill_items$sigma), as.character(trueskill_items$item_id))
  A_id <- as.character(order_vals[["A_id"]])
  B_id <- as.character(order_vals[["B_id"]])
  p_ij <- .adaptive_link_predictive_prob_oriented(
    state = state,
    controller = .adaptive_controller_resolve(state),
    spoke_id = as.integer(spoke_id),
    A_id = A_id,
    B_id = B_id
  )

  list(
    i = as.integer(idx_map[[i_id]]),
    j = as.integer(idx_map[[j_id]]),
    A = as.integer(idx_map[[A_id]]),
    B = as.integer(idx_map[[B_id]]),
    is_explore_step = FALSE,
    explore_mode = NA_character_,
    explore_reason = NA_character_,
    explore_rate_used = as.double(NA_real_),
    local_priority_mode = NA_character_,
    long_gate_pass = NA,
    long_gate_reason = NA_character_,
    star_override_used = FALSE,
    star_override_reason = NA_character_,
    candidate_starved = FALSE,
    fallback_used = "probe_panel",
    fallback_path = "probe_panel",
    starvation_reason = NA_character_,
    round_id = as.integer(state$round$round_id %||% NA_integer_),
    round_stage = "probe_panel",
    pair_type = "probe_panel",
    used_in_round_i = NA_integer_,
    used_in_round_j = NA_integer_,
    is_anchor_i = NA,
    is_anchor_j = NA,
    stratum_i = NA_integer_,
    stratum_j = NA_integer_,
    dist_stratum = NA_integer_,
    stage_committed_so_far = NA_integer_,
    stage_quota = NA_integer_,
    n_candidates_generated = 1L,
    n_candidates_after_hard_filters = 1L,
    n_candidates_after_duplicates = 1L,
    n_candidates_after_star_caps = 1L,
    n_candidates_scored = 1L,
    deg_i = as.integer(counts$deg[[i_id]]),
    deg_j = as.integer(counts$deg[[j_id]]),
    recent_deg_i = as.integer(recent_deg[[i_id]]),
    recent_deg_j = as.integer(recent_deg[[j_id]]),
    mu_i = as.double(mu_vals[[i_id]]),
    mu_j = as.double(mu_vals[[j_id]]),
    sigma_i = as.double(sigma_vals[[i_id]]),
    sigma_j = as.double(sigma_vals[[j_id]]),
    p_ij = as.double(p_ij),
    U0_ij = as.double(p_ij * (1 - p_ij)),
    utility_mode = NA_character_,
    star_cap_rejects = 0L,
    star_cap_reject_items = 0L,
    link_spoke_id_selected = as.integer(spoke_id),
    run_mode = "link_probe_holdout",
    probe_panel_id = as.character(probe_row$probe_panel_id[[1L]]),
    link_epoch_id_selected = as.integer(probe_row$link_epoch_id[[1L]])
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_register_commit <- function(state, step_row) {
  row <- tibble::as_tibble(step_row)
  run_mode <- as.character(row$run_mode[[1L]] %||% NA_character_)
  if (nrow(row) != 1L ||
    !isTRUE(row$is_probe_step[[1L]] %||% FALSE) ||
    !identical(run_mode, "link_probe_holdout")) {
    return(state)
  }
  out <- state
  out$linking <- out$linking %||% list()
  probe <- .adaptive_link_probe_state(out)
  spoke_id <- as.integer(row$link_spoke_id[[1L]] %||% NA_integer_)
  if (is.na(spoke_id)) {
    out$linking$probe <- probe
    return(out)
  }

  ids <- as.character(out$item_ids)
  A_id <- ids[as.integer(row$A[[1L]] %||% NA_integer_)]
  B_id <- ids[as.integer(row$B[[1L]] %||% NA_integer_)]
  set_map <- stats::setNames(as.integer(out$items$set_id), as.character(out$items$item_id))
  hub_id <- as.integer(.adaptive_controller_resolve(out)$hub_id %||% 1L)
  hub_item_id <- if (as.integer(set_map[[A_id]] %||% NA_integer_) == hub_id) A_id else B_id
  spoke_item_id <- if (identical(hub_item_id, A_id)) B_id else A_id
  pair_key <- make_unordered_key(hub_item_id, spoke_item_id)

  panel_key <- as.character(spoke_id)
  panel <- probe$panels_by_spoke[[panel_key]] %||% .adaptive_link_probe_empty_panel()
  panel <- tibble::as_tibble(panel)
  if (nrow(panel) < 1L) {
    rlang::abort(
      paste0(
        "Phase B probe-panel invariant failed: committed probe step has no current panel for ",
        "spoke_id=", as.integer(spoke_id),
        "."
      )
    )
  }
  hit <- which(as.character(panel$pair_key) == pair_key)
  if (length(hit) < 1L) {
    rlang::abort(
      paste0(
        "Phase B probe-panel invariant failed: committed probe pair is not present in the current ",
        "panel for spoke_id=", as.integer(spoke_id),
        "."
      )
    )
  }
  idx <- hit[[1L]]
  panel$realized[[idx]] <- TRUE
  panel$realized_step_id[[idx]] <- as.integer(row$step_id[[1L]] %||% NA_integer_)
  panel$realized_pair_id[[idx]] <- as.integer(row$pair_id[[1L]] %||% NA_integer_)
  panel$realized_run_mode[[idx]] <- as.character(row$run_mode[[1L]] %||% NA_character_)
  probe$panels_by_spoke[[panel_key]] <- panel
  probe$realized_edges <- append_canonical_row(
    probe$realized_edges,
    list(
      step_id = as.integer(row$step_id[[1L]] %||% NA_integer_),
      pair_id = as.integer(row$pair_id[[1L]] %||% NA_integer_),
      run_mode = as.character(row$run_mode[[1L]] %||% NA_character_),
      spoke_id = as.integer(spoke_id),
      link_epoch_id = as.integer(
        .adaptive_link_probe_epoch_for_spoke(out, spoke_id = spoke_id)
      ),
      probe_panel_id = if (nrow(panel) > 0L) {
        as.character(panel$probe_panel_id[[1L]] %||% NA_character_)
      } else {
        NA_character_
      },
      hub_item_id = as.character(hub_item_id %||% NA_character_),
      spoke_item_id = as.character(spoke_item_id %||% NA_character_),
      pair_key = as.character(pair_key),
      Y = as.integer(row$Y[[1L]] %||% NA_integer_)
    ),
    schema = c(
      step_id = "integer",
      pair_id = "integer",
      run_mode = "character",
      spoke_id = "integer",
      link_epoch_id = "integer",
      probe_panel_id = "character",
      hub_item_id = "character",
      spoke_item_id = "character",
      pair_key = "character",
      Y = "integer"
    )
  )
  out$linking$probe <- probe
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_cache_predictions <- function(state, refit_id, spoke_id) {
  out <- state
  probe <- .adaptive_link_probe_state(out)
  epoch_id <- .adaptive_link_probe_epoch_for_spoke(out, spoke_id = spoke_id)
  panel <- .adaptive_link_probe_panel_for_spoke(
    out,
    spoke_id = as.integer(spoke_id),
    epoch_id = epoch_id
  )
  realized_log <- .adaptive_link_probe_realized_log_for_panel(
    out,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(epoch_id),
    panel = panel
  )
  if (nrow(realized_log) > 0L) {
    panel <- dplyr::inner_join(
      panel,
      realized_log[, c("pair_key", "probe_panel_id"), drop = FALSE],
      by = "pair_key",
      suffix = c("", "_realized")
    )
    if ("probe_panel_id_realized" %in% names(panel)) {
      use_realized_id <- !is.na(panel$probe_panel_id_realized) & nzchar(panel$probe_panel_id_realized)
      panel$probe_panel_id[use_realized_id] <- panel$probe_panel_id_realized[use_realized_id]
      panel$probe_panel_id_realized <- NULL
    }
  } else {
    panel <- panel[0, , drop = FALSE]
  }
  if (nrow(panel) < 1L) {
    out$linking$probe <- probe
    return(out)
  }
  cache_rows <- lapply(seq_len(nrow(panel)), function(idx) {
    p <- .adaptive_link_predictive_prob_oriented(
      state = out,
      controller = .adaptive_controller_resolve(out),
      spoke_id = as.integer(spoke_id),
      A_id = as.character(panel$hub_item_id[[idx]]),
      B_id = as.character(panel$spoke_item_id[[idx]])
    )
    tibble::tibble(
      refit_id = as.integer(refit_id),
      spoke_id = as.integer(spoke_id),
      link_epoch_id = as.integer(panel$link_epoch_id[[idx]] %||% 1L),
      probe_panel_id = as.character(panel$probe_panel_id[[idx]] %||% NA_character_),
      hub_item_id = as.character(panel$hub_item_id[[idx]]),
      spoke_item_id = as.character(panel$spoke_item_id[[idx]]),
      pred_prob = as.double(p)
    )
  })
  probe$prediction_cache <- dplyr::bind_rows(
    probe$prediction_cache,
    dplyr::bind_rows(cache_rows)
  )
  out$linking$probe <- probe
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_realized_log_for_panel <- function(state, spoke_id, epoch_id, panel = NULL) {
  probe <- .adaptive_link_probe_state(state)
  panel <- tibble::as_tibble(panel %||% .adaptive_link_probe_panel_for_spoke(
    state,
    spoke_id = spoke_id,
    epoch_id = epoch_id
  ))
  if (nrow(panel) < 1L) {
    realized_edges <- tibble::as_tibble(probe$realized_edges %||% .adaptive_link_probe_empty_realized_log())
    return(realized_edges[0, , drop = FALSE])
  }
  realized_edges <- tibble::as_tibble(probe$realized_edges %||% .adaptive_link_probe_empty_realized_log())
  if (nrow(realized_edges) < 1L) {
    return(.adaptive_link_probe_empty_realized_log())
  }
  realized_edges <- realized_edges[
    as.integer(realized_edges$spoke_id) == as.integer(spoke_id) &
      as.integer(realized_edges$link_epoch_id) == as.integer(epoch_id) &
      as.character(realized_edges$pair_key) %in% as.character(panel$pair_key),
    ,
    drop = FALSE
  ]
  if (nrow(realized_edges) < 1L) {
    return(.adaptive_link_probe_empty_realized_log())
  }
  realized_edges[
    !duplicated(as.character(realized_edges$pair_key), fromLast = TRUE),
    ,
    drop = FALSE
  ]
}

#' @keywords internal
#' @noRd
.adaptive_link_apply_stop_state <- function(state, link_rows) {
  out <- state
  rows <- tibble::as_tibble(link_rows %||% tibble::tibble())
  if (nrow(rows) < 1L) {
    return(out)
  }
  if (!all(c("spoke_id", "link_stop_pass", "refit_id") %in% names(rows))) {
    return(out)
  }

  controller <- .adaptive_controller_resolve(out)
  stopped_map <- controller$link_stopped_by_spoke %||% list()
  stop_refit_map <- controller$link_stop_refit_id_by_spoke %||% list()
  stop_reason_map <- controller$link_stop_reason_by_spoke %||% list()
  frozen_map <- controller$link_transform_frozen_by_spoke %||% list()
  frozen_delta_map <- controller$link_transform_frozen_delta_by_spoke %||% list()
  frozen_log_alpha_map <- controller$link_transform_frozen_log_alpha_by_spoke %||% list()
  frozen_refit_map <- controller$link_transform_frozen_refit_id_by_spoke %||% list()
  state_map <- controller$link_transform_state_by_spoke %||% list()

  for (idx in seq_len(nrow(rows))) {
    spoke_id <- as.integer(rows$spoke_id[[idx]] %||% NA_integer_)
    if (is.na(spoke_id)) {
      next
    }
    key <- as.character(spoke_id)
    if (isTRUE(rows$link_stop_pass[[idx]])) {
      stopped_map[[key]] <- TRUE
      stop_refit_map[[key]] <- as.integer(rows$refit_id[[idx]] %||% NA_integer_)
      stop_reason_map[[key]] <- "link_stop_pass"
      if (!isTRUE(frozen_map[[key]])) {
        delta_val <- if ("delta_spoke_mean" %in% names(rows)) {
          as.double(rows$delta_spoke_mean[[idx]] %||% NA_real_)
        } else {
          NA_real_
        }
        log_alpha_val <- if ("log_alpha_spoke_mean" %in% names(rows)) {
          as.double(rows$log_alpha_spoke_mean[[idx]] %||% NA_real_)
        } else {
          NA_real_
        }
        state_val <- if ("link_transform_state" %in% names(rows)) {
          as.character(rows$link_transform_state[[idx]] %||% NA_character_)
        } else {
          NA_character_
        }
        frozen_map[[key]] <- TRUE
        frozen_refit_map[[key]] <- as.integer(rows$refit_id[[idx]] %||% NA_integer_)
        frozen_delta_map[[key]] <- delta_val
        frozen_log_alpha_map[[key]] <- log_alpha_val
        if (is.character(state_val) && length(state_val) == 1L && !is.na(state_val) && state_val != "") {
          state_map[[key]] <- state_val
        } else {
          state_map[[key]] <- as.character(state_map[[key]] %||% "shift_only")
        }
      }
    } else if (is.null(stopped_map[[key]])) {
      stopped_map[[key]] <- FALSE
    }
  }

  controller$link_stopped_by_spoke <- stopped_map
  controller$link_stop_refit_id_by_spoke <- stop_refit_map
  controller$link_stop_reason_by_spoke <- stop_reason_map
  controller$link_transform_frozen_by_spoke <- frozen_map
  controller$link_transform_frozen_delta_by_spoke <- frozen_delta_map
  controller$link_transform_frozen_log_alpha_by_spoke <- frozen_log_alpha_map
  controller$link_transform_frozen_refit_id_by_spoke <- frozen_refit_map
  controller$link_transform_state_by_spoke <- state_map
  out$controller <- controller
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_all_spokes_stopped <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(FALSE)
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(FALSE)
  }
  spoke_ids <- as.integer(phase_ctx$active_spokes %||% phase_ctx$ready_spokes %||% integer())
  spoke_ids <- sort(unique(spoke_ids[!is.na(spoke_ids)]))
  if (length(spoke_ids) < 1L) {
    return(FALSE)
  }
  stopped_map <- controller$link_stopped_by_spoke %||% list()
  all_stopped <- all(vapply(as.character(spoke_ids), function(key) isTRUE(stopped_map[[key]]), logical(1L)))
  if (!isTRUE(all_stopped)) {
    return(FALSE)
  }

  probe_cap <- as.integer(controller$probe_pairs_per_refit_per_spoke %||% 2L)
  probe_cap <- max(0L, probe_cap)
  if (probe_cap > 0L) {
    return(FALSE)
  }
  TRUE
}

#' @keywords internal
#' @noRd
.adaptive_link_effective_active_spokes <- function(state,
                                                   controller = NULL,
                                                   refit_id = NULL,
                                                   exclude_exhausted = FALSE) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(integer())
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(integer())
  }

  spoke_ids <- as.integer(phase_ctx$active_spokes %||% phase_ctx$ready_spokes %||% integer())
  spoke_ids <- sort(unique(spoke_ids[!is.na(spoke_ids)]))
  if (length(spoke_ids) < 1L) {
    return(integer())
  }

  stopped_map <- controller$link_stopped_by_spoke %||% list()
  keep <- vapply(as.character(spoke_ids), function(key) !isTRUE(stopped_map[[key]]), logical(1L))
  spoke_ids <- as.integer(spoke_ids[keep])
  if (length(spoke_ids) < 1L || !isTRUE(exclude_exhausted)) {
    return(as.integer(spoke_ids))
  }

  refit_id <- as.integer(refit_id %||% .adaptive_link_refit_window_id(state))
  if (is.na(refit_id)) {
    return(as.integer(spoke_ids))
  }
  exhausted_map <- .adaptive_link_refit_exhausted_map(state)
  stage_order <- .adaptive_stage_order()
  keep <- vapply(spoke_ids, function(spoke_id) {
    key <- .adaptive_link_refit_spoke_key(refit_id = refit_id, spoke_id = as.integer(spoke_id))
    exhausted <- exhausted_map[[key]] %||% list()
    !all(vapply(stage_order, function(stage_name) isTRUE(exhausted[[stage_name]]), logical(1L)))
  }, logical(1L))
  as.integer(spoke_ids[keep])
}

#' @keywords internal
#' @noRd
.adaptive_global_stop_allowed <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(TRUE)
  }

  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(FALSE)
  }
  if (length(as.integer(phase_ctx$pending_run_sets %||% integer())) > 0L) {
    return(FALSE)
  }

  isTRUE(.adaptive_link_all_spokes_stopped(state))
}

#' @keywords internal
#' @noRd
.adaptive_link_all_spokes_exhausted <- function(state, refit_id) {
  controller <- .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(FALSE)
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(FALSE)
  }
  active_spokes <- .adaptive_link_effective_active_spokes(
    state,
    controller = controller,
    refit_id = refit_id,
    exclude_exhausted = FALSE
  )
  if (length(active_spokes) < 1L) {
    return(FALSE)
  }
  exhausted_map <- .adaptive_link_refit_exhausted_map(state)
  stage_order <- .adaptive_stage_order()
  refit_id <- as.integer(refit_id %||% NA_integer_)
  if (is.na(refit_id)) {
    return(FALSE)
  }
  all(vapply(active_spokes, function(spoke_id) {
    key <- .adaptive_link_refit_spoke_key(refit_id = refit_id, spoke_id = as.integer(spoke_id))
    exhausted <- exhausted_map[[key]] %||% list()
    all(vapply(stage_order, function(stage_name) isTRUE(exhausted[[stage_name]]), logical(1L)))
  }, logical(1L)))
}

#' @keywords internal
#' @noRd
.adaptive_clear_stale_global_stop_state <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  if (!.adaptive_link_mode_active(controller)) {
    return(out)
  }
  if (isTRUE(.adaptive_global_stop_allowed(out))) {
    return(out)
  }

  out$meta <- out$meta %||% list()
  stop_reason <- as.character(out$meta$stop_reason %||% NA_character_)
  boundary_refit_id <- as.integer(out$meta$stop_boundary_refit_id %||% NA_integer_)
  boundary_step_id <- as.integer(out$meta$stop_boundary_step_id %||% NA_integer_)
  has_boundary <- is.finite(boundary_refit_id) || is.finite(boundary_step_id)
  stale_btl_stop <- identical(stop_reason, "btl_converged")
  if (!isTRUE(has_boundary) && !isTRUE(stale_btl_stop)) {
    return(out)
  }

  out$meta$stop_decision <- FALSE
  out$meta$stop_reason <- NA_character_
  out$meta$stop_boundary_refit_id <- NA_integer_
  out$meta$stop_boundary_step_id <- NA_integer_
  out$meta$pairs_committed_after_stop <- 0L
  out
}

#' @keywords internal
#' @noRd
.adaptive_stop_boundary_bootstrap <- function(state) {
  out <- state
  out$meta <- out$meta %||% list()
  out$meta$stop_boundary_refit_id <- as.integer(out$meta$stop_boundary_refit_id %||% NA_integer_)
  out$meta$stop_boundary_step_id <- as.integer(out$meta$stop_boundary_step_id %||% NA_integer_)
  out$meta$pairs_committed_after_stop <- as.integer(out$meta$pairs_committed_after_stop %||% 0L)
  if (!is.finite(out$meta$pairs_committed_after_stop) ||
    is.na(out$meta$pairs_committed_after_stop) ||
    out$meta$pairs_committed_after_stop < 0L) {
    out$meta$pairs_committed_after_stop <- 0L
  }

  boundary_step <- as.integer(out$meta$stop_boundary_step_id %||% NA_integer_)
  if (!is.na(boundary_step)) {
    step_log <- tibble::as_tibble(out$step_log %||% tibble::tibble())
    if (nrow(step_log) > 0L && all(c("step_id", "pair_id") %in% names(step_log))) {
      after_stop <- as.integer(step_log$step_id) > boundary_step & !is.na(step_log$pair_id)
      out$meta$pairs_committed_after_stop <- as.integer(sum(after_stop, na.rm = TRUE))
    } else {
      out$meta$pairs_committed_after_stop <- 0L
    }
  }

  out
}

#' @keywords internal
#' @noRd
.adaptive_stop_boundary_budget_status <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  max_pairs_after_stop <- as.integer(controller$max_pairs_after_stop %||% 0L)
  if (!is.finite(max_pairs_after_stop) || is.na(max_pairs_after_stop) || max_pairs_after_stop < 0L) {
    max_pairs_after_stop <- 0L
  }
  boundary_step <- as.integer(state$meta$stop_boundary_step_id %||% NA_integer_)
  pairs_after_stop <- as.integer(state$meta$pairs_committed_after_stop %||% 0L)
  if (!is.finite(pairs_after_stop) || is.na(pairs_after_stop) || pairs_after_stop < 0L) {
    pairs_after_stop <- 0L
  }
  active <- !is.na(boundary_step) && !isTRUE(state$meta$stop_decision %||% FALSE)
  exhausted <- isTRUE(active) && pairs_after_stop >= max_pairs_after_stop
  list(
    active = isTRUE(active),
    exhausted = isTRUE(exhausted),
    max_pairs_after_stop = as.integer(max_pairs_after_stop),
    pairs_after_stop = as.integer(pairs_after_stop)
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_abort_feasibility_failure <- function(refit_id,
                                                     spoke_id,
                                                     stage_name,
                                                     helper_name,
                                                     error) {
  rlang::abort(
    message = paste0(
      "Phase B feasibility computation failed before quota reduction at refit_id=",
      as.integer(refit_id %||% NA_integer_),
      ", spoke_id=",
      as.integer(spoke_id %||% NA_integer_),
      ", stage_name=`",
      as.character(stage_name %||% NA_character_),
      "`, helper=`",
      as.character(helper_name %||% NA_character_),
      "`. No feasibility-based quota reduction was authorized. Underlying error: ",
      conditionMessage(error)
    ),
    parent = error
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_stage_feasibility_snapshot <- function(state, controller, spoke_id, stage_order) {
  round <- state$round %||% list()
  defaults <- adaptive_defaults(as.integer(state$n_items))
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, as.character(state$item_ids))
  recent_deg <- .adaptive_recent_deg(history, as.character(state$item_ids), defaults$W_cap)
  link_controller <- controller
  link_controller$current_link_spoke_id <- as.integer(spoke_id)
  refit_id <- as.integer(.adaptive_link_refit_window_id(state))
  feasible_counts <- stats::setNames(rep.int(0L, length(stage_order)), stage_order)
  feasible_utility_mass <- stats::setNames(rep.int(0, length(stage_order)), stage_order)

  for (idx in seq_along(stage_order)) {
    stage_name <- as.character(stage_order[[idx]])
    stage_seed <- 1000L + (37L * idx) + (1009L * as.integer(spoke_id))
    generated <- tryCatch(
      generate_stage_candidates_from_state(
        state = state,
        stage_name = stage_name,
        fallback_name = "base",
        C_max = defaults$C_max,
        seed = stage_seed,
        link_spoke_id = as.integer(spoke_id)
      ),
      error = function(e) {
        .adaptive_link_abort_feasibility_failure(
          refit_id = refit_id,
          spoke_id = as.integer(spoke_id),
          stage_name = stage_name,
          helper_name = "generate_stage_candidates_from_state",
          error = e
        )
      }
    )
    filtered <- tryCatch(
      .adaptive_filter_link_backfill_candidates(
        candidates = generated,
        counts = counts,
        round = round,
        recent_deg = recent_deg,
        defaults = defaults
      ),
      error = function(e) {
        .adaptive_link_abort_feasibility_failure(
          refit_id = refit_id,
          spoke_id = as.integer(spoke_id),
          stage_name = stage_name,
          helper_name = ".adaptive_filter_link_backfill_candidates",
          error = e
        )
      }
    )
    cand <- tibble::as_tibble(filtered$candidates)
    feasible_counts[[stage_name]] <- as.integer(nrow(cand))
    if (nrow(cand) < 1L) {
      next
    }
    if (!"p" %in% names(cand)) {
      cand$p <- rep(0.5, nrow(cand))
    }
    if (!"u0" %in% names(cand)) {
      cand$u0 <- rep(0, nrow(cand))
    }
    cand <- tryCatch(
      .adaptive_link_attach_predictive_utility(
        candidates = cand,
        state = state,
        controller = link_controller,
        spoke_id = as.integer(spoke_id)
      ),
      error = function(e) {
        .adaptive_link_abort_feasibility_failure(
          refit_id = refit_id,
          spoke_id = as.integer(spoke_id),
          stage_name = stage_name,
          helper_name = ".adaptive_link_attach_predictive_utility",
          error = e
        )
      }
    )
    utility_col <- .adaptive_resolve_selection_column("linking_d_optimal")
    utility_vals <- if (!is.na(utility_col) && utility_col %in% names(cand)) {
      as.double(cand[[utility_col]])
    } else {
      rep_len(0, nrow(cand))
    }
    utility_vals[!is.finite(utility_vals) | utility_vals < 0] <- 0
    feasible_utility_mass[[stage_name]] <- as.double(sum(utility_vals))
  }

  list(
    feasible_counts = feasible_counts,
    feasible_utility_mass = feasible_utility_mass
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_adjust_stage_quotas_for_feasibility <- function(state,
                                                               controller,
                                                               spoke_id,
                                                               stage_quotas,
                                                               stage_order,
                                                               refit_id = NULL) {
  quotas <- as.integer(stage_quotas[stage_order])
  names(quotas) <- as.character(stage_order)
  quotas[!is.finite(quotas)] <- 0L
  meta <- attr(stage_quotas, "quota_meta") %||% list()
  refit_id <- as.integer(refit_id %||% .adaptive_link_refit_window_id(state))
  if (is.na(as.integer(spoke_id)) || sum(quotas, na.rm = TRUE) < 1L) {
    meta$feasibility_reallocation_used <- FALSE
    meta$feasibility_reallocation_rule <- "none"
    meta$feasible_stage_capacity_anchor_link <- as.integer(quotas[["anchor_link"]] %||% 0L)
    meta$feasible_stage_capacity_long_link <- as.integer(quotas[["long_link"]] %||% 0L)
    meta$feasible_stage_capacity_mid_link <- as.integer(quotas[["mid_link"]] %||% 0L)
    meta$feasible_stage_capacity_local_link <- as.integer(quotas[["local_link"]] %||% 0L)
    attr(quotas, "quota_meta") <- meta
    return(quotas)
  }

  snapshot <- .adaptive_link_stage_feasibility_snapshot(
    state = state,
    controller = controller,
    spoke_id = as.integer(spoke_id),
    stage_order = stage_order
  )
  feasible_counts <- as.integer(snapshot$feasible_counts[stage_order])
  names(feasible_counts) <- stage_order
  feasible_counts[!is.finite(feasible_counts)] <- 0L
  utility_mass <- as.double(snapshot$feasible_utility_mass[stage_order])
  names(utility_mass) <- stage_order
  utility_mass[!is.finite(utility_mass)] <- 0
  if (sum(feasible_counts, na.rm = TRUE) < 1L) {
    meta$refit_id <- as.integer(refit_id)
    meta$feasibility_reallocation_used <- FALSE
    meta$feasibility_reallocation_rule <- "none"
    meta$feasible_stage_capacity_anchor_link <- as.integer(feasible_counts[["anchor_link"]] %||% 0L)
    meta$feasible_stage_capacity_long_link <- as.integer(feasible_counts[["long_link"]] %||% 0L)
    meta$feasible_stage_capacity_mid_link <- as.integer(feasible_counts[["mid_link"]] %||% 0L)
    meta$feasible_stage_capacity_local_link <- as.integer(feasible_counts[["local_link"]] %||% 0L)
    meta$feasibility_budget_released <- 0L
    attr(quotas, "quota_meta") <- meta
    return(quotas)
  }

  adjusted <- pmin(quotas, feasible_counts)
  adjusted[!is.finite(adjusted)] <- 0L
  released <- as.integer(sum(pmax(0L, quotas - adjusted), na.rm = TRUE))
  slack <- pmax(0L, feasible_counts - adjusted)
  names(slack) <- stage_order
  slack[!is.finite(slack)] <- 0L
  weights <- utility_mass
  weights[!is.finite(weights) | weights < 0] <- 0
  blocker_weights <- .adaptive_link_blocker_weights_for_spoke(
    controller = controller,
    spoke_id = as.integer(spoke_id)
  )
  stage_weights <- .adaptive_link_blocker_stage_weights(
    blocker_weights = blocker_weights,
    linking_identified = isTRUE(meta$linking_identified %||% FALSE)
  )
  weights <- pmax(weights, as.double(slack))
  if (isTRUE(meta$linking_identified %||% FALSE)) {
    stage_weights[["anchor_link"]] <- stage_weights[["anchor_link"]] * 0.70
    stage_weights[["long_link"]] <- stage_weights[["long_link"]] * 0.75
    stage_weights[["mid_link"]] <- stage_weights[["mid_link"]] * 1.20
    stage_weights[["local_link"]] <- stage_weights[["local_link"]] * 1.35
  }
  names(weights) <- stage_order
  weights <- weights * stage_weights[names(weights)]

  remaining <- as.integer(released %||% 0L)
  if (length(remaining) != 1L || !is.finite(remaining) || is.na(remaining) || remaining < 1L) {
    remaining <- 0L
  }
  while (isTRUE(remaining > 0L) && isTRUE(any(slack > 0L, na.rm = TRUE))) {
    eligible <- stage_order[slack[stage_order] > 0L]
    if (length(eligible) < 1L) {
      break
    }
    score <- as.double(weights[eligible])
    score[!is.finite(score)] <- 0
    eligible <- eligible[order(-score, match(eligible, stage_order))]
    allocated <- FALSE
    for (stage_name in eligible) {
      if (!isTRUE(remaining > 0L) || !isTRUE(slack[stage_name] > 0L)) {
        next
      }
      adjusted[stage_name] <- as.integer(adjusted[stage_name] + 1L)
      slack[stage_name] <- as.integer(slack[stage_name] - 1L)
      remaining <- as.integer(remaining - 1L)
      allocated <- TRUE
      if (!isTRUE(remaining > 0L)) {
        break
      }
    }
    if (!isTRUE(allocated)) {
      break
    }
  }

  meta$refit_id <- as.integer(refit_id)
  meta$feasibility_reallocation_used <- isTRUE(released > 0L)
  meta$feasibility_reallocation_rule <- if (released > 0L) {
    "pooled_utility_backfill"
  } else {
    "none"
  }
  meta$feasible_stage_capacity_anchor_link <- as.integer(feasible_counts[["anchor_link"]] %||% 0L)
  meta$feasible_stage_capacity_long_link <- as.integer(feasible_counts[["long_link"]] %||% 0L)
  meta$feasible_stage_capacity_mid_link <- as.integer(feasible_counts[["mid_link"]] %||% 0L)
  meta$feasible_stage_capacity_local_link <- as.integer(feasible_counts[["local_link"]] %||% 0L)
  meta$feasibility_budget_released <- as.integer(released)
  attr(adjusted, "quota_meta") <- meta
  adjusted
}

#' @keywords internal
#' @noRd
.adaptive_link_stage_progress <- function(state,
                                         spoke_id,
                                         stage_quotas,
                                         stage_order,
                                         refit_id = NULL,
                                         adjust_for_feasibility = TRUE) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  controller <- .adaptive_controller_resolve(state)
  stage_order <- as.character(stage_order %||% .adaptive_stage_order())
  quota_meta <- attr(stage_quotas, "quota_meta") %||% list()
  stage_quotas <- as.integer(stage_quotas[stage_order])
  names(stage_quotas) <- stage_order
  attr(stage_quotas, "quota_meta") <- quota_meta
  if (isTRUE(adjust_for_feasibility)) {
    stage_quotas <- .adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      stage_quotas = stage_quotas,
      stage_order = stage_order,
      refit_id = refit_id
    )
  }
  committed_actual <- stats::setNames(rep.int(0L, length(stage_order)), stage_order)
  refit_id <- as.integer(refit_id %||% .adaptive_link_refit_window_id(state))
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
  if (nrow(step_log) > 0L &&
    all(c("pair_id", "is_cross_set", "link_spoke_id", "step_id") %in% names(step_log))) {
    stage_col <- if ("link_stage" %in% names(step_log)) "link_stage" else "round_stage"
    rows <- step_log[
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
        as.integer(step_log$step_id) > last_refit_step &
        as.character(step_log[[stage_col]]) %in% stage_order,
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      tab <- table(factor(as.character(rows[[stage_col]]), levels = stage_order))
      committed_actual[names(tab)] <- as.integer(tab)
    }
  }

  committed <- committed_actual
  exhausted_map <- .adaptive_link_refit_exhausted_map(state)
  key <- .adaptive_link_refit_spoke_key(refit_id = refit_id, spoke_id = spoke_id)
  exhausted_stage <- exhausted_map[[key]] %||% list()
  for (stage in stage_order) {
    if (isTRUE(exhausted_stage[[stage]])) {
      committed[[stage]] <- max(committed[[stage]], stage_quotas[[stage]])
    }
  }

  deficits <- pmax(0L, stage_quotas - committed)
  deficits[!is.finite(deficits)] <- 0L
  committed[!is.finite(committed)] <- 0L
  committed_actual[!is.finite(committed_actual)] <- 0L
  stage_quotas[!is.finite(stage_quotas)] <- 0L
  backfill_active <- sum(committed_actual, na.rm = TRUE) < sum(stage_quotas, na.rm = TRUE) &&
    !any(deficits > 0L, na.rm = TRUE)
  active_stage <- if (isTRUE(backfill_active)) {
    "pooled_backfill"
  } else if (any(deficits > 0L, na.rm = TRUE)) {
    stage_order[[which(deficits > 0L)[[1L]]]]
  } else {
    stage_order[[length(stage_order)]]
  }

  list(
    active_stage = as.character(active_stage),
    backfill_active = isTRUE(backfill_active),
    stage_realized = committed_actual,
    stage_committed = committed,
    stage_quotas = stage_quotas,
    budget_remaining_actual = as.integer(max(0L, sum(stage_quotas, na.rm = TRUE) -
      sum(committed_actual, na.rm = TRUE)))
  )
}

#' @keywords internal
#' @noRd
.adaptive_round_active_stage <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    round <- state$round %||% NULL
    if (is.null(round) || !isTRUE(round$staged_active)) {
      return("warm_start")
    }
    idx <- as.integer(round$stage_index %||% 1L)
    order <- as.character(round$stage_order %||% .adaptive_stage_order())
    if (idx < 1L || idx > length(order)) {
      return(NA_character_)
    }
    return(order[[idx]])
  }

  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (.adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")) {
    eligible_spokes <- .adaptive_link_effective_active_spokes(
      state,
      controller = controller,
      refit_id = .adaptive_link_refit_window_id(state),
      exclude_exhausted = TRUE
    )
    budget_map <- .adaptive_link_budget_map_for_refit(
      state = state,
      controller = controller,
      eligible_spoke_ids = eligible_spokes
    )
    spoke_id <- .adaptive_link_active_spoke(
      state = state,
      controller = controller,
      eligible_spoke_ids = eligible_spokes
    )
    if (!is.na(spoke_id)) {
      refit_id <- .adaptive_link_refit_window_id(state)
      quota_controller <- controller
      quota_controller$current_link_spoke_id <- as.integer(spoke_id)
      quota_controller$B_spoke_refit_budget <- as.integer(
        budget_map[[as.character(spoke_id)]]$B_spoke_refit_budget %||% NA_integer_
      )
      quota_controller$B_spoke_refit_budget_source <- as.character(
        budget_map[[as.character(spoke_id)]]$B_spoke_refit_budget_source %||% "single_spoke_default"
      )
      stage_quotas <- .adaptive_round_compute_quotas(
        round_id = as.integer(state$round$round_id %||% 1L),
        n_items = as.integer(state$n_items),
        controller = quota_controller
      )
      progress <- .adaptive_link_stage_progress(
        state = state,
        spoke_id = spoke_id,
        stage_quotas = stage_quotas,
        stage_order = .adaptive_stage_order(),
        refit_id = refit_id
      )
      return(as.character(progress$active_stage))
    }
  }

  round <- state$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return("warm_start")
  }
  idx <- as.integer(round$stage_index %||% 1L)
  order <- as.character(round$stage_order %||% .adaptive_stage_order())
  if (idx < 1L || idx > length(order)) {
    return(NA_character_)
  }
  order[[idx]]
}

#' @keywords internal
#' @noRd
.adaptive_round_advance_stage <- function(state, shortfall = 0L) {
  out <- state
  round <- out$round
  idx <- as.integer(round$stage_index %||% 1L)
  order <- as.character(round$stage_order %||% .adaptive_stage_order())
  if (idx < 1L || idx > length(order)) {
    return(out)
  }
  stage <- order[[idx]]
  round$stage_shortfalls[[stage]] <- as.integer(
    (round$stage_shortfalls[[stage]] %||% 0L) + as.integer(shortfall %||% 0L)
  )
  round$stage_index <- as.integer(idx + 1L)
  out$round <- round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_start_next <- function(state) {
  out <- state
  out$controller <- .adaptive_controller_with_phase_scope(out, controller = .adaptive_controller_resolve(out))
  phase_ctx <- .adaptive_link_phase_context(out, controller = out$controller)
  out$controller$link_phase <- as.character(phase_ctx$phase %||% "phase_a")
  prior <- out$round %||% list(round_id = 0L, committed_total = 0L)
  out$refit_meta$last_completed_round_summary <- list(
    round_id = as.integer(prior$round_id %||% NA_integer_),
    global_identified = as.logical(prior$global_identified %||% NA),
    long_quota_raw = as.integer(prior$long_quota_raw %||% NA_integer_),
    long_quota_effective = as.integer(prior$long_quota_effective %||% NA_integer_),
    long_quota_removed = as.integer(prior$long_quota_removed %||% NA_integer_),
    realloc_to_mid = as.integer(prior$realloc_to_mid %||% NA_integer_),
    realloc_to_local = as.integer(prior$realloc_to_local %||% NA_integer_)
  )
  next_id <- as.integer((prior$round_id %||% 0L) + 1L)
  next_round <- .adaptive_new_round_state(
    item_ids = out$item_ids,
    round_id = next_id,
    staged_active = TRUE,
    controller = out$controller
  )
  next_round$committed_total <- as.integer(prior$committed_total %||% 0L)
  out$round <- next_round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_commit <- function(state, step_row) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return(out)
  }
  is_adaptive <- inherits(out, "adaptive_state")
  if (isTRUE(is_adaptive)) {
    controller <- .adaptive_controller_resolve(out)
    phase_ctx <- .adaptive_link_phase_context(out, controller = controller)
  } else {
    controller <- list(run_mode = "within_set")
    phase_ctx <- list(phase = "phase_a")
  }

  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  if (is.na(stage) || !stage %in% round$stage_order) {
    return(out)
  }
  round$committed_total <- as.integer((round$committed_total %||% 0L) + 1L)
  round$round_committed <- as.integer((round$round_committed %||% 0L) + 1L)
  if (!(.adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b"))) {
    round$stage_committed[[stage]] <- as.integer((round$stage_committed[[stage]] %||% 0L) + 1L)
  }

  A <- as.integer(step_row$A[[1L]] %||% NA_integer_)
  B <- as.integer(step_row$B[[1L]] %||% NA_integer_)
  ids <- out$item_ids
  if (!is.na(A) && A >= 1L && A <= length(ids)) {
    a_id <- as.character(ids[[A]])
    a_prev <- as.integer(round$per_round_item_uses[[a_id]] %||% 0L)
    round$per_round_item_uses[[a_id]] <- as.integer((round$per_round_item_uses[[a_id]] %||% 0L) + 1L)
  } else {
    a_prev <- 0L
  }
  if (!is.na(B) && B >= 1L && B <= length(ids)) {
    b_id <- as.character(ids[[B]])
    b_prev <- as.integer(round$per_round_item_uses[[b_id]] %||% 0L)
    round$per_round_item_uses[[b_id]] <- as.integer((round$per_round_item_uses[[b_id]] %||% 0L) + 1L)
  } else {
    b_prev <- 0L
  }

  repeat_item_uses <- as.integer((a_prev > 0L) + (b_prev > 0L))
  if (repeat_item_uses > 0L) {
    round$repeat_in_round_used <- as.integer((round$repeat_in_round_used %||% 0L) + repeat_item_uses)
  }
  star_override_used <- FALSE
  if ("star_override_used" %in% names(step_row)) {
    star_override_used <- isTRUE(step_row$star_override_used[[1L]] %||% FALSE)
  }
  if (isTRUE(star_override_used)) {
    round$star_override_used <- as.integer((round$star_override_used %||% 0L) + 1L)
  }

  if (.adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")) {
    out$round <- round
    if ((round$round_committed %||% 0L) >= (round$round_pairs_target %||% 0L)) {
      out <- .adaptive_round_start_next(out)
    }
    return(out)
  }

  quota <- as.integer(round$stage_quotas[[stage]] %||% 0L)
  done <- as.integer(round$stage_committed[[stage]] %||% 0L)
  if (done >= quota) {
    out$round <- round
    out <- .adaptive_round_advance_stage(out, shortfall = 0L)
    round <- out$round
  } else {
    out$round <- round
  }

  stage_count <- length(round$stage_order %||% .adaptive_stage_order())
  if ((round$stage_index %||% 1L) > stage_count ||
    (round$round_committed %||% 0L) >= (round$round_pairs_target %||% 0L)) {
    out <- .adaptive_round_start_next(out)
  }

  out
}

#' @keywords internal
#' @noRd
.adaptive_round_commit_warm_start <- function(state) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round)) {
    return(out)
  }
  round$committed_total <- as.integer((round$committed_total %||% 0L) + 1L)
  round$round_committed <- as.integer((round$round_committed %||% 0L) + 1L)
  out$round <- round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_starvation <- function(state, step_row) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return(list(state = out, exhausted = TRUE))
  }
  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  is_adaptive <- inherits(out, "adaptive_state")
  controller <- if (isTRUE(is_adaptive)) .adaptive_controller_resolve(out) else list(run_mode = "within_set")
  phase_ctx <- if (isTRUE(is_adaptive)) {
    .adaptive_link_phase_context(out, controller = controller)
  } else {
    list(phase = "phase_a")
  }
  stage_order <- as.character(round$stage_order %||% .adaptive_stage_order())
  mark_phase_b_spoke_exhaustion <- function(state, controller, phase_ctx, stage_name, mark_all_stages = FALSE) {
    starvation_reason <- if ("starvation_reason" %in% names(step_row)) {
      as.character(step_row$starvation_reason[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    concurrent_mode <- identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
    spokes_to_mark <- as.integer()
    if (isTRUE(concurrent_mode) && identical(starvation_reason, "all_eligible_spokes_infeasible")) {
      spokes_to_mark <- as.integer(phase_ctx$active_spokes %||% integer())
    } else {
      spoke_id <- as.integer(step_row$link_spoke_id[[1L]] %||% NA_integer_)
      if (is.na(spoke_id)) {
        spoke_id <- as.integer(controller$current_link_spoke_id %||% NA_integer_)
      }
      if (!is.na(spoke_id)) {
        spokes_to_mark <- as.integer(spoke_id)
      }
    }
    if (length(spokes_to_mark) < 1L) {
      return(list(state = state, exhausted = TRUE))
    }

    out <- state
    out$controller <- controller
    refit_id <- .adaptive_link_refit_window_id(out)
    shortfalls <- .adaptive_link_refit_shortfalls_map(out)
    exhausted_map <- .adaptive_link_refit_exhausted_map(out)
    budget_map <- .adaptive_link_budget_map_for_refit(
      state = out,
      controller = controller,
      eligible_spoke_ids = unique(as.integer(spokes_to_mark))
    )
    for (spoke_id in unique(as.integer(spokes_to_mark))) {
      quota_controller <- controller
      quota_controller$current_link_spoke_id <- as.integer(spoke_id)
      quota_controller$B_spoke_refit_budget <- as.integer(
        budget_map[[as.character(spoke_id)]]$B_spoke_refit_budget %||% NA_integer_
      )
      quota_controller$B_spoke_refit_budget_source <- as.character(
        budget_map[[as.character(spoke_id)]]$B_spoke_refit_budget_source %||% "single_spoke_default"
      )
      stage_quotas <- .adaptive_round_compute_quotas(
        round_id = as.integer(round$round_id %||% 1L),
        n_items = as.integer(out$n_items),
        controller = quota_controller
      )
      progress <- .adaptive_link_stage_progress(
        state = out,
        spoke_id = as.integer(spoke_id),
        stage_quotas = stage_quotas,
        stage_order = stage_order,
        refit_id = refit_id,
        adjust_for_feasibility = FALSE
      )
      key <- .adaptive_link_refit_spoke_key(refit_id = refit_id, spoke_id = as.integer(spoke_id))
      existing_shortfall <- shortfalls[[key]] %||% list()
      existing_exhausted <- exhausted_map[[key]] %||% list()
      stages_to_mark <- if (isTRUE(mark_all_stages)) stage_order else as.character(stage_name)
      for (stage_name_i in stages_to_mark) {
        shortfall <- max(
          0L,
          as.integer(progress$stage_quotas[[stage_name_i]] %||% 0L) -
            as.integer(progress$stage_committed[[stage_name_i]] %||% 0L)
        )
        existing_shortfall[[stage_name_i]] <- as.integer(
          (existing_shortfall[[stage_name_i]] %||% 0L) + shortfall
        )
        existing_exhausted[[stage_name_i]] <- TRUE
      }
      shortfalls[[key]] <- existing_shortfall
      exhausted_map[[key]] <- existing_exhausted
    }
    out$refit_meta$link_stage_shortfalls_by_refit_spoke <- shortfalls
    out$refit_meta$link_stage_exhausted_by_refit_spoke <- exhausted_map
    if (length(unique(as.integer(spokes_to_mark))) == 1L) {
      out$controller$current_link_spoke_id <- as.integer(unique(as.integer(spokes_to_mark))[[1L]])
    }
    out$round <- round
    list(state = out, exhausted = FALSE)
  }
  if (isTRUE(is_adaptive) && .adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")) {
    if (identical(stage, "pooled_backfill")) {
      return(mark_phase_b_spoke_exhaustion(
        state = out,
        controller = controller,
        phase_ctx = phase_ctx,
        stage_name = stage,
        mark_all_stages = TRUE
      ))
    }
  }
  if (is.na(stage) || !stage %in% stage_order) {
    return(list(state = out, exhausted = TRUE))
  }
  if (isTRUE(is_adaptive) && .adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")) {
    return(mark_phase_b_spoke_exhaustion(
      state = out,
      controller = controller,
      phase_ctx = phase_ctx,
      stage_name = stage,
      mark_all_stages = FALSE
    ))
  }

  shortfall <- max(0L, as.integer(round$stage_quotas[[stage]] %||% 0L) -
    as.integer(round$stage_committed[[stage]] %||% 0L))
  out <- .adaptive_round_advance_stage(out, shortfall = shortfall)
  stage_count <- length(out$round$stage_order %||% .adaptive_stage_order())
  exhausted <- (out$round$stage_index %||% 1L) > stage_count
  list(state = out, exhausted = exhausted)
}

#' Adaptive ranking
#'
#' @description
#' Initialize an adaptive ranking session and canonical state object.
#'
#' @details
#' This function creates the stepwise controller state and seeds all canonical
#' logs used in the adaptive pairing workflow. Warm start pair construction
#' follows the shuffled chain design, which guarantees a connected comparison
#' graph after \eqn{N - 1} committed comparisons.
#'
#' Pair selection in this framework is stepwise and uncertainty-aware.
#' Within-set routing uses TrueSkill base utility
#' \deqn{U_0 = p_{ij}(1 - p_{ij})} where \eqn{p_{ij}} is the current TrueSkill
#' win probability for pair \eqn{\{i, j\}}. In linking Phase B, anchor/strata
#' routing uses a linking-global score derived from Phase A raw summaries plus
#' the current spoke transform (\eqn{\delta_s}, optional \eqn{\log \alpha_s}).
#' In linking Phase B, eligible cross-set candidates are ranked by
#' ridge-stabilized D-optimal log-det information gain on spoke transform
#' parameters using order-averaged Model D probabilities. Linking inference
#' parameters are used for
#' inference/diagnostics/stopping, not as direct selection objectives.
#' When \code{judge_param_mode = "phase_specific"}, the first Phase B startup
#' step may use deterministic fallback from available within/shared judge
#' estimates if link-specific estimates are not yet available; once link-specific
#' estimates are expected, missing/non-finite values abort.
#' Bayesian BTL posterior draws are not used as general pair-selection
#' objectives; within-set pairing remains TrueSkill-routed, with accepted
#' posterior refits contributing only to the long-link probability gate.
#' Linking transform refits use Bayesian posterior estimation and posterior
#' summaries/diagnostics are logged per spoke at each linking refit.
#'
#' The returned state contains canonical logs:
#' \itemize{
#'   \item \code{step_log}: one row per attempted step,
#'   \item \code{round_log}: one row per posterior refit,
#'   \item \code{item_log}: per-item posterior summaries by refit.
#' }
#' If \code{session_dir} is supplied, the initialized state is persisted
#' immediately using [save_adaptive_session()].
#'
#' @param items A vector or data frame of items. Data frames must include an
#'   `item_id` column (or `id`/`ID`). For linking run modes, items must also
#'   include integer `set_id` values and globally unique `global_item_id`
#'   values. Item IDs may be character; internal logs use integer indices
#'   derived from these IDs.
#' @param seed Integer seed used for deterministic warm-start shuffling and
#'   selection randomness. Default is `1L`.
#' @param adaptive_config Optional named list of adaptive controller overrides.
#'   Unknown fields and invalid values abort with an actionable error. See
#'   [adaptive_rank()] for the full list of supported keys, detailed semantics,
#'   and defaults.
#' @param session_dir Optional directory for saving session artifacts.
#'   Default is `NULL`.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#'   Default is `FALSE`.
#' @param ... Internal/testing only. Supply `now_fn` to override the clock used
#'   for timestamps.
#'
#' @return An adaptive state object containing `step_log`, `round_log`, and
#'   `item_log`. The object includes class \code{"adaptive_state"}, item ID
#'   mappings, TrueSkill state, warm-start queue, refit metadata, and runtime
#'   configuration.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 11)
#' summarize_adaptive(state)
#'
#' @seealso [adaptive_rank_run_live()], [adaptive_rank_resume()],
#'   [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_start <- function(items,
                                seed = 1L,
                                session_dir = NULL,
                                persist_item_log = FALSE,
                                ...,
                                adaptive_config = NULL) {
  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(dot_names == "")) {
      rlang::abort("Only named `now_fn` is supported in `...` for now.")
    }
    bad <- setdiff(dot_names, "now_fn")
    if (length(bad) > 0L) {
      rlang::abort("Only `now_fn` is supported in `...` for now.")
    }
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.logical(persist_item_log) ||
    length(persist_item_log) != 1L ||
    is.na(persist_item_log)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }
  seed <- .adaptive_validate_seed(seed)
  now_fn <- dots$now_fn %||% function() Sys.time()
  state <- new_adaptive_state(items, now_fn = now_fn)
  state$meta$seed <- seed
  state$warm_start_pairs <- .adaptive_build_warm_start_pairs(state$item_ids, seed)
  state$warm_start_idx <- 1L
  state$warm_start_done <- nrow(state$warm_start_pairs) == 0L
  state <- .adaptive_apply_controller_config(state, adaptive_config = adaptive_config)
  state$controller <- .adaptive_controller_with_phase_scope(state, controller = .adaptive_controller_resolve(state))
  state <- .adaptive_phase_a_prepare(state)
  state <- .adaptive_phase_a_finalize_if_ready(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = state$controller)
  state$controller$link_phase <- as.character(phase_ctx$phase %||% "phase_a")
  state$controller <- .adaptive_controller_with_phase_scope(state, controller = state$controller)
  state$round <- .adaptive_new_round_state(
    item_ids = state$item_ids,
    round_id = 1L,
    staged_active = isTRUE(state$warm_start_done),
    controller = state$controller
  )
  state$config$session_dir <- session_dir %||% NULL
  state$config$persist_item_log <- isTRUE(persist_item_log)
  if (!is.null(session_dir)) {
    save_adaptive_session(state, session_dir = session_dir, overwrite = FALSE)
  }
  state
}

#' Adaptive ranking live runner
#'
#' @description
#' Execute stepwise adaptive ranking with a user-supplied judge.
#'
#' @details
#' Each iteration attempts at most one pair evaluation ("one-pair step"), then
#' applies transactional updates if and only if the judge response is valid.
#' Invalid responses produce a logged step with
#' \code{pair_id = NA} and must not update committed-comparison state.
#'
#' Within-set routing is TrueSkill-based with utility
#' \deqn{U_0 = p_{ij}(1 - p_{ij})}.
#' After an accepted posterior refit is available, the long-link gate uses the
#' BTL posterior win probability for candidate eligibility; before that it
#' falls back deterministically to TrueSkill.
#' In linking Phase B, anchor/strata routing uses linking-global scores built
#' from Phase A raw summaries and the current spoke transform.
#' Linking Phase B routing ranks eligible cross-set candidates by
#' ridge-stabilized D-optimal log-det information gain on spoke transform
#' parameters using order-averaged Model D probabilities. Linking inference
#' parameters remain inference-only
#' (diagnostics and stopping) and are not direct pair-selection objectives.
#' When \code{judge_param_mode = "phase_specific"}, startup can use deterministic
#' fallback from within/shared judge estimates only until link-specific estimates
#' are expected, after which malformed link estimates abort.
#' In linking \code{joint_refit} mode, hub+spoke item abilities and transform
#' parameters are estimated together for the active hub+spoke graph, with hub
#' behavior controlled by \code{hub_lock_mode} (\code{hard_lock}
#' or \code{soft_lock}); \code{soft_lock} uses
#' \code{hub_lock_kappa}-scaled regularization to Phase A hub summaries.
#' Exploration/exploitation routing and fallback handling are recorded in
#' \code{step_log}.
#'
#' Round scheduling uses stage-specific admissibility:
#' \itemize{
#'   \item rolling-anchor links compare one anchor and one non-anchor endpoint;
#'   \item long/mid links exclude anchor endpoints and enforce stratum-distance
#'   bounds;
#'   \item local-link routing admits same-stratum pairs and anchor-involving
#'   pairs within local stage bounds.
#' }
#'
#' Exposure and repeat handling are soft, stage-local constraints:
#' under-represented exploration uses degree set `deg <= D_min + 1`, while
#' repeat-pressure gating uses bottom-quantile `recent_deg` (default quantile
#' `0.25`) and per-endpoint repeat-slot accounting against
#' `repeat_in_round_budget`.
#'
#' Top-band defaults for stratum construction are
#' `top_band_pct = 0.10` and `top_band_bins = 5`, with top-band size
#' `ceiling(top_band_pct * N)`.
#'
#' Bayesian BTL refits are triggered on step-based cadence and evaluated with
#' diagnostics gates (including ESS thresholds), reliability, and lagged
#' stability criteria. Refit-level outcomes are
#' appended to \code{round_log}; per-item posterior summaries are appended to
#' \code{item_log}. Controller behavior can change after refits via
#' identifiability-gated settings in \code{adaptive_config}; those controls
#' affect pair routing and quotas, while BTL remains inference-only.
#' If \code{adaptive_config$max_pairs_after_stop > 0}, the run records a stop
#' boundary at the first refit with \code{stop_decision = TRUE} and allows at
#' most that many additional committed comparisons before deterministic
#' termination. Round logs record
#' \code{max_pairs_after_stop} and \code{pairs_committed_after_stop}.
#'
#' @param state An adaptive state object created by [adaptive_rank_start()].
#' @param judge A function called as `judge(A, B, state, ...)` that returns a
#'   list with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
#'   `invalid_reason`.
#' @param n_steps Maximum number of attempted adaptive steps to execute in this
#'   call. The run may terminate earlier if candidate starvation is encountered
#'   or if BTL stopping criteria are met at a refit. Each attempted step counts
#'   toward this budget, including invalid judge responses.
#' @param fit_fn Optional BTL fit function for deterministic testing; defaults
#'   to `default_btl_fit_fn()` when a refit is due.
#' @param adaptive_config Optional named list overriding adaptive controller
#'   behavior. Unknown fields and invalid values abort with an actionable error.
#'   See [adaptive_rank()] for the full list of supported keys, detailed
#'   semantics, and defaults.
#' @param btl_config Optional named list overriding BTL refit cadence, stopping
#'   thresholds, and selected round-log diagnostics. Supported fields:
#'   \describe{
#'   \item{`refit_pairs_target`}{Minimum new committed comparisons required
#'   before the next BTL refit. Default is `ceiling(N / 2)` clamped to
#'   `[20L, 5000L]` (Phase A linking uses the active set size).}
#'   \item{`model_variant`}{BTL MCMC variant: `"btl"`, `"btl_e"`, `"btl_b"`,
#'   or `"btl_e_b"`. Default is `"btl_e_b"`.}
#'   \item{`ess_bulk_min`}{Minimum bulk ESS required for diagnostics to pass.
#'   Default is `max(400, round(20 * sqrt(N)))`.}
#'   \item{`ess_bulk_min_near_stop`}{Stricter ESS requirement when a run is
#'   close to stopping. Default is `max(1000, round(50 * sqrt(N)))`.}
#'   \item{`max_rhat`}{Maximum allowed split-\eqn{\hat{R}} diagnostic value.
#'   Default is `1.01`.}
#'   \item{`divergences_max`}{Maximum allowed divergent transitions. Default is
#'   `0L`.}
#'   \item{`eap_reliability_min`}{Minimum EAP reliability to allow stopping.
#'   Default is `0.90`.}
#'   \item{`stability_lag`}{Lag (in refits) used for stability checks. Default
#'   is `2L`.}
#'   \item{`theta_corr_min`}{Minimum lagged correlation of posterior means.
#'   Default is `0.95`.}
#'   \item{`theta_sd_rel_change_max`}{Maximum relative change in posterior SD
#'   allowed by stability checks. Default is `0.10`.}
#'   \item{`rank_spearman_min`}{Minimum lagged Spearman rank correlation.
#'   Default is `0.95`.}
#'   \item{`near_tie_p_low`, `near_tie_p_high`}{Probability band used only for
#'   near-tie diagnostics in round logging (not used for stopping decisions).
#'   Defaults are `0.40` and `0.60`.}
#'   }
#'   Defaults are resolved from the current item count `N`, then merged with
#'   user overrides.
#' @param session_dir Optional directory for saving session artifacts.
#'   If `NULL`, uses `state$config$session_dir`. Default is `NULL`.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#'   If `NULL`, uses `state$config$persist_item_log`. Default is `NULL`.
#' @param progress Progress output: `"all"`, `"refits"`, `"steps"`, or `"none"`.
#'   Default is `"all"`.
#' @param progress_redraw_every Redraw progress bar every N steps. Default is
#'   `10L`.
#' @param progress_show_events Logical; when TRUE, print notable step events.
#'   Default is `TRUE`.
#' @param progress_errors Logical; when TRUE, include invalid-step events.
#'   Default is `TRUE`.
#' @param ... Additional arguments passed through to `judge()`.
#'
#' @return An updated \code{adaptive_state}. The returned state includes
#'   appended \code{step_log} rows for attempted steps and, when refits occur,
#'   appended \code{round_log} and \code{item_log} entries.
#'
#' @examples
#' # ------------------------------------------------------------------
#' # Offline end-to-end workflow (fast, deterministic, CRAN-safe)
#' # ------------------------------------------------------------------
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' items <- dplyr::rename(
#'   example_writing_samples[1:8, c("ID", "text", "quality_score")],
#'   item_id = ID
#' )
#'
#' # Use the package defaults for trait and prompt template.
#' trait <- trait_description("overall_quality")
#' prompt_template <- set_prompt_template()
#'
#' # Deterministic local judge based on fixture quality scores.
#' sim_judge <- function(A, B, state, ...) {
#'   y <- as.integer(A$quality_score[[1]] >= B$quality_score[[1]])
#'   list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
#' }
#'
#' session_dir <- tempfile("pwllm-adaptive-session-")
#'
#' state <- adaptive_rank_start(
#'   items = items,
#'   seed = 42,
#'   adaptive_config = list(
#'     global_identified_reliability_min = 0.85,
#'     star_override_budget_per_round = 2L
#'   ),
#'   session_dir = session_dir,
#'   persist_item_log = TRUE
#' )
#'
#' state <- adaptive_rank_run_live(
#'   state = state,
#'   judge = sim_judge,
#'   n_steps = 6,
#'   btl_config = list(
#'     # Keep examples lightweight while showing custom stop config inputs.
#'     refit_pairs_target = 50L,
#'     ess_bulk_min = 400,
#'     eap_reliability_min = 0.90
#'   ),
#'   adaptive_config = list(
#'     explore_taper_mult = 0.40,
#'     boundary_frac = 0.20
#'   ),
#'   progress = "steps",
#'   progress_redraw_every = 1L,
#'   progress_show_events = TRUE,
#'   progress_errors = TRUE
#' )
#'
#' # Print and inspect run outputs.
#' print(state)
#' run_summary <- summarize_adaptive(state)
#' step_view <- adaptive_step_log(state)
#' logs <- adaptive_get_logs(state)
#'
#' run_summary
#' head(step_view)
#' names(logs)
#'
#' # Resume from disk and continue.
#' resumed <- adaptive_rank_resume(session_dir)
#' resumed <- adaptive_rank_run_live(
#'   state = resumed,
#'   judge = sim_judge,
#'   n_steps = 4,
#'   progress = "none"
#' )
#' summarize_adaptive(resumed)
#'
#' # ------------------------------------------------------------------
#' # Live OpenAI workflow via backend-agnostic llm_compare_pair()
#' # ------------------------------------------------------------------
#' \dontrun{
#' # Requires network + OPENAI_API_KEY. This incurs API cost.
#' # check_llm_api_keys() is a quick preflight.
#' check_llm_api_keys()
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' live_items <- dplyr::rename(
#'   example_writing_samples[1:12, c("ID", "text")],
#'   item_id = ID
#' )
#'
#' # Default trait/template setup used by the backend-agnostic runner.
#' trait <- trait_description("overall_quality")
#' prompt_template <- set_prompt_template()
#'
#' live_session_dir <- file.path(tempdir(), "pwllm-adaptive-openai")
#'
#' judge_openai <- function(A, B, state, ...) {
#'   res <- llm_compare_pair(
#'     ID1 = A$item_id[[1]],
#'     text1 = A$text[[1]],
#'     ID2 = B$item_id[[1]],
#'     text2 = B$text[[1]],
#'     model = "gpt-5.1",
#'     trait_name = trait$name,
#'     trait_description = trait$description,
#'     prompt_template = prompt_template,
#'     backend = "openai",
#'     endpoint = "responses",
#'     reasoning = "low",
#'     service_tier = "flex",
#'     include_thoughts = FALSE,
#'     temperature = NULL,
#'     top_p = NULL,
#'     logprobs = NULL
#'   )
#'
#'   better_id <- res$better_id[[1]]
#'   ok_ids <- c(A$item_id[[1]], B$item_id[[1]])
#'   if (is.na(better_id) || !(better_id %in% ok_ids)) {
#'     return(list(
#'       is_valid = FALSE,
#'       Y = NA_integer_,
#'       invalid_reason = "model_response_invalid"
#'     ))
#'   }
#'
#'   list(
#'     is_valid = TRUE,
#'     Y = as.integer(identical(better_id, A$item_id[[1]])),
#'     invalid_reason = NA_character_
#'   )
#' }
#'
#' state_live <- adaptive_rank_start(
#'   items = live_items,
#'   seed = 2026,
#'   session_dir = live_session_dir,
#'   persist_item_log = TRUE
#' )
#'
#' state_live <- adaptive_rank_run_live(
#'   state = state_live,
#'   judge = judge_openai,
#'   n_steps = 120L,
#'   btl_config = list(
#'     refit_pairs_target = 20L,
#'     ess_bulk_min = 500,
#'     ess_bulk_min_near_stop = 1200,
#'     max_rhat = 1.01,
#'     divergences_max = 0L,
#'     eap_reliability_min = 0.92,
#'     stability_lag = 2L,
#'     theta_corr_min = 0.97,
#'     theta_sd_rel_change_max = 0.08,
#'     rank_spearman_min = 0.97
#'   ),
#'   progress = "all",
#'   progress_redraw_every = 1L,
#'   progress_show_events = TRUE,
#'   progress_errors = TRUE
#' )
#'
#' # Reporting outputs for end users.
#' print(state_live)
#' run_summary <- summarize_adaptive(state_live)
#' refit_summary <- summarize_refits(state_live)
#' item_summary <- summarize_items(state_live)
#' logs <- adaptive_get_logs(state_live)
#'
#' # Store outputs for audit/reproducibility.
#' saveRDS(
#'   list(
#'     run_summary = run_summary,
#'     refit_summary = refit_summary,
#'     item_summary = item_summary,
#'     logs = logs
#'   ),
#'   file.path(live_session_dir, "adaptive_outputs.rds")
#' )
#'
#' # Resume from stored state and continue sampling.
#' state_live <- adaptive_rank_resume(live_session_dir)
#' state_live <- adaptive_rank_run_live(
#'   state = state_live,
#'   judge = judge_openai,
#'   n_steps = 40L,
#'   progress = "refits"
#' )
#' print(summarize_adaptive(state_live))
#' }
#'
#' @seealso [adaptive_rank_start()], [adaptive_rank_resume()],
#'   [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_run_live <- function(state,
                                   judge,
                                   n_steps = 1L,
                                   fit_fn = NULL,
                                   adaptive_config = NULL,
                                   btl_config = NULL,
                                   session_dir = NULL,
                                   persist_item_log = NULL,
                                   progress = c("all", "refits", "steps", "none"),
                                   progress_redraw_every = 10L,
                                   progress_show_events = TRUE,
                                   progress_errors = TRUE,
                                   ...) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!is.function(judge)) {
    rlang::abort("`judge` must be a function.")
  }
  n_steps <- as.integer(n_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 1L) {
    rlang::abort("`n_steps` must be a positive integer.")
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.null(persist_item_log) &&
    (!is.logical(persist_item_log) || length(persist_item_log) != 1L)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }

  if (!is.null(session_dir)) {
    state$config$session_dir <- session_dir
  }
  if (!is.null(persist_item_log)) {
    state$config$persist_item_log <- isTRUE(persist_item_log)
  }
  resumed_from_session <- .adaptive_is_resumed_session(state)
  state$config$resumed_from_session <- isTRUE(resumed_from_session)
  state$meta$resumed_from_session <- isTRUE(resumed_from_session)
  state <- .adaptive_apply_controller_config(state, adaptive_config = adaptive_config)
  state <- .adaptive_stop_boundary_bootstrap(state)
  state$controller <- .adaptive_controller_with_phase_scope(state, controller = .adaptive_controller_resolve(state))
  state <- .adaptive_phase_a_prepare(state)
  state <- .adaptive_phase_a_finalize_if_ready(state)
  state$controller <- .adaptive_controller_with_phase_scope(state, controller = .adaptive_controller_resolve(state))
  state <- .adaptive_clear_stale_global_stop_state(state)
  .adaptive_phase_a_gate_or_abort(state)
  state <- .adaptive_link_sync_warm_start(state)

  cfg <- .adaptive_progress_config(
    progress = progress,
    progress_redraw_every = progress_redraw_every,
    progress_show_events = progress_show_events,
    progress_errors = progress_errors
  )
  btl_cfg <- .adaptive_btl_resolve_config(state, btl_config)
  btl_cfg$refit_pairs_target <- .adaptive_refit_pairs_target(state, btl_cfg)
  cfg$refit_pairs_target <- btl_cfg$refit_pairs_target
  cfg$stop_thresholds <- btl_cfg
  state$refit_meta$refit_pairs_target_current <- as.integer(btl_cfg$refit_pairs_target)
  state$controller <- .adaptive_controller_resolve(state)
  state$controller$refit_pairs_target <- as.integer(btl_cfg$refit_pairs_target)

  progress_handle <- adaptive_progress_init(state, cfg)
  on.exit(adaptive_progress_finish(progress_handle), add = TRUE)

  remaining <- n_steps
  while (remaining > 0L) {
    state <- .adaptive_stop_boundary_bootstrap(state)
    state <- .adaptive_phase_a_prepare(state)
    state <- .adaptive_phase_a_finalize_if_ready(state)
    state <- .adaptive_clear_stale_global_stop_state(state)
    .adaptive_phase_a_gate_or_abort(state)
    if (isTRUE(.adaptive_link_all_spokes_exhausted(
      state,
      refit_id = .adaptive_link_refit_window_id(state)
    ))) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "all_spokes_exhausted"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }
    if (isTRUE(.adaptive_link_all_spokes_stopped(state))) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "all_spokes_stopped"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }
    budget_status <- .adaptive_stop_boundary_budget_status(state)
    if (isTRUE(budget_status$active) && isTRUE(budget_status$exhausted)) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "max_pairs_after_stop_exhausted"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }
    state <- .adaptive_link_sync_warm_start(state)
    state <- .adaptive_round_activate_if_ready(state)
    state <- run_one_step(state, judge, ...)
    step_row <- tibble::as_tibble(state$step_log)[nrow(state$step_log), , drop = FALSE]
    event <- adaptive_progress_step_event(step_row, cfg)
    if (!is.null(event)) {
      cli::cli_inform(event)
    }
    if (isTRUE(step_row$status[[1L]] == "ok")) {
      if (identical(step_row$round_stage[[1L]], "warm_start")) {
        state <- .adaptive_round_commit_warm_start(state)
      } else {
        state <- .adaptive_round_commit(state, step_row)
      }
      budget_status <- .adaptive_stop_boundary_budget_status(state)
      if (isTRUE(budget_status$active)) {
        state$meta$pairs_committed_after_stop <- as.integer(
          state$meta$pairs_committed_after_stop %||% 0L
        ) + 1L
        budget_status <- .adaptive_stop_boundary_budget_status(state)
        if (isTRUE(budget_status$exhausted)) {
          state$meta$stop_decision <- TRUE
          state$meta$stop_reason <- "max_pairs_after_stop_exhausted"
          if (!is.null(state$config$session_dir)) {
            save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
          }
          return(state)
        }
      }
    } else if (isTRUE(step_row$candidate_starved[[1L]]) &&
      !identical(step_row$round_stage[[1L]], "warm_start")) {
      starve <- .adaptive_round_starvation(state, step_row)
      state <- starve$state
      if (isTRUE(starve$exhausted)) {
        controller <- .adaptive_controller_resolve(state)
        phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
        is_link_phase_a <- .adaptive_link_mode_active(controller) &&
          !identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")

        if (isTRUE(is_link_phase_a)) {
          round_committed <- as.integer((state$round %||% list())$round_committed %||% 0L)
          if (round_committed > 0L) {
            state <- .adaptive_round_start_next(state)
            state <- .adaptive_link_sync_warm_start(state)
          } else {
            active_set <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
            msg <- paste0(
              "Phase A unresolved for set_id=",
              ifelse(is.na(active_set), "NA", as.character(active_set)),
              ": no committed pairs in exhausted round."
            )
            state <- .adaptive_phase_a_mark_unresolved(state, set_id = active_set, message = msg)
            state$meta$stop_decision <- TRUE
            state$meta$stop_reason <- "phase_a_set_unresolved"
            if (!is.null(state$config$session_dir)) {
              save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
            }
            return(state)
          }
        } else {
          controller <- .adaptive_controller_resolve(state)
          phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
          is_link_phase_b <- .adaptive_link_mode_active(controller) &&
            identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")
          if (isTRUE(is_link_phase_b)) {
            refit_id <- .adaptive_link_refit_window_id(state)
            if (isTRUE(.adaptive_link_all_spokes_exhausted(state, refit_id = refit_id))) {
              state$meta$stop_decision <- TRUE
              state$meta$stop_reason <- "all_spokes_exhausted"
              if (!is.null(state$config$session_dir)) {
                save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
              }
              return(state)
            }
          } else {
            state$meta$stop_decision <- TRUE
            state$meta$stop_reason <- "candidate_starvation"
            if (!is.null(state$config$session_dir)) {
              save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
            }
            return(state)
          }
        }
      }
    } else if (isTRUE(step_row$candidate_starved[[1L]])) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "candidate_starvation"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }

    refit_out <- maybe_refit_btl(state, config = btl_cfg, fit_fn = fit_fn)
    state <- refit_out$state
    if (isTRUE(refit_out$refit_performed)) {
      state <- .adaptive_linking_refit_update_state(
        state = state,
        refit_context = refit_out$refit_context
      )
      cfg$stop_thresholds <- refit_out$config
      metrics <- compute_stop_metrics(state, config = refit_out$config)
      state$stop_metrics <- metrics
      state <- .adaptive_maybe_enter_phase3(state, metrics, refit_out$config)
      stop_decision <- should_stop(metrics, config = refit_out$config)
      stop_reason <- if (isTRUE(stop_decision)) "btl_converged" else NA_character_

      round_row <- .adaptive_round_log_row(
        state = state,
        metrics = metrics,
        stop_decision = stop_decision,
        stop_reason = stop_reason,
        refit_context = refit_out$refit_context,
        config = refit_out$config
      )
      state$round_log <- append_round_log(state$round_log, round_row)
      phase_ctx_post_refit <- .adaptive_link_phase_context(state, controller = .adaptive_controller_resolve(state))
      for (spoke_id in as.integer(phase_ctx_post_refit$active_spokes %||% integer())) {
        state <- .adaptive_link_probe_cache_predictions(
          state,
          refit_id = as.integer(round_row$refit_id),
          spoke_id = as.integer(spoke_id)
        )
      }
      link_rows <- .adaptive_link_stage_refit_rows(
        state = state,
        refit_id = as.integer(round_row$refit_id),
        refit_context = refit_out$refit_context
      )
      if (nrow(link_rows) > 0L) {
        .adaptive_assert_link_stage_rows_completeness(link_rows)
        state$link_stage_log <- append_link_stage_log(
          state$link_stage_log %||% new_link_stage_log(),
          link_rows
        )
        state <- .adaptive_link_apply_stop_state(state, link_rows)
      }
      item_log_tbl <- .adaptive_build_item_log_refit(
        state,
        refit_id = round_row$refit_id
      )
      state <- .adaptive_append_item_log(state, item_log_tbl)
      if (!is.null(state$config$session_dir) &&
        isTRUE(state$config$persist_item_log)) {
        paths <- .adaptive_session_paths(state$config$session_dir)
        .adaptive_write_item_log_files(state$item_log, paths$item_log_dir)
      }
      if (cfg$progress %in% c("all", "refits")) {
        block <- adaptive_progress_refit_block(
          tibble::as_tibble(round_row),
          cfg,
          link_stage_rows = link_rows
        )
        if (length(block) > 0L) {
          cat(paste(block, collapse = "\n"), "\n")
        }
      }
      global_stop_allowed <- isTRUE(.adaptive_global_stop_allowed(state))
      if (isTRUE(stop_decision) && isTRUE(global_stop_allowed)) {
        round_row_tbl <- tibble::as_tibble(round_row)
        boundary_refit_id <- if ("refit_id" %in% names(round_row_tbl)) {
          as.integer(round_row_tbl$refit_id[[1L]] %||% NA_integer_)
        } else {
          NA_integer_
        }
        boundary_step_id <- if ("step_id_at_refit" %in% names(round_row_tbl)) {
          as.integer(round_row_tbl$step_id_at_refit[[1L]] %||% NA_integer_)
        } else {
          NA_integer_
        }
        if (is.na(as.integer(state$meta$stop_boundary_step_id %||% NA_integer_))) {
          state$meta$stop_boundary_refit_id <- boundary_refit_id
          state$meta$stop_boundary_step_id <- boundary_step_id
          state$meta$pairs_committed_after_stop <- 0L
        }
        budget_status <- .adaptive_stop_boundary_budget_status(state)
        if (budget_status$max_pairs_after_stop <= 0L) {
          state$meta$stop_decision <- TRUE
          state$meta$stop_reason <- stop_reason
          if (!is.null(state$config$session_dir)) {
            save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
          }
          return(state)
        }
      }
      if (isTRUE(.adaptive_link_all_spokes_exhausted(
        state,
        refit_id = as.integer(round_row$refit_id %||% NA_integer_)
      ))) {
        state$meta$stop_decision <- TRUE
        state$meta$stop_reason <- "all_spokes_exhausted"
        if (!is.null(state$config$session_dir)) {
          save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
        }
        return(state)
      }
      if (isTRUE(.adaptive_link_all_spokes_stopped(state))) {
        state$meta$stop_decision <- TRUE
        state$meta$stop_reason <- "all_spokes_stopped"
        if (!is.null(state$config$session_dir)) {
          save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
        }
        return(state)
      }
    }
    state <- .adaptive_phase_a_prepare(state)
    state <- .adaptive_phase_a_finalize_if_ready(state)
    state$controller <- .adaptive_controller_with_phase_scope(state, controller = .adaptive_controller_resolve(state))
    state <- .adaptive_clear_stale_global_stop_state(state)
    .adaptive_phase_a_gate_or_abort(state)
    if (!is.null(state$config$session_dir)) {
      save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
    }
    progress_handle <- adaptive_progress_update(progress_handle, state, cfg)
    remaining <- remaining - 1L
  }

  state
}

#' Adaptive ranking resume
#'
#' @description
#' Resume a previously persisted adaptive pairing session.
#'
#' @details
#' This is a thin wrapper around [load_adaptive_session()] and performs schema
#' and log-shape checks during load. Returned state preserves canonical
#' \code{step_log}, \code{round_log}, and \code{item_log} contents used for
#' adaptive auditability.
#'
#' @param session_dir Directory containing session artifacts.
#' @param ... Reserved for future extensions; currently unused.
#'
#' @return An \code{adaptive_state} object restored from disk.
#'
#' @examples
#' dir <- tempfile("pwllm-session-")
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 3)
#' save_adaptive_session(state, dir, overwrite = TRUE)
#' restored <- adaptive_rank_resume(dir)
#' summarize_adaptive(restored)
#'
#' @seealso [adaptive_rank_start()], [adaptive_rank_run_live()],
#'   [save_adaptive_session()], [load_adaptive_session()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_resume <- function(session_dir, ...) {
  if (missing(session_dir) || is.null(session_dir)) {
    rlang::abort("`session_dir` must be provided.")
  }
  load_adaptive_session(session_dir)
}
