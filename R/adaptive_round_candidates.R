# -------------------------------------------------------------------------
# Adaptive stage candidates: anchors, strata, and per-stage generation.
# -------------------------------------------------------------------------

.adaptive_rank_proxy <- function(state, prefer_btl = FALSE) {
  ids <- as.character(state$trueskill_state$items$item_id)
  mu <- as.double(state$trueskill_state$items$mu)
  names(mu) <- ids
  refit_id <- as.integer(state$refit_meta$last_refit_round_id %||% 0L)
  if (isTRUE(prefer_btl)) {
    theta_mean <- tryCatch(
      .adaptive_btl_fit_theta_mean(state$btl_fit %||% list()),
      error = function(e) NULL
    )

    if (is.numeric(theta_mean) && length(theta_mean) > 0L && !is.null(names(theta_mean))) {
      theta <- as.double(theta_mean)
      names(theta) <- as.character(names(theta_mean))
      theta_scores <- as.double(theta[ids])
      names(theta_scores) <- ids
      if (length(theta_scores) == length(ids) && all(is.finite(theta_scores))) {
        return(list(
          scores = theta_scores,
          source = "btl_theta_eap",
          refit_id = refit_id
        ))
      }
    }
  }

  list(
    scores = mu[ids],
    source = "trueskill_mu",
    refit_id = refit_id
  )
}

.adaptive_rank_index_from_scores <- function(scores) {
  ids <- as.character(names(scores))
  ord <- order(-as.double(scores), ids)
  stats::setNames(seq_along(ids), ids[ord])
}

.adaptive_bucket_counts <- function(total, parts) {
  parts <- as.double(parts)
  raw <- total * parts / sum(parts)
  out <- floor(raw)
  rem <- as.integer(total - sum(out))
  if (rem > 0L) {
    frac <- raw - floor(raw)
    add_idx <- order(-frac, seq_along(parts))[seq_len(rem)]
    out[add_idx] <- out[add_idx] + 1L
  }
  as.integer(out)
}

.adaptive_select_rolling_anchors <- function(scores, defaults) {
  rank_index <- .adaptive_rank_index_from_scores(scores)
  ids_sorted <- names(sort(rank_index))
  n <- length(ids_sorted)

  if (n < 10L) {
    n_anchor <- max(1L, as.integer(round(defaults$anchor_frac_total * n)))
  } else {
    n_anchor <- .btl_mcmc_clamp(10L, 40L, as.integer(round(defaults$anchor_frac_total * n)))
  }
  n_anchor <- min(max(1L, n - 1L), n_anchor)

  bucket_n <- .adaptive_bucket_counts(
    total = n_anchor,
    parts = c(defaults$anchor_top_weight, defaults$anchor_mid_weight, defaults$anchor_bottom_weight)
  )
  n_top <- bucket_n[[1L]]
  n_mid <- bucket_n[[2L]]
  n_bottom <- bucket_n[[3L]]

  top_ids <- if (n_top > 0L) ids_sorted[seq_len(n_top)] else character()
  bottom_ids <- if (n_bottom > 0L) rev(ids_sorted)[seq_len(n_bottom)] else character()
  center <- floor((n + 1L) / 2L)
  mid_radius <- floor((n_mid - 1L) / 2L)
  mid_start <- max(1L, center - mid_radius)
  mid_end <- min(n, mid_start + n_mid - 1L)
  if ((mid_end - mid_start + 1L) < n_mid) {
    mid_start <- max(1L, mid_end - n_mid + 1L)
  }
  mid_ids <- if (n_mid > 0L) ids_sorted[mid_start:mid_end] else character()

  anchors <- unique(c(top_ids, mid_ids, bottom_ids))
  anchors <- anchors[anchors %in% ids_sorted]
  if (length(anchors) > n_anchor) {
    anchors <- anchors[seq_len(n_anchor)]
  } else if (length(anchors) < n_anchor) {
    fill <- ids_sorted[!ids_sorted %in% anchors]
    anchors <- c(anchors, fill[seq_len(min(length(fill), n_anchor - length(anchors)))])
  }

  as.character(anchors)
}

.adaptive_assign_strata <- function(scores, defaults) {
  rank_index <- .adaptive_rank_index_from_scores(scores)
  ids_sorted <- names(sort(rank_index))
  n <- length(ids_sorted)

  top_band_pct <- as.double(defaults$top_band_pct %||% 0.10)
  top_band_bins <- as.integer(defaults$top_band_bins %||% 5L)
  top_band_n <- as.integer(ceiling(top_band_pct * n))
  top_band_n <- max(1L, min(n, top_band_n))
  top_band_ids <- ids_sorted[seq_len(top_band_n)]
  rest_ids <- ids_sorted[(top_band_n + 1L):n]
  rest_ids <- rest_ids[!is.na(rest_ids)]

  top_k <- max(1L, min(top_band_bins, top_band_n))
  top_seq <- seq_len(top_band_n)
  top_strata <- floor((top_seq - 1L) * top_k / top_band_n) + 1L

  rest_n <- length(rest_ids)
  base_k <- max(1L, min(defaults$k_base, max(1L, rest_n)))
  if (rest_n > 0L) {
    rest_seq <- seq_len(rest_n)
    rest_strata <- floor((rest_seq - 1L) * base_k / rest_n) + 1L + top_k
  } else {
    rest_strata <- integer()
  }

  strata <- stats::setNames(integer(0), character(0))
  strata <- c(strata, stats::setNames(as.integer(top_strata), top_band_ids))
  strata <- c(strata, stats::setNames(as.integer(rest_strata), rest_ids))

  list(
    rank_index = rank_index,
    stratum_id = as.integer(strata[names(rank_index)]),
    stratum_map = stats::setNames(as.integer(strata), names(strata)),
    top_band_ids = as.character(top_band_ids)
  )
}

.adaptive_round_anchor_needs_refresh <- function(state, defaults) {
  round <- state$round %||% list()
  if (length(round$anchor_ids %||% character()) == 0L) {
    return(TRUE)
  }

  last_refit <- as.integer(state$refit_meta$last_refit_round_id %||% 0L)
  current_refit <- as.integer(round$anchor_refit_round_id %||% 0L)
  if (last_refit > current_refit) {
    return(TRUE)
  }

  if (isTRUE(defaults$anchor_refresh_on_round)) {
    anchor_round <- as.integer(round$anchor_round_id %||% 0L)
    round_id <- as.integer(round$round_id %||% 0L)
    if (round_id > anchor_round) {
      return(TRUE)
    }
  }

  FALSE
}

#' @keywords internal
#' @noRd
.adaptive_refresh_round_anchors <- function(state) {
  out <- state
  defaults <- adaptive_defaults(length(out$item_ids))
  if (!.adaptive_round_anchor_needs_refresh(out, defaults)) {
    return(out)
  }

  proxy <- .adaptive_rank_proxy(out, prefer_btl = TRUE)
  anchors <- .adaptive_select_rolling_anchors(proxy$scores, defaults)
  out$round$anchor_ids <- as.character(anchors)
  out$round$anchor_refresh_source <- as.character(proxy$source)
  out$round$anchor_refit_round_id <- as.integer(proxy$refit_id)
  out$round$anchor_round_id <- as.integer(out$round$round_id %||% 0L)
  out
}

.adaptive_stage_distance_bounds <- function(stage_name, fallback_name, defaults) {
  if (identical(stage_name, "anchor_link")) {
    return(list(min = 0L, max = .Machine$integer.max))
  }

  if (identical(stage_name, "long_link")) {
    min_gap <- as.integer(defaults$long_min_dist)
    if (identical(fallback_name, "expand_locality")) {
      min_gap <- as.integer(defaults$long_min_dist)
    } else if (identical(fallback_name, "global_safe")) {
      min_gap <- as.integer(defaults$long_min_dist)
    }
    return(list(min = min_gap, max = .Machine$integer.max))
  }

  if (identical(stage_name, "mid_link")) {
    min_gap <- as.integer(defaults$mid_min_dist)
    max_gap <- as.integer(defaults$mid_max_dist)
    if (identical(fallback_name, "expand_locality")) {
      min_gap <- max(1L, min_gap - 1L)
      max_gap <- max(max_gap, min_gap)
      max_gap <- max_gap + 1L
    } else if (identical(fallback_name, "global_safe")) {
      min_gap <- 1L
      max_gap <- .Machine$integer.max
    }
    return(list(min = min_gap, max = max_gap))
  }

  # local_link
  max_gap <- as.integer(defaults$local_max_dist)
  if (identical(fallback_name, "expand_locality")) {
    max_gap <- as.integer(defaults$local_expand_max_dist)
  } else if (identical(fallback_name, "global_safe")) {
    max_gap <- .Machine$integer.max
  }
  list(min = 0L, max = max_gap)
}

.adaptive_uniform_subsample_pairs <- function(candidates, C_max, seed) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) <= C_max) {
    return(cand)
  }
  withr::with_seed(seed, {
    keep <- sample.int(nrow(cand), size = C_max, replace = FALSE)
    cand[keep, , drop = FALSE]
  })
}

.adaptive_link_mode_active <- function(controller) {
  as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")
}

.adaptive_set_candidate_filter_counts <- function(candidates, counts) {
  attr(candidates, "candidate_filter_counts") <- counts
  candidates
}

.adaptive_link_ranked_spokes <- function(state, controller, eligible_spoke_ids = NULL) {
  controller <- .adaptive_runtime_controller_resolve(state, controller)
  set_ids <- unique(as.integer(state$items$set_id))
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- setdiff(set_ids, hub_id)
  if (!is.null(eligible_spoke_ids)) {
    spoke_ids <- intersect(spoke_ids, as.integer(eligible_spoke_ids))
  }
  if (.adaptive_link_mode_active(controller)) {
    refit_id <- .adaptive_link_refit_window_id(state)
    effective_spokes <- .adaptive_link_effective_active_spokes(
      state,
      controller = controller,
      refit_id = refit_id,
      exclude_exhausted = TRUE
    )
    if (length(effective_spokes) > 0L) {
      spoke_ids <- intersect(spoke_ids, as.integer(effective_spokes))
    }
  }
  if (length(spoke_ids) < 1L) {
    return(integer())
  }
  frozen_map <- .adaptive_link_state_frozen_by_spoke(controller)
  keep_spokes <- vapply(
    spoke_ids,
    function(spoke_id) !isTRUE(frozen_map[[as.character(spoke_id)]]),
    logical(1L)
  )
  spoke_ids <- as.integer(spoke_ids[keep_spokes])
  if (length(spoke_ids) < 1L) {
    return(integer())
  }
  mode <- as.character(controller$run_mode %||% "within_set")
  if (!identical(mode, "link_multi_spoke")) {
    current <- as.integer(controller$current_link_spoke_id %||% NA_integer_)
    if (!is.na(current) && current %in% spoke_ids) {
      tail_ids <- as.integer(sort(setdiff(spoke_ids, current)))
      return(as.integer(c(current, tail_ids)))
    }
    return(as.integer(sort(spoke_ids)))
  }

  # In multi-spoke mode, route deterministically across spokes.
  spoke_ids <- as.integer(sort(spoke_ids))
  concurrent_mode <- identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
  if (!isTRUE(concurrent_mode)) {
    refit_id <- .adaptive_link_refit_window_id(state)
    refit_context <- list(last_refit_step = as.integer(state$refit_meta$last_refit_step %||% 0L))
    counts <- stats::setNames(vapply(
      spoke_ids,
      function(spoke_id) {
        summary <- .adaptive_link_refit_summary_current(
          state = state,
          refit_id = refit_id,
          spoke_id = as.integer(spoke_id),
          refit_context = refit_context
        )
        as.integer(summary$n_cross_edges_active_since_last_refit %||% 0L)
      },
      integer(1L)
    ), as.character(spoke_ids))
    cached_refit_id <- as.integer(controller$link_budget_refit_id %||% NA_integer_)
    current_refit_id <- as.integer(.adaptive_link_refit_window_id(state))
    cached_map <- controller$link_budget_map %||% list()
    if (!is.na(cached_refit_id) && identical(cached_refit_id, current_refit_id) && length(cached_map) > 0L) {
      cached_active <- names(cached_map)[vapply(
        cached_map,
        function(entry) as.integer(entry$B_spoke_refit_budget %||% 0L) > 0L,
        logical(1L)
      )]
      cached_active <- as.integer(cached_active)
      cached_active <- cached_active[cached_active %in% spoke_ids]
      if (length(cached_active) > 0L) {
        tail_ids <- as.integer(sort(setdiff(spoke_ids, cached_active[[1L]])))
        return(as.integer(c(cached_active[[1L]], tail_ids)))
      }
    }
    if (any(counts > 0L)) {
      ord_counts <- order(as.integer(counts), as.integer(names(counts)))
      return(as.integer(names(counts)[ord_counts]))
    }
  }

  if (isTRUE(concurrent_mode)) {
    blocker_totals <- vapply(
      spoke_ids,
      function(spoke_id) {
        sum(.adaptive_link_blocker_weights_for_spoke(controller, spoke_id = as.integer(spoke_id)))
      },
      numeric(1L)
    )
    names(blocker_totals) <- as.character(spoke_ids)
    refit_id <- .adaptive_link_refit_window_id(state)
    refit_context <- list(last_refit_step = as.integer(state$refit_meta$last_refit_step %||% 0L))
    counts <- stats::setNames(vapply(
      spoke_ids,
      function(spoke_id) {
        summary <- .adaptive_link_refit_summary_current(
          state = state,
          refit_id = refit_id,
          spoke_id = as.integer(spoke_id),
          refit_context = refit_context
        )
        as.integer(summary$n_cross_edges_active_since_last_refit %||% 0L)
      },
      integer(1L)
    ), as.character(spoke_ids))

    budget_map <- .adaptive_link_budget_map_for_refit(
      state = state,
      controller = controller,
      eligible_spoke_ids = spoke_ids
    )
    utility_mass <- vapply(
      as.character(spoke_ids),
      function(key) as.double(budget_map[[key]]$concurrent_utility_mass %||% 0),
      numeric(1L)
    )
    floor_pairs <- vapply(
      as.character(spoke_ids),
      function(key) as.integer(budget_map[[key]]$concurrent_floor_pairs %||% 0L),
      integer(1L)
    )
    target_pairs <- vapply(
      as.character(spoke_ids),
      function(key) as.integer(budget_map[[key]]$B_spoke_refit_budget %||% 0L),
      integer(1L)
    )
    floor_deficit <- pmax(0L, floor_pairs - counts)
    if (any(floor_deficit > 0L)) {
      eligible_counts <- counts[floor_deficit > 0L]
      ord_floor <- order(
        -floor_deficit[names(eligible_counts)],
        -blocker_totals[names(eligible_counts)],
        -utility_mass[names(eligible_counts)],
        eligible_counts,
        as.integer(names(eligible_counts))
      )
      return(as.integer(names(eligible_counts)[ord_floor]))
    }

    target_deficit <- as.integer(target_pairs[names(counts)] - counts)
    target_deficit[!is.finite(target_deficit)] <- 0L
    if (any(target_deficit > 0L)) {
      eligible_counts <- counts[target_deficit > 0L]
      ord_deficit <- order(
        -target_deficit[names(eligible_counts)],
        -blocker_totals[names(eligible_counts)],
        -utility_mass[names(eligible_counts)],
        eligible_counts,
        as.integer(names(eligible_counts))
      )
      return(as.integer(names(eligible_counts)[ord_deficit]))
    }

    return(integer())
  }

  current <- as.integer(controller$current_link_spoke_id %||% NA_integer_)
  if (!is.na(current) && current %in% spoke_ids) {
    tail_ids <- as.integer(sort(setdiff(spoke_ids, current)))
    return(as.integer(c(current, tail_ids)))
  }
  as.integer(sort(spoke_ids))
}

.adaptive_link_active_spoke <- function(state, controller, eligible_spoke_ids = NULL) {
  ranked <- .adaptive_link_ranked_spokes(
    state = state,
    controller = controller,
    eligible_spoke_ids = eligible_spoke_ids
  )
  if (length(ranked) < 1L) {
    return(NA_integer_)
  }
  as.integer(ranked[[1L]])
}

.adaptive_link_spoke_bins <- function(spoke_ids, scores, bins) {
  spoke_ids <- as.character(spoke_ids)
  bins <- as.integer(max(1L, bins))
  ord <- order(-as.double(scores[spoke_ids]), spoke_ids)
  sorted <- spoke_ids[ord]
  n <- length(sorted)
  if (n < 1L) {
    return(stats::setNames(integer(), character()))
  }
  use_bins <- max(1L, min(as.integer(bins), n))
  idx <- floor((seq_len(n) - 1L) * use_bins / n) + 1L
  stats::setNames(as.integer(idx), sorted)
}

.adaptive_link_require_phase_a_theta_map <- function(state,
                                                     set_id,
                                                     field,
                                                     required_item_ids,
                                                     helper_name) {
  set_id <- as.integer(set_id)
  required_item_ids <- unique(as.character(required_item_ids))
  required_item_ids <- required_item_ids[!is.na(required_item_ids)]

  theta_map <- tryCatch(
    .adaptive_link_phase_a_theta_map(state, set_id = set_id, field = field),
    error = function(e) {
      rlang::abort(
        sprintf(
          "%s invariant failed: Phase A %s unavailable for set_id=%s.",
          helper_name,
          field,
          set_id
        ),
        parent = e
      )
    }
  )

  theta_vals <- as.double(theta_map[required_item_ids])
  names(theta_vals) <- required_item_ids
  if (any(!is.finite(theta_vals))) {
    rlang::abort(
      sprintf(
        "%s invariant failed: Phase A %s missing/non-finite for set_id=%s.",
        helper_name,
        field,
        set_id
      )
    )
  }

  theta_vals
}

.adaptive_link_phase_b_routing_scores <- function(state, controller, active_ids, hub_id) {
  active_ids <- as.character(active_ids)
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  link_refit_mode <- as.character(controller$link_refit_mode %||% "shift_only")
  use_current_theta <- identical(link_refit_mode, "joint_refit")
  link_estimation_mode <- as.character(controller$link_estimation_mode %||% "transform")
  active_sets <- sort(unique(as.integer(set_map[active_ids])))
  active_sets <- active_sets[!is.na(active_sets)]
  if (length(active_sets) < 1L) {
    return(stats::setNames(numeric(), character()))
  }

  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  scores <- stats::setNames(rep(NA_real_, length(active_ids)), active_ids)
  for (set_id in active_sets) {
    set_items <- active_ids[as.integer(set_map[active_ids]) == as.integer(set_id)]
    phase_a_theta <- function() {
      .adaptive_link_require_phase_a_theta_map(
        state = state,
        set_id = set_id,
        field = "theta_raw_mean",
        required_item_ids = set_items,
        helper_name = "Linking routing"
      )
    }
    raw_theta <- if (isTRUE(use_current_theta)) {
      current_theta <- .adaptive_link_theta_mean_map(state, set_id = set_id)
      current_vals <- as.double(current_theta[set_items])
      names(current_vals) <- as.character(set_items)
      missing_current <- !is.finite(current_vals)
      if (any(missing_current)) {
        phase_vals <- as.double(phase_a_theta()[set_items])
        names(phase_vals) <- as.character(set_items)
        current_vals[missing_current] <- phase_vals[missing_current]
      }
      current_vals
    } else {
      phase_vals <- as.double(phase_a_theta()[set_items])
      names(phase_vals) <- as.character(set_items)
      phase_vals
    }
    names(raw_theta) <- as.character(set_items)
    if (identical(link_estimation_mode, "anchored_joint") &&
      as.integer(set_id) == as.integer(hub_id)) {
      raw_theta <- as.double(phase_a_theta()[set_items])
      names(raw_theta) <- as.character(set_items)
    }
    if (any(!is.finite(raw_theta))) {
      source_label <- if (isTRUE(use_current_theta)) "current theta_mean" else "Phase A theta_raw_mean"
      rlang::abort(
        sprintf(
          "Linking routing invariant failed: %s missing/non-finite for set_id=%s.",
          source_label,
          as.integer(set_id)
        )
      )
    }

    if (as.integer(set_id) == as.integer(hub_id)) {
      scores[set_items] <- as.double(raw_theta)
      next
    }

    if (identical(link_estimation_mode, "anchored_joint")) {
      accepted_map <- (state$linking$anchored_joint %||% list())$accepted_state_by_spoke %||% list()
      accepted_state <- accepted_map[[as.character(set_id)]] %||% NULL
      if (is.null(accepted_state)) {
        accepted_state <- .adaptive_anchored_joint_artifact_copy_init(
          state = state,
          spoke_id = as.integer(set_id),
          controller = controller
        )
      }
      spoke_scores <- as.double(accepted_state$theta_spoke_global_mean[set_items])
      names(spoke_scores) <- as.character(set_items)
      if (any(!is.finite(spoke_scores))) {
        rlang::abort(
          sprintf(
            "Anchored-joint routing invariant failed: accepted spoke scores missing/non-finite for set_id=%s.",
            as.integer(set_id)
          )
        )
      }
      scores[set_items] <- spoke_scores
      next
    }

    mode <- .adaptive_link_transform_state_for_spoke(controller, spoke_id = as.integer(set_id))
    stats_row <- link_stats[[as.character(set_id)]] %||% list()
    delta <- as.double(stats_row$delta_spoke_mean %||% 0)
    if (!is.finite(delta)) {
      delta <- 0
    }
    alpha <- 1
    if (identical(mode, "shift_scale")) {
      log_alpha <- as.double(stats_row$log_alpha_spoke_mean %||% 0)
      if (!is.finite(log_alpha)) {
        log_alpha <- 0
      }
      alpha <- exp(log_alpha)
    }
    scores[set_items] <- as.double(delta + alpha * raw_theta)
  }

  if (any(!is.finite(scores[active_ids]))) {
    rlang::abort("Linking routing score invariant failed: non-finite routing scores in phase_b.")
  }
  out <- as.double(scores[active_ids])
  names(out) <- as.character(active_ids)
  out
}

.adaptive_link_phase_b_hub_anchors <- function(state, hub_ids, hub_scores, defaults) {
  hub_ids <- as.character(hub_ids)
  hub_scores <- as.double(hub_scores[hub_ids])
  names(hub_scores) <- hub_ids
  anchors <- .adaptive_select_rolling_anchors(hub_scores, defaults)
  anchors <- as.character(anchors[anchors %in% hub_ids])
  if (length(anchors) < 1L) {
    return(character())
  }

  uses <- as.integer((state$round %||% list())$per_round_item_uses %||% integer())
  names(uses) <- names((state$round %||% list())$per_round_item_uses %||% uses)
  ranked <- names(sort(.adaptive_rank_index_from_scores(hub_scores)))
  ranked <- ranked[ranked %in% hub_ids]
  if (length(ranked) < 1L) {
    return(anchors)
  }
  ranked_use <- uses[ranked]
  ranked_use[is.na(ranked_use)] <- 0L
  ranked_unused <- ranked[ranked_use == 0L]
  if (length(ranked_unused) < 1L) {
    return(anchors)
  }

  n_anchor <- max(length(anchors), min(2L, length(ranked)))
  primary <- ranked_unused[seq_len(min(length(ranked_unused), n_anchor))]
  if (length(primary) < n_anchor) {
    fill <- ranked[!ranked %in% primary]
    primary <- c(primary, fill[seq_len(min(length(fill), n_anchor - length(primary)))])
  }
  as.character(primary)
}

.adaptive_link_spoke_coverage <- function(state,
                                          controller,
                                          spoke_id,
                                          spoke_ids,
                                          routing_scores,
                                          score_source = "linking_global_score") {
  spoke_id <- as.integer(spoke_id)
  bins_target <- as.integer(controller$spoke_quantile_coverage_bins %||% 3L)
  bins_used <- max(1L, bins_target)
  n_spoke <- length(spoke_ids)
  while (bins_used > 1L && n_spoke < (3L * bins_used)) {
    bins_used <- bins_used - 1L
  }

  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  cumulative_cross_count <- 0L
  if (nrow(step_log) > 0L && all(c("pair_id", "is_cross_set", "link_spoke_id") %in% names(step_log))) {
    cumulative_cross_count <- as.integer(sum(
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == spoke_id,
      na.rm = TRUE
    ))
  }

  source <- as.character(score_source %||% "linking_global_score")
  spoke_scores <- as.double(routing_scores[spoke_ids])
  names(spoke_scores) <- as.character(spoke_ids)

  if (cumulative_cross_count < 10L) {
    phase_a_rank <- tryCatch(
      .adaptive_link_phase_a_theta_map(state, set_id = spoke_id, field = "rank_mu_raw"),
      error = function(e) stats::setNames(numeric(), character())
    )
    phase_a_rank <- as.double(phase_a_rank[spoke_ids])
    names(phase_a_rank) <- as.character(spoke_ids)

    if (all(is.finite(phase_a_rank))) {
      # Lower rank is better; convert to descending score for quantile binning.
      spoke_scores <- -phase_a_rank
      source <- "phase_a_rank_mu_raw"
    }
  }

  if (any(!is.finite(spoke_scores))) {
    rlang::abort("Linking coverage invariant failed: routing scores must be finite for spoke items.")
  }

  bin_map <- .adaptive_link_spoke_bins(spoke_ids, spoke_scores, bins = bins_used)
  min_per_bin <- as.integer(controller$spoke_quantile_coverage_min_per_bin_per_refit %||% 1L)
  min_per_bin <- max(1L, min_per_bin)
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
  bin_counts <- stats::setNames(rep.int(0L, bins_used), as.character(seq_len(bins_used)))
  if (nrow(step_log) > 0L &&
    all(c("pair_id", "step_id", "is_cross_set", "link_spoke_id", "set_i", "set_j", "i", "j") %in% names(step_log))) {
    win <- step_log[
      !is.na(step_log$pair_id) &
        step_log$step_id > last_refit_step &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == spoke_id,
      ,
      drop = FALSE
    ]
    if (nrow(win) > 0L) {
      spoke_item <- vapply(seq_len(nrow(win)), function(idx) {
        if (as.integer(win$set_i[[idx]]) == spoke_id) {
          state$item_ids[[as.integer(win$i[[idx]])]]
        } else if (as.integer(win$set_j[[idx]]) == spoke_id) {
          state$item_ids[[as.integer(win$j[[idx]])]]
        } else {
          NA_character_
        }
      }, character(1L))
      bins <- as.integer(bin_map[spoke_item])
      bins <- bins[!is.na(bins)]
      if (length(bins) > 0L) {
        tab <- table(as.character(bins))
        bin_counts[names(tab)] <- as.integer(tab)
      }
    }
  }
  under <- as.integer(names(bin_counts)[bin_counts < min_per_bin])
  under <- under[!is.na(under)]
  list(
    bin_map = bin_map,
    bins_used = as.integer(bins_used),
    bins_undercovered = as.integer(under),
    source = as.character(source)
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_quantile_bins <- function(item_ids, scores, bins) {
  ids <- as.character(item_ids)
  scores <- as.double(scores[ids])
  ord <- order(scores, ids)
  ids <- ids[ord]
  n <- length(ids)
  if (n < 1L) {
    return(stats::setNames(integer(), character()))
  }
  bins <- max(1L, min(as.integer(bins), n))
  idx <- floor((seq_len(n) - 1L) * bins / n) + 1L
  stats::setNames(as.integer(idx), ids)
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_construct_panel <- function(state, controller, spoke_id) {
  spoke_id <- as.integer(spoke_id)
  hub_id <- as.integer(controller$hub_id %||% 1L)
  epoch_id <- .adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)
  spoke_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == spoke_id])
  hub_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == hub_id])
  n_spoke_start <- as.integer(length(spoke_ids))
  if (n_spoke_start < 1L || length(hub_ids) < 1L) {
    return(.adaptive_link_probe_empty_panel())
  }

  hub_theta_all <- .adaptive_link_require_phase_a_theta_map(
    state = state,
    set_id = hub_id,
    field = "theta_raw_mean",
    required_item_ids = hub_ids,
    helper_name = "Probe panel construction"
  )
  spoke_theta_all <- .adaptive_link_require_phase_a_theta_map(
    state = state,
    set_id = spoke_id,
    field = "theta_raw_mean",
    required_item_ids = spoke_ids,
    helper_name = "Probe panel construction"
  )

  routing_scores <- .adaptive_link_phase_b_routing_scores(
    state = state,
    controller = controller,
    active_ids = unique(c(hub_ids, spoke_ids)),
    hub_id = hub_id
  )
  hub_anchors <- .adaptive_link_phase_b_hub_anchors(
    state = state,
    hub_ids = hub_ids,
    hub_scores = routing_scores,
    defaults = adaptive_defaults(max(2L, length(unique(c(hub_ids, spoke_ids)))))
  )
  anchor_required <- isTRUE(controller$hub_anchor_required_phase_b %||% TRUE)
  if (anchor_required) {
    hub_pool <- unique(as.character(hub_anchors))
    if (length(hub_pool) < 1L) {
      rlang::abort(
        paste0(
          "Phase B probe-panel invariant failed: `HubEligible` anchor pool is empty for spoke_id=",
          as.integer(spoke_id),
          " while `hub_anchor_required_phase_b=TRUE`."
        )
      )
    }
  } else {
    hub_pool <- unique(as.character(hub_ids))
  }

  spoke_theta <- as.double(spoke_theta_all[spoke_ids])
  hub_theta <- as.double(hub_theta_all[hub_pool])
  names(spoke_theta) <- spoke_ids
  names(hub_theta) <- hub_pool

  q_bins <- max(1L, as.integer(controller$spoke_quantile_coverage_bins %||% 3L))
  h_bins <- 3L
  spoke_bin_map <- .adaptive_link_probe_quantile_bins(spoke_ids, spoke_theta, q_bins)
  hub_bin_map <- .adaptive_link_probe_quantile_bins(hub_pool, hub_theta, h_bins)
  target_edges <- .adaptive_link_probe_panel_size(
    n_spoke_items = n_spoke_start,
    probe_panel_edges = controller$probe_panel_edges %||% NA_integer_
  )

  observed_keys <- character()
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) > 0L &&
    all(c("pair_id", "is_cross_set", "link_spoke_id", "A", "B") %in% names(step_log))) {
    ids_all <- as.character(state$item_ids)
    cross_rows <- step_log[
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == spoke_id,
      ,
      drop = FALSE
    ]
    if (nrow(cross_rows) > 0L) {
      cross_rows <- cross_rows[!.adaptive_link_is_holdout_probe_rows(cross_rows), , drop = FALSE]
    }
    if (nrow(cross_rows) > 0L) {
      observed_keys <- vapply(seq_len(nrow(cross_rows)), function(idx) {
        make_unordered_key(
          ids_all[[as.integer(cross_rows$A[[idx]])]],
          ids_all[[as.integer(cross_rows$B[[idx]])]]
        )
      }, character(1L))
    }
  }

  legal_pairs <- expand.grid(
    hub_item_id = sort(hub_pool),
    spoke_item_id = sort(spoke_ids),
    stringsAsFactors = FALSE
  )
  legal_pairs$pair_key <- vapply(seq_len(nrow(legal_pairs)), function(idx) {
    make_unordered_key(legal_pairs$hub_item_id[[idx]], legal_pairs$spoke_item_id[[idx]])
  }, character(1L))
  legal_pairs <- legal_pairs[!legal_pairs$pair_key %in% observed_keys, , drop = FALSE]
  feasible_target_edges <- .adaptive_link_probe_panel_feasible_size(
    target_edges = target_edges,
    n_available_pairs = as.integer(nrow(legal_pairs))
  )

  planned <- vector("list", length = 0L)
  seen_keys <- observed_keys
  spoke_q_targets <- rep.int(as.integer(target_edges %/% q_bins), q_bins)
  if ((target_edges %% q_bins) > 0L) {
    spoke_q_targets[seq_len(target_edges %% q_bins)] <-
      spoke_q_targets[seq_len(target_edges %% q_bins)] + 1L
  }
  cell_shortfall_detected <- FALSE

  for (q in seq_len(q_bins)) {
    q_target <- spoke_q_targets[[q]]
    hub_targets <- rep.int(as.integer(q_target %/% h_bins), h_bins)
    if ((q_target %% h_bins) > 0L) {
      hub_targets[seq_len(q_target %% h_bins)] <- hub_targets[seq_len(q_target %% h_bins)] + 1L
    }
    spoke_bin_ids <- names(spoke_bin_map)[as.integer(spoke_bin_map) == q]
    for (h in seq_len(h_bins)) {
      cell_target <- as.integer(hub_targets[[h]])
      hub_bin_ids <- names(hub_bin_map)[as.integer(hub_bin_map) == h]
      if (cell_target < 1L || length(spoke_bin_ids) < 1L || length(hub_bin_ids) < 1L) {
        if (cell_target > 0L) {
          cell_shortfall_detected <- TRUE
        }
        next
      }
      cell_pairs <- legal_pairs[
        as.character(legal_pairs$hub_item_id) %in% hub_bin_ids &
          as.character(legal_pairs$spoke_item_id) %in% spoke_bin_ids &
          !as.character(legal_pairs$pair_key) %in% seen_keys,
        ,
        drop = FALSE
      ]
      if (nrow(cell_pairs) < 1L) {
        cell_shortfall_detected <- TRUE
        next
      }
      seed <- as.integer((state$meta$seed %||% 1L) + (spoke_id * 1009L) + (q * 101L) + h)
      take <- min(cell_target, nrow(cell_pairs))
      if (take < cell_target) {
        cell_shortfall_detected <- TRUE
      }
      picked_idx <- withr::with_seed(seed, sample.int(nrow(cell_pairs), size = take, replace = FALSE))
      picked <- cell_pairs[picked_idx, , drop = FALSE]
      picked <- picked[order(picked$hub_item_id, picked$spoke_item_id), , drop = FALSE]
      seen_keys <- c(seen_keys, picked$pair_key)
      planned <- c(planned, lapply(seq_len(nrow(picked)), function(idx) {
        list(
          link_epoch_id = as.integer(epoch_id),
          spoke_id = as.integer(spoke_id),
          hub_item_id = as.character(picked$hub_item_id[[idx]]),
          spoke_item_id = as.character(picked$spoke_item_id[[idx]]),
          spoke_bin = as.integer(q),
          hub_bin = as.integer(h),
          pair_key = as.character(picked$pair_key[[idx]])
        )
      }))
    }
  }

  fallback_reallocation_count <- 0L
  if (length(planned) < feasible_target_edges) {
    existing_keys <- unique(c(seen_keys, vapply(planned, function(x) x$pair_key, character(1L))))
    fallback_pairs <- legal_pairs[!as.character(legal_pairs$pair_key) %in% existing_keys, , drop = FALSE]
    if (nrow(fallback_pairs) > 0L) {
      fallback_pairs$spoke_bin <- as.integer(spoke_bin_map[fallback_pairs$spoke_item_id])
      fallback_pairs$hub_bin <- as.integer(hub_bin_map[fallback_pairs$hub_item_id])
      fallback_pairs <- fallback_pairs[
        order(
          as.integer(fallback_pairs$spoke_bin),
          as.integer(fallback_pairs$hub_bin),
          as.character(fallback_pairs$hub_item_id),
          as.character(fallback_pairs$spoke_item_id)
        ),
        ,
        drop = FALSE
      ]
      take <- min(feasible_target_edges - length(planned), nrow(fallback_pairs))
      picked <- fallback_pairs[seq_len(take), , drop = FALSE]
      fallback_reallocation_count <- as.integer(nrow(picked))
      planned <- c(planned, lapply(seq_len(nrow(picked)), function(idx) {
        list(
          link_epoch_id = as.integer(epoch_id),
          spoke_id = as.integer(spoke_id),
          hub_item_id = as.character(picked$hub_item_id[[idx]]),
          spoke_item_id = as.character(picked$spoke_item_id[[idx]]),
          spoke_bin = as.integer(picked$spoke_bin[[idx]] %||% NA_integer_),
          hub_bin = as.integer(picked$hub_bin[[idx]] %||% NA_integer_),
          pair_key = as.character(picked$pair_key[[idx]])
        )
      }))
    }
  }

  if (length(planned) < 1L) {
    if (nrow(legal_pairs) > 0L) {
      rlang::abort(
        paste0(
          "Phase B probe-panel invariant failed: legal held-out probe candidates exist for spoke_id=",
          as.integer(spoke_id),
          " in link_epoch_id=",
          as.integer(epoch_id),
          " but construction produced an empty panel."
        )
      )
    }
    return(.adaptive_link_probe_empty_panel())
  }

  panel <- tibble::as_tibble(do.call(rbind, lapply(planned, as.data.frame, stringsAsFactors = FALSE)))
  panel$probe_edges_planned <- as.integer(target_edges)
  panel$probe_panel_reallocation_used <- isTRUE(cell_shortfall_detected) &&
    as.integer(fallback_reallocation_count) > 0L
  panel$planned_rank <- as.integer(seq_len(nrow(panel)))
  panel$realized <- FALSE
  panel$realized_step_id <- NA_integer_
  panel$realized_pair_id <- NA_integer_
  panel$realized_run_mode <- NA_character_
  panel <- panel[, c(
    "link_epoch_id", "spoke_id", "hub_item_id", "spoke_item_id", "spoke_bin", "hub_bin",
    "probe_edges_planned", "probe_panel_reallocation_used",
    "planned_rank", "pair_key", "realized", "realized_step_id", "realized_pair_id", "realized_run_mode"
  )]
  panel$probe_panel_id <- .adaptive_link_probe_panel_id(panel)
  panel <- dplyr::relocate(panel, "probe_panel_id")
  panel
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_ensure_panels <- function(state, controller = NULL, spoke_ids = NULL) {
  out <- state
  controller <- controller %||% .adaptive_controller_resolve(out)
  phase_ctx <- .adaptive_link_phase_context(out, controller = controller)
  if (!(.adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b"))) {
    return(out)
  }
  spoke_ids <- as.integer(spoke_ids %||% phase_ctx$active_spokes %||% integer())
  if (length(spoke_ids) < 1L) {
    return(out)
  }
  out$linking <- out$linking %||% list()
  probe <- .adaptive_link_probe_state(out)
  link_stage_log <- tibble::as_tibble(out$link_stage_log %||% new_link_stage_log())
  for (spoke_id in unique(spoke_ids)) {
    epoch_id <- .adaptive_link_probe_epoch_for_spoke(out, spoke_id = spoke_id)
    panel <- probe$panels_by_spoke[[as.character(spoke_id)]] %||% .adaptive_link_probe_empty_panel()
    panel <- tibble::as_tibble(panel)
    if (nrow(panel) < 1L || !all(as.integer(panel$link_epoch_id) == epoch_id)) {
      epoch_realized <- .adaptive_link_probe_realized_log_for_epoch(
        state = out,
        spoke_id = as.integer(spoke_id),
        epoch_id = as.integer(epoch_id)
      )
      has_realized_epoch_evidence <- nrow(epoch_realized) > 0L
      stage_rows <- link_stage_log[
        as.integer(link_stage_log$spoke_id) == as.integer(spoke_id) &
          as.integer(link_stage_log$link_epoch_id) == as.integer(epoch_id),
        ,
        drop = FALSE
      ]
      stage_panel_ids <- unique(as.character(stage_rows$probe_panel_id))
      stage_panel_ids <- stage_panel_ids[!is.na(stage_panel_ids) & nzchar(stage_panel_ids)]
      latest_stage_planned <- if (nrow(stage_rows) > 0L) {
        planned_vals <- as.integer(stage_rows$probe_edges_planned)
        if (any(is.finite(planned_vals), na.rm = TRUE)) {
          suppressWarnings(max(planned_vals, na.rm = TRUE))
        } else {
          NA_integer_
        }
      } else {
        NA_integer_
      }
      realized_panel_ids <- unique(as.character(epoch_realized$probe_panel_id))
      realized_panel_ids <- realized_panel_ids[!is.na(realized_panel_ids) & nzchar(realized_panel_ids)]
      has_stage_probe_evidence <- nrow(stage_rows) > 0L && any(
        (!is.na(stage_rows$probe_panel_id) & nzchar(as.character(stage_rows$probe_panel_id))) |
          (as.integer(stage_rows$probe_edges_planned) > 0L) |
          (as.integer(stage_rows$probe_edges_realized) > 0L),
        na.rm = TRUE
      )
      if (.adaptive_is_resumed_session(out) &&
        (isTRUE(has_realized_epoch_evidence) || isTRUE(has_stage_probe_evidence))) {
        built_panel <- .adaptive_link_probe_construct_panel(
          state = out,
          controller = controller,
          spoke_id = spoke_id
        )
        built_panel <- tibble::as_tibble(built_panel)
        if (nrow(built_panel) < 1L) {
          .adaptive_link_probe_resume_abort(
            paste0(
              "no persisted held-out probe panel is available for current link_epoch_id=",
              as.integer(epoch_id),
              " and deterministic reconstruction also failed"
            ),
            spoke_id = spoke_id
          )
        }
        built_panel_id <- as.character(built_panel$probe_panel_id[[1L]] %||% NA_character_)
        realized_pairs_compatible <- nrow(epoch_realized) < 1L ||
          all(as.character(epoch_realized$pair_key) %in% as.character(built_panel$pair_key))
        panel_planned_edges <- .adaptive_link_probe_planned_edges(built_panel)
        planned_size_compatible <- !is.finite(latest_stage_planned) ||
          is.na(latest_stage_planned) ||
          latest_stage_planned <= 0L ||
          identical(as.integer(latest_stage_planned), as.integer(panel_planned_edges))
        stage_id_mismatch <- length(stage_panel_ids) > 1L ||
          (length(stage_panel_ids) == 1L && !identical(stage_panel_ids[[1L]], built_panel_id))
        realized_id_mismatch <- length(realized_panel_ids) > 1L ||
          (length(realized_panel_ids) == 1L && !identical(realized_panel_ids[[1L]], built_panel_id))
        if ((isTRUE(stage_id_mismatch) || isTRUE(realized_id_mismatch)) &&
          isTRUE(realized_pairs_compatible) &&
          isTRUE(planned_size_compatible)) {
          if (nrow(stage_rows) > 0L) {
            stage_idx <- which(
              as.integer(link_stage_log$spoke_id) == as.integer(spoke_id) &
                as.integer(link_stage_log$link_epoch_id) == as.integer(epoch_id)
            )
            if (length(stage_idx) > 0L) {
              link_stage_log$probe_panel_id[stage_idx] <- built_panel_id
              out$link_stage_log <- link_stage_log
              stage_rows$probe_panel_id[] <- built_panel_id
            }
            stage_panel_ids <- built_panel_id
          }
          if (nrow(epoch_realized) > 0L) {
            realized_idx <- which(
              as.integer(probe$realized_edges$spoke_id) == as.integer(spoke_id) &
                as.integer(probe$realized_edges$link_epoch_id) == as.integer(epoch_id)
            )
            if (length(realized_idx) > 0L) {
              probe$realized_edges$probe_panel_id[realized_idx] <- built_panel_id
              probe$realized_index_by_panel <- .adaptive_link_probe_realized_index_build(probe$realized_edges)
              out$linking$probe <- probe
              epoch_realized$probe_panel_id[] <- built_panel_id
            }
            realized_panel_ids <- built_panel_id
          }
        }
        if (length(stage_panel_ids) > 1L ||
          (length(stage_panel_ids) == 1L && !identical(stage_panel_ids[[1L]], built_panel_id))) {
          .adaptive_link_probe_resume_abort(
            "reconstructed probe panel id does not match canonical `link_stage_log$probe_panel_id`",
            spoke_id = spoke_id
          )
        }
        if (length(realized_panel_ids) > 1L ||
          (length(realized_panel_ids) == 1L && !identical(realized_panel_ids[[1L]], built_panel_id))) {
          .adaptive_link_probe_resume_abort(
            "reconstructed probe panel id does not match canonical `realized_edges$probe_panel_id`",
            spoke_id = spoke_id
          )
        }
        if (nrow(epoch_realized) > 0L &&
          !isTRUE(realized_pairs_compatible)) {
          .adaptive_link_probe_resume_abort(
            "reconstructed probe panel does not contain all canonical realized probe edges",
            spoke_id = spoke_id
          )
        }
        if (is.finite(latest_stage_planned) &&
          !is.na(latest_stage_planned) &&
          latest_stage_planned > 0L &&
          !isTRUE(planned_size_compatible)) {
          .adaptive_link_probe_resume_abort(
            "reconstructed probe panel size does not match canonical `probe_edges_planned`",
            spoke_id = spoke_id
          )
        }
        probe$panels_by_spoke[[as.character(spoke_id)]] <- built_panel
        next
      }
      built_panel <- .adaptive_link_probe_construct_panel(
        state = out,
        controller = controller,
        spoke_id = spoke_id
      )
      built_panel <- tibble::as_tibble(built_panel)
      if (nrow(built_panel) < 1L) {
        rlang::abort(
          paste0(
            "Phase B probe-panel invariant failed: no held-out panel could be constructed for spoke_id=",
            as.integer(spoke_id),
            " in link_epoch_id=",
            as.integer(epoch_id),
            "."
          )
        )
      }
      probe$panels_by_spoke[[as.character(spoke_id)]] <- built_panel
    }
  }
  out$linking$probe <- probe
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_max_non_anchor_pairs <- function(active_hub_ids, spoke_ids) {
  as.integer(length(unique(as.character(active_hub_ids))) * length(unique(as.character(spoke_ids))))
}

#' @keywords internal
#' @noRd
.adaptive_link_assert_active_domain_count <- function(stage_name,
                                                      n_candidates_after_active_domain,
                                                      active_hub_ids,
                                                      spoke_ids,
                                                      spoke_id) {
  if (!stage_name %in% c("long_link", "mid_link", "local_link")) {
    return(invisible(NULL))
  }
  observed <- as.integer(n_candidates_after_active_domain %||% NA_integer_)
  if (!is.finite(observed) || is.na(observed)) {
    return(invisible(NULL))
  }
  max_pairs <- .adaptive_link_max_non_anchor_pairs(
    active_hub_ids = active_hub_ids,
    spoke_ids = spoke_ids
  )
  if (observed > max_pairs) {
    rlang::abort(
      paste0(
        "Phase B active-domain invariant failed for stage `",
        stage_name,
        "` and spoke_id=",
        as.integer(spoke_id),
        ": n_candidates_after_active_domain=",
        observed,
        " exceeds the maximum possible active-domain cross-set pairs=",
        max_pairs,
        "."
      )
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.adaptive_link_assert_non_anchor_candidate_domain <- function(candidates,
                                                              stage_name,
                                                              spoke_id,
                                                              hub_id,
                                                              active_hub_ids,
                                                              reserved_keys = character(),
                                                              set_map) {
  if (!stage_name %in% c("long_link", "mid_link", "local_link")) {
    return(invisible(NULL))
  }
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) < 1L) {
    return(invisible(NULL))
  }
  hub_id <- as.integer(hub_id)
  spoke_id <- as.integer(spoke_id)
  active_hub_ids <- unique(as.character(active_hub_ids))
  set_map <- stats::setNames(as.integer(set_map), names(set_map))
  pair_keys <- make_unordered_key(cand$i, cand$j)

  invalid_idx <- vapply(seq_len(nrow(cand)), function(idx) {
    i_id <- as.character(cand$i[[idx]])
    j_id <- as.character(cand$j[[idx]])
    i_set <- as.integer(set_map[[i_id]] %||% NA_integer_)
    j_set <- as.integer(set_map[[j_id]] %||% NA_integer_)
    i_hub <- identical(i_set, hub_id)
    j_hub <- identical(j_set, hub_id)
    if (!isTRUE(xor(i_hub, j_hub))) {
      return(TRUE)
    }
    hub_item_id <- if (isTRUE(i_hub)) i_id else j_id
    spoke_item_set <- if (isTRUE(i_hub)) j_set else i_set
    !hub_item_id %in% active_hub_ids || !identical(spoke_item_set, spoke_id)
  }, logical(1L))

  if (any(invalid_idx)) {
    bad_keys <- unique(pair_keys[invalid_idx])
    rlang::abort(
      paste0(
        "Phase B non-anchor routing invariant failed for stage `",
        stage_name,
        "` and spoke_id=",
        spoke_id,
        ": generated candidates fell outside active_link_items(s). Bad pair keys: ",
        paste(bad_keys, collapse = ", "),
        "."
      )
    )
  }

  reserved_keys <- unique(as.character(reserved_keys))
  if (length(reserved_keys) > 0L && any(pair_keys %in% reserved_keys)) {
    bad_keys <- unique(pair_keys[pair_keys %in% reserved_keys])
    rlang::abort(
      paste0(
        "Phase B probe isolation invariant failed for stage `",
        stage_name,
        "` and spoke_id=",
        spoke_id,
        ": reserved held-out probe pairs entered linking-active candidates. Bad pair keys: ",
        paste(bad_keys, collapse = ", "),
        "."
      )
    )
  }

  invisible(NULL)
}

#' @keywords internal
#' @noRd
.adaptive_within_set_same_group_pairs <- function(item_ids, rank_index, dist_stratum_global = 0L) {
  item_ids <- unique(as.character(item_ids))
  if (length(item_ids) < 2L) {
    return(tibble::tibble(
      i = character(),
      j = character(),
      dist_stratum_global = integer()
    ))
  }

  item_rank <- as.integer(rank_index[item_ids])
  if (any(!is.finite(item_rank))) {
    rlang::abort(
      "Within-set candidate construction invariant failed: ranked items must have finite ranks."
    )
  }
  order_rank <- order(item_rank)
  item_ids <- item_ids[order_rank]

  pair_idx <- utils::combn(seq_along(item_ids), 2L)
  tibble::tibble(
    i = as.character(item_ids[pair_idx[1L, ]]),
    j = as.character(item_ids[pair_idx[2L, ]]),
    dist_stratum_global = rep.int(as.integer(dist_stratum_global), ncol(pair_idx))
  )
}

#' @keywords internal
#' @noRd
.adaptive_within_set_cross_group_pairs <- function(left_ids,
                                                   right_ids,
                                                   rank_index,
                                                   stratum_map) {
  left_ids <- unique(as.character(left_ids))
  right_ids <- unique(as.character(right_ids))
  if (length(left_ids) < 1L || length(right_ids) < 1L) {
    return(tibble::tibble(
      i = character(),
      j = character(),
      dist_stratum_global = integer()
    ))
  }

  left_rank <- as.integer(rank_index[left_ids])
  right_rank <- as.integer(rank_index[right_ids])
  left_strata <- as.integer(stratum_map[left_ids])
  right_strata <- as.integer(stratum_map[right_ids])
  if (any(!is.finite(left_rank)) || any(!is.finite(right_rank))) {
    rlang::abort(
      "Within-set candidate construction invariant failed: ranked items must have finite ranks."
    )
  }
  if (any(!is.finite(left_strata)) || any(!is.finite(right_strata))) {
    rlang::abort(
      "Within-set candidate construction invariant failed: ranked items must have finite strata."
    )
  }

  left_order <- order(left_rank)
  right_order <- order(right_rank)
  left_ids <- left_ids[left_order]
  right_ids <- right_ids[right_order]
  left_rank <- left_rank[left_order]
  right_rank <- right_rank[right_order]
  left_strata <- left_strata[left_order]
  right_strata <- right_strata[right_order]

  n_left <- length(left_ids)
  n_right <- length(right_ids)
  left_vec <- rep(left_ids, each = n_right)
  right_vec <- rep(right_ids, times = n_left)
  left_rank_vec <- rep(left_rank, each = n_right)
  right_rank_vec <- rep(right_rank, times = n_left)
  left_strata_vec <- rep(left_strata, each = n_right)
  right_strata_vec <- rep(right_strata, times = n_left)

  i_vals <- ifelse(left_rank_vec < right_rank_vec, left_vec, right_vec)
  j_vals <- ifelse(left_rank_vec < right_rank_vec, right_vec, left_vec)
  i_rank <- pmin(left_rank_vec, right_rank_vec)
  j_rank <- pmax(left_rank_vec, right_rank_vec)
  order_idx <- order(i_rank, j_rank)

  tibble::tibble(
    i = as.character(i_vals[order_idx]),
    j = as.character(j_vals[order_idx]),
    dist_stratum_global = as.integer(abs(left_strata_vec[order_idx] - right_strata_vec[order_idx]))
  )
}

#' @keywords internal
#' @noRd
.adaptive_within_set_stage_sorted_inputs <- function(ids,
                                                     anchor_ids,
                                                     rank_index,
                                                     stratum_map,
                                                     stage_name) {
  ids <- unique(as.character(ids))
  if (length(ids) < 2L) {
    return(list(
      sorted_ids = character(),
      sorted_anchor = logical(),
      sorted_strata = integer()
    ))
  }

  item_rank <- as.integer(rank_index[ids])
  if (any(!is.finite(item_rank))) {
    rlang::abort(
      "Within-set candidate construction invariant failed: ranked items must have finite ranks."
    )
  }
  order_rank <- order(item_rank)
  sorted_ids <- ids[order_rank]
  sorted_anchor <- sorted_ids %in% intersect(unique(as.character(anchor_ids)), sorted_ids)

  if (stage_name %in% c("long_link", "mid_link")) {
    keep <- !sorted_anchor
    sorted_ids <- sorted_ids[keep]
    sorted_anchor <- sorted_anchor[keep]
  }

  if (length(sorted_ids) < 2L) {
    return(list(
      sorted_ids = character(),
      sorted_anchor = logical(),
      sorted_strata = integer()
    ))
  }

  sorted_strata <- as.integer(stratum_map[sorted_ids])
  if (any(!is.finite(sorted_strata))) {
    rlang::abort(
      "Within-set candidate construction invariant failed: ranked items must have finite strata."
    )
  }

  list(
    sorted_ids = as.character(sorted_ids),
    sorted_anchor = as.logical(sorted_anchor),
    sorted_strata = as.integer(sorted_strata)
  )
}

#' @keywords internal
#' @noRd
.adaptive_within_set_stage_right_mask <- function(right_strata,
                                                  right_anchor,
                                                  left_stratum,
                                                  left_anchor,
                                                  stage_name,
                                                  bounds) {
  if (identical(stage_name, "anchor_link")) {
    return(xor(as.logical(right_anchor), rep.int(as.logical(left_anchor), length(right_anchor))))
  }

  dist <- abs(as.integer(right_strata) - as.integer(left_stratum))
  dist >= as.integer(bounds$min) & dist <= as.integer(bounds$max)
}

#' @keywords internal
#' @noRd
.adaptive_within_set_anchor_pairs_bounded <- function(sorted_ids,
                                                      sorted_anchor,
                                                      sorted_strata,
                                                      C_max = NULL,
                                                      seed = NULL) {
  sorted_ids <- as.character(sorted_ids)
  sorted_anchor <- as.logical(sorted_anchor)
  sorted_strata <- as.integer(sorted_strata)

  if (length(sorted_ids) < 2L) {
    return(list(
      candidates = tibble::tibble(
        i = character(),
        j = character(),
        dist_stratum_global = integer()
      ),
      total_legal = 0L,
      bounded_used = FALSE
    ))
  }

  anchor_positions <- which(sorted_anchor)
  nonanchor_positions <- which(!sorted_anchor)
  if (length(anchor_positions) < 1L || length(nonanchor_positions) < 1L) {
    return(list(
      candidates = tibble::tibble(
        i = character(),
        j = character(),
        dist_stratum_global = integer()
      ),
      total_legal = 0L,
      bounded_used = FALSE
    ))
  }

  n <- length(sorted_ids)
  anchor_suffix <- rev(cumsum(rev(as.integer(sorted_anchor))))
  nonanchor_suffix <- rev(cumsum(rev(as.integer(!sorted_anchor))))
  counts_by_left <- ifelse(
    sorted_anchor[seq_len(n - 1L)],
    nonanchor_suffix[seq.int(2L, n)],
    anchor_suffix[seq.int(2L, n)]
  )
  total_legal <- as.integer(sum(counts_by_left))

  if (total_legal < 1L) {
    return(list(
      candidates = tibble::tibble(
        i = character(),
        j = character(),
        dist_stratum_global = integer()
      ),
      total_legal = 0L,
      bounded_used = FALSE
    ))
  }

  C_max <- as.integer(C_max %||% NA_integer_)
  bounded_used <- is.finite(C_max) && total_legal > C_max
  keep_positions_raw <- if (isTRUE(bounded_used)) {
    withr::with_seed(as.integer(seed), {
      sample.int(total_legal, size = C_max, replace = FALSE)
    })
  } else {
    seq_len(total_legal)
  }
  keep_sort_order <- order(keep_positions_raw)
  keep_positions <- keep_positions_raw[keep_sort_order]

  anchor_cum <- cumsum(as.integer(sorted_anchor))
  nonanchor_cum <- cumsum(as.integer(!sorted_anchor))
  out_n <- length(keep_positions)
  out_i <- character(out_n)
  out_j <- character(out_n)
  out_dist <- integer(out_n)
  out_idx <- 1L
  keep_idx <- 1L
  offset <- 0L

  for (left_idx in seq_len(n - 1L)) {
    count_left <- as.integer(counts_by_left[[left_idx]])
    if (count_left < 1L) {
      next
    }
    block_end <- offset + count_left
    if (keep_idx > out_n || keep_positions[[keep_idx]] > block_end) {
      offset <- block_end
      next
    }

    block_keep_end <- keep_idx
    while (block_keep_end <= out_n && keep_positions[[block_keep_end]] <= block_end) {
      block_keep_end <- block_keep_end + 1L
    }

    local_positions <- keep_positions[keep_idx:(block_keep_end - 1L)] - offset
    selected_right_idx <- if (isTRUE(sorted_anchor[[left_idx]])) {
      nonanchor_offset <- as.integer(nonanchor_cum[[left_idx]] %||% 0L)
      nonanchor_positions[nonanchor_offset + local_positions]
    } else {
      anchor_offset <- as.integer(anchor_cum[[left_idx]] %||% 0L)
      anchor_positions[anchor_offset + local_positions]
    }

    write_n <- length(selected_right_idx)
    write_range <- out_idx:(out_idx + write_n - 1L)
    out_i[write_range] <- sorted_ids[[left_idx]]
    out_j[write_range] <- sorted_ids[selected_right_idx]
    out_dist[write_range] <- abs(sorted_strata[selected_right_idx] - sorted_strata[[left_idx]])
    out_idx <- out_idx + write_n
    keep_idx <- block_keep_end
    offset <- block_end
    if (keep_idx > out_n) {
      break
    }
  }

  cand <- tibble::tibble(
    i = as.character(out_i),
    j = as.character(out_j),
    dist_stratum_global = as.integer(out_dist)
  )
  if (isTRUE(bounded_used)) {
    cand <- cand[order(keep_sort_order), , drop = FALSE]
  }

  list(
    candidates = cand,
    total_legal = as.integer(total_legal),
    bounded_used = as.logical(bounded_used)
  )
}

#' @keywords internal
#' @noRd
.adaptive_within_set_direct_pairs_bounded <- function(ids,
                                                      anchor_ids,
                                                      rank_index,
                                                      stratum_map,
                                                      stage_name,
                                                      bounds,
                                                      C_max = NULL,
                                                      seed = NULL) {
  stage_inputs <- .adaptive_within_set_stage_sorted_inputs(
    ids = ids,
    anchor_ids = anchor_ids,
    rank_index = rank_index,
    stratum_map = stratum_map,
    stage_name = stage_name
  )
  sorted_ids <- as.character(stage_inputs$sorted_ids)
  sorted_anchor <- as.logical(stage_inputs$sorted_anchor)
  sorted_strata <- as.integer(stage_inputs$sorted_strata)

  if (length(sorted_ids) < 2L) {
    return(list(
      candidates = tibble::tibble(
        i = character(),
        j = character(),
        dist_stratum_global = integer()
      ),
      total_legal = 0L,
      bounded_used = FALSE
    ))
  }

  if (identical(stage_name, "anchor_link")) {
    return(.adaptive_within_set_anchor_pairs_bounded(
      sorted_ids = sorted_ids,
      sorted_anchor = sorted_anchor,
      sorted_strata = sorted_strata,
      C_max = C_max,
      seed = seed
    ))
  }

  counts_by_left <- integer(length(sorted_ids) - 1L)
  total_legal <- 0L
  for (left_idx in seq_len(length(sorted_ids) - 1L)) {
    right_idx <- seq.int(left_idx + 1L, length(sorted_ids))
    right_mask <- .adaptive_within_set_stage_right_mask(
      right_strata = sorted_strata[right_idx],
      right_anchor = sorted_anchor[right_idx],
      left_stratum = sorted_strata[[left_idx]],
      left_anchor = sorted_anchor[[left_idx]],
      stage_name = stage_name,
      bounds = bounds
    )
    counts_by_left[[left_idx]] <- as.integer(sum(right_mask))
    total_legal <- total_legal + counts_by_left[[left_idx]]
  }

  if (total_legal < 1L) {
    return(list(
      candidates = tibble::tibble(
        i = character(),
        j = character(),
        dist_stratum_global = integer()
      ),
      total_legal = 0L,
      bounded_used = FALSE
    ))
  }

  C_max <- as.integer(C_max %||% NA_integer_)
  bounded_used <- is.finite(C_max) && total_legal > C_max
  keep_positions_raw <- if (isTRUE(bounded_used)) {
    withr::with_seed(as.integer(seed), {
      sample.int(total_legal, size = C_max, replace = FALSE)
    })
  } else {
    seq_len(total_legal)
  }
  keep_sort_order <- order(keep_positions_raw)
  keep_positions <- keep_positions_raw[keep_sort_order]

  out_n <- length(keep_positions)
  out_i <- character(out_n)
  out_j <- character(out_n)
  out_dist <- integer(out_n)
  out_idx <- 1L
  keep_idx <- 1L
  offset <- 0L

  for (left_idx in seq_len(length(sorted_ids) - 1L)) {
    count_left <- as.integer(counts_by_left[[left_idx]])
    if (count_left < 1L) {
      next
    }
    block_end <- offset + count_left
    if (keep_idx > out_n || keep_positions[[keep_idx]] > block_end) {
      offset <- block_end
      next
    }
    block_keep_end <- keep_idx
    while (block_keep_end <= out_n && keep_positions[[block_keep_end]] <= block_end) {
      block_keep_end <- block_keep_end + 1L
    }
    local_positions <- keep_positions[keep_idx:(block_keep_end - 1L)] - offset
    right_idx <- seq.int(left_idx + 1L, length(sorted_ids))
    right_mask <- .adaptive_within_set_stage_right_mask(
      right_strata = sorted_strata[right_idx],
      right_anchor = sorted_anchor[right_idx],
      left_stratum = sorted_strata[[left_idx]],
      left_anchor = sorted_anchor[[left_idx]],
      stage_name = stage_name,
      bounds = bounds
    )
    legal_right_idx <- right_idx[right_mask]
    selected_right_idx <- legal_right_idx[local_positions]
    write_n <- length(selected_right_idx)
    write_range <- out_idx:(out_idx + write_n - 1L)
    out_i[write_range] <- sorted_ids[[left_idx]]
    out_j[write_range] <- sorted_ids[selected_right_idx]
    out_dist[write_range] <- abs(sorted_strata[selected_right_idx] - sorted_strata[[left_idx]])
    out_idx <- out_idx + write_n
    keep_idx <- block_keep_end
    offset <- block_end
    if (keep_idx > out_n) {
      break
    }
  }

  cand <- tibble::tibble(
    i = as.character(out_i),
    j = as.character(out_j),
    dist_stratum_global = as.integer(out_dist)
  )
  if (isTRUE(bounded_used)) {
    cand <- cand[order(keep_sort_order), , drop = FALSE]
  }

  list(
    candidates = cand,
    total_legal = as.integer(total_legal),
    bounded_used = as.logical(bounded_used)
  )
}

#' @keywords internal
#' @noRd
.adaptive_within_set_direct_pairs <- function(ids,
                                              anchor_ids,
                                              rank_index,
                                              stratum_map,
                                              stage_name,
                                              bounds,
                                              C_max = NULL,
                                              seed = NULL) {
  .adaptive_within_set_direct_pairs_bounded(
    ids = ids,
    anchor_ids = anchor_ids,
    rank_index = rank_index,
    stratum_map = stratum_map,
    stage_name = stage_name,
    bounds = bounds,
    C_max = C_max,
    seed = seed
  )$candidates
}

#' @keywords internal
#' @noRd
.adaptive_link_direct_cross_pairs <- function(hub_item_ids,
                                              spoke_ids,
                                              rank_index,
                                              stratum_map) {
  hub_item_ids <- unique(as.character(hub_item_ids))
  spoke_ids <- unique(as.character(spoke_ids))
  if (length(hub_item_ids) < 1L || length(spoke_ids) < 1L) {
    return(tibble::tibble(
      hub_item_id = character(),
      spoke_item_id = character(),
      i = character(),
      j = character(),
      pair_key = character(),
      dist_stratum_global = integer()
    ))
  }

  hub_rank <- as.integer(rank_index[hub_item_ids])
  spoke_rank <- as.integer(rank_index[spoke_ids])
  names(hub_rank) <- hub_item_ids
  names(spoke_rank) <- spoke_ids
  if (any(!is.finite(hub_rank)) || any(!is.finite(spoke_rank))) {
    rlang::abort(
      "Phase B candidate construction invariant failed: ranked hub/spoke items must have finite routing ranks."
    )
  }

  hub_strata <- as.integer(stratum_map[hub_item_ids])
  spoke_strata <- as.integer(stratum_map[spoke_ids])
  names(hub_strata) <- hub_item_ids
  names(spoke_strata) <- spoke_ids
  if (any(!is.finite(hub_strata)) || any(!is.finite(spoke_strata))) {
    rlang::abort(
      "Phase B candidate construction invariant failed: ranked hub/spoke items must have finite routing strata."
    )
  }

  n_hub <- length(hub_item_ids)
  n_spoke <- length(spoke_ids)
  hub_item_id <- rep(hub_item_ids, each = n_spoke)
  spoke_item_id <- rep(spoke_ids, times = n_hub)
  hub_rank_vec <- rep(hub_rank, each = n_spoke)
  spoke_rank_vec <- rep(spoke_rank, times = n_hub)
  hub_strata_vec <- rep(hub_strata, each = n_spoke)
  spoke_strata_vec <- rep(spoke_strata, times = n_hub)
  pair_key_vec <- make_unordered_key(hub_item_id, spoke_item_id)

  i_vals <- ifelse(hub_rank_vec < spoke_rank_vec, hub_item_id, spoke_item_id)
  j_vals <- ifelse(hub_rank_vec < spoke_rank_vec, spoke_item_id, hub_item_id)
  order_idx <- order(
    pmin(hub_rank_vec, spoke_rank_vec),
    pmax(hub_rank_vec, spoke_rank_vec)
  )

  tibble::tibble(
    hub_item_id = as.character(hub_item_id[order_idx]),
    spoke_item_id = as.character(spoke_item_id[order_idx]),
    i = as.character(i_vals[order_idx]),
    j = as.character(j_vals[order_idx]),
    pair_key = as.character(pair_key_vec[order_idx]),
    dist_stratum_global = as.integer(abs(hub_strata_vec[order_idx] - spoke_strata_vec[order_idx]))
  )
}

#' @keywords internal
#' @noRd
generate_stage_candidates_from_state <- function(state,
                                                 stage_name,
                                                 fallback_name,
                                                 C_max,
                                                 seed,
                                                 link_spoke_id = NA_integer_) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!stage_name %in% .adaptive_stage_order()) {
    rlang::abort("`stage_name` must be one of the stage labels.")
  }

  state <- .adaptive_refresh_round_anchors(state)
  proxy <- .adaptive_rank_proxy(state)
  controller <- .adaptive_controller_resolve(state)
  is_link_mode <- .adaptive_link_mode_active(controller)
  hub_id <- as.integer(controller$hub_id %||% 1L)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  link_phase_b_active <- isTRUE(is_link_mode) && identical(phase_ctx$phase, "phase_b")
  effective_n <- as.integer(length(state$item_ids))
  if (isTRUE(is_link_mode) && !isTRUE(link_phase_b_active)) {
    active_set_id <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
    if (!is.na(active_set_id)) {
      scoped_n <- as.integer(sum(as.integer(state$items$set_id) == active_set_id, na.rm = TRUE))
      if (is.finite(scoped_n) && scoped_n >= 2L) {
        effective_n <- scoped_n
      }
    }
  }
  defaults <- adaptive_defaults(effective_n)

  if (isTRUE(link_phase_b_active)) {
    eligible_spokes <- as.integer(phase_ctx$active_spokes %||% integer())
    if (length(eligible_spokes) < 1L) {
      rlang::abort(
        "Phase metadata and routing mode disagree: phase marked phase_b but no ready spokes are eligible."
      )
    }
    requested_spoke <- as.integer(link_spoke_id %||% NA_integer_)
    spoke_id <- if (!is.na(requested_spoke)) {
      requested_spoke
    } else {
      .adaptive_link_active_spoke(
        state,
        controller,
        eligible_spoke_ids = eligible_spokes
      )
    }
    if (is.na(spoke_id)) {
      return(tibble::tibble(i = character(), j = character()))
    }
    if (!spoke_id %in% eligible_spokes) {
      rlang::abort(
        paste0(
          "Cross-set candidate generation invariant failed: requested spoke_id=",
          as.integer(spoke_id),
          " is not eligible in phase_b (eligible: ",
          paste(sort(unique(eligible_spokes)), collapse = ", "),
          ")."
        )
      )
    }
    if (isTRUE(controller$allow_spoke_spoke_cross_set %||% FALSE)) {
      .adaptive_abort_unsupported_phase_b_public_control(
        field = "`allow_spoke_spoke_cross_set = TRUE`",
        detail = "The current reviewed hub-and-spoke runtime supports only hub<->spoke Phase B routing."
      )
    }
    hub_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == hub_id])
    spoke_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == spoke_id])
    active_items <- .adaptive_link_active_item_ids(state, spoke_id = spoke_id, hub_id = hub_id)
    active_hub_ids <- as.character(active_items$active_hub)
    if (length(hub_ids) < 1L) {
      rlang::abort(
        paste0(
          "Cross-set candidate generation invariant failed: ",
          "no hub items found for hub_id=",
          as.integer(hub_id),
          "."
        )
      )
    }
    if (length(spoke_ids) < 1L) {
      rlang::abort(
        paste0(
          "Cross-set candidate generation invariant failed: no spoke items found for spoke_id=",
          as.integer(spoke_id),
          "."
        )
      )
    }
    local_inputs <- .adaptive_link_refit_local_inputs(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      defaults = defaults
    )
    hub_ids <- as.character(local_inputs$hub_ids)
    spoke_ids <- as.character(local_inputs$spoke_ids)
    active_items <- local_inputs$active_items %||% list()
    active_hub_ids <- as.character(active_items$active_hub %||% character())
    routing_hub_ids <- if (identical(stage_name, "anchor_link")) hub_ids else active_hub_ids
    active_ids <- unique(c(routing_hub_ids, spoke_ids))
    if (length(active_ids) < 2L) {
      return(tibble::tibble(i = character(), j = character()))
    }
    active_scores <- as.double((local_inputs$routing_scores %||% numeric())[active_ids])
    names(active_scores) <- as.character(active_ids)
    strata <- .adaptive_assign_strata(active_scores, defaults)
    rank_index <- strata$rank_index
    stratum_map <- strata$stratum_map
    # In linking Phase B, hub anchors are derived from hub-only ranks.
    hub_anchor_ids <- as.character(local_inputs$hub_anchor_ids %||% character())
    coverage <- local_inputs$coverage %||% list(
      bin_map = stats::setNames(integer(), character()),
      bins_used = NA_integer_,
      bins_undercovered = integer(),
      source = NA_character_
    )
  } else if (isTRUE(is_link_mode)) {
    active_set <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
    if (is.na(active_set)) {
      return(tibble::tibble(i = character(), j = character()))
    }
    active_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == active_set])
    if (length(active_ids) < 2L) {
      return(tibble::tibble(i = character(), j = character()))
    }
    active_scores <- proxy$scores[active_ids]
    strata <- .adaptive_assign_strata(active_scores, defaults)
    rank_index <- strata$rank_index
    stratum_map <- strata$stratum_map
    ids <- names(sort(rank_index))
    anchor_ids <- .adaptive_select_rolling_anchors(active_scores, defaults)
  } else {
    strata <- .adaptive_assign_strata(proxy$scores, defaults)
    rank_index <- strata$rank_index
    stratum_map <- strata$stratum_map
    ids <- names(sort(rank_index))
    anchor_ids <- as.character(state$round$anchor_ids %||% character())
    if (length(anchor_ids) == 0L) {
      anchor_ids <- .adaptive_select_rolling_anchors(proxy$scores, defaults)
    }
  }

  bounds <- .adaptive_stage_distance_bounds(stage_name, fallback_name, defaults)
  n_after_route_filters <- NA_integer_
  n_after_active_domain <- NA_integer_
  n_after_stage_filters <- NA_integer_
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))

  if (isTRUE(link_phase_b_active)) {
    n_after_route_filters <- 0L
    n_after_active_domain <- 0L
    n_after_stage_filters <- 0L
  }

  if (isTRUE(link_phase_b_active)) {
    routed_pairs <- .adaptive_link_direct_cross_pairs(
      hub_item_ids = routing_hub_ids,
      spoke_ids = spoke_ids,
      rank_index = rank_index,
      stratum_map = stratum_map
    )
    n_after_route_filters <- as.integer(nrow(routed_pairs))
    active_domain_pairs <- if (identical(stage_name, "anchor_link")) {
      routed_pairs
    } else {
      routed_pairs[as.character(routed_pairs$hub_item_id) %in% active_hub_ids, , drop = FALSE]
    }
    n_after_active_domain <- as.integer(nrow(active_domain_pairs))
    .adaptive_link_assert_active_domain_count(
      stage_name = stage_name,
      n_candidates_after_active_domain = n_after_active_domain,
      active_hub_ids = active_hub_ids,
      spoke_ids = spoke_ids,
      spoke_id = spoke_id
    )

    if (nrow(active_domain_pairs) < 1L) {
      return(.adaptive_set_candidate_filter_counts(
        tibble::tibble(i = character(), j = character()),
        list(
          n_candidates_after_route_filters = as.integer(n_after_route_filters %||% NA_integer_),
          n_candidates_after_active_domain = as.integer(n_after_active_domain %||% NA_integer_),
          n_candidates_after_stage_filters = 0L
        )
      ))
    }

    stage_keep <- if (identical(stage_name, "anchor_link")) {
      as.character(active_domain_pairs$hub_item_id) %in% hub_anchor_ids
    } else {
      as.integer(active_domain_pairs$dist_stratum_global) >= bounds$min &
        as.integer(active_domain_pairs$dist_stratum_global) <= bounds$max
    }
    cand <- active_domain_pairs[stage_keep, , drop = FALSE]

    if (nrow(cand) > 0L) {
      spoke_bin <- as.integer(coverage$bin_map[as.character(cand$spoke_item_id)])
      bins_undercovered <- as.integer(coverage$bins_undercovered %||% integer())
      cand$coverage_priority <- as.integer(!is.na(spoke_bin) & spoke_bin %in% bins_undercovered)
      cand$coverage_bin_spoke <- spoke_bin
      cand$link_spoke_id <- as.integer(spoke_id)
      cand$coverage_bins_used <- as.integer(coverage$bins_used)
      cand$coverage_source <- as.character(coverage$source)
    }

    reserved_keys <- character()
    if (!isTRUE(.adaptive_link_spoke_is_frozen(controller, spoke_id))) {
      current_epoch_id <- .adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)
      reserved_keys <- .adaptive_link_probe_reserved_keys(
        state,
        spoke_id = spoke_id,
        epoch_id = current_epoch_id
      )
      if (length(reserved_keys) < 1L) {
        fallback_panel <- .adaptive_link_probe_panel_for_spoke(
          state,
          spoke_id = spoke_id,
          epoch_id = NULL
        )
        reserved_keys <- unique(as.character(fallback_panel$pair_key))
      }
      if (length(reserved_keys) > 0L && nrow(cand) > 0L) {
        cand <- cand[!as.character(cand$pair_key) %in% reserved_keys, , drop = FALSE]
      }
    }

    if (nrow(cand) > 0L) {
      cand <- cand[, c(
        "i", "j", "dist_stratum_global", "coverage_priority", "coverage_bin_spoke",
        "link_spoke_id", "coverage_bins_used", "coverage_source"
      )]
    } else {
      cand <- tibble::tibble(i = character(), j = character())
    }

    .adaptive_link_assert_non_anchor_candidate_domain(
      candidates = cand,
      stage_name = stage_name,
      spoke_id = spoke_id,
      hub_id = hub_id,
      active_hub_ids = active_hub_ids,
      reserved_keys = reserved_keys,
      set_map = set_map
    )

    if (nrow(cand) < 1L) {
      return(.adaptive_set_candidate_filter_counts(
        tibble::tibble(i = character(), j = character()),
        list(
          n_candidates_after_route_filters = as.integer(n_after_route_filters %||% NA_integer_),
          n_candidates_after_active_domain = as.integer(n_after_active_domain %||% NA_integer_),
          n_candidates_after_stage_filters = 0L
        )
      ))
    }

    cand <- .adaptive_uniform_subsample_pairs(cand, C_max = as.integer(C_max), seed = as.integer(seed))
    return(.adaptive_set_candidate_filter_counts(
      cand,
      list(
        n_candidates_after_route_filters = as.integer(n_after_route_filters %||% NA_integer_),
        n_candidates_after_active_domain = as.integer(n_after_active_domain %||% NA_integer_),
        n_candidates_after_stage_filters = as.integer(nrow(cand))
      )
    ))
  }

  direct_pairs <- .adaptive_within_set_direct_pairs_bounded(
    ids = ids,
    anchor_ids = anchor_ids,
    rank_index = rank_index,
    stratum_map = stratum_map,
    stage_name = stage_name,
    bounds = bounds,
    C_max = as.integer(C_max),
    seed = as.integer(seed)
  )
  cand <- tibble::as_tibble(direct_pairs$candidates)
  n_after_stage_filters <- as.integer(direct_pairs$total_legal)
  if (nrow(cand) < 1L) {
    return(.adaptive_set_candidate_filter_counts(
      tibble::tibble(i = character(), j = character()),
      list(
        n_candidates_after_route_filters = as.integer(n_after_route_filters %||% NA_integer_),
        n_candidates_after_active_domain = as.integer(n_after_active_domain %||% NA_integer_),
        n_candidates_after_stage_filters = as.integer(n_after_stage_filters %||% NA_integer_),
        n_candidates_legal_domain_total = as.integer(direct_pairs$total_legal),
        bounded_direct_construction_used = isTRUE(direct_pairs$bounded_used)
      )
    ))
  }

  .adaptive_set_candidate_filter_counts(
    cand,
    list(
      n_candidates_after_route_filters = as.integer(n_after_route_filters %||% NA_integer_),
      n_candidates_after_active_domain = as.integer(n_after_active_domain %||% NA_integer_),
      n_candidates_after_stage_filters = as.integer(nrow(cand)),
      n_candidates_legal_domain_total = as.integer(direct_pairs$total_legal),
      bounded_direct_construction_used = isTRUE(direct_pairs$bounded_used)
    )
  )
}

#' @keywords internal
#' @noRd
.adaptive_linking_selection_order <- function(candidates,
                                              utility_mode = "linking_d_optimal_transform",
                                              stage_name = NA_character_,
                                              spoke_id = NA_integer_) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) == 0L) {
    return(integer())
  }
  idx <- seq_len(nrow(cand))
  if ("coverage_priority" %in% names(cand)) {
    coverage_idx <- idx[as.integer(cand$coverage_priority[idx]) > 0L]
    if (length(coverage_idx) > 0L) {
      idx <- coverage_idx
    }
  }
  utility_col <- .adaptive_resolve_selection_column(utility_mode)
  if (is.na(utility_col) || !utility_col %in% names(cand)) {
    rlang::abort(sprintf(
      paste0(
        ".adaptive_linking_selection_order invariant failed: canonical D-opt ordering ",
        "could not proceed%s%s because `%s` is unavailable."
      ),
      if (!is.na(stage_name)) paste0(" for stage=", stage_name) else "",
      if (!is.na(spoke_id)) paste0(", spoke_id=", as.integer(spoke_id)) else "",
      "link_d_opt_gain"
    ))
  }
  utility <- as.double(cand[[utility_col]][idx])
  if (!any(is.finite(utility))) {
    rlang::abort(sprintf(
      paste0(
        ".adaptive_linking_selection_order invariant failed: canonical D-opt ordering ",
        "could not proceed%s%s because all `%s` values were non-finite."
      ),
      if (!is.na(stage_name)) paste0(" for stage=", stage_name) else "",
      if (!is.na(spoke_id)) paste0(", spoke_id=", as.integer(spoke_id)) else "",
      "link_d_opt_gain"
    ))
  }
  utility[!is.finite(utility)] <- -Inf
  idx[order(-utility, cand$i[idx], cand$j[idx])]
}

#' @keywords internal
#' @noRd
.adaptive_link_stage_priority <- function() {
  c(anchor_link = 1L, long_link = 2L, mid_link = 3L, local_link = 4L)
}

#' @keywords internal
#' @noRd
.adaptive_link_candidate_pool <- function(state,
                                          controller,
                                          spoke_id,
                                          include_utility = TRUE,
                                          C_max = NULL,
                                          seed = 1L) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  spoke_id <- as.integer(spoke_id %||% NA_integer_)
  if (is.na(spoke_id)) {
    return(tibble::tibble())
  }
  C_max <- as.integer(C_max %||% adaptive_defaults(as.integer(state$n_items))$C_max)
  stage_order <- .adaptive_stage_order()
  pools <- lapply(stage_order, function(stage_name) {
    cand <- generate_stage_candidates_from_state(
      state = state,
      stage_name = stage_name,
      fallback_name = "base",
      C_max = C_max,
      seed = as.integer(seed + match(stage_name, stage_order)),
      link_spoke_id = as.integer(spoke_id)
    )
    if (nrow(cand) < 1L) {
      return(NULL)
    }
    cand$link_stage <- as.character(stage_name)
    cand
  })
  pool <- dplyr::bind_rows(pools)
  if (nrow(pool) < 1L) {
    return(pool)
  }
  if (isTRUE(include_utility)) {
    pool <- .adaptive_link_attach_predictive_utility(
      candidates = pool,
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id)
    )
  }
  pool
}

#' @keywords internal
#' @noRd
.adaptive_link_spoke_utility_mass <- function(state,
                                              controller,
                                              spoke_id,
                                              top_k = NULL,
                                              C_max = NULL,
                                              seed = 1L) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  top_k <- as.integer(top_k %||% controller$multi_spoke_budget_top_k %||% 10L)
  stage_order <- .adaptive_stage_order()
  defaults <- adaptive_defaults(as.integer(state$n_items))
  snapshot <- tryCatch(
    .adaptive_link_stage_feasibility_snapshot(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      stage_order = stage_order,
      C_max = as.integer(C_max %||% defaults$C_max),
      seed_base = as.integer(seed),
      seed_stride = 1L
    ),
    error = function(e) .adaptive_link_stage_feasibility_snapshot_empty(stage_order)
  )
  utility <- as.double(unlist(
    snapshot$utility_values_by_stage[stage_order],
    recursive = FALSE,
    use.names = FALSE
  ))
  utility[!is.finite(utility) | utility < 0] <- 0
  ordered <- sort(utility, decreasing = TRUE)
  k_used <- min(top_k, length(ordered))
  candidate_count <- as.integer(
    snapshot$candidate_count %||%
      sum(as.integer(snapshot$feasible_counts[stage_order] %||% 0L), na.rm = TRUE)
  )
  list(
    utility_mass = as.double(sum(utils::head(ordered, k_used))),
    top_k_used = as.integer(k_used),
    candidate_count = as.integer(candidate_count),
    pool = NULL
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_backfill_order <- function(candidates,
                                         hub_id,
                                         set_map,
                                         blocker_stage_weights = NULL,
                                         spoke_id = NA_integer_) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) < 1L) {
    return(integer())
  }
  utility_col <- .adaptive_resolve_selection_column("linking_d_optimal_transform")
  if (is.na(utility_col) || !utility_col %in% names(cand)) {
    rlang::abort(sprintf(
      paste0(
        ".adaptive_link_backfill_order invariant failed: canonical D-opt ordering ",
        "could not proceed for stage=pooled_backfill%s because `%s` is unavailable."
      ),
      if (!is.na(spoke_id)) paste0(", spoke_id=", as.integer(spoke_id)) else "",
      "link_d_opt_gain"
    ))
  }
  utility <- as.double(cand[[utility_col]])
  if (!any(is.finite(utility))) {
    rlang::abort(sprintf(
      paste0(
        ".adaptive_link_backfill_order invariant failed: canonical D-opt ordering ",
        "could not proceed for stage=pooled_backfill%s because all `%s` values were non-finite."
      ),
      if (!is.na(spoke_id)) paste0(", spoke_id=", as.integer(spoke_id)) else "",
      "link_d_opt_gain"
    ))
  }
  utility[!is.finite(utility)] <- -Inf
  stage_priority <- .adaptive_link_stage_priority()
  cand_stage <- if ("link_stage" %in% names(cand)) as.character(cand$link_stage) else rep(NA_character_, nrow(cand))
  priority <- as.integer(stage_priority[cand_stage])
  priority[is.na(priority)] <- as.integer(length(stage_priority) + 1L)
  if (is.null(blocker_stage_weights) || length(blocker_stage_weights) < 1L) {
    blocker_stage_weights <- stats::setNames(rep(1, length(stage_priority)), names(stage_priority))
  }
  stage_weights <- as.double(blocker_stage_weights[cand_stage])
  stage_weights[!is.finite(stage_weights) | stage_weights <= 0] <- 1
  utility <- utility * stage_weights
  i_set <- as.integer(set_map[as.character(cand$i)])
  j_set <- as.integer(set_map[as.character(cand$j)])
  hub_item <- ifelse(i_set == as.integer(hub_id), as.character(cand$i), as.character(cand$j))
  spoke_item <- ifelse(i_set == as.integer(hub_id), as.character(cand$j), as.character(cand$i))
  order(-utility, priority, hub_item, spoke_item)
}

#' @keywords internal
#' @noRd
.adaptive_selected_dist_stratum_global <- function(selected_pair) {
  cand <- tibble::as_tibble(selected_pair)
  if (nrow(cand) < 1L || !"dist_stratum_global" %in% names(cand)) {
    return(NA_integer_)
  }
  as.integer(cand$dist_stratum_global[[1L]] %||% NA_integer_)
}

#' @keywords internal
#' @noRd
.adaptive_selected_coverage_meta <- function(selected_pair) {
  cand <- tibble::as_tibble(selected_pair)
  if (nrow(cand) < 1L) {
    return(list(
      coverage_bins_used = NA_integer_,
      coverage_source = NA_character_,
      link_spoke_id = NA_integer_
    ))
  }
  bins_used <- if ("coverage_bins_used" %in% names(cand)) cand$coverage_bins_used[[1L]] else NA_integer_
  source <- if ("coverage_source" %in% names(cand)) cand$coverage_source[[1L]] else NA_character_
  spoke <- if ("link_spoke_id" %in% names(cand)) cand$link_spoke_id[[1L]] else NA_integer_
  list(
    coverage_bins_used = as.integer(bins_used %||% NA_integer_),
    coverage_source = as.character(source %||% NA_character_),
    link_spoke_id = as.integer(spoke %||% NA_integer_)
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_mode <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(FALSE)
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  identical(phase_ctx$phase, "phase_b")
}

#' @keywords internal
#' @noRd
.adaptive_round_exposure_filter <- function(candidates,
                                            round,
                                            recent_deg,
                                            defaults,
                                            allow_repeat_pressure = FALSE) {
  cand <- tibble::as_tibble(candidates)
  .with_exposure_counts <- function(out) {
    .adaptive_set_candidate_filter_counts(
      out,
      utils::modifyList(
        attr(out, "candidate_filter_counts") %||% list(),
        list(
          n_candidates_after_exposure_filters = as.integer(nrow(out))
        )
      )
    )
  }
  if (nrow(cand) == 0L) {
    return(.with_exposure_counts(cand))
  }

  uses <- round$per_round_item_uses %||% integer()
  uses <- as.integer(uses)
  names(uses) <- names(round$per_round_item_uses %||% uses)
  repeat_budget <- as.integer(round$repeat_in_round_budget %||% defaults$repeat_in_round_budget)
  repeat_used <- as.integer(round$repeat_in_round_used %||% 0L)
  repeat_remaining <- max(0L, repeat_budget - repeat_used)

  i_ids <- as.character(cand$i)
  j_ids <- as.character(cand$j)
  i_used <- uses[i_ids]
  j_used <- uses[j_ids]
  i_used[is.na(i_used)] <- 0L
  j_used[is.na(j_used)] <- 0L
  base_keep <- (i_used == 0L) & (j_used == 0L)

  if (!isTRUE(allow_repeat_pressure)) {
    return(.with_exposure_counts(cand[base_keep, , drop = FALSE]))
  }

  if (repeat_remaining <= 0L) {
    return(.with_exposure_counts(cand[base_keep, , drop = FALSE]))
  }

  recent <- as.double(recent_deg)
  names(recent) <- names(recent_deg)
  if (length(recent) == 0L || all(is.na(recent))) {
    return(.with_exposure_counts(cand[base_keep, , drop = FALSE]))
  }

  underrep_q <- as.double(defaults$exposure_underrep_q %||% 0.25)
  underrep_q <- min(max(underrep_q, 0), 1)
  underrep_threshold <- stats::quantile(recent,
    probs = underrep_q,
    names = FALSE,
    type = 7,
    na.rm = TRUE
  )

  i_recent <- recent[i_ids]
  j_recent <- recent[j_ids]
  i_underrep <- !is.na(i_recent) & i_recent <= underrep_threshold
  j_underrep <- !is.na(j_recent) & j_recent <= underrep_threshold

  i_repeat <- i_used > 0L
  j_repeat <- j_used > 0L
  repeat_slots_needed <- as.integer(i_repeat) + as.integer(j_repeat)
  repeated_endpoint_ok <- (!i_repeat | i_underrep) & (!j_repeat | j_underrep)

  allow_repeat <- repeat_slots_needed > 0L &
    repeat_slots_needed <= repeat_remaining &
    repeated_endpoint_ok

  keep <- base_keep | allow_repeat
  .with_exposure_counts(cand[keep, , drop = FALSE])
}
