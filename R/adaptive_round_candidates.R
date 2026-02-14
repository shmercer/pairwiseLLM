# -------------------------------------------------------------------------
# Adaptive stage candidates: anchors, strata, and per-stage generation.
# -------------------------------------------------------------------------

.adaptive_rank_proxy <- function(state) {
  ids <- as.character(state$trueskill_state$items$item_id)
  mu <- as.double(state$trueskill_state$items$mu)
  names(mu) <- ids
  refit_id <- as.integer(state$refit_meta$last_refit_round_id %||% 0L)

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

  proxy <- .adaptive_rank_proxy(out)
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

.adaptive_link_ranked_spokes <- function(state, controller, eligible_spoke_ids = NULL) {
  set_ids <- unique(as.integer(state$items$set_id))
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- setdiff(set_ids, hub_id)
  if (!is.null(eligible_spoke_ids)) {
    spoke_ids <- intersect(spoke_ids, as.integer(eligible_spoke_ids))
  }
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
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  required <- c("pair_id", "step_id", "is_cross_set", "link_spoke_id")
  step_subset <- tibble::tibble()
  if (nrow(step_log) > 0L && all(required %in% names(step_log))) {
    eligible <- !is.na(step_log$pair_id) &
      step_log$is_cross_set %in% TRUE &
      as.integer(step_log$link_spoke_id) %in% spoke_ids
    step_subset <- step_log[eligible, , drop = FALSE]
  }

  if (identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")) {
    if (nrow(step_subset) > 0L) {
      last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
      step_subset <- step_subset[as.integer(step_subset$step_id) > last_refit_step, , drop = FALSE]
    }
    counts <- rep.int(0L, length(spoke_ids))
    names(counts) <- as.character(spoke_ids)
    if (nrow(step_subset) > 0L) {
      tab <- table(factor(
        as.integer(step_subset$link_spoke_id),
        levels = spoke_ids
      ))
      counts[names(tab)] <- as.integer(tab)
    }

    floor_pairs <- as.integer(controller$min_cross_set_pairs_per_spoke_per_refit %||% 5L)
    floor_pairs <- max(0L, floor_pairs)
    floor_deficit <- pmax(0L, floor_pairs - counts)
    if (any(floor_deficit > 0L)) {
      ord_floor <- order(-floor_deficit, counts, as.integer(names(counts)))
      return(as.integer(names(counts)[ord_floor]))
    }

    # Route by uncertainty-weighted target deficit for the next committed step.
    link_stats <- controller$link_refit_stats_by_spoke %||% list()
    spoke_stats <- lapply(as.character(spoke_ids), function(key) {
      link_stats[[key]] %||% list(uncertainty = 0)
    })
    names(spoke_stats) <- as.character(spoke_ids)
    projected_total <- as.integer(sum(counts) + 1L)
    projected_total <- max(projected_total, as.integer(length(spoke_ids) * floor_pairs))
    targets <- .adaptive_link_concurrent_targets(
      spoke_stats = spoke_stats,
      total_pairs = projected_total,
      floor_pairs = floor_pairs
    )
    target_deficit <- as.integer(targets[names(counts)] - counts)
    target_deficit[!is.finite(target_deficit)] <- 0L
    if (any(target_deficit > 0L)) {
      weights <- vapply(spoke_stats, function(x) as.double(x$uncertainty %||% 0), numeric(1L))
      weights[!is.finite(weights)] <- 0
      ord_deficit <- order(-target_deficit, -weights, counts, as.integer(names(counts)))
      return(as.integer(names(counts)[ord_deficit]))
    }

    ord_counts <- order(counts, as.integer(names(counts)))
    return(as.integer(names(counts)[ord_counts]))
  }

  if (nrow(step_subset) > 0L) {
    counts <- table(factor(
      as.integer(step_subset$link_spoke_id),
      levels = spoke_ids
    ))
    ord_counts <- order(as.integer(counts), as.integer(names(counts)))
    return(as.integer(names(counts)[ord_counts]))
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

.adaptive_link_spoke_coverage <- function(state, controller, spoke_id, spoke_ids, proxy_scores) {
  spoke_id <- as.integer(spoke_id)
  bins_target <- as.integer(controller$spoke_quantile_coverage_bins %||% 3L)
  bins_used <- max(1L, bins_target)
  n_spoke <- length(spoke_ids)
  while (bins_used > 1L && n_spoke < (3L * bins_used)) {
    bins_used <- bins_used - 1L
  }

  source <- "global_rank"
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) > 0L && all(c("pair_id", "is_cross_set", "link_spoke_id") %in% names(step_log))) {
    spoke_done <- sum(
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == spoke_id,
      na.rm = TRUE
    )
  } else {
    spoke_done <- 0L
  }

  spoke_scores <- proxy_scores[spoke_ids]
  if (spoke_done < 10L) {
    phase_a <- state$linking$phase_a %||% list()
    artifact <- (phase_a$artifacts %||% list())[[as.character(spoke_id)]] %||% NULL
    items_tbl <- artifact$items %||% NULL
    if (is.data.frame(items_tbl) && "global_item_id" %in% names(items_tbl) &&
      "rank_mu_raw" %in% names(items_tbl)) {
      state_map <- stats::setNames(
        as.character(state$items$item_id),
        as.character(state$items$global_item_id)
      )
      ids <- state_map[as.character(items_tbl$global_item_id)]
      keep <- !is.na(ids) & ids %in% spoke_ids
      if (any(keep)) {
        rank_raw <- as.double(items_tbl$rank_mu_raw[keep])
        ids <- as.character(ids[keep])
        if (all(is.finite(rank_raw)) && length(ids) > 0L) {
          spoke_scores <- stats::setNames(-rank_raw, ids)
          source <- "phase_a_rank_mu_raw"
        }
      }
    }
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
  defaults <- adaptive_defaults(length(state$item_ids))
  proxy <- .adaptive_rank_proxy(state)
  controller <- .adaptive_controller_resolve(state)
  is_link_mode <- .adaptive_link_mode_active(controller)
  hub_id <- as.integer(controller$hub_id %||% 1L)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  link_phase_b_active <- isTRUE(is_link_mode) && identical(phase_ctx$phase, "phase_b")

  if (isTRUE(link_phase_b_active)) {
    eligible_spokes <- as.integer(phase_ctx$ready_spokes %||% integer())
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
    if (!spoke_id %in% eligible_spokes) {
      return(tibble::tibble(i = character(), j = character()))
    }
    if (is.na(spoke_id)) {
      rlang::abort(
        "Phase metadata and routing mode disagree: no active spoke could be selected for phase_b."
      )
    }
    hub_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == hub_id])
    spoke_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == spoke_id])
    active_ids <- unique(c(hub_ids, spoke_ids))
    if (length(active_ids) < 2L) {
      return(tibble::tibble(i = character(), j = character()))
    }
    active_scores <- proxy$scores[active_ids]
    strata <- .adaptive_assign_strata(active_scores, defaults)
    rank_index <- strata$rank_index
    stratum_map <- strata$stratum_map
    ids <- names(sort(rank_index))
    # In linking Phase B, hub anchors must be derived from hub-only ranks.
    hub_anchor_ids <- .adaptive_select_rolling_anchors(active_scores[hub_ids], defaults)
    hub_anchor_ids <- hub_anchor_ids[hub_anchor_ids %in% hub_ids]
    coverage <- .adaptive_link_spoke_coverage(
      state = state,
      controller = controller,
      spoke_id = spoke_id,
      spoke_ids = spoke_ids,
      proxy_scores = active_scores
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
  i_vals <- character()
  j_vals <- character()
  dist_vals <- integer()
  coverage_priority <- integer()
  coverage_bin <- integer()
  link_spoke_id <- integer()
  coverage_bins_used <- integer()
  coverage_source <- character()

  for (a in seq_len(length(ids) - 1L)) {
    i_id <- ids[[a]]
    for (b in (a + 1L):length(ids)) {
      j_id <- ids[[b]]
      keep <- FALSE
      dist <- abs(as.integer(stratum_map[[i_id]]) - as.integer(stratum_map[[j_id]]))

      if (isTRUE(link_phase_b_active)) {
        i_hub <- i_id %in% hub_ids
        j_hub <- j_id %in% hub_ids
        if (!isTRUE(xor(i_hub, j_hub))) {
          next
        }
        i_anchor <- i_id %in% hub_anchor_ids
        j_anchor <- j_id %in% hub_anchor_ids
        if (identical(stage_name, "anchor_link")) {
          keep <- xor(i_anchor, j_anchor)
        } else {
          keep <- dist >= bounds$min && dist <= bounds$max
        }
      } else {
        i_anchor <- i_id %in% anchor_ids
        j_anchor <- j_id %in% anchor_ids
        if (identical(stage_name, "anchor_link")) {
          keep <- xor(i_anchor, j_anchor)
        } else {
          if (identical(stage_name, "long_link") || identical(stage_name, "mid_link")) {
            if (i_anchor || j_anchor) {
              next
            }
          }
          keep <- dist >= bounds$min && dist <= bounds$max
        }
      }

      if (isTRUE(keep)) {
        i_vals <- c(i_vals, i_id)
        j_vals <- c(j_vals, j_id)
        dist_vals <- c(dist_vals, as.integer(dist))
        if (isTRUE(link_phase_b_active)) {
          spoke_item <- if (i_id %in% spoke_ids) i_id else j_id
          spoke_bin <- as.integer(coverage$bin_map[[spoke_item]] %||% NA_integer_)
          priority <- as.integer(!is.na(spoke_bin) && spoke_bin %in% coverage$bins_undercovered)
          coverage_priority <- c(coverage_priority, priority)
          coverage_bin <- c(coverage_bin, spoke_bin)
          link_spoke_id <- c(link_spoke_id, as.integer(spoke_id))
          coverage_bins_used <- c(coverage_bins_used, as.integer(coverage$bins_used))
          coverage_source <- c(coverage_source, as.character(coverage$source))
        }
      }
    }
  }

  if (length(i_vals) == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  cand <- tibble::tibble(i = as.character(i_vals), j = as.character(j_vals))
  cand$dist_stratum_global <- as.integer(dist_vals)
  if (isTRUE(link_phase_b_active)) {
    cand$coverage_priority <- as.integer(coverage_priority)
    cand$coverage_bin_spoke <- as.integer(coverage_bin)
    cand$link_spoke_id <- as.integer(link_spoke_id)
    cand$coverage_bins_used <- as.integer(coverage_bins_used)
    cand$coverage_source <- as.character(coverage_source)
  }
  cand <- .adaptive_uniform_subsample_pairs(cand, C_max = as.integer(C_max), seed = as.integer(seed))
  cand
}

#' @keywords internal
#' @noRd
.adaptive_linking_selection_order <- function(candidates) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) == 0L) {
    return(integer())
  }
  if ("coverage_priority" %in% names(cand)) {
    return(order(-as.integer(cand$coverage_priority), -cand$u0, cand$i, cand$j))
  }
  order(-cand$u0, cand$i, cand$j)
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
  if (nrow(cand) == 0L) {
    return(cand)
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
    return(cand[base_keep, , drop = FALSE])
  }

  if (repeat_remaining <= 0L) {
    return(cand[base_keep, , drop = FALSE])
  }

  recent <- as.double(recent_deg)
  names(recent) <- names(recent_deg)
  if (length(recent) == 0L || all(is.na(recent))) {
    return(cand[base_keep, , drop = FALSE])
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
  cand[keep, , drop = FALSE]
}
