# -------------------------------------------------------------------------
# Adaptive stage candidates: anchors, strata, and per-stage generation.
# -------------------------------------------------------------------------

.adaptive_rank_proxy <- function(state) {
  ids <- as.character(state$trueskill_state$items$item_id)
  mu <- as.double(state$trueskill_state$items$mu)
  names(mu) <- ids

  theta <- state$btl_fit$theta_mean %||% NULL
  refit_id <- as.integer(state$refit_meta$last_refit_round_id %||% 0L)
  if (is.numeric(theta) && length(theta) > 0L && refit_id > 0L) {
    theta_names <- as.character(names(theta))
    theta <- as.double(theta)
    names(theta) <- theta_names
    if (all(ids %in% names(theta))) {
      return(list(
        scores = theta[ids],
        source = "refit_theta_mean",
        refit_id = refit_id
      ))
    }
  }

  list(
    scores = mu[ids],
    source = "trueskill_mu",
    refit_id = 0L
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

  top_band_n <- as.integer(round(defaults$top_band_frac * n))
  top_band_n <- max(1L, min(n, top_band_n))
  top_band_ids <- ids_sorted[seq_len(top_band_n)]
  rest_ids <- ids_sorted[(top_band_n + 1L):n]
  rest_ids <- rest_ids[!is.na(rest_ids)]

  top_k <- max(1L, min(defaults$top_band_strata, top_band_n))
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
      min_gap <- max(1L, min_gap - 1L)
    } else if (identical(fallback_name, "global_safe")) {
      min_gap <- 1L
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

#' @keywords internal
#' @noRd
generate_stage_candidates_from_state <- function(state, stage_name, fallback_name, C_max, seed) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!stage_name %in% .adaptive_stage_order()) {
    rlang::abort("`stage_name` must be one of the stage labels.")
  }

  state <- .adaptive_refresh_round_anchors(state)
  defaults <- adaptive_defaults(length(state$item_ids))
  proxy <- .adaptive_rank_proxy(state)
  strata <- .adaptive_assign_strata(proxy$scores, defaults)
  rank_index <- strata$rank_index
  stratum_map <- strata$stratum_map
  ids <- names(sort(rank_index))
  anchor_ids <- as.character(state$round$anchor_ids %||% character())
  if (length(anchor_ids) == 0L) {
    anchor_ids <- .adaptive_select_rolling_anchors(proxy$scores, defaults)
  }

  bounds <- .adaptive_stage_distance_bounds(stage_name, fallback_name, defaults)
  i_vals <- character()
  j_vals <- character()

  for (a in seq_len(length(ids) - 1L)) {
    i_id <- ids[[a]]
    for (b in (a + 1L):length(ids)) {
      j_id <- ids[[b]]
      i_anchor <- i_id %in% anchor_ids
      j_anchor <- j_id %in% anchor_ids
      keep <- FALSE

      if (identical(stage_name, "anchor_link")) {
        keep <- xor(i_anchor, j_anchor)
      } else {
        if (identical(stage_name, "long_link") || identical(stage_name, "mid_link")) {
          if (i_anchor || j_anchor) {
            next
          }
        }
        dist <- abs(as.integer(stratum_map[[i_id]]) - as.integer(stratum_map[[j_id]]))
        keep <- dist >= bounds$min && dist <= bounds$max
      }

      if (isTRUE(keep)) {
        i_vals <- c(i_vals, i_id)
        j_vals <- c(j_vals, j_id)
      }
    }
  }

  if (length(i_vals) == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  cand <- tibble::tibble(i = as.character(i_vals), j = as.character(j_vals))
  cand <- .adaptive_uniform_subsample_pairs(cand, C_max = as.integer(C_max), seed = as.integer(seed))
  cand
}

#' @keywords internal
#' @noRd
.adaptive_round_exposure_filter <- function(candidates, round, deg, defaults, allow_repeat_pressure = FALSE) {
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

  underrep <- .adaptive_underrep_set(deg)
  allow_repeat <- (i_ids %in% underrep) | (j_ids %in% underrep)
  keep <- base_keep | allow_repeat
  cand[keep, , drop = FALSE]
}
