# -------------------------------------------------------------------------
# Adaptive stepwise selector (one pair per call)
# -------------------------------------------------------------------------

adaptive_defaults <- function(N) {
  N <- as.integer(N)
  if (is.na(N) || N < 2L) {
    rlang::abort("`N` must be a positive integer >= 2.")
  }

  W <- .btl_mcmc_clamp(5L, 60L, as.integer(round(2 * sqrt(N))))
  explore_rate <- .btl_mcmc_clamp(0.10, 0.25, 0.20 - 0.02 * log10(N))
  refit_pairs_target <- .btl_mcmc_clamp(100L, 5000L, as.integer(ceiling(N / 2)))
  W_cap <- max(200L, min(2000L, refit_pairs_target))
  round_pairs_target <- as.integer(ceiling(refit_pairs_target / 2))
  k_base <- if (N < 60L) {
    5L
  } else if (N < 150L) {
    10L
  } else if (N < 400L) {
    12L
  } else {
    20L
  }

  list(
    W = as.integer(W),
    C_max = 20000L,
    quota_eps = 0.20,
    explore_rate = as.double(explore_rate),
    explore_resample_max = 10L,
    explore_nonlocal_rate = 0.15,
    cap_frac = 0.08,
    dup_p_margin = 0.05,
    dup_max_obs = 2L,
    dup_max_obs_relaxed = 3L,
    q = 0.90,
    refit_pairs_target = as.integer(refit_pairs_target),
    round_pairs_target = round_pairs_target,
    W_cap = as.integer(W_cap),
    repeat_in_round_budget = 2L,
    anchor_frac_early = 0.30,
    anchor_frac_late = 0.20,
    anchor_rounds_early = 4L,
    long_frac_early = 0.10,
    long_frac_late = 0.05,
    long_rounds_early = 4L,
    mid_frac = 0.10,
    k_base = as.integer(k_base),
    top_band_frac = 0.20,
    top_band_strata = 2L,
    anchor_frac_total = 0.10,
    anchor_count_min = 10L,
    anchor_top_weight = 0.30,
    anchor_mid_weight = 0.40,
    anchor_bottom_weight = 0.30,
    anchor_refresh_on_round = FALSE,
    long_min_dist = as.integer(ceiling(0.5 * k_base)),
    mid_min_dist = 2L,
    mid_max_dist = as.integer(min(4L, k_base - 1L)),
    local_max_dist = 1L,
    local_expand_max_dist = 2L,
    exposure_underrep_q = 0.20
  )
}

.adaptive_history_tbl <- function(state) {
  history <- state$history_pairs %||% tibble::tibble()
  history <- tibble::as_tibble(history)
  if (!all(c("A_id", "B_id") %in% names(history))) {
    if (all(c("A", "B") %in% names(history))) {
      history <- dplyr::rename(history, A_id = .data$A, B_id = .data$B)
    } else if (all(c("i", "j") %in% names(history))) {
      history <- dplyr::rename(history, A_id = .data$i, B_id = .data$j)
    } else {
      history <- tibble::tibble(A_id = character(), B_id = character())
    }
  }
  history$A_id <- as.character(history$A_id)
  history$B_id <- as.character(history$B_id)
  history
}

.adaptive_pair_counts <- function(history, ids) {
  ids <- as.character(ids)
  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  posA <- deg
  posB <- deg

  if (nrow(history) == 0L) {
    return(list(
      deg = deg,
      posA = posA,
      posB = posB,
      pair_count = integer(),
      pair_last_order = list()
    ))
  }

  A_id <- as.character(history$A_id)
  B_id <- as.character(history$B_id)

  for (idx in seq_len(nrow(history))) {
    A <- A_id[[idx]]
    B <- B_id[[idx]]
    if (!A %in% ids || !B %in% ids || identical(A, B)) next
    posA[[A]] <- posA[[A]] + 1L
    posB[[B]] <- posB[[B]] + 1L
    deg[[A]] <- deg[[A]] + 1L
    deg[[B]] <- deg[[B]] + 1L
  }

  unordered_key <- make_unordered_key(A_id, B_id)
  pair_count <- table(unordered_key)
  pair_count <- as.integer(pair_count)
  names(pair_count) <- names(table(unordered_key))

  pair_last_order <- list()
  for (idx in seq_len(nrow(history))) {
    key <- unordered_key[[idx]]
    pair_last_order[[key]] <- c(A_id[[idx]], B_id[[idx]])
  }

  list(
    deg = deg,
    posA = posA,
    posB = posB,
    pair_count = pair_count,
    pair_last_order = pair_last_order
  )
}

.adaptive_recent_deg <- function(history, ids, W_cap) {
  ids <- as.character(ids)
  recent <- stats::setNames(rep.int(0L, length(ids)), ids)
  if (nrow(history) == 0L) {
    return(recent)
  }
  window <- utils::tail(history, n = min(nrow(history), W_cap))
  A_id <- as.character(window$A_id)
  B_id <- as.character(window$B_id)
  for (idx in seq_len(nrow(window))) {
    A <- A_id[[idx]]
    B <- B_id[[idx]]
    if (!A %in% ids || !B %in% ids || identical(A, B)) next
    recent[[A]] <- recent[[A]] + 1L
    recent[[B]] <- recent[[B]] + 1L
  }
  recent
}

.adaptive_low_degree_set <- function(deg) {
  if (length(deg) == 0L) return(character())
  min_deg <- min(deg)
  names(deg)[deg == min_deg]
}

.adaptive_underrep_set <- function(deg) {
  if (length(deg) == 0L) return(character())
  d_min <- min(deg)
  out <- names(deg)[deg <= (d_min + 1L)]
  if (length(out) == 0L) {
    names(deg)
  } else {
    out
  }
}

.adaptive_rank_index <- function(state) {
  mu <- state$trueskill_state$items$mu
  ids <- as.character(state$trueskill_state$items$item_id)
  ord <- order(-mu, ids)
  stats::setNames(seq_along(ids), ids[ord])
}

.adaptive_strata_index <- function(rank_index, k_base) {
  ids <- names(rank_index)
  n <- length(ids)
  k_base <- max(1L, min(as.integer(k_base), n))
  breaks <- floor((seq_len(n) - 1L) * k_base / n) + 1L
  stats::setNames(as.integer(breaks), ids)
}

.adaptive_anchor_ids <- function(rank_index) {
  ids <- names(rank_index)
  n <- length(ids)
  n_anchor <- max(1L, as.integer(round(0.10 * n)))
  n_anchor <- min(n_anchor, n - 1L)
  if (n_anchor < 1L) {
    return(character())
  }
  ordered <- names(sort(rank_index))
  as.character(ordered[seq_len(n_anchor)])
}

.adaptive_stage_candidate_filter <- function(candidates, stage_name, fallback_name, rank_index, defaults) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) == 0L) {
    return(cand)
  }
  strata <- .adaptive_strata_index(rank_index, defaults$k_base)
  anchor_ids <- .adaptive_anchor_ids(rank_index)
  i_ids <- as.character(cand$i)
  j_ids <- as.character(cand$j)
  dist <- abs(strata[i_ids] - strata[j_ids])

  if (identical(stage_name, "anchor_link")) {
    keep <- (i_ids %in% anchor_ids & !j_ids %in% anchor_ids) |
      (j_ids %in% anchor_ids & !i_ids %in% anchor_ids)
    return(cand[keep, , drop = FALSE])
  }

  non_anchor <- !i_ids %in% anchor_ids & !j_ids %in% anchor_ids
  if (identical(stage_name, "long_link")) {
    min_dist <- defaults$long_min_dist
    if (identical(fallback_name, "expand_locality")) {
      min_dist <- max(1L, min_dist - 1L)
    } else if (identical(fallback_name, "global_safe")) {
      min_dist <- 1L
    }
    keep <- non_anchor & dist >= min_dist
    return(cand[keep, , drop = FALSE])
  }

  if (identical(stage_name, "mid_link")) {
    min_dist <- defaults$mid_min_dist
    max_dist <- defaults$mid_max_dist
    if (identical(fallback_name, "expand_locality")) {
      min_dist <- max(1L, min_dist - 1L)
      max_dist <- min(defaults$k_base - 1L, max_dist + 1L)
    } else if (identical(fallback_name, "global_safe")) {
      min_dist <- 1L
      max_dist <- max(1L, defaults$k_base - 1L)
    }
    keep <- non_anchor & dist >= min_dist & dist <= max_dist
    return(cand[keep, , drop = FALSE])
  }

  # local_link
  max_dist <- 1L
  if (identical(fallback_name, "expand_locality")) {
    max_dist <- 2L
  } else if (identical(fallback_name, "global_safe")) {
    max_dist <- max(1L, defaults$k_base - 1L)
  }
  cand[dist <= max_dist, , drop = FALSE]
}

.adaptive_star_cap_filter <- function(candidates, recent_deg, cap_count) {
  if (nrow(candidates) == 0L) {
    return(list(
      candidates = candidates,
      rejects = 0L,
      reject_items = character(),
      reject_items_count = 0L
    ))
  }
  i_ids <- as.character(candidates$i)
  j_ids <- as.character(candidates$j)
  i_excess <- recent_deg[i_ids] > cap_count
  j_excess <- recent_deg[j_ids] > cap_count
  reject <- i_excess | j_excess
  reject_items <- unique(c(i_ids[i_excess], j_ids[j_excess]))
  reject_items_count <- length(reject_items)
  list(
    candidates = candidates[!reject, , drop = FALSE],
    rejects = sum(reject),
    reject_items = as.character(reject_items),
    reject_items_count = as.integer(reject_items_count)
  )
}

.adaptive_duplicate_filter <- function(candidates, pair_count, dup_max_obs, allow_repeats = FALSE,
                                       dup_max_obs_default = NULL, dup_p_margin = NULL,
                                       p_vals = NULL, u0_vals = NULL, u0_quantile = NULL) {
  if (nrow(candidates) == 0L) {
    return(candidates)
  }
  unordered_key <- make_unordered_key(candidates$i, candidates$j)
  count_vals <- pair_count[unordered_key]
  count_vals[is.na(count_vals)] <- 0L

  if (!isTRUE(allow_repeats)) {
    keep <- count_vals < dup_max_obs
    return(candidates[keep, , drop = FALSE])
  }

  if (is.null(dup_max_obs_default)) {
    dup_max_obs_default <- dup_max_obs
  }

  keep <- count_vals < dup_max_obs_default
  if (!any(!keep)) {
    return(candidates)
  }

  relaxed_ok <- count_vals < dup_max_obs
  if (is.null(dup_p_margin) || is.null(p_vals) || is.null(u0_vals) || is.null(u0_quantile)) {
    return(candidates[keep & relaxed_ok, , drop = FALSE])
  }

  margin_ok <- abs(p_vals - 0.5) <= dup_p_margin
  u0_ok <- u0_vals >= u0_quantile
  allow_repeat <- (!keep) & relaxed_ok & margin_ok & u0_ok
  candidates[keep | allow_repeat, , drop = FALSE]
}

.adaptive_select_partner <- function(candidates, i_id, mu, recent_deg, mode, rank_index = NULL) {
  cand <- candidates[candidates$i == i_id | candidates$j == i_id, , drop = FALSE]
  if (nrow(cand) == 0L) return(NULL)
  other <- ifelse(cand$i == i_id, cand$j, cand$i)

  mu_i <- mu[[as.character(i_id)]]
  mu_j <- mu[as.character(other)]
  recent_j <- recent_deg[as.character(other)]
  u0 <- cand$u0

  if (identical(mode, "nonlocal")) {
    if (!is.null(rank_index)) {
      rank_dist <- abs(rank_index[[as.character(i_id)]] - rank_index[as.character(other)])
    } else {
      rank_dist <- abs(mu_i - mu_j)
    }
    order_idx <- order(-rank_dist, -u0, recent_j, other)
  } else {
    rank_dist <- abs(mu_i - mu_j)
    order_idx <- order(rank_dist, -u0, recent_j, other)
  }

  cand[order_idx[[1L]], , drop = FALSE]
}

.adaptive_assign_order <- function(pair, posA, posB, pair_last_order) {
  i_id <- as.character(pair$i)
  j_id <- as.character(pair$j)
  key <- make_unordered_key(i_id, j_id)
  last_order <- pair_last_order[[key]]

  if (!is.null(last_order) && length(last_order) == 2L) {
    A_id <- last_order[[2L]]
    B_id <- last_order[[1L]]
    return(c(A_id = A_id, B_id = B_id))
  }

  imb_i <- posA[[i_id]] - posB[[i_id]]
  imb_j <- posA[[j_id]] - posB[[j_id]]

  if (imb_i < imb_j) {
    return(c(A_id = i_id, B_id = j_id))
  }
  if (imb_j < imb_i) {
    return(c(A_id = j_id, B_id = i_id))
  }

  ordered <- sort(c(i_id, j_id))
  c(A_id = ordered[[1L]], B_id = ordered[[2L]])
}

.adaptive_select_stage <- function(
  stage,
  state,
  config,
  round,
  history,
  counts,
  step_id,
  seed_base,
  candidates = NULL
) {
  ids <- as.character(state$trueskill_state$items$item_id)
  candidates <- tibble::as_tibble(candidates %||% tibble::tibble(i = character(), j = character()))

  n_generated <- nrow(candidates)
  if (n_generated == 0L) {
    return(list(
      selected = NULL,
      counts = list(
        n_candidates_generated = 0L,
        n_candidates_after_hard_filters = 0L,
        n_candidates_after_duplicates = 0L,
        n_candidates_after_star_caps = 0L,
        n_candidates_scored = 0L
      ),
      star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L)
    ))
  }

  candidates <- tibble::as_tibble(candidates)
  candidates <- dplyr::filter(candidates, .data$i != .data$j)
  if (nrow(candidates) > 0L) {
    candidates <- score_candidates_u0(candidates, state$trueskill_state)
    candidates$p <- vapply(seq_len(nrow(candidates)), function(idx) {
      trueskill_win_probability(candidates$i[[idx]], candidates$j[[idx]], state$trueskill_state)
    }, numeric(1L))
  }
  if (nrow(candidates) > 0L) {
    unordered_key <- make_unordered_key(candidates$i, candidates$j)
    pair_count <- counts$pair_count[unordered_key]
    pair_count[is.na(pair_count)] <- 0L
    has_order <- vapply(seq_along(unordered_key), function(idx) {
      if (pair_count[[idx]] < 1L) {
        return(TRUE)
      }
      last_order <- counts$pair_last_order[[unordered_key[[idx]]]]
      !is.null(last_order) && length(last_order) == 2L
    }, logical(1L))
    candidates <- candidates[has_order, , drop = FALSE]
  }
  n_after_hard <- nrow(candidates)

  cap_count <- ceiling(config$cap_frac * config$W_cap)
  recent_deg <- .adaptive_recent_deg(history, ids, config$W_cap)
  allow_repeats <- identical(stage$dup_policy, "relaxed")

  .apply_downstream_filters <- function(candidates_in) {
    u0_quantile <- NULL
    if (nrow(candidates_in) > 0L) {
      star_filtered_local <- .adaptive_star_cap_filter(candidates_in, recent_deg, cap_count)
      if (nrow(star_filtered_local$candidates) > 0L) {
        u0_quantile <- stats::quantile(star_filtered_local$candidates$u0,
          probs = config$q, names = FALSE, type = 7
        )
      }
    }

    after_dup <- .adaptive_duplicate_filter(
      candidates = candidates_in,
      pair_count = counts$pair_count,
      dup_max_obs = if (allow_repeats) config$dup_max_obs_relaxed else config$dup_max_obs,
      allow_repeats = allow_repeats,
      dup_max_obs_default = config$dup_max_obs,
      dup_p_margin = config$dup_p_margin,
      p_vals = candidates_in$p,
      u0_vals = candidates_in$u0,
      u0_quantile = u0_quantile
    )

    star_filtered_local <- .adaptive_star_cap_filter(after_dup, recent_deg, cap_count)
    list(
      candidates = star_filtered_local$candidates,
      n_after_dup = nrow(after_dup),
      star_filtered = star_filtered_local
    )
  }

  candidates_hard <- candidates
  candidates_base <- .adaptive_round_exposure_filter(candidates_hard,
    round = round,
    deg = counts$deg,
    defaults = config,
    allow_repeat_pressure = FALSE
  )
  filtered <- .apply_downstream_filters(candidates_base)
  candidates <- filtered$candidates
  n_after_dup <- filtered$n_after_dup
  star_filtered <- filtered$star_filtered

  if (nrow(candidates) == 0L) {
    candidates_repeat <- .adaptive_round_exposure_filter(candidates_hard,
      round = round,
      deg = counts$deg,
      defaults = config,
      allow_repeat_pressure = TRUE
    )
    filtered_repeat <- .apply_downstream_filters(candidates_repeat)
    candidates <- filtered_repeat$candidates
    n_after_dup <- filtered_repeat$n_after_dup
    star_filtered <- filtered_repeat$star_filtered
  }

  n_after_star <- nrow(candidates)

  candidates$u <- candidates$u0

  list(
    selected = candidates,
    counts = list(
      n_candidates_generated = n_generated,
      n_candidates_after_hard_filters = n_after_hard,
      n_candidates_after_duplicates = n_after_dup,
      n_candidates_after_star_caps = n_after_star,
      n_candidates_scored = n_after_star
    ),
    star_caps = list(
      rejects = as.integer(star_filtered$rejects),
      reject_items = star_filtered$reject_items,
      reject_items_count = as.integer(star_filtered$reject_items_count)
    ),
    recent_deg = recent_deg
  )
}

#' @keywords internal
#' @noRd
select_next_pair <- function(state, step_id = NULL, candidates = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (is.null(state$trueskill_state)) {
    rlang::abort("`state$trueskill_state` must be set.")
  }
  validate_trueskill_state(state$trueskill_state)

  ids <- as.character(state$trueskill_state$items$item_id)
  defaults <- adaptive_defaults(length(ids))
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, ids)
  step_id <- as.integer(step_id %||% (nrow(state$step_log) + 1L))
  if (length(step_id) != 1L || is.na(step_id) || step_id < 1L) {
    rlang::abort("`step_id` must be a positive integer.")
  }
  seed_base <- as.integer(state$meta$seed %||% 1L)
  round <- state$round %||% list()
  round_stage <- as.character(.adaptive_round_active_stage(state) %||% "warm_start")
  generation_stage <- if (identical(round_stage, "warm_start")) {
    if (length(ids) <= 2L) "anchor_link" else "local_link"
  } else {
    round_stage
  }
  stage_quota <- NA_integer_
  stage_committed_so_far <- NA_integer_
  if (!identical(round_stage, "warm_start")) {
    stage_quota <- as.integer(round$stage_quotas[[round_stage]] %||% NA_integer_)
    stage_committed_so_far <- as.integer(round$stage_committed[[round_stage]] %||% 0L)
  }

  stage_defs <- list(
    list(name = "base", W_used = defaults$W, dup_policy = "default", explore_boost = 1),
    list(name = "expand_locality", W_used = min(2L * defaults$W, length(ids) - 1L),
      dup_policy = "default", explore_boost = 1),
    list(name = "uncertainty_pool", W_used = defaults$W,
      dup_policy = "default", explore_boost = 2),
    list(name = "dup_relax", W_used = defaults$W, dup_policy = "relaxed", explore_boost = 2),
    list(name = "global_safe", W_used = length(ids) - 1L,
      dup_policy = "default", explore_boost = 1)
  )

  fallback_path <- character()
  last_counts <- NULL
  last_star_caps <- list(rejects = 0L, reject_items = character(), reject_items_count = 0L)
  selected_pair <- NULL
  selected_stage <- NULL
  explore_mode <- NA_character_
  explore_reason <- NA_character_
  is_explore_step <- FALSE
  recent_deg <- .adaptive_recent_deg(history, ids, defaults$W_cap)

  for (idx in seq_along(stage_defs)) {
    stage <- stage_defs[[idx]]
    stage$idx <- idx
    fallback_path <- c(fallback_path, stage$name)
    stage_seed <- .adaptive_stage_seed(seed_base, step_id, stage$idx, offset = 11L)
    stage_candidates <- if (idx == 1L && !is.null(candidates)) {
      tibble::as_tibble(candidates)
    } else {
      generate_stage_candidates_from_state(
        state = state,
        stage_name = generation_stage,
        fallback_name = stage$name,
        C_max = defaults$C_max,
        seed = stage_seed
      )
    }

    stage_out <- .adaptive_select_stage(
      stage = stage,
      state = state,
      config = defaults,
      round = round,
      history = history,
      counts = counts,
      step_id = step_id,
      seed_base = seed_base,
      candidates = stage_candidates
    )
    last_counts <- stage_out$counts
    last_star_caps <- stage_out$star_caps
    recent_deg <- stage_out$recent_deg %||% recent_deg

    cand <- stage_out$selected
    if (is.null(cand) || nrow(cand) == 0L) next

    explore_rate <- defaults$explore_rate
    if (stage$explore_boost > 1) {
      explore_rate <- min(0.50, explore_rate * stage$explore_boost)
    }

    low_degree_set <- .adaptive_low_degree_set(counts$deg)
    min_degree <- min(counts$deg)
    quota_active <- min_degree < 2L
    quota_eps <- defaults$quota_eps
    quota_pick <- FALSE
    if (quota_active) {
      quota_seed <- .adaptive_stage_seed(seed_base, step_id, stage$idx, offset = 1L)
      quota_pick <- .adaptive_with_seed(quota_seed, stats::runif(1) < quota_eps)
    }

    stage_is_explore <- FALSE
    stage_explore_mode <- NA_character_
    stage_explore_reason <- NA_character_

    if (quota_pick) {
      eligible <- cand[cand$i %in% low_degree_set | cand$j %in% low_degree_set, , drop = FALSE]
      if (nrow(eligible) == 0L) next
      cand <- eligible
      stage_is_explore <- TRUE
      stage_explore_reason <- "coverage_quota_override"
    } else {
      explore_seed <- .adaptive_stage_seed(seed_base, step_id, stage$idx, offset = 2L)
      stage_is_explore <- .adaptive_with_seed(explore_seed, stats::runif(1) < explore_rate)
      if (stage_is_explore) {
        stage_explore_reason <- "probabilistic"
      }
    }

    if (stage_is_explore) {
      underrep <- .adaptive_underrep_set(counts$deg)
      if (length(underrep) == 0L) {
        underrep <- ids
      }

      nonlocal_seed <- .adaptive_stage_seed(seed_base, step_id, stage$idx, offset = 3L)
      explore_nonlocal <- .adaptive_with_seed(
        nonlocal_seed,
        stats::runif(1) < defaults$explore_nonlocal_rate
      )
      stage_explore_mode <- if (explore_nonlocal) "nonlocal" else "local"

      mu_vals <- state$trueskill_state$items$mu
      names(mu_vals) <- as.character(state$trueskill_state$items$item_id)
      rank_order <- order(-mu_vals, names(mu_vals))
      rank_index <- stats::setNames(seq_along(rank_order), names(mu_vals)[rank_order])

      selected <- NULL
      for (attempt in seq_len(defaults$explore_resample_max)) {
        attempt_seed <- .adaptive_stage_seed(seed_base, step_id, stage$idx, offset = 100L + attempt)
        i_id <- .adaptive_with_seed(attempt_seed, sample(underrep, size = 1L))
        candidate <- .adaptive_select_partner(
          cand,
          i_id,
          mu_vals,
          recent_deg,
          stage_explore_mode,
          rank_index
        )
        if (!is.null(candidate)) {
          selected <- candidate
          break
        }
      }
      if (is.null(selected)) next
      selected_pair <- selected
    } else {
      order_idx <- order(-cand$u0, cand$i, cand$j)
      selected_pair <- cand[order_idx[[1L]], , drop = FALSE]
    }

    is_explore_step <- stage_is_explore
    explore_mode <- stage_explore_mode
    explore_reason <- stage_explore_reason
    selected_stage <- stage
    break
  }

  if (is.null(selected_pair) || nrow(selected_pair) == 0L) {
    return(list(
      i = NA_integer_,
      j = NA_integer_,
      A = NA_integer_,
      B = NA_integer_,
      is_explore_step = FALSE,
      explore_mode = NA_character_,
      explore_reason = NA_character_,
      candidate_starved = TRUE,
      fallback_used = "global_safe",
      fallback_path = paste(fallback_path, collapse = ">"),
      starvation_reason = "few_candidates_generated",
      round_id = as.integer(round$round_id %||% NA_integer_),
      round_stage = as.character(round_stage),
      pair_type = as.character(round_stage),
      used_in_round_i = NA_integer_,
      used_in_round_j = NA_integer_,
      is_anchor_i = NA,
      is_anchor_j = NA,
      stratum_i = NA_integer_,
      stratum_j = NA_integer_,
      dist_stratum = NA_integer_,
      stage_committed_so_far = stage_committed_so_far,
      stage_quota = stage_quota,
      n_candidates_generated = last_counts$n_candidates_generated %||% 0L,
      n_candidates_after_hard_filters = last_counts$n_candidates_after_hard_filters %||% 0L,
      n_candidates_after_duplicates = last_counts$n_candidates_after_duplicates %||% 0L,
      n_candidates_after_star_caps = last_counts$n_candidates_after_star_caps %||% 0L,
      n_candidates_scored = last_counts$n_candidates_scored %||% 0L,
      deg_i = NA_integer_,
      deg_j = NA_integer_,
      recent_deg_i = NA_integer_,
      recent_deg_j = NA_integer_,
      mu_i = NA_real_,
      mu_j = NA_real_,
      sigma_i = NA_real_,
      sigma_j = NA_real_,
      p_ij = NA_real_,
      U0_ij = NA_real_,
      star_cap_rejects = as.integer(last_star_caps$rejects %||% 0L),
      star_cap_reject_items = as.integer(last_star_caps$reject_items_count %||% 0L)
    ))
  }

  selected_pair <- tibble::as_tibble(selected_pair)
  order_vals <- .adaptive_assign_order(selected_pair, counts$posA, counts$posB, counts$pair_last_order)
  i_id <- as.character(selected_pair$i[[1L]])
  j_id <- as.character(selected_pair$j[[1L]])

  mu_vals <- state$trueskill_state$items$mu
  sigma_vals <- state$trueskill_state$items$sigma
  names(mu_vals) <- as.character(state$trueskill_state$items$item_id)
  names(sigma_vals) <- as.character(state$trueskill_state$items$item_id)

  p_ij <- trueskill_win_probability(i_id, j_id, state$trueskill_state)
  u0_ij <- p_ij * (1 - p_ij)

  idx_map <- state$item_index %||% stats::setNames(seq_along(ids), ids)
  per_round_uses <- round$per_round_item_uses %||% integer()
  per_round_uses <- as.integer(per_round_uses)
  names(per_round_uses) <- names(round$per_round_item_uses %||% per_round_uses)
  anchor_ids <- as.character(round$anchor_ids %||% character())
  proxy <- .adaptive_rank_proxy(state)
  strata <- .adaptive_assign_strata(proxy$scores, defaults)
  stratum_map <- strata$stratum_map
  stratum_i <- as.integer(stratum_map[[i_id]] %||% NA_integer_)
  stratum_j <- as.integer(stratum_map[[j_id]] %||% NA_integer_)
  dist_stratum <- if (!is.na(stratum_i) && !is.na(stratum_j)) {
    as.integer(abs(stratum_i - stratum_j))
  } else {
    NA_integer_
  }

  list(
    i = as.integer(idx_map[[i_id]]),
    j = as.integer(idx_map[[j_id]]),
    A = as.integer(idx_map[[order_vals[["A_id"]]]]),
    B = as.integer(idx_map[[order_vals[["B_id"]]]]),
    is_explore_step = is_explore_step,
    explore_mode = explore_mode,
    explore_reason = explore_reason,
    candidate_starved = FALSE,
    fallback_used = selected_stage$name,
    fallback_path = paste(fallback_path, collapse = ">"),
    starvation_reason = NA_character_,
    round_id = as.integer(round$round_id %||% NA_integer_),
    round_stage = as.character(round_stage),
    pair_type = as.character(round_stage),
    used_in_round_i = as.integer(per_round_uses[[i_id]] %||% 0L),
    used_in_round_j = as.integer(per_round_uses[[j_id]] %||% 0L),
    is_anchor_i = as.logical(i_id %in% anchor_ids),
    is_anchor_j = as.logical(j_id %in% anchor_ids),
    stratum_i = stratum_i,
    stratum_j = stratum_j,
    dist_stratum = dist_stratum,
    stage_committed_so_far = stage_committed_so_far,
    stage_quota = stage_quota,
    n_candidates_generated = last_counts$n_candidates_generated %||% 0L,
    n_candidates_after_hard_filters = last_counts$n_candidates_after_hard_filters %||% 0L,
    n_candidates_after_duplicates = last_counts$n_candidates_after_duplicates %||% 0L,
    n_candidates_after_star_caps = last_counts$n_candidates_after_star_caps %||% 0L,
    n_candidates_scored = last_counts$n_candidates_scored %||% 0L,
    deg_i = as.integer(counts$deg[[i_id]]),
    deg_j = as.integer(counts$deg[[j_id]]),
    recent_deg_i = as.integer(recent_deg[[i_id]]),
    recent_deg_j = as.integer(recent_deg[[j_id]]),
    mu_i = as.double(mu_vals[[i_id]]),
    mu_j = as.double(mu_vals[[j_id]]),
    sigma_i = as.double(sigma_vals[[i_id]]),
    sigma_j = as.double(sigma_vals[[j_id]]),
    p_ij = as.double(p_ij),
    U0_ij = as.double(u0_ij),
    star_cap_rejects = as.integer(last_star_caps$rejects %||% 0L),
    star_cap_reject_items = as.integer(last_star_caps$reject_items_count %||% 0L)
  )
}
