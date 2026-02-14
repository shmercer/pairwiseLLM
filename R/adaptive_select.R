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
    top_band_pct = 0.10,
    top_band_bins = 5L,
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
    exposure_underrep_q = 0.25,
    global_identified_reliability_min = 0.80,
    global_identified_rank_corr_min = 0.90,
    long_taper_mult = 0.25,
    long_frac_floor = 0.02,
    mid_bonus_frac = 0.20,
    p_long_low = 0.10,
    p_long_high = 0.90,
    explore_taper_mult = 0.50,
    boundary_k = 20L,
    boundary_window = as.integer(max(10L, ceiling(0.05 * N))),
    boundary_frac = 0.15,
    p_star_override_margin = 0.05,
    star_override_budget_per_round = 1L
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
  .adaptive_underrep_set(deg)
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
    # Keep long-link semantics strict across all fallback stages.
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

.adaptive_resolve_controller <- function(state, defaults) {
  controller <- state$controller %||% list()
  list(
    run_mode = as.character(controller$run_mode %||% "within_set"),
    hub_id = as.integer(controller$hub_id %||% 1L),
    multi_spoke_mode = as.character(controller$multi_spoke_mode %||% "independent"),
    min_cross_set_pairs_per_spoke_per_refit = as.integer(
      controller$min_cross_set_pairs_per_spoke_per_refit %||% 5L
    ),
    current_link_spoke_id = as.integer(controller$current_link_spoke_id %||% NA_integer_),
    link_refit_stats_by_spoke = controller$link_refit_stats_by_spoke %||% list(),
    global_identified = isTRUE(controller$global_identified %||% FALSE),
    global_identified_reliability_min = as.double(
      controller$global_identified_reliability_min %||%
        defaults$global_identified_reliability_min
    ),
    global_identified_rank_corr_min = as.double(
      controller$global_identified_rank_corr_min %||%
        defaults$global_identified_rank_corr_min
    ),
    p_long_low = as.double(controller$p_long_low %||% defaults$p_long_low),
    p_long_high = as.double(controller$p_long_high %||% defaults$p_long_high),
    explore_taper_mult = as.double(controller$explore_taper_mult %||% defaults$explore_taper_mult),
    boundary_k = as.integer(controller$boundary_k %||% defaults$boundary_k),
    boundary_window = as.integer(controller$boundary_window %||% defaults$boundary_window),
    boundary_frac = as.double(controller$boundary_frac %||% defaults$boundary_frac),
    p_star_override_margin = as.double(
      controller$p_star_override_margin %||%
        defaults$p_star_override_margin
    ),
    star_override_budget_per_round = as.integer(
      controller$star_override_budget_per_round %||%
        defaults$star_override_budget_per_round
    )
  )
}

.adaptive_selection_mode_is_linking <- function(run_mode, is_cross_set = FALSE) {
  as.character(run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke") &&
    isTRUE(is_cross_set)
}

.adaptive_selection_utility_mode <- function(run_mode, has_regularization = FALSE, is_cross_set = FALSE) {
  if (.adaptive_selection_mode_is_linking(run_mode = run_mode, is_cross_set = is_cross_set)) {
    return("linking_cross_set_p_times_1_minus_p")
  }
  if (isTRUE(has_regularization)) {
    return("pairing_trueskill_u")
  }
  "pairing_trueskill_u0"
}

.adaptive_resolve_selection_column <- function(utility_mode) {
  mode <- as.character(utility_mode %||% NA_character_)
  if (identical(mode, "pairing_trueskill_u0")) {
    return("u0")
  }
  if (identical(mode, "pairing_trueskill_u")) {
    return("u")
  }
  if (identical(mode, "linking_cross_set_p_times_1_minus_p")) {
    return("link_u")
  }
  NA_character_
}

.adaptive_local_priority_select <- function(cand, state, round, stage_committed_so_far, stage_quota, defaults) {
  if (nrow(cand) == 0L) {
    return(list(candidates = cand, mode = "standard"))
  }

  proxy <- .adaptive_rank_proxy(state)
  rank_index <- .adaptive_rank_index_from_scores(proxy$scores)
  n_items <- length(rank_index)
  boundary_k <- max(1L, min(as.integer(defaults$boundary_k), n_items))
  boundary_window <- max(1L, min(as.integer(defaults$boundary_window), n_items))
  half_window <- as.integer(floor(boundary_window / 2))
  boundary_lo <- max(1L, boundary_k - half_window)
  boundary_hi <- min(n_items, boundary_k + half_window)

  i_rank <- as.integer(rank_index[as.character(cand$i)])
  j_rank <- as.integer(rank_index[as.character(cand$j)])
  is_boundary <- (i_rank >= boundary_lo & i_rank <= boundary_hi) |
    (j_rank >= boundary_lo & j_rank <= boundary_hi)

  boundary_target <- max(1L, as.integer(ceiling(as.double(defaults$boundary_frac) * max(1L, stage_quota))))
  use_boundary <- is.finite(stage_committed_so_far) &&
    stage_committed_so_far < boundary_target &&
    any(is_boundary, na.rm = TRUE)

  if (isTRUE(use_boundary)) {
    boundary_cand <- cand[is_boundary %in% TRUE, , drop = FALSE]
    ord <- order(abs(boundary_cand$p - 0.5), -boundary_cand$u0, boundary_cand$i, boundary_cand$j)
    return(list(candidates = boundary_cand[ord, , drop = FALSE], mode = "boundary"))
  }

  ord <- order(abs(cand$p - 0.5), -cand$u0, cand$i, cand$j)
  list(candidates = cand[ord, , drop = FALSE], mode = "near_tie")
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

.adaptive_link_safe_theta_map <- function(state, set_id, prefer_current = FALSE) {
  current_map <- .adaptive_link_theta_mean_map(state, set_id = set_id)
  phase_map <- tryCatch(
    .adaptive_link_phase_a_theta_map(state, set_id = set_id, field = "theta_raw_mean"),
    error = function(e) stats::setNames(numeric(), character())
  )
  if (isTRUE(prefer_current)) {
    if (length(current_map) > 0L) {
      return(current_map)
    }
    return(phase_map)
  }
  if (length(phase_map) > 0L) {
    return(phase_map)
  }
  current_map
}

.adaptive_link_attach_predictive_utility <- function(candidates, state, controller, spoke_id) {
  cand <- tibble::as_tibble(candidates)
  if (nrow(cand) < 1L || is.na(spoke_id)) {
    return(cand)
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  transform_mode <- .adaptive_link_transform_mode_for_spoke(controller, spoke_id)
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  stats_row <- link_stats[[as.character(spoke_id)]] %||% list()
  delta <- as.double(stats_row$delta_spoke_mean %||% 0)
  if (!is.finite(delta)) {
    delta <- 0
  }
  log_alpha <- as.double(stats_row$log_alpha_spoke_mean %||% NA_real_)
  alpha <- if (identical(transform_mode, "shift_scale") && is.finite(log_alpha)) exp(log_alpha) else 1

  prefer_current_theta <- identical(as.character(controller$link_refit_mode %||% "shift_only"), "joint_refit")
  hub_theta <- .adaptive_link_safe_theta_map(
    state,
    set_id = hub_id,
    prefer_current = prefer_current_theta
  )
  spoke_theta <- .adaptive_link_safe_theta_map(
    state,
    set_id = spoke_id,
    prefer_current = prefer_current_theta
  )
  if (length(hub_theta) < 1L || length(spoke_theta) < 1L) {
    cand$link_p <- NA_real_
    cand$link_u <- NA_real_
    return(cand)
  }
  spoke_theta_global <- delta + alpha * as.double(spoke_theta)
  names(spoke_theta_global) <- names(spoke_theta)
  theta_global <- c(hub_theta, spoke_theta_global)
  theta_global <- theta_global[!duplicated(names(theta_global))]

  startup_gap <- .adaptive_link_phase_b_startup_gap_for_spoke(state, spoke_id = as.integer(spoke_id))
  judge_params <- .adaptive_link_judge_params(
    state,
    controller,
    scope = "link",
    allow_cold_start_fallback = isTRUE(startup_gap),
    expected_link_params = !isTRUE(startup_gap)
  )
  epsilon <- as.double(judge_params$epsilon %||% 0)
  beta <- as.double(judge_params$beta %||% 0)
  if (!is.finite(epsilon)) {
    epsilon <- 0
  }
  if (!is.finite(beta)) {
    beta <- 0
  }
  epsilon <- max(0, min(1, epsilon))

  p_link <- vapply(seq_len(nrow(cand)), function(idx) {
    i_id <- as.character(cand$i[[idx]])
    j_id <- as.character(cand$j[[idx]])
    theta_i <- as.double(theta_global[[i_id]] %||% NA_real_)
    theta_j <- as.double(theta_global[[j_id]] %||% NA_real_)
    if (!is.finite(theta_i) || !is.finite(theta_j)) {
      return(NA_real_)
    }
    p_base <- stats::plogis(theta_i - theta_j + beta)
    as.double((1 - epsilon) * p_base + epsilon * 0.5)
  }, numeric(1L))
  cand$link_p <- as.double(p_link)
  cand$link_u <- as.double(p_link * (1 - p_link))
  cand
}

.adaptive_link_predictive_prob_oriented <- function(state, controller, spoke_id, A_id, B_id) {
  if (is.na(spoke_id) || is.na(A_id) || is.na(B_id)) {
    return(NA_real_)
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  transform_mode <- .adaptive_link_transform_mode_for_spoke(controller, spoke_id)
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  stats_row <- link_stats[[as.character(spoke_id)]] %||% list()
  delta <- as.double(stats_row$delta_spoke_mean %||% 0)
  if (!is.finite(delta)) {
    delta <- 0
  }
  log_alpha <- as.double(stats_row$log_alpha_spoke_mean %||% NA_real_)
  alpha <- if (identical(transform_mode, "shift_scale") && is.finite(log_alpha)) exp(log_alpha) else 1

  prefer_current_theta <- identical(as.character(controller$link_refit_mode %||% "shift_only"), "joint_refit")
  hub_theta <- .adaptive_link_safe_theta_map(
    state,
    set_id = hub_id,
    prefer_current = prefer_current_theta
  )
  spoke_theta <- .adaptive_link_safe_theta_map(
    state,
    set_id = spoke_id,
    prefer_current = prefer_current_theta
  )
  if (length(hub_theta) < 1L || length(spoke_theta) < 1L) {
    return(NA_real_)
  }
  spoke_theta_global <- delta + alpha * as.double(spoke_theta)
  names(spoke_theta_global) <- names(spoke_theta)
  theta_global <- c(hub_theta, spoke_theta_global)
  theta_global <- theta_global[!duplicated(names(theta_global))]

  startup_gap <- .adaptive_link_phase_b_startup_gap_for_spoke(state, spoke_id = as.integer(spoke_id))
  judge_params <- .adaptive_link_judge_params(
    state,
    controller,
    scope = "link",
    allow_cold_start_fallback = isTRUE(startup_gap),
    expected_link_params = !isTRUE(startup_gap)
  )
  epsilon <- as.double(judge_params$epsilon %||% 0)
  beta <- as.double(judge_params$beta %||% 0)
  if (!is.finite(epsilon)) {
    epsilon <- 0
  }
  if (!is.finite(beta)) {
    beta <- 0
  }
  epsilon <- max(0, min(1, epsilon))

  theta_A <- as.double(theta_global[[as.character(A_id)]] %||% NA_real_)
  theta_B <- as.double(theta_global[[as.character(B_id)]] %||% NA_real_)
  if (!is.finite(theta_A) || !is.finite(theta_B)) {
    return(NA_real_)
  }

  p_base <- stats::plogis(theta_A - theta_B + beta)
  as.double((1 - epsilon) * p_base + epsilon * 0.5)
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
  controller,
  generation_stage = NULL,
  round,
  history,
  counts,
  step_id,
  seed_base,
  candidates = NULL
) {
  generation_stage <- as.character(generation_stage %||% .adaptive_round_active_stage(state) %||% "warm_start")
  ids <- as.character(state$trueskill_state$items$item_id)
  candidates <- tibble::as_tibble(candidates %||% tibble::tibble(i = character(), j = character()))

  n_generated <- nrow(candidates)
  long_gate_pass <- NA
  long_gate_reason <- NA_character_
  star_override_used <- FALSE
  star_override_reason <- NA_character_
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
      star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
      long_gate_pass = long_gate_pass,
      long_gate_reason = long_gate_reason,
      star_override_used = star_override_used,
      star_override_reason = star_override_reason
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

  gate_active <- identical(generation_stage, "long_link") && isTRUE(controller$global_identified)
  if (isTRUE(gate_active) && nrow(candidates) > 0L) {
    p_long_low <- as.double(controller$p_long_low)
    p_long_high <- as.double(controller$p_long_high)
    p_gate <- as.double(candidates$p)
    keep <- p_gate >= p_long_low & p_gate <= p_long_high
    if (!any(keep)) {
      long_gate_reason <- "trueskill_extreme"
    }
    long_gate_pass <- any(keep)
    candidates <- candidates[keep, , drop = FALSE]
  }
  n_after_hard <- nrow(candidates)

  cap_count <- ceiling(config$cap_frac * config$W_cap)
  recent_deg <- .adaptive_recent_deg(history, ids, config$W_cap)
  allow_repeats <- identical(stage$dup_policy, "relaxed")

  .apply_downstream_filters <- function(candidates_in) {
    star_override_used_local <- FALSE
    star_override_reason_local <- NA_character_
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

    if (nrow(star_filtered_local$candidates) == 0L &&
      nrow(after_dup) > 0L &&
      isTRUE(controller$global_identified) &&
      generation_stage %in% c("local_link", "mid_link")) {
      budget <- as.integer(round$star_override_budget_per_round %||% controller$star_override_budget_per_round)
      used <- as.integer(round$star_override_used %||% 0L)
      remaining <- max(0L, budget - used)

      if (remaining > 0L) {
        i_ids <- as.character(after_dup$i)
        j_ids <- as.character(after_dup$j)
        i_excess <- recent_deg[i_ids] > cap_count
        j_excess <- recent_deg[j_ids] > cap_count
        rejected <- after_dup[i_excess | j_excess, , drop = FALSE]

        if (nrow(rejected) > 0L) {
          near_mask <- abs(rejected$p - 0.5) <= as.double(controller$p_star_override_margin)
          if (any(near_mask)) {
            near_tie <- rejected[near_mask, , drop = FALSE]
            deg_vals <- as.double(counts$deg)
            deg_extreme_cutoff <- if (length(deg_vals) > 0L) {
              stats::quantile(deg_vals, probs = 0.90, names = FALSE, type = 7)
            } else {
              Inf
            }
            i_deg <- as.double(counts$deg[as.character(near_tie$i)])
            j_deg <- as.double(counts$deg[as.character(near_tie$j)])
            keep_deg <- (i_deg <= deg_extreme_cutoff) & (j_deg <= deg_extreme_cutoff)
            if (any(keep_deg)) {
              override_cand <- near_tie[keep_deg, , drop = FALSE]
              star_filtered_local$candidates <- override_cand
              star_override_used_local <- TRUE
              star_override_reason_local <- "near_tie_override"
            } else {
              star_override_reason_local <- "deg_extreme"
            }
          }
        }
      } else {
        star_override_reason_local <- "budget_exhausted"
      }
    }

    list(
      candidates = star_filtered_local$candidates,
      n_after_dup = nrow(after_dup),
      star_filtered = star_filtered_local,
      star_override_used = star_override_used_local,
      star_override_reason = star_override_reason_local
    )
  }

  candidates_hard <- candidates
  candidates_base <- .adaptive_round_exposure_filter(candidates_hard,
    round = round,
    recent_deg = recent_deg,
    defaults = config,
    allow_repeat_pressure = FALSE
  )
  n_after_hard <- nrow(candidates_base)
  filtered <- .apply_downstream_filters(candidates_base)
  candidates <- filtered$candidates
  n_after_dup <- filtered$n_after_dup
  star_filtered <- filtered$star_filtered
  star_override_used <- filtered$star_override_used %||% FALSE
  star_override_reason <- filtered$star_override_reason %||% NA_character_

  if (nrow(candidates) == 0L) {
    candidates_repeat <- .adaptive_round_exposure_filter(candidates_hard,
      round = round,
      recent_deg = recent_deg,
      defaults = config,
      allow_repeat_pressure = TRUE
    )
    n_after_hard <- nrow(candidates_repeat)
    filtered_repeat <- .apply_downstream_filters(candidates_repeat)
    candidates <- filtered_repeat$candidates
    n_after_dup <- filtered_repeat$n_after_dup
    star_filtered <- filtered_repeat$star_filtered
    star_override_used <- filtered_repeat$star_override_used %||% FALSE
    star_override_reason <- filtered_repeat$star_override_reason %||% NA_character_
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
    recent_deg = recent_deg,
    long_gate_pass = long_gate_pass,
    long_gate_reason = long_gate_reason,
    star_override_used = star_override_used,
    star_override_reason = star_override_reason
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
  controller <- .adaptive_resolve_controller(state, defaults)
  # Use the full controller for linking predictive utility paths, which need
  # transform and judge-mode fields not carried by the reduced selector view.
  link_controller <- .adaptive_controller_resolve(state)
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, ids)
  step_id <- as.integer(step_id %||% (nrow(state$step_log) + 1L))
  if (length(step_id) != 1L || is.na(step_id) || step_id < 1L) {
    rlang::abort("`step_id` must be a positive integer.")
  }
  seed_base <- as.integer(state$meta$seed %||% 1L)
  round <- state$round %||% list()
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  link_phase_b <- .adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")
  link_phase_b_concurrent <- isTRUE(link_phase_b) &&
    identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
  active_link_spoke <- as.integer(NA_integer_)
  ranked_link_spokes <- integer()
  link_progress <- NULL
  round_stage <- as.character(.adaptive_round_active_stage(state) %||% "warm_start")
  if (isTRUE(link_phase_b)) {
    eligible_spokes <- as.integer(phase_ctx$active_spokes %||% integer())
    ranked_link_spokes <- .adaptive_link_ranked_spokes(
      state = state,
      controller = controller,
      eligible_spoke_ids = eligible_spokes
    )
    if (length(ranked_link_spokes) > 0L) {
      active_link_spoke <- as.integer(ranked_link_spokes[[1L]])
    }
    if (!is.na(active_link_spoke)) {
      refit_id <- .adaptive_link_refit_window_id(state)
      quota_controller <- controller
      quota_controller$current_link_spoke_id <- as.integer(active_link_spoke)
      stage_quotas <- .adaptive_round_compute_quotas(
        round_id = as.integer(round$round_id %||% 1L),
        n_items = as.integer(state$n_items),
        controller = quota_controller
      )
      link_progress <- .adaptive_link_stage_progress(
        state = state,
        spoke_id = as.integer(active_link_spoke),
        stage_quotas = stage_quotas,
        stage_order = round$stage_order %||% .adaptive_stage_order(),
        refit_id = refit_id
      )
      round_stage <- as.character(link_progress$active_stage %||% round_stage)
    }
  }
  is_link_mode <- .adaptive_link_mode(state)
  generation_stage <- if (identical(round_stage, "warm_start")) {
    if (length(ids) <= 2L) "anchor_link" else "local_link"
  } else {
    round_stage
  }
  stage_quota <- NA_integer_
  stage_committed_so_far <- NA_integer_
  if (!identical(round_stage, "warm_start")) {
    if (isTRUE(link_phase_b) && !is.null(link_progress)) {
      stage_quota <- as.integer(link_progress$stage_quotas[[round_stage]] %||% NA_integer_)
      stage_committed_so_far <- as.integer(link_progress$stage_committed[[round_stage]] %||% 0L)
    } else {
      stage_quota <- as.integer(round$stage_quotas[[round_stage]] %||% NA_integer_)
      stage_committed_so_far <- as.integer(round$stage_committed[[round_stage]] %||% 0L)
    }
  }
  .resolve_link_stage_context <- function(spoke_id = NA_integer_) {
    ctx_round_stage <- round_stage
    ctx_generation_stage <- generation_stage
    ctx_stage_quota <- stage_quota
    ctx_stage_committed <- stage_committed_so_far
    if (!(isTRUE(link_phase_b) && !is.na(as.integer(spoke_id)))) {
      return(list(
        round_stage = as.character(ctx_round_stage),
        generation_stage = as.character(ctx_generation_stage),
        stage_quota = as.integer(ctx_stage_quota),
        stage_committed_so_far = as.integer(ctx_stage_committed)
      ))
    }
    refit_id <- .adaptive_link_refit_window_id(state)
    quota_controller <- controller
    quota_controller$current_link_spoke_id <- as.integer(spoke_id)
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer(round$round_id %||% 1L),
      n_items = as.integer(state$n_items),
      controller = quota_controller
    )
    progress <- .adaptive_link_stage_progress(
      state = state,
      spoke_id = as.integer(spoke_id),
      stage_quotas = stage_quotas,
      stage_order = round$stage_order %||% .adaptive_stage_order(),
      refit_id = refit_id
    )
    ctx_round_stage <- as.character(progress$active_stage %||% ctx_round_stage)
    if (identical(ctx_round_stage, "warm_start")) {
      ctx_generation_stage <- if (length(ids) <= 2L) "anchor_link" else "local_link"
    } else {
      ctx_generation_stage <- as.character(ctx_round_stage)
    }
    ctx_stage_quota <- as.integer(progress$stage_quotas[[ctx_round_stage]] %||% NA_integer_)
    ctx_stage_committed <- as.integer(progress$stage_committed[[ctx_round_stage]] %||% 0L)
    list(
      round_stage = as.character(ctx_round_stage),
      generation_stage = as.character(ctx_generation_stage),
      stage_quota = as.integer(ctx_stage_quota),
      stage_committed_so_far = as.integer(ctx_stage_committed)
    )
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
  last_long_gate_pass <- NA
  last_long_gate_reason <- NA_character_
  last_star_override_used <- FALSE
  last_star_override_reason <- NA_character_
  selected_pair <- NULL
  selected_stage <- NULL
  explore_mode <- NA_character_
  explore_reason <- NA_character_
  explore_rate_used <- as.double(defaults$explore_rate)
  local_priority_mode <- NA_character_
  is_explore_step <- FALSE
  selected_link_spoke_attempt <- as.integer(NA_integer_)
  selected_round_stage <- as.character(round_stage)
  selected_stage_quota <- as.integer(stage_quota)
  selected_stage_committed_so_far <- as.integer(stage_committed_so_far)
  recent_deg <- .adaptive_recent_deg(history, ids, defaults$W_cap)

  for (idx in seq_along(stage_defs)) {
    stage <- stage_defs[[idx]]
    stage$idx <- idx
    fallback_path <- c(fallback_path, stage$name)
    attempt_spokes <- if (isTRUE(link_phase_b_concurrent) && length(ranked_link_spokes) > 0L &&
      !(idx == 1L && !is.null(candidates))) {
      as.integer(ranked_link_spokes)
    } else {
      as.integer(NA_integer_)
    }
    stage_selected <- FALSE

    for (spoke_attempt in attempt_spokes) {
      stage_ctx <- if (isTRUE(link_phase_b_concurrent) && !is.na(spoke_attempt)) {
        .resolve_link_stage_context(spoke_id = as.integer(spoke_attempt))
      } else {
        .resolve_link_stage_context(spoke_id = NA_integer_)
      }
      attempt_round_stage <- as.character(stage_ctx$round_stage)
      attempt_generation_stage <- as.character(stage_ctx$generation_stage)
      attempt_stage_quota <- as.integer(stage_ctx$stage_quota)
      attempt_stage_committed_so_far <- as.integer(stage_ctx$stage_committed_so_far)
      stage_seed <- .adaptive_stage_seed(
        seed_base,
        step_id,
        stage$idx,
        offset = 11L + ifelse(is.na(spoke_attempt), 0L, as.integer(spoke_attempt))
      )
      stage_candidates <- if (idx == 1L && !is.null(candidates)) {
        tibble::as_tibble(candidates)
      } else if (isTRUE(link_phase_b_concurrent)) {
        generate_stage_candidates_from_state(
          state = state,
          stage_name = attempt_generation_stage,
          fallback_name = stage$name,
          C_max = defaults$C_max,
          seed = stage_seed,
          link_spoke_id = ifelse(is.na(spoke_attempt), NA_integer_, as.integer(spoke_attempt))
        )
      } else {
        generate_stage_candidates_from_state(
          state = state,
          stage_name = attempt_generation_stage,
          fallback_name = stage$name,
          C_max = defaults$C_max,
          seed = stage_seed
        )
      }

      stage_out <- .adaptive_select_stage(
        stage = stage,
        state = state,
        config = defaults,
        controller = controller,
        generation_stage = attempt_generation_stage,
        round = round,
        history = history,
        counts = counts,
        step_id = step_id,
        seed_base = seed_base,
        candidates = stage_candidates
      )
      last_counts <- stage_out$counts
      last_star_caps <- stage_out$star_caps
      if (!is.na(stage_out$long_gate_pass %||% NA)) {
        last_long_gate_pass <- stage_out$long_gate_pass
        last_long_gate_reason <- stage_out$long_gate_reason %||% NA_character_
      }
      last_star_override_used <- isTRUE(stage_out$star_override_used)
      last_star_override_reason <- stage_out$star_override_reason
      recent_deg <- stage_out$recent_deg %||% recent_deg

      cand <- stage_out$selected
      if (is.null(cand) || nrow(cand) == 0L) {
        next
      }
      if (isTRUE(is_link_mode) && isTRUE(link_phase_b)) {
        spoke_for_utility <- if ("link_spoke_id" %in% names(cand)) {
          as.integer(unique(stats::na.omit(as.integer(cand$link_spoke_id)))[1L] %||% NA_integer_)
        } else {
          as.integer(spoke_attempt %||% NA_integer_)
        }
        cand <- .adaptive_link_attach_predictive_utility(
          candidates = cand,
          state = state,
          controller = link_controller,
          spoke_id = as.integer(spoke_for_utility)
        )
      }

      explore_rate <- defaults$explore_rate
      if (isTRUE(controller$global_identified)) {
        explore_rate <- explore_rate * as.double(controller$explore_taper_mult)
      }
      if (stage$explore_boost > 1) {
        explore_rate <- min(0.50, explore_rate * stage$explore_boost)
      }
      explore_rate_used <- as.double(explore_rate)

      underrep_set <- .adaptive_underrep_set(counts$deg)
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
      stage_local_priority_mode <- NA_character_

      if (quota_pick) {
        eligible <- cand[cand$i %in% underrep_set | cand$j %in% underrep_set, , drop = FALSE]
        if (nrow(eligible) == 0L) {
          next
        }
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
        if (is.null(selected)) {
          next
        }
        selected_pair <- selected
      } else {
        if (!isTRUE(is_link_mode) &&
          identical(attempt_generation_stage, "local_link") &&
          isTRUE(controller$global_identified)) {
          prioritized <- .adaptive_local_priority_select(
            cand = cand,
            state = state,
            round = round,
            stage_committed_so_far = attempt_stage_committed_so_far %||% 0L,
            stage_quota = attempt_stage_quota %||% nrow(cand),
            defaults = controller
          )
          cand <- prioritized$candidates
          stage_local_priority_mode <- prioritized$mode
        } else if (!isTRUE(is_link_mode) && identical(attempt_generation_stage, "local_link")) {
          stage_local_priority_mode <- "standard"
        } else {
          stage_local_priority_mode <- NA_character_
        }
        has_regularized_utility <- "u" %in% names(cand) &&
          "u0" %in% names(cand) &&
          any(
            is.finite(as.double(cand$u)) &
              is.finite(as.double(cand$u0)) &
              abs(as.double(cand$u) - as.double(cand$u0)) > sqrt(.Machine$double.eps),
            na.rm = TRUE
          )
        selected_utility_mode <- .adaptive_selection_utility_mode(
          run_mode = controller$run_mode,
          has_regularization = isTRUE(has_regularized_utility),
          is_cross_set = isTRUE(is_link_mode) && isTRUE(link_phase_b)
        )
        if (isTRUE(is_link_mode) && isTRUE(link_phase_b)) {
          # Linking mode keeps canonical candidate generation/filtering via
          # TrueSkill and hard invariants; this call only applies the
          # linking-specific final ordering priority.
          order_idx <- .adaptive_linking_selection_order(
            cand,
            utility_mode = selected_utility_mode
          )
        } else {
          utility_col <- .adaptive_resolve_selection_column(selected_utility_mode)
          utility <- if (!is.na(utility_col) && utility_col %in% names(cand)) {
            as.double(cand[[utility_col]])
          } else {
            rep_len(NA_real_, nrow(cand))
          }
          if (!any(is.finite(utility))) {
            tie_utility <- if ("u0" %in% names(cand)) as.double(cand$u0) else rep_len(NA_real_, nrow(cand))
            if (any(is.finite(tie_utility))) {
              tie_utility[!is.finite(tie_utility)] <- -Inf
              order_idx <- order(-tie_utility, cand$i, cand$j)
            } else {
              order_idx <- order(cand$i, cand$j)
            }
          } else {
            utility[!is.finite(utility)] <- -Inf
            order_idx <- order(-utility, cand$i, cand$j)
          }
        }
        selected_pair <- cand[order_idx[[1L]], , drop = FALSE]
      }

      is_explore_step <- stage_is_explore
      explore_mode <- stage_explore_mode
      explore_reason <- stage_explore_reason
      local_priority_mode <- stage_local_priority_mode
      selected_stage <- stage
      selected_link_spoke_attempt <- as.integer(spoke_attempt %||% NA_integer_)
      selected_round_stage <- as.character(attempt_round_stage)
      selected_stage_quota <- as.integer(attempt_stage_quota)
      selected_stage_committed_so_far <- as.integer(attempt_stage_committed_so_far)
      stage_selected <- TRUE
      break
    }

    if (isTRUE(stage_selected)) {
      break
    }
  }

  if (is.null(selected_pair) || nrow(selected_pair) == 0L) {
    starvation_reason <- if (isTRUE(link_phase_b_concurrent) && length(ranked_link_spokes) > 0L) {
      "all_eligible_spokes_infeasible"
    } else {
      "few_candidates_generated"
    }
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
      starvation_reason = starvation_reason,
      round_id = as.integer(round$round_id %||% NA_integer_),
      round_stage = as.character(selected_round_stage),
      pair_type = as.character(selected_round_stage),
      explore_rate_used = as.double(explore_rate_used),
      local_priority_mode = as.character(local_priority_mode),
      long_gate_pass = last_long_gate_pass,
      long_gate_reason = as.character(last_long_gate_reason),
      star_override_used = NA,
      star_override_reason = as.character(last_star_override_reason),
      used_in_round_i = NA_integer_,
      used_in_round_j = NA_integer_,
      is_anchor_i = NA,
      is_anchor_j = NA,
      stratum_i = NA_integer_,
      stratum_j = NA_integer_,
      dist_stratum = NA_integer_,
      dist_stratum_global = NA_integer_,
      coverage_bins_used = NA_integer_,
      coverage_source = NA_character_,
      link_spoke_id_selected = NA_integer_,
      stage_committed_so_far = as.integer(selected_stage_committed_so_far),
      stage_quota = as.integer(selected_stage_quota),
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
  dist_stratum_global <- .adaptive_selected_dist_stratum_global(selected_pair)
  coverage_meta <- .adaptive_selected_coverage_meta(selected_pair)
  selected_spoke_id <- as.integer(coverage_meta$link_spoke_id %||% NA_integer_)
  if (is.na(selected_spoke_id) && !is.na(selected_link_spoke_attempt)) {
    selected_spoke_id <- as.integer(selected_link_spoke_attempt)
  }
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  set_i_selected <- as.integer(set_map[[i_id]] %||% NA_integer_)
  set_j_selected <- as.integer(set_map[[j_id]] %||% NA_integer_)
  selected_is_cross_set <- !is.na(set_i_selected) && !is.na(set_j_selected) && set_i_selected != set_j_selected
  if (isTRUE(selected_is_cross_set) && is.na(selected_spoke_id) && isTRUE(is_link_mode)) {
    hub_id <- as.integer(link_controller$hub_id %||% 1L)
    if (identical(set_i_selected, hub_id)) {
      selected_spoke_id <- set_j_selected
    } else if (identical(set_j_selected, hub_id)) {
      selected_spoke_id <- set_i_selected
    }
  }
  A_id <- as.character(order_vals[["A_id"]] %||% NA_character_)
  B_id <- as.character(order_vals[["B_id"]] %||% NA_character_)
  p_ij_ts <- trueskill_win_probability(A_id, B_id, state$trueskill_state)
  p_ij <- as.double(p_ij_ts)
  has_regularized_utility <- "u" %in% names(selected_pair) &&
    "u0" %in% names(selected_pair) &&
    is.finite(as.double(selected_pair$u[[1L]])) &&
    is.finite(as.double(selected_pair$u0[[1L]])) &&
    abs(as.double(selected_pair$u[[1L]]) - as.double(selected_pair$u0[[1L]])) > sqrt(.Machine$double.eps)
  utility_mode <- .adaptive_selection_utility_mode(
    run_mode = controller$run_mode,
    has_regularization = isTRUE(has_regularized_utility),
    is_cross_set = isTRUE(selected_is_cross_set)
  )
  if (isTRUE(is_link_mode) && !is.na(selected_spoke_id)) {
    p_link_oriented <- .adaptive_link_predictive_prob_oriented(
      state = state,
      controller = link_controller,
      spoke_id = as.integer(selected_spoke_id),
      A_id = as.character(A_id),
      B_id = as.character(B_id)
    )
    if (is.finite(p_link_oriented)) {
      p_ij <- as.double(p_link_oriented)
    }
  }
  u0_ij <- as.double(p_ij * (1 - p_ij))

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
    round_stage = as.character(selected_round_stage),
    pair_type = as.character(selected_round_stage),
    explore_rate_used = as.double(explore_rate_used),
    local_priority_mode = as.character(local_priority_mode),
    long_gate_pass = last_long_gate_pass,
    long_gate_reason = as.character(last_long_gate_reason),
    star_override_used = as.logical(last_star_override_used),
    star_override_reason = as.character(last_star_override_reason),
    used_in_round_i = as.integer(per_round_uses[[i_id]] %||% 0L),
    used_in_round_j = as.integer(per_round_uses[[j_id]] %||% 0L),
    is_anchor_i = as.logical(i_id %in% anchor_ids),
    is_anchor_j = as.logical(j_id %in% anchor_ids),
    stratum_i = stratum_i,
    stratum_j = stratum_j,
    dist_stratum = dist_stratum,
    dist_stratum_global = as.integer(dist_stratum_global),
    coverage_bins_used = as.integer(coverage_meta$coverage_bins_used),
    coverage_source = as.character(coverage_meta$coverage_source),
    link_spoke_id_selected = as.integer(selected_spoke_id),
    stage_committed_so_far = as.integer(selected_stage_committed_so_far),
    stage_quota = as.integer(selected_stage_quota),
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
    utility_mode = as.character(utility_mode),
    star_cap_rejects = as.integer(last_star_caps$rejects %||% 0L),
    star_cap_reject_items = as.integer(last_star_caps$reject_items_count %||% 0L)
  )
}
