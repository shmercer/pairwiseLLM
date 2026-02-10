# -------------------------------------------------------------------------
# Adaptive step execution (transactional).
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_judge_result <- function(result, A_id, B_id) {
  invalid <- function(reason) {
    list(
      Y = NA_integer_,
      is_valid = FALSE,
      invalid_reason = reason,
      raw = result
    )
  }

  if (!is.list(result) || is.data.frame(result)) {
    return(invalid("invalid_contract"))
  }

  if (!"is_valid" %in% names(result)) {
    return(invalid("invalid_contract"))
  }

  is_valid <- result$is_valid
  if (!is.logical(is_valid) || length(is_valid) != 1L || is.na(is_valid)) {
    return(invalid("invalid_contract"))
  }

  if (isTRUE(is_valid)) {
    if (!"Y" %in% names(result)) {
      return(invalid("invalid_contract"))
    }
    y_raw <- result$Y
    if (!.adaptive_is_integerish(y_raw) || length(y_raw) != 1L) {
      return(invalid("invalid_contract"))
    }
    y_val <- as.integer(y_raw)
    if (!y_val %in% c(0L, 1L)) {
      return(invalid("invalid_contract"))
    }
    return(list(
      Y = y_val,
      is_valid = TRUE,
      invalid_reason = NA_character_,
      raw = result
    ))
  }

  invalid_reason <- result$invalid_reason %||% NA_character_
  if (!is.character(invalid_reason) || length(invalid_reason) != 1L ||
    is.na(invalid_reason) || invalid_reason == "") {
    invalid_reason <- "invalid_contract"
  }

  list(
    Y = NA_integer_,
    is_valid = FALSE,
    invalid_reason = invalid_reason,
    raw = result
  )
}

#' @keywords internal
#' @noRd
.adaptive_warm_start_active <- function(state) {
  pairs <- state$warm_start_pairs %||% tibble::tibble(i_id = character(), j_id = character())
  idx <- as.integer(state$warm_start_idx %||% 1L)
  !isTRUE(state$warm_start_done) && nrow(pairs) > 0L && idx >= 1L && idx <= nrow(pairs)
}

#' @keywords internal
#' @noRd
.adaptive_warm_start_selection <- function(state, step_id) {
  pairs <- state$warm_start_pairs %||% tibble::tibble(i_id = character(), j_id = character())
  idx <- as.integer(state$warm_start_idx %||% 1L)
  pair <- pairs[idx, , drop = FALSE]
  if (nrow(pair) != 1L) {
    return(NULL)
  }

  i_id <- as.character(pair$i_id[[1L]])
  j_id <- as.character(pair$j_id[[1L]])
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, state$item_ids)

  order_vals <- .adaptive_assign_order(
    tibble::tibble(i = i_id, j = j_id),
    counts$posA,
    counts$posB,
    counts$pair_last_order
  )

  trueskill_state <- state$trueskill_state
  mu_vals <- trueskill_state$items$mu
  sigma_vals <- trueskill_state$items$sigma
  names(mu_vals) <- as.character(trueskill_state$items$item_id)
  names(sigma_vals) <- as.character(trueskill_state$items$item_id)

  p_ij <- trueskill_win_probability(i_id, j_id, trueskill_state)
  u0_ij <- p_ij * (1 - p_ij)

  idx_map <- state$item_index %||% stats::setNames(seq_along(state$item_ids), state$item_ids)
  recent_deg <- .adaptive_recent_deg(history, state$item_ids, adaptive_defaults(length(state$item_ids))$W_cap)
  defaults <- adaptive_defaults(length(state$item_ids))

  list(
    i = as.integer(idx_map[[i_id]]),
    j = as.integer(idx_map[[j_id]]),
    A = as.integer(idx_map[[order_vals[["A_id"]]]]),
    B = as.integer(idx_map[[order_vals[["B_id"]]]]),
    is_explore_step = FALSE,
    explore_mode = NA_character_,
    explore_reason = NA_character_,
    explore_rate_used = as.double(defaults$explore_rate),
    local_priority_mode = NA_character_,
    long_gate_pass = NA,
    long_gate_reason = NA_character_,
    star_override_used = FALSE,
    star_override_reason = NA_character_,
    candidate_starved = FALSE,
    fallback_used = "warm_start",
    fallback_path = "warm_start",
    starvation_reason = NA_character_,
    round_id = as.integer(state$round$round_id %||% NA_integer_),
    round_stage = "warm_start",
    pair_type = "warm_start",
    used_in_round_i = NA_integer_,
    used_in_round_j = NA_integer_,
    is_anchor_i = NA,
    is_anchor_j = NA,
    stratum_i = NA_integer_,
    stratum_j = NA_integer_,
    dist_stratum = NA_integer_,
    stage_committed_so_far = NA_integer_,
    stage_quota = NA_integer_,
    n_candidates_generated = NA_integer_,
    n_candidates_after_hard_filters = NA_integer_,
    n_candidates_after_duplicates = NA_integer_,
    n_candidates_after_star_caps = NA_integer_,
    n_candidates_scored = NA_integer_,
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
    star_cap_rejects = 0L,
    star_cap_reject_items = 0L
  )
}

#' @keywords internal
#' @noRd
apply_step_update <- function(state, step) {
  out <- state
  out$step_log <- append_step_log(out$step_log, step$row)

  if (!isTRUE(step$is_valid)) {
    return(out)
  }

  winner_id <- if (step$Y == 1L) step$A_id else step$B_id
  loser_id <- if (step$Y == 1L) step$B_id else step$A_id
  out$trueskill_state <- update_trueskill_state(out$trueskill_state, winner_id, loser_id)

  new_history <- tibble::tibble(
    A_id = as.character(step$A_id),
    B_id = as.character(step$B_id)
  )
  out$history_pairs <- dplyr::bind_rows(out$history_pairs, new_history)

  items <- out$trueskill_state$items
  item_ids <- as.character(items$item_id)
  counts <- .adaptive_pair_counts(out$history_pairs, item_ids)
  degree <- as.integer(counts$deg[item_ids])

  rows <- tibble::tibble(
    step_id = as.integer(step$row$step_id),
    timestamp = step$row$timestamp,
    item_id = as.integer(out$item_index[item_ids]),
    mu = as.double(items$mu),
    sigma = as.double(items$sigma),
    degree = degree
  )
  out$item_step_log <- append_item_step_log(out$item_step_log, rows)
  out
}

#' @keywords internal
#' @noRd
run_one_step <- function(state, judge, ...) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!is.function(judge)) {
    rlang::abort("`judge` must be a function.")
  }

  now_fn <- state$meta$now_fn %||% function() Sys.time()
  step_id <- as.integer(nrow(state$step_log) + 1L)
  timestamp <- now_fn()

  if (.adaptive_warm_start_active(state)) {
    selection <- .adaptive_warm_start_selection(state, step_id = step_id)
  } else {
    state <- .adaptive_refresh_round_anchors(state)
    selection <- select_next_pair(state, step_id = step_id)
  }

  is_valid <- FALSE
  invalid_reason <- NA_character_
  Y <- NA_integer_

  A_id <- NA_character_
  B_id <- NA_character_

  if (!isTRUE(selection$candidate_starved) &&
    !is.na(selection$A) &&
    !is.na(selection$B)) {
    A_id <- state$item_ids[[selection$A]]
    B_id <- state$item_ids[[selection$B]]

    A_item <- state$items[state$items$item_id == A_id, , drop = FALSE]
    B_item <- state$items[state$items$item_id == B_id, , drop = FALSE]

    result <- judge(A_item, B_item, state, ...)
    validated <- validate_judge_result(result, A_id = A_id, B_id = B_id)
    is_valid <- isTRUE(validated$is_valid)
    invalid_reason <- validated$invalid_reason
    Y <- as.integer(validated$Y)
  } else {
    invalid_reason <- "candidate_starved"
  }

  status <- if (isTRUE(is_valid)) {
    "ok"
  } else if (isTRUE(selection$candidate_starved)) {
    "starved"
  } else {
    "invalid"
  }

  pair_id <- if (isTRUE(is_valid)) {
    as.integer(nrow(state$history_pairs) + 1L)
  } else {
    NA_integer_
  }

  step_row <- list(
    step_id = step_id,
    timestamp = timestamp,
    pair_id = pair_id,
    i = selection$i,
    j = selection$j,
    A = selection$A,
    B = selection$B,
    Y = if (isTRUE(is_valid)) Y else NA_integer_,
    status = status,
    round_id = selection$round_id,
    round_stage = selection$round_stage,
    pair_type = selection$pair_type,
    used_in_round_i = selection$used_in_round_i,
    used_in_round_j = selection$used_in_round_j,
    is_anchor_i = selection$is_anchor_i,
    is_anchor_j = selection$is_anchor_j,
    stratum_i = selection$stratum_i,
    stratum_j = selection$stratum_j,
    dist_stratum = selection$dist_stratum,
    stage_committed_so_far = selection$stage_committed_so_far,
    stage_quota = selection$stage_quota,
    is_explore_step = selection$is_explore_step,
    explore_mode = selection$explore_mode,
    explore_reason = selection$explore_reason,
    explore_rate_used = selection$explore_rate_used,
    local_priority_mode = selection$local_priority_mode,
    long_gate_pass = selection$long_gate_pass,
    long_gate_reason = selection$long_gate_reason,
    star_override_used = selection$star_override_used,
    star_override_reason = selection$star_override_reason,
    candidate_starved = selection$candidate_starved,
    fallback_used = selection$fallback_used,
    fallback_path = selection$fallback_path,
    starvation_reason = selection$starvation_reason,
    n_candidates_generated = selection$n_candidates_generated,
    n_candidates_after_hard_filters = selection$n_candidates_after_hard_filters,
    n_candidates_after_duplicates = selection$n_candidates_after_duplicates,
    n_candidates_after_star_caps = selection$n_candidates_after_star_caps,
    n_candidates_scored = selection$n_candidates_scored,
    deg_i = selection$deg_i,
    deg_j = selection$deg_j,
    recent_deg_i = selection$recent_deg_i,
    recent_deg_j = selection$recent_deg_j,
    mu_i = selection$mu_i,
    mu_j = selection$mu_j,
    sigma_i = selection$sigma_i,
    sigma_j = selection$sigma_j,
    p_ij = if (isTRUE(is_valid)) selection$p_ij else NA_real_,
    U0_ij = if (isTRUE(is_valid)) selection$U0_ij else NA_real_,
    star_cap_rejects = selection$star_cap_rejects,
    star_cap_reject_items = selection$star_cap_reject_items
  )

  out <- apply_step_update(state, list(
    row = step_row,
    is_valid = is_valid,
    invalid_reason = invalid_reason,
    A_id = A_id,
    B_id = B_id,
    Y = Y
  ))

  if (.adaptive_warm_start_active(out) && isTRUE(is_valid)) {
    out$warm_start_idx <- as.integer(out$warm_start_idx %||% 1L) + 1L
    if (out$warm_start_idx > nrow(out$warm_start_pairs)) {
      out$warm_start_done <- TRUE
    }
  }

  out
}
