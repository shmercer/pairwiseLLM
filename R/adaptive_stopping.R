# -------------------------------------------------------------------------
# Stopping checks for adaptive refinement (fast inference + confirmation).
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
compute_adjacent_certainty <- function(theta_draws, ranking_ids) {
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`theta_draws` must have at least two draws.")
  }
  ids <- colnames(theta_draws)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`theta_draws` must have non-empty column names.")
  }

  ranking_ids <- as.character(ranking_ids)
  if (length(ranking_ids) < 2L) {
    rlang::abort("`ranking_ids` must contain at least two ids.")
  }
  if (anyDuplicated(ranking_ids)) {
    rlang::abort("`ranking_ids` must not contain duplicates.")
  }
  if (!setequal(ranking_ids, ids) || length(ranking_ids) != length(ids)) {
    rlang::abort("`ranking_ids` must match `theta_draws` column names.")
  }

  idx <- match(ranking_ids, ids)
  q_vals <- numeric(length(idx) - 1L)
  for (k in seq_len(length(q_vals))) {
    lhs <- theta_draws[, idx[k], drop = TRUE]
    rhs <- theta_draws[, idx[k + 1L], drop = TRUE]
    q_vals[k] <- mean(lhs > rhs)
  }
  q_vals
}

#' @keywords internal
#' @noRd
compute_Umax <- function(utilities_tbl) {
  if (is.null(utilities_tbl) || nrow(utilities_tbl) == 0L) {
    rlang::warn("Candidate utilities are empty; treating U_max as 0.")
    return(0)
  }
  if (!is.data.frame(utilities_tbl)) {
    rlang::abort("`utilities_tbl` must be a data frame or tibble.")
  }
  utilities_tbl <- tibble::as_tibble(utilities_tbl)
  .adaptive_required_cols(utilities_tbl, "utilities_tbl", "utility_raw")
  if (!is.numeric(utilities_tbl$utility_raw)) {
    rlang::abort("`utilities_tbl$utility_raw` must be numeric.")
  }
  if (all(is.na(utilities_tbl$utility_raw))) {
    rlang::warn("All `utility_raw` values are missing; treating U_max as 0.")
    return(0)
  }
  max(utilities_tbl$utility_raw, na.rm = TRUE)
}

#' @keywords internal
#' @noRd
stopping_check <- function(
    state,
    fast_fit,
    ranking_ids,
    candidates,
    utilities_tbl
) {
  validate_state(state)
  if (!is.list(fast_fit) || is.null(fast_fit$theta_draws)) {
    rlang::abort("`fast_fit` must contain `theta_draws`.")
  }
  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }

  CW <- state$config$CW %||% floor(state$N / 2)
  CW <- as.integer(CW)
  if (is.na(CW) || CW < 1L) {
    rlang::abort("`CW` must be a positive integer.")
  }
  check_due <- (state$comparisons_observed - state$last_check_at) >= CW

  if (!check_due) {
    return(list(
      state = state,
      check_performed = FALSE,
      condition_A = NA,
      condition_B = NA,
      q_summary = list(median = NA_real_, p10 = NA_real_),
      U_max = NA_real_,
      U_0 = state$U0,
      stop_candidate = state$stop_candidate,
      checks_passed_in_row = state$checks_passed_in_row
    ))
  }

  q_vals <- compute_adjacent_certainty(fast_fit$theta_draws, ranking_ids)
  q_median <- stats::median(q_vals)
  q_p10 <- as.double(stats::quantile(q_vals, probs = 0.1, names = FALSE))
  condition_A <- (q_median > 0.95) && (q_p10 > 0.80)

  U_max <- compute_Umax(utilities_tbl)
  if (!is.finite(state$U0)) {
    state$U0 <- as.double(U_max)
  }
  U_0 <- state$U0

  condition_B <- is.finite(U_max) && is.finite(U_0) && U_0 > 0 &&
    (U_max < 0.05 * U_0)

  if (isTRUE(condition_A) && isTRUE(condition_B)) {
    state$stop_candidate <- TRUE
    state$checks_passed_in_row <- as.integer(state$checks_passed_in_row + 1L)
  } else {
    state$stop_candidate <- FALSE
    state$checks_passed_in_row <- 0L
  }
  state$last_check_at <- as.integer(state$comparisons_observed)

  list(
    state = state,
    check_performed = TRUE,
    condition_A = condition_A,
    condition_B = condition_B,
    q_summary = list(median = q_median, p10 = q_p10),
    U_max = U_max,
    U_0 = U_0,
    stop_candidate = state$stop_candidate,
    checks_passed_in_row = state$checks_passed_in_row
  )
}

#' @keywords internal
#' @noRd
near_stop_from_state <- function(state) {
  validate_state(state)
  isTRUE(state$stop_candidate) || state$checks_passed_in_row > 0L
}
