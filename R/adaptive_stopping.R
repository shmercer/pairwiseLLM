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
  for (k in seq_along(q_vals)) {
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

#' @keywords internal
#' @noRd
compute_stop_metrics <- function(state, fit, candidates_with_utility, config) {
  validate_state(state)
  if (!is.list(fit) || is.null(fit$theta_draws)) {
    rlang::abort("`fit` must include `theta_draws`.")
  }
  if (!is.null(fit$epsilon_mean) && !is.null(fit$diagnostics)) {
    validate_v3_fit_contract(fit, ids = state$ids)
  }
  theta_draws <- fit$theta_draws
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`fit$theta_draws` must have at least two draws.")
  }

  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  S_subset <- as.integer(config$S_subset)
  if (is.na(S_subset) || S_subset < 1L) {
    rlang::abort("`config$S_subset` must be a positive integer.")
  }
  S_subset <- min(S_subset, nrow(theta_summary))
  theta_summary <- dplyr::arrange(
    theta_summary,
    dplyr::desc(.data$theta_sd),
    .data$item_id
  )
  theta_subset <- dplyr::slice_head(theta_summary, n = S_subset)
  theta_sd_median_S <- stats::median(theta_subset$theta_sd)

  tau <- config$tau_fn(state$N)
  if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau)) {
    rlang::abort("`config$tau_fn` must return a finite numeric scalar.")
  }
  theta_sd_pass <- is.finite(theta_sd_median_S) && theta_sd_median_S <= tau

  utilities_tbl <- candidates_with_utility %||% tibble::tibble()
  if (!is.data.frame(utilities_tbl)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  utilities_tbl <- tibble::as_tibble(utilities_tbl)

  utility_vals <- numeric()
  if (nrow(utilities_tbl) > 0L) {
    util_col <- NULL
    if ("utility" %in% names(utilities_tbl)) {
      util_col <- "utility"
    } else if ("utility_raw" %in% names(utilities_tbl)) {
      util_col <- "utility_raw"
    }
    if (is.null(util_col)) {
      rlang::abort("`candidates_with_utility` must include `utility` or `utility_raw`.")
    }
    utility_vals <- as.double(utilities_tbl[[util_col]])
    utility_vals <- utility_vals[is.finite(utility_vals)]
  }

  U_top_median <- NA_real_
  if (length(utility_vals) > 0L) {
    K_top <- as.integer(config$K_top)
    if (is.na(K_top) || K_top < 1L) {
      rlang::abort("`config$K_top` must be a positive integer.")
    }
    K_top <- min(K_top, length(utility_vals))
    utility_vals <- sort(utility_vals, decreasing = TRUE)
    U_top_median <- stats::median(utility_vals[seq_len(K_top)])
  }

  U0 <- state$U0
  if (!is.finite(U0) && is.finite(U_top_median)) {
    U0 <- as.double(U_top_median)
  }
  U_abs <- as.double(config$U_abs)
  U_pass <- is.finite(U_top_median) && is.finite(U_abs) && U_top_median <= U_abs

  diagnostics_pass <- state$posterior$diagnostics_pass %||% NA
  if (!is.na(diagnostics_pass)) {
    diagnostics_pass <- as.logical(diagnostics_pass)
    if (length(diagnostics_pass) != 1L || is.na(diagnostics_pass)) {
      rlang::abort("`state$posterior$diagnostics_pass` must be TRUE, FALSE, or NA.")
    }
  }

  total_pairs <- state$N * (state$N - 1L) / 2
  hard_cap_threshold <- ceiling(0.40 * total_pairs)
  n_unique_pairs_seen <- sum(state$pair_count >= 1L)
  hard_cap_reached <- n_unique_pairs_seen >= hard_cap_threshold

  list(
    hard_cap_reached = hard_cap_reached,
    hard_cap_threshold = hard_cap_threshold,
    n_unique_pairs_seen = n_unique_pairs_seen,
    diagnostics_pass = diagnostics_pass,
    theta_sd_median_S = theta_sd_median_S,
    theta_sd_pass = theta_sd_pass,
    tau = tau,
    U0 = U0,
    U_top_median = U_top_median,
    U_abs = U_abs,
    U_pass = U_pass
  )
}

#' @keywords internal
#' @noRd
should_stop <- function(metrics, state, config) {
  validate_state(state)
  if (!is.list(metrics)) {
    rlang::abort("`metrics` must be a list.")
  }

  if (isTRUE(metrics$hard_cap_reached)) {
    state$mode <- "stopped"
    state$stop_reason <- "hard_cap_40pct"
    return(list(
      state = state,
      stop_decision = TRUE,
      stop_reason = state$stop_reason
    ))
  }

  if (identical(state$mode, "stopped")) {
    return(list(
      state = state,
      stop_decision = TRUE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  if (!is.finite(state$U0) && is.finite(metrics$U0)) {
    state$U0 <- as.double(metrics$U0)
  }

  min_comparisons <- state$M1_target %||% NA_integer_
  if (!is.integer(min_comparisons) || length(min_comparisons) != 1L) {
    rlang::abort("`state$M1_target` must be a length-1 integer.")
  }
  if (!is.na(min_comparisons) && state$comparisons_observed < min_comparisons) {
    state$checks_passed_in_row <- 0L
    return(list(
      state = state,
      stop_decision = FALSE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  diagnostics_pass <- isTRUE(metrics$diagnostics_pass)
  theta_sd_pass <- isTRUE(metrics$theta_sd_pass)
  U_pass <- isTRUE(metrics$U_pass)

  if (diagnostics_pass && theta_sd_pass && U_pass) {
    state$checks_passed_in_row <- as.integer(state$checks_passed_in_row + 1L)
  } else {
    state$checks_passed_in_row <- 0L
  }

  stop_decision <- isTRUE(state$checks_passed_in_row >= config$checks_passed_target)
  if (stop_decision) {
    state$mode <- "stopped"
    state$stop_reason <- "v3_converged"
  }

  list(
    state = state,
    stop_decision = stop_decision,
    stop_reason = state$stop_reason %||% NA_character_
  )
}
