# -------------------------------------------------------------------------
# Adaptive v3 summary helpers for diagnostics and inspection.
# -------------------------------------------------------------------------

.adaptive_summary_empty_value <- function(type) {
  if (identical(type, "integer")) {
    return(integer())
  }
  if (identical(type, "double")) {
    return(double())
  }
  if (identical(type, "logical")) {
    return(logical())
  }
  if (identical(type, "character")) {
    return(character())
  }
  if (identical(type, "posixct")) {
    return(as.POSIXct(character(), tz = "UTC"))
  }
  rlang::abort("Unknown summary column type.")
}

.adaptive_summary_col <- function(log, name, default, n) {
  if (!is.null(log) && name %in% names(log)) {
    value <- log[[name]]
    if (length(value) == n) {
      return(value)
    }
    return(rep_len(value, n))
  }
  rep_len(default, n)
}

.adaptive_summary_validate_last_n <- function(last_n) {
  if (is.null(last_n)) {
    return(NULL)
  }
  if (!is.numeric(last_n) || length(last_n) != 1L || is.na(last_n)) {
    rlang::abort("`last_n` must be a single positive number or NULL.")
  }
  last_n <- as.integer(last_n)
  if (last_n < 1L) {
    rlang::abort("`last_n` must be a single positive number or NULL.")
  }
  last_n
}

.adaptive_iteration_summary_schema <- function(include_optional = TRUE) {
  required <- tibble::tibble(
    iter = .adaptive_summary_empty_value("integer"),
    phase = .adaptive_summary_empty_value("character"),
    mode = .adaptive_summary_empty_value("character"),
    iter_start_time = .adaptive_summary_empty_value("posixct"),
    batch_size_target = .adaptive_summary_empty_value("integer"),
    n_pairs_selected = .adaptive_summary_empty_value("integer"),
    n_pairs_completed = .adaptive_summary_empty_value("integer"),
    candidate_starved = .adaptive_summary_empty_value("logical"),
    reason_short_batch = .adaptive_summary_empty_value("character"),
    n_explore_selected = .adaptive_summary_empty_value("integer"),
    n_exploit_selected = .adaptive_summary_empty_value("integer"),
    gini_degree = .adaptive_summary_empty_value("double"),
    gini_pos_A = .adaptive_summary_empty_value("double")
  )
  if (!isTRUE(include_optional)) {
    return(required)
  }

  optional <- tibble::tibble(
    n_pairs_failed = .adaptive_summary_empty_value("integer"),
    backlog_unjudged = .adaptive_summary_empty_value("integer"),
    n_explore_target = .adaptive_summary_empty_value("integer"),
    n_exploit_target = .adaptive_summary_empty_value("integer"),
    n_candidates_generated = .adaptive_summary_empty_value("integer"),
    n_candidates_after_filters = .adaptive_summary_empty_value("integer"),
    W_used = .adaptive_summary_empty_value("integer"),
    explore_rate_used = .adaptive_summary_empty_value("double"),
    utility_selected_p50 = .adaptive_summary_empty_value("double"),
    utility_selected_p90 = .adaptive_summary_empty_value("double"),
    utility_candidate_p90 = .adaptive_summary_empty_value("double"),
    iter_exit_path = .adaptive_summary_empty_value("character"),
    diagnostics_pass = .adaptive_summary_empty_value("logical"),
    theta_sd_pass = .adaptive_summary_empty_value("logical"),
    U_pass = .adaptive_summary_empty_value("logical"),
    rank_stability_pass = .adaptive_summary_empty_value("logical"),
    hard_cap_reached = .adaptive_summary_empty_value("logical"),
    stop_decision = .adaptive_summary_empty_value("logical"),
    stop_reason = .adaptive_summary_empty_value("character"),
    divergences = .adaptive_summary_empty_value("integer"),
    min_ess_bulk = .adaptive_summary_empty_value("double"),
    max_rhat = .adaptive_summary_empty_value("double"),
    reliability_EAP = .adaptive_summary_empty_value("double"),
    epsilon_mean = .adaptive_summary_empty_value("double"),
    n_unique_pairs_seen = .adaptive_summary_empty_value("integer"),
    hard_cap_threshold = .adaptive_summary_empty_value("integer")
  )
  dplyr::bind_cols(required, optional)
}

.adaptive_refit_summary_schema <- function(include_optional = TRUE) {
  schema <- round_log_schema()
  gini <- tibble::tibble(
    gini_degree = .adaptive_summary_empty_value("double"),
    gini_pos_A = .adaptive_summary_empty_value("double")
  )
  if (isTRUE(include_optional)) {
    return(dplyr::bind_cols(schema, gini))
  }

  required <- c(
    "round_id",
    "iter_at_refit",
    "new_pairs",
    "divergences",
    "max_rhat",
    "min_ess_bulk",
    "epsilon_mean",
    "reliability_EAP",
    "theta_sd_median",
    "tau",
    "theta_sd_pass",
    "U0",
    "U_top_median",
    "U_abs",
    "U_pass",
    "U_dup_threshold",
    "hard_cap_reached",
    "hard_cap_threshold",
    "n_unique_pairs_seen",
    "rank_stability_pass",
    "frac_weak_adj",
    "min_adj_prob",
    "weak_adj_threshold",
    "weak_adj_frac_max",
    "min_adj_prob_threshold",
    "min_new_pairs_for_check",
    "diagnostics_pass",
    "stop_decision",
    "stop_reason",
    "mode"
  )
  required <- unique(c(required, "gini_degree", "gini_pos_A"))
  schema <- dplyr::bind_cols(schema, gini)
  schema[, required, drop = FALSE]
}

.adaptive_item_summary_schema <- function(include_optional = TRUE) {
  required <- tibble::tibble(
    item_id = .adaptive_summary_empty_value("character"),
    theta_mean = .adaptive_summary_empty_value("double"),
    theta_sd = .adaptive_summary_empty_value("double"),
    theta_q05 = .adaptive_summary_empty_value("double"),
    theta_q95 = .adaptive_summary_empty_value("double"),
    rank_mean = .adaptive_summary_empty_value("double"),
    rank_q05 = .adaptive_summary_empty_value("double"),
    rank_q95 = .adaptive_summary_empty_value("double"),
    degree = .adaptive_summary_empty_value("integer"),
    pos_A_count = .adaptive_summary_empty_value("integer"),
    pos_A_rate = .adaptive_summary_empty_value("double")
  )
  if (!isTRUE(include_optional)) {
    return(required)
  }

  optional <- tibble::tibble(
    theta_q025 = .adaptive_summary_empty_value("double"),
    theta_q975 = .adaptive_summary_empty_value("double"),
    rank_sd = .adaptive_summary_empty_value("double"),
    repeated_pairs = .adaptive_summary_empty_value("integer"),
    adjacent_prev_prob = .adaptive_summary_empty_value("double"),
    adjacent_next_prob = .adaptive_summary_empty_value("double")
  )
  dplyr::bind_cols(required, optional)
}

.adaptive_extract_theta_draws <- function(posterior, ids) {
  if (is.matrix(posterior) && is.numeric(posterior)) {
    return(posterior)
  }
  if (!is.list(posterior)) {
    return(NULL)
  }
  if (!is.null(posterior$theta_draws)) {
    return(posterior$theta_draws)
  }
  if (!is.null(posterior$draws)) {
    return(tryCatch(
      .btl_mcmc_v3_theta_draws(posterior$draws, item_id = ids),
      error = function(e) NULL
    ))
  }
  NULL
}

.adaptive_repeated_pairs_by_item <- function(state) {
  ids <- as.character(state$ids %||% character())
  if (length(ids) == 0L) {
    return(integer())
  }
  pair_count <- state$pair_count %||% integer()
  if (length(pair_count) == 0L || is.null(names(pair_count))) {
    return(stats::setNames(rep_len(NA_integer_, length(ids)), ids))
  }
  repeats <- pmax(as.integer(pair_count) - 1L, 0L)
  names(repeats) <- names(pair_count)
  if (all(repeats == 0L)) {
    return(stats::setNames(rep.int(0L, length(ids)), ids))
  }

  keys <- names(repeats)
  parts <- strsplit(keys, ":", fixed = TRUE)
  left <- vapply(parts, `[[`, character(1L), 1L)
  right <- vapply(parts, `[[`, character(1L), 2L)
  out <- stats::setNames(rep.int(0L, length(ids)), ids)
  for (idx in seq_along(repeats)) {
    if (repeats[[idx]] > 0L) {
      out[[left[[idx]]]] <- out[[left[[idx]]]] + repeats[[idx]]
      out[[right[[idx]]]] <- out[[right[[idx]]]] + repeats[[idx]]
    }
  }
  out
}

.adaptive_apply_sort_and_top_n <- function(summary, sort_by, top_n) {
  top_n <- .adaptive_summary_validate_last_n(top_n)
  descending <- sort_by %in% c("theta_mean", "theta_sd", "degree", "pos_A_rate")

  summary <- summary |>
    dplyr::mutate(
      .sort_value = dplyr::if_else(
        is.na(.data[[sort_by]]),
        if (descending) -Inf else Inf,
        .data[[sort_by]]
      )
    )

  if (descending) {
    summary <- summary |> dplyr::arrange(dplyr::desc(.data[[".sort_value"]]))
  } else {
    summary <- summary |> dplyr::arrange(.data[[".sort_value"]])
  }

  summary <- summary |> dplyr::select(-dplyr::all_of(".sort_value"))
  if (!is.null(top_n)) {
    summary <- summary |> dplyr::slice_head(n = top_n)
  }
  summary
}

#' Summarize adaptive iterations
#'
#' Build a per-iteration diagnostics summary from the adaptive batch log. When
#' per-iteration Gini metrics are not logged, this helper uses the current
#' state to compute \code{gini_degree} and \code{gini_pos_A}.
#'
#' @param state An \code{adaptive_state}.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per iteration.
#' @export
summarize_iterations <- function(state, last_n = NULL, include_optional = TRUE) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  last_n <- .adaptive_summary_validate_last_n(last_n)
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  log <- state$batch_log %||% tibble::tibble()
  if (!is.data.frame(log)) {
    log <- tibble::tibble()
  }
  log <- .adaptive_align_log_schema(log, batch_log_schema())
  log <- tibble::as_tibble(log)

  if (!is.null(last_n)) {
    log <- utils::tail(log, last_n)
  }

  if (nrow(log) == 0L) {
    return(.adaptive_iteration_summary_schema(include_optional = include_optional))
  }

  n <- nrow(log)
  iter_start_time <- if ("created_at" %in% names(log)) {
    as.POSIXct(log$created_at, tz = "UTC")
  } else {
    rep_len(as.POSIXct(NA, tz = "UTC"), n)
  }

  gini_degree <- if ("gini_degree" %in% names(log)) {
    as.double(log$gini_degree)
  } else {
    rep_len(compute_gini_degree(state$deg), n)
  }
  gini_pos_A <- if ("gini_pos_A" %in% names(log)) {
    as.double(log$gini_pos_A)
  } else {
    rep_len(compute_gini_posA(state$pos1), n)
  }

  summary <- tibble::tibble(
    iter = as.integer(log$iter),
    phase = as.character(log$phase),
    mode = as.character(log$mode),
    iter_start_time = iter_start_time,
    batch_size_target = as.integer(log$batch_size_target),
    n_pairs_selected = as.integer(log$n_pairs_selected),
    n_pairs_completed = as.integer(log$n_pairs_completed),
    candidate_starved = as.logical(log$candidate_starved),
    reason_short_batch = as.character(log$reason_short_batch),
    n_explore_selected = as.integer(log$n_explore_selected),
    n_exploit_selected = as.integer(log$n_exploit_selected),
    gini_degree = gini_degree,
    gini_pos_A = gini_pos_A
  )

  if (isTRUE(include_optional)) {
    summary <- summary |>
      dplyr::mutate(
        n_pairs_failed = as.integer(.adaptive_summary_col(log, "n_pairs_failed", NA_integer_, n)),
        backlog_unjudged = as.integer(.adaptive_summary_col(log, "backlog_unjudged", NA_integer_, n)),
        n_explore_target = as.integer(.adaptive_summary_col(log, "n_explore_target", NA_integer_, n)),
        n_exploit_target = as.integer(.adaptive_summary_col(log, "n_exploit_target", NA_integer_, n)),
        n_candidates_generated = as.integer(
          .adaptive_summary_col(log, "n_candidates_generated", NA_integer_, n)
        ),
        n_candidates_after_filters = as.integer(.adaptive_summary_col(
          log, "n_candidates_after_filters", NA_integer_, n
        )),
        W_used = as.integer(.adaptive_summary_col(log, "W_used", NA_integer_, n)),
        explore_rate_used = as.double(
          .adaptive_summary_col(log, "explore_rate_used", NA_real_, n)
        ),
        utility_selected_p50 = as.double(
          .adaptive_summary_col(log, "utility_selected_p50", NA_real_, n)
        ),
        utility_selected_p90 = as.double(
          .adaptive_summary_col(log, "utility_selected_p90", NA_real_, n)
        ),
        utility_candidate_p90 = as.double(
          .adaptive_summary_col(log, "utility_candidate_p90", NA_real_, n)
        ),
        iter_exit_path = as.character(.adaptive_summary_col(log, "iter_exit_path", NA_character_, n)),
        diagnostics_pass = as.logical(.adaptive_summary_col(log, "diagnostics_pass", NA, n)),
        theta_sd_pass = as.logical(.adaptive_summary_col(log, "theta_sd_pass", NA, n)),
        U_pass = as.logical(.adaptive_summary_col(log, "U_pass", NA, n)),
        rank_stability_pass = as.logical(
          .adaptive_summary_col(log, "rank_stability_pass", NA, n)
        ),
        hard_cap_reached = as.logical(.adaptive_summary_col(log, "hard_cap_reached", NA, n)),
        stop_decision = as.logical(.adaptive_summary_col(log, "stop_decision", NA, n)),
        stop_reason = as.character(.adaptive_summary_col(log, "stop_reason", NA_character_, n)),
        divergences = as.integer(.adaptive_summary_col(log, "divergences", NA_integer_, n)),
        min_ess_bulk = as.double(.adaptive_summary_col(log, "min_ess_bulk", NA_real_, n)),
        max_rhat = as.double(.adaptive_summary_col(log, "max_rhat", NA_real_, n)),
        reliability_EAP = as.double(.adaptive_summary_col(log, "reliability_EAP", NA_real_, n)),
        epsilon_mean = as.double(.adaptive_summary_col(log, "epsilon_mean", NA_real_, n)),
        n_unique_pairs_seen = as.integer(
          .adaptive_summary_col(log, "n_unique_pairs_seen", NA_integer_, n)
        ),
        hard_cap_threshold = as.integer(.adaptive_summary_col(log, "hard_cap_threshold", NA_integer_, n))
      )
  }

  schema <- .adaptive_iteration_summary_schema(include_optional = include_optional)
  summary <- .adaptive_align_log_schema(summary, schema)
  summary[, names(schema), drop = FALSE]
}

#' Summarize adaptive refits
#'
#' Build a per-refit diagnostics summary from the adaptive round log. When
#' per-refit Gini metrics are not logged, this helper uses the current state to
#' compute \code{gini_degree} and \code{gini_pos_A}.
#'
#' @param state An \code{adaptive_state}.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per refit.
#' @export
summarize_refits <- function(state, last_n = NULL, include_optional = TRUE) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  last_n <- .adaptive_summary_validate_last_n(last_n)
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  log <- state$config$round_log %||% tibble::tibble()
  if (!is.data.frame(log)) {
    log <- tibble::tibble()
  }
  log <- .adaptive_align_log_schema(log, round_log_schema())
  log <- tibble::as_tibble(log)

  if (!is.null(last_n)) {
    log <- utils::tail(log, last_n)
  }

  if (nrow(log) == 0L) {
    return(.adaptive_refit_summary_schema(include_optional = include_optional))
  }

  n <- nrow(log)
  gini_degree <- if ("gini_degree" %in% names(log)) {
    as.double(log$gini_degree)
  } else {
    rep_len(compute_gini_degree(state$deg), n)
  }
  gini_pos_A <- if ("gini_pos_A" %in% names(log)) {
    as.double(log$gini_pos_A)
  } else {
    rep_len(compute_gini_posA(state$pos1), n)
  }

  summary <- log |>
    dplyr::mutate(
      gini_degree = gini_degree,
      gini_pos_A = gini_pos_A
    )

  schema <- .adaptive_refit_summary_schema(include_optional = include_optional)
  summary <- .adaptive_align_log_schema(summary, schema)
  summary[, names(schema), drop = FALSE]
}

#' Summarize adaptive items
#'
#' Build an item-level diagnostics summary combining posterior draws and
#' exposure statistics.
#'
#' @param state An \code{adaptive_state}.
#' @param posterior Optional fit object or draws matrix; defaults to
#'   \code{state$fit} when available.
#' @param top_n Optional positive integer; return only the top \code{n} rows
#'   after sorting.
#' @param sort_by Column used for sorting. Defaults to \code{"rank_mean"}.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per item.
#' @export
summarize_items <- function(state,
    posterior = NULL,
    top_n = NULL,
    sort_by = c("rank_mean", "theta_mean", "theta_sd", "degree", "pos_A_rate"),
    include_optional = TRUE) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  sort_by <- match.arg(sort_by)
  posterior <- posterior %||% state$fit
  ids <- as.character(state$ids)
  n_items <- length(ids)
  degree <- as.integer(state$deg)
  pos_A_count <- as.integer(state$pos1)
  pos_A_rate <- rep_len(NA_real_, n_items)
  positive <- degree > 0L
  pos_A_rate[positive] <- pos_A_count[positive] / degree[positive]
  repeated_pairs <- .adaptive_repeated_pairs_by_item(state)

  theta_draws <- .adaptive_extract_theta_draws(posterior, ids)
  if (is.null(theta_draws) || !is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    summary <- tibble::tibble(
      item_id = ids,
      theta_mean = NA_real_,
      theta_sd = NA_real_,
      theta_q05 = NA_real_,
      theta_q95 = NA_real_,
      rank_mean = NA_real_,
      rank_q05 = NA_real_,
      rank_q95 = NA_real_,
      degree = degree,
      pos_A_count = pos_A_count,
      pos_A_rate = as.double(pos_A_rate)
    )
  } else {
    if (is.null(colnames(theta_draws))) {
      colnames(theta_draws) <- ids
    }
    theta_draws <- theta_draws[, ids, drop = FALSE]
    theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")

    theta_mean <- as.double(colMeans(theta_draws))
    theta_sd <- as.double(apply(theta_draws, 2, stats::sd))
    theta_q05 <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.05, names = FALSE))
    theta_q95 <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.95, names = FALSE))
    theta_q025 <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.025, names = FALSE))
    theta_q975 <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.975, names = FALSE))

    rank_mat <- t(apply(theta_draws, 1, function(row) {
      rank(-row, ties.method = "average")
    }))
    colnames(rank_mat) <- ids
    rank_mean <- as.double(colMeans(rank_mat))
    rank_sd <- as.double(apply(rank_mat, 2, stats::sd))
    rank_q05 <- as.double(apply(rank_mat, 2, stats::quantile, probs = 0.05, names = FALSE))
    rank_q95 <- as.double(apply(rank_mat, 2, stats::quantile, probs = 0.95, names = FALSE))

    summary <- tibble::tibble(
      item_id = ids,
      theta_mean = theta_mean,
      theta_sd = theta_sd,
      theta_q05 = theta_q05,
      theta_q95 = theta_q95,
      rank_mean = rank_mean,
      rank_q05 = rank_q05,
      rank_q95 = rank_q95,
      degree = degree,
      pos_A_count = pos_A_count,
      pos_A_rate = as.double(pos_A_rate),
      theta_q025 = theta_q025,
      theta_q975 = theta_q975,
      rank_sd = rank_sd,
      repeated_pairs = as.integer(repeated_pairs),
      adjacent_prev_prob = NA_real_,
      adjacent_next_prob = NA_real_
    )
  }

  if (isTRUE(include_optional)) {
    summary <- summary |>
      dplyr::mutate(
        theta_q025 = as.double(.adaptive_summary_col(summary, "theta_q025", NA_real_, n_items)),
        theta_q975 = as.double(.adaptive_summary_col(summary, "theta_q975", NA_real_, n_items)),
        rank_sd = as.double(.adaptive_summary_col(summary, "rank_sd", NA_real_, n_items)),
        repeated_pairs = as.integer(
          .adaptive_summary_col(summary, "repeated_pairs", NA_integer_, n_items)
        ),
        adjacent_prev_prob = as.double(
          .adaptive_summary_col(summary, "adjacent_prev_prob", NA_real_, n_items)
        ),
        adjacent_next_prob = as.double(
          .adaptive_summary_col(summary, "adjacent_next_prob", NA_real_, n_items)
        )
      )
  }

  schema <- .adaptive_item_summary_schema(include_optional = include_optional)
  summary <- .adaptive_align_log_schema(summary, schema)
  summary <- summary[, names(schema), drop = FALSE]

  summary <- .adaptive_apply_sort_and_top_n(
    summary,
    sort_by = sort_by,
    top_n = top_n
  )
  summary
}
