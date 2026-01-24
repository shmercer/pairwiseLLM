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

.adaptive_summary_extract_source <- function(state) {
  if (inherits(state, "adaptive_state")) {
    return(list(
      batch_log = state$batch_log %||% tibble::tibble(),
      round_log = state$config$round_log %||% tibble::tibble(),
      item_summary = state$config$item_summary %||% state$item_summary %||% NULL
    ))
  }
  if (is.list(state) && inherits(state$state, "adaptive_state")) {
    return(.adaptive_summary_extract_source(state$state))
  }
  if (is.list(state) && any(c("batch_log", "round_log", "item_summary") %in% names(state))) {
    return(list(
      batch_log = state$batch_log %||% tibble::tibble(),
      round_log = state$round_log %||% tibble::tibble(),
      item_summary = state$item_summary %||% NULL
    ))
  }
  rlang::abort("`state` must be an adaptive_state or list with adaptive logs.")
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
    n_exploit_selected = .adaptive_summary_empty_value("integer")
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
  if (isTRUE(include_optional)) {
    return(schema)
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
    "theta_sd_eap",
    "rho_theta_lag",
    "delta_sd_theta_lag",
    "rho_rank_lag",
    "hard_cap_threshold",
    "n_unique_pairs_seen",
    "rank_stability_pass",
    "diagnostics_pass",
    "stop_passes",
    "stop_eligible",
    "stop_decision",
    "stop_reason",
    "mode"
  )
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
#' Build a per-iteration diagnostics summary from the adaptive batch log. This
#' is a pure view over \code{batch_log} and does not recompute metrics.
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per iteration.
#' @export
summarize_iterations <- function(state, last_n = NULL, include_optional = TRUE) {
  last_n <- .adaptive_summary_validate_last_n(last_n)
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  source <- .adaptive_summary_extract_source(state)
  log <- source$batch_log %||% tibble::tibble()
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
    n_exploit_selected = as.integer(log$n_exploit_selected)
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
#' Build a per-refit diagnostics summary from the adaptive round log. This is
#' a pure view over \code{round_log} and does not recompute metrics.
#'
#' @details
#' The round log is the canonical stop-audit trail and groups fields by
#' identity/cadence, run-scale counts, design knobs, coverage/imbalance,
#' posterior percentiles, diagnostics, stop quality metrics, stop bookkeeping,
#' stop decision, candidate health, and MCMC configuration. Run-scale counts
#' include scheduled/completed pairs and \code{backlog_unjudged} (scheduled
#' minus completed). Percentile columns are fixed (e.g.,
#' \code{epsilon_p2.5}, \code{epsilon_p50}, \code{epsilon_p97.5}) and remain
#' present with \code{NA} values when a parameter is not part of a model
#' variant. Stopping metrics report posterior quality (e.g.,
#' \code{reliability_EAP}) and stability checks (e.g.,
#' \code{rank_stability_pass}) without recomputation.
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per refit.
#' @export
summarize_refits <- function(state, last_n = NULL, include_optional = TRUE) {
  last_n <- .adaptive_summary_validate_last_n(last_n)
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  source <- .adaptive_summary_extract_source(state)
  log <- source$round_log %||% tibble::tibble()
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

  summary <- log

  schema <- .adaptive_refit_summary_schema(include_optional = include_optional)
  summary <- .adaptive_align_log_schema(summary, schema)
  summary[, names(schema), drop = FALSE]
}

#' Summarize adaptive items
#'
#' Build an item-level diagnostics summary from the canonical item summary. This
#' is a pure view and does not recompute posterior draws or exposure metrics.
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param posterior Optional item summary table (or list containing
#'   \code{item_summary}). When \code{NULL}, uses the cached
#'   \code{state$config$item_summary} when available.
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
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }

  top_n <- .adaptive_summary_validate_last_n(top_n)
  sort_by <- match.arg(sort_by)
  source <- .adaptive_summary_extract_source(state)

  item_summary <- posterior %||% source$item_summary %||% NULL
  if (is.list(item_summary) && !is.data.frame(item_summary)) {
    item_summary <- item_summary$item_summary %||% NULL
  }
  if (is.null(item_summary) || !is.data.frame(item_summary)) {
    if (!is.null(posterior)) {
      rlang::warn("`posterior` must be an item summary table; returning an empty view.")
    }
    return(.adaptive_item_summary_schema(include_optional = include_optional))
  }
  item_summary <- tibble::as_tibble(item_summary)
  n_items <- nrow(item_summary)

  summary <- tibble::tibble(
    item_id = as.character(.adaptive_summary_col(item_summary, "ID", NA_character_, n_items)),
    theta_mean = as.double(.adaptive_summary_col(item_summary, "theta_mean", NA_real_, n_items)),
    theta_sd = as.double(.adaptive_summary_col(item_summary, "theta_sd", NA_real_, n_items)),
    theta_q05 = as.double(.adaptive_summary_col(item_summary, "theta_ci90_lo", NA_real_, n_items)),
    theta_q95 = as.double(.adaptive_summary_col(item_summary, "theta_ci90_hi", NA_real_, n_items)),
    rank_mean = as.double(.adaptive_summary_col(item_summary, "rank_mean", NA_real_, n_items)),
    rank_q05 = as.double(.adaptive_summary_col(item_summary, "rank_q05", NA_real_, n_items)),
    rank_q95 = as.double(.adaptive_summary_col(item_summary, "rank_q95", NA_real_, n_items)),
    degree = as.integer(.adaptive_summary_col(item_summary, "deg", NA_integer_, n_items)),
    pos_A_count = as.integer(.adaptive_summary_col(item_summary, "pos_A_count", NA_integer_, n_items)),
    pos_A_rate = as.double(.adaptive_summary_col(item_summary, "posA_prop", NA_real_, n_items))
  )

  if (isTRUE(include_optional)) {
    summary <- summary |>
      dplyr::mutate(
        theta_q025 = as.double(.adaptive_summary_col(item_summary, "theta_ci95_lo", NA_real_, n_items)),
        theta_q975 = as.double(.adaptive_summary_col(item_summary, "theta_ci95_hi", NA_real_, n_items)),
        rank_sd = as.double(.adaptive_summary_col(item_summary, "rank_sd", NA_real_, n_items)),
        repeated_pairs = as.integer(.adaptive_summary_col(item_summary, "repeated_pairs", NA_integer_, n_items)),
        adjacent_prev_prob = as.double(.adaptive_summary_col(item_summary, "adjacent_prev_prob", NA_real_, n_items)),
        adjacent_next_prob = as.double(.adaptive_summary_col(item_summary, "adjacent_next_prob", NA_real_, n_items))
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
