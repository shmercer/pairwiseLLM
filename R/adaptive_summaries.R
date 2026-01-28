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
      item_log_list = state$logs$item_log_list %||% NULL
    ))
  }
  if (is.list(state) && inherits(state$state, "adaptive_state")) {
    return(.adaptive_summary_extract_source(state$state))
  }
  if (is.list(state) && any(c("batch_log", "round_log", "item_log_list") %in% names(state))) {
    return(list(
      batch_log = state$batch_log %||% tibble::tibble(),
      round_log = state$round_log %||% tibble::tibble(),
      item_log_list = state$item_log_list %||% NULL
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

.adaptive_compute_scheduled_exposure <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }

  ids <- as.character(state$ids %||% character())
  if (length(ids) == 0L) {
    return(list(
      deg_scheduled = integer(),
      posA_scheduled = integer(),
      pos_balance_sd_scheduled = NA_real_,
      mean_degree_scheduled = NA_real_,
      min_degree_scheduled = NA_integer_
    ))
  }

  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  posA <- stats::setNames(rep.int(0L, length(ids)), ids)
  pairs <- state$history_pairs

  if (is.data.frame(pairs) && nrow(pairs) > 0L) {
    idx_A <- match(as.character(pairs$A_id), ids)
    idx_B <- match(as.character(pairs$B_id), ids)
    if (any(is.na(idx_A) | is.na(idx_B))) {
      rlang::abort("`state$history_pairs` must reference valid ids.")
    }

    count_A <- tabulate(idx_A, nbins = length(ids))
    count_B <- tabulate(idx_B, nbins = length(ids))
    deg <- as.integer(count_A + count_B)
    posA <- as.integer(count_A)
    names(deg) <- ids
    names(posA) <- ids
  }

  pos_balance <- as.double(posA) / pmax(as.double(deg), 1)
  pos_balance_sd <- if (length(pos_balance) == 0L) {
    NA_real_
  } else {
    stats::sd(pos_balance)
  }

  list(
    deg_scheduled = deg,
    posA_scheduled = posA,
    pos_balance_sd_scheduled = as.double(pos_balance_sd),
    mean_degree_scheduled = as.double(mean(as.double(deg))),
    min_degree_scheduled = as.integer(min(deg))
  )
}

.adaptive_iteration_summary_schema <- function(include_optional = TRUE) {
  required <- tibble::tibble(
    iter = .adaptive_summary_empty_value("integer"),
    phase = .adaptive_summary_empty_value("character"),
    mode = .adaptive_summary_empty_value("character"),
    created_at = .adaptive_summary_empty_value("posixct"),
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
    "lag_eligible",
    "stop_decision",
    "stop_reason",
    "mode"
  )
  schema[, required, drop = FALSE]
}

.adaptive_item_log_schema <- function(include_optional = TRUE) {
  required <- tibble::tibble(
    refit_id = .adaptive_summary_empty_value("integer"),
    ID = .adaptive_summary_empty_value("character"),
    deg = .adaptive_summary_empty_value("integer"),
    posA_prop = .adaptive_summary_empty_value("double"),
    theta_mean = .adaptive_summary_empty_value("double"),
    theta_p2.5 = .adaptive_summary_empty_value("double"),
    theta_p5 = .adaptive_summary_empty_value("double"),
    theta_p50 = .adaptive_summary_empty_value("double"),
    theta_p95 = .adaptive_summary_empty_value("double"),
    theta_p97.5 = .adaptive_summary_empty_value("double"),
    theta_sd = .adaptive_summary_empty_value("double"),
    rank_mean = .adaptive_summary_empty_value("double"),
    rank_p2.5 = .adaptive_summary_empty_value("double"),
    rank_p5 = .adaptive_summary_empty_value("double"),
    rank_p50 = .adaptive_summary_empty_value("double"),
    rank_p95 = .adaptive_summary_empty_value("double"),
    rank_p97.5 = .adaptive_summary_empty_value("double"),
    rank_sd = .adaptive_summary_empty_value("double")
  )
  if (!isTRUE(include_optional)) {
    return(required)
  }

  optional <- tibble::tibble(
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
#' @details
#' Run-scale iteration counts reflect a single scheduling cycle. The batch log
#' records \code{n_pairs_selected} (pairs scheduled this iteration),
#' \code{n_pairs_completed} (newly observed since the previous log),
#' \code{n_pairs_failed} (newly failed attempts since the previous log), and
#' \code{backlog_unjudged} (scheduled minus completed, measured after
#' scheduling). Candidate health is captured by \code{candidate_starved} when
#' fewer than the target pairs are selected. This summary omits detailed
#' fallback-stage fields; see \code{adaptive_rank_start()} for the full
#' \code{batch_log} schema.
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per iteration. Required columns:
#'   \describe{
#'     \item{\code{iter}}{Iteration index.}
#'     \item{\code{phase}}{Phase label.}
#'     \item{\code{mode}}{Run mode.}
#'     \item{\code{created_at}}{Iteration start time (UTC).}
#'     \item{\code{batch_size_target}}{Target batch size.}
#'     \item{\code{n_pairs_selected}}{Pairs scheduled this iteration.}
#'     \item{\code{n_pairs_completed}}{New results observed since last log.}
#'     \item{\code{candidate_starved}}{TRUE when fewer than target pairs were
#'     scheduled.}
#'     \item{\code{reason_short_batch}}{Reason for short batch, if any.}
#'     \item{\code{n_explore_selected}}{Exploration pairs scheduled.}
#'     \item{\code{n_exploit_selected}}{Exploitation pairs scheduled.}
#'   }
#'   Optional columns (when \code{include_optional = TRUE}) include
#'   \code{n_pairs_failed}, \code{backlog_unjudged}, exploration/exploitation
#'   targets, candidate counts, configuration used, utility percentiles, exit
#'   path markers, diagnostics gates, and stop indicators.
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
  log <- tibble::as_tibble(log)

  if (!is.null(last_n)) {
    log <- utils::tail(log, last_n)
  }

  if (!isTRUE(include_optional)) {
    required <- c(
      "iter",
      "phase",
      "mode",
      "created_at",
      "batch_size_target",
      "n_pairs_selected",
      "n_pairs_completed",
      "candidate_starved",
      "reason_short_batch",
      "n_explore_selected",
      "n_exploit_selected"
    )
    log <- log |> dplyr::select(dplyr::any_of(required))
  }

  log
}

#' Summarize adaptive refits
#'
#' Build a per-refit diagnostics summary from the adaptive round log. This is
#' a pure view over \code{round_log} and does not recompute metrics.
#'
#' @details
#' The round log is the canonical stop-audit trail and groups fields by
#' identity/cadence, run-scale counts, design knobs, coverage/imbalance,
#' posterior percentiles, diagnostics, stop quality metrics,
#' stop decision, candidate health, and MCMC configuration. Run-scale counts
#' include \code{total_pairs} (\eqn{N(N-1)/2}), \code{hard_cap_threshold}
#' (\eqn{\lceil0.40 * total\_pairs\rceil}), \code{n_unique_pairs_seen} (unique
#' unordered pairs with \code{pair_count >= 1}), \code{scheduled_pairs},
#' \code{completed_pairs}, \code{backlog_unjudged} (scheduled minus completed),
#' \code{new_pairs} (newly completed since the previous refit), and
#' \code{proposed_pairs} (candidate unordered pairs evaluated at refit).
#' Percentile columns are fixed (e.g.,
#' \code{epsilon_p2.5}, \code{epsilon_p50}, \code{epsilon_p97.5}) and remain
#' present with \code{NA} values when a parameter is not part of a model
#' variant. Stopping metrics report posterior quality (e.g.,
#' \code{reliability_EAP}) and stability checks (e.g.,
#' \code{rank_stability_pass}) without recomputation. See
#' \code{adaptive_rank_start()} for the full \code{round_log} column
#' definitions.
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param last_n Optional positive integer; return only the last \code{n} rows.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per refit (canonical \code{round_log} schema).
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
  log <- tibble::as_tibble(log)

  if (!is.null(last_n)) {
    log <- utils::tail(log, last_n)
  }

  if (!isTRUE(include_optional)) {
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
      "lag_eligible",
      "stop_decision",
      "stop_reason",
      "mode"
    )
    log <- log |> dplyr::select(dplyr::any_of(required))
  }

  log
}

#' Summarize adaptive items
#'
#' Build an item-level diagnostics summary from the canonical item logs. This
#' is a pure view and does not recompute posterior draws or exposure metrics.
#'
#' @details
#' Rank percentiles are computed from the per-draw induced ranks (lower is
#' better). Rank uncertainty grows when draws disagree on the ordering. Degree
#' and position exposure metrics summarize how frequently each item was shown
#' and whether it appeared as the first option (A position).
#'
#' @param state An \code{adaptive_state} or list containing adaptive logs.
#' @param posterior Optional \code{item_log_list} (list of item log tables) or
#'   an item log table. When \code{NULL}, uses \code{state$logs$item_log_list}
#'   when available.
#' @param refit Optional refit index. When \code{NULL}, the most recent refit is
#'   returned.
#' @param bind Logical; when \code{TRUE}, stack all refits into a single table.
#' @param top_n Optional positive integer; return only the top \code{n} rows
#'   after sorting.
#' @param sort_by Column used for sorting. Defaults to \code{"rank_mean"}.
#' @param include_optional Logical; include optional diagnostic columns.
#' @return A tibble with one row per item per refit. Columns reflect the
#'   canonical item log schema (for example \code{refit_id}, \code{ID},
#'   \code{theta_mean}, \code{rank_mean}, \code{deg}, and \code{posA_prop}).
#'   Rank percentiles summarize per-draw induced ranks (lower is better). When
#'   \code{include_optional = FALSE}, optional columns such as repeated-pair or
#'   adjacency diagnostics are dropped if present.
#' @export
summarize_items <- function(state,
    posterior = NULL,
    refit = NULL,
    bind = FALSE,
    top_n = NULL,
    sort_by = c("rank_mean", "theta_mean", "theta_sd", "degree", "pos_A_rate"),
    include_optional = TRUE) {
  if (!is.logical(include_optional) ||
    length(include_optional) != 1L ||
    is.na(include_optional)) {
    rlang::abort("`include_optional` must be TRUE or FALSE.")
  }
  if (!is.logical(bind) || length(bind) != 1L || is.na(bind)) {
    rlang::abort("`bind` must be TRUE or FALSE.")
  }

  top_n <- .adaptive_summary_validate_last_n(top_n)
  sort_by <- match.arg(sort_by)
  source <- .adaptive_summary_extract_source(state)

  item_log_list <- NULL
  if (!is.null(posterior)) {
    if (is.data.frame(posterior)) {
      item_log_list <- list(posterior)
    } else if (is.list(posterior) && is.list(posterior$item_log_list)) {
      item_log_list <- posterior$item_log_list
    }
  }
  item_log_list <- item_log_list %||% source$item_log_list %||% NULL
  if (is.null(item_log_list) || !is.list(item_log_list) || length(item_log_list) == 0L) {
    if (!is.null(posterior)) {
      rlang::warn("`posterior` must be an item log list; returning an empty view.")
    }
    return(tibble::tibble())
  }
  if (!is.null(refit)) {
    if (!is.numeric(refit) || length(refit) != 1L || is.na(refit)) {
      rlang::abort("`refit` must be a single positive integer or NULL.")
    }
    refit <- as.integer(refit)
    if (refit < 1L) {
      rlang::abort("`refit` must be a single positive integer or NULL.")
    }
  }
  if (isTRUE(bind) && !is.null(refit)) {
    rlang::abort("`refit` must be NULL when `bind` is TRUE.")
  }

  if (isTRUE(bind)) {
    first <- item_log_list[[1L]]
    if (!is.data.frame(first)) {
      rlang::abort("`item_log_list` entries must be data frames.")
    }
    expected_names <- names(first)
    for (item_log in item_log_list) {
      if (!is.data.frame(item_log)) {
        rlang::abort("`item_log_list` entries must be data frames.")
      }
      if (!identical(names(item_log), expected_names)) {
        rlang::abort("`item_log_list` entries must have identical columns when `bind = TRUE`.")
      }
    }
    item_log <- dplyr::bind_rows(lapply(item_log_list, tibble::as_tibble))
  } else {
    idx <- refit %||% length(item_log_list)
    if (idx < 1L || idx > length(item_log_list)) {
      rlang::abort(paste0(
        "Requested refit ",
        idx,
        ", but only ",
        length(item_log_list),
        " refits are available."
      ))
    }
    item_log <- item_log_list[[idx]]
    if (!is.data.frame(item_log)) {
      rlang::abort("`item_log_list` entries must be data frames.")
    }
    item_log <- tibble::as_tibble(item_log)
  }

  if (!isTRUE(include_optional)) {
    optional <- c("repeated_pairs", "adjacent_prev_prob", "adjacent_next_prob")
    item_log <- item_log |> dplyr::select(-dplyr::any_of(optional))
  }

  if (!sort_by %in% names(item_log)) {
    rlang::abort("`sort_by` must be a column in the item log.")
  }

  item_log <- .adaptive_apply_sort_and_top_n(
    item_log,
    sort_by = sort_by,
    top_n = top_n
  )
  item_log
}
