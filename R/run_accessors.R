#' Extract the final theta table from a run
#'
#' The `pairwiseLLM` runner functions return a list with several tables.
#' This accessor returns the `theta` table when present (converted to a tibble),
#' or `NULL` if the run did not compute theta (e.g., early exit).
#'
#' @param run A run object returned by [bt_run_adaptive()],
#'   [bt_run_core_linking()], or [bt_run_adaptive_core_linking()] (or a
#'   similarly-structured list).
#'
#' @return A tibble of theta estimates (typically containing at least `ID` and
#'   `theta`) or `NULL` when unavailable.
#'
#' @seealso [bt_run_adaptive()], [bt_run_core_linking()],
#'   [bt_run_adaptive_core_linking()].
#'
#' @examples
#' run <- list(
#'   theta = tibble::tibble(ID = c("A", "B"), theta = c(0.5, -0.5)),
#'   estimates = NULL,
#'   pairing_diagnostics = NULL
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' bt_get_theta(run)
#'
#' # Missing theta -> NULL
#' run$theta <- NULL
#' bt_get_theta(run)
#'
#' @export
bt_get_theta <- function(run) {
  .bt_assert_run_like(run)
  .bt_get_run_tbl(run, "theta")
}

#' Extract the final estimates table from a run
#'
#' Many runners can optionally compute a final per-ID `estimates` table
#' (for example via a final refit step). This accessor returns that table when
#' present (converted to a tibble), or `NULL` if it is unavailable.
#'
#' @param run A run object returned by [bt_run_adaptive()],
#'   [bt_run_core_linking()], or [bt_run_adaptive_core_linking()] (or a
#'   similarly-structured list).
#'
#' @return A tibble of per-ID estimates or `NULL` when unavailable.
#'
#' @seealso [bt_run_adaptive()], [bt_run_core_linking()],
#'   [bt_run_adaptive_core_linking()].
#'
#' @examples
#' run <- list(
#'   estimates = tibble::tibble(ID = c("A", "B"), theta_rc = c(0.2, -0.2)),
#'   theta = NULL,
#'   pairing_diagnostics = NULL
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' bt_get_estimates(run)
#'
#' # Missing estimates -> NULL
#' run$estimates <- NULL
#' bt_get_estimates(run)
#'
#' @export
bt_get_estimates <- function(run) {
  .bt_assert_run_like(run)
  .bt_get_run_tbl(run, "estimates")
}

#' Extract pairing diagnostics from a run
#'
#' Some runners record per-round pairing diagnostics (graph connectivity and
#' stability metrics). This accessor returns the `pairing_diagnostics` table when
#' present (converted to a tibble), or `NULL` if it is unavailable.
#'
#' @param run A run object returned by [bt_run_adaptive()],
#'   [bt_run_core_linking()], or [bt_run_adaptive_core_linking()] (or a
#'   similarly-structured list).
#'
#' @return A tibble of per-round pairing diagnostics or `NULL` when unavailable.
#'
#' @seealso [bt_run_adaptive()], [bt_run_core_linking()],
#'   [bt_run_adaptive_core_linking()].
#'
#' @examples
#' run <- list(
#'   pairing_diagnostics = tibble::tibble(
#'     round = 1L,
#'     degree_min = 1,
#'     largest_component_frac = 1,
#'     rms_theta_delta = NA_real_,
#'     topk_overlap = NA_real_
#'   ),
#'   theta = NULL,
#'   estimates = NULL
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' bt_get_pairing_diagnostics(run)
#'
#' # Missing diagnostics -> NULL
#' run$pairing_diagnostics <- NULL
#' bt_get_pairing_diagnostics(run)
#'
#' @export
bt_get_pairing_diagnostics <- function(run) {
  .bt_assert_run_like(run)
  .bt_get_run_tbl(run, "pairing_diagnostics")
}


#' Stable 1-row stop summary for a run
#'
#' `bt_stop_summary()` returns a schema-stable, 1-row tibble describing why/when a
#' run stopped and the last-round diagnostics when available.
#'
#' The output always contains the same columns (NAs allowed):
#'
#' * `stop_reason`, `stop_round`, `theta_engine`
#' * `degree_min`, `largest_component_frac`, `rms_theta_delta`, `topk_overlap`
#' * `stop_blocked_by`, `stop_blocked_candidates`
#'
#' @param run A run object returned by [bt_run_adaptive()],
#'   [bt_run_core_linking()], or [bt_run_adaptive_core_linking()] (or a
#'   similarly-structured list).
#'
#' @return A 1-row tibble with stable columns (NAs allowed).
#'
#' @seealso [bt_run_adaptive()], [bt_run_core_linking()],
#'   [bt_run_adaptive_core_linking()].
#'
#' @examples
#' # Minimal run object (no diagnostics recorded) -> last-round metrics are NA
#' run <- list(
#'   results = NULL,
#'   estimates = NULL,
#'   theta = NULL,
#'   theta_engine = "mock",
#'   fit_provenance = list(),
#'   stop_reason = "max_rounds",
#'   stop_round = 3L,
#'   pairing_diagnostics = NULL
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' bt_stop_summary(run)
#'
#' # With pairing diagnostics -> last row is used
#' run$pairing_diagnostics <- tibble::tibble(
#'   round = c(1L, 2L),
#'   degree_min = c(1, 2),
#'   largest_component_frac = c(1, 1),
#'   rms_theta_delta = c(NA_real_, 0.01),
#'   topk_overlap = c(NA_real_, 0.9),
#'   stop_blocked_by = c(NA_character_, "budget"),
#'   stop_blocked_candidates = c(NA_character_, "A,B,C")
#' )
#'
#' bt_stop_summary(run)
#'
#' @export
bt_stop_summary <- function(run) {
  .bt_assert_run_like(run)

  stop_reason <- if (!is.null(run$stop_reason)) as.character(run$stop_reason) else NA_character_
  stop_round <- if (!is.null(run$stop_round)) as.integer(run$stop_round) else NA_integer_
  theta_engine <- if (!is.null(run$theta_engine)) as.character(run$theta_engine) else NA_character_

  diag_last <- NULL
  diag <- .bt_get_run_tbl(run, "pairing_diagnostics")
  if (!is.null(diag) && nrow(diag) > 0L) {
    diag_last <- diag[nrow(diag), , drop = FALSE]
  }

  get_last <- function(col, default) {
    if (is.null(diag_last) || !(col %in% names(diag_last))) {
      return(default)
    }
    diag_last[[col]][[1]]
  }

  tibble::tibble(
    stop_reason = stop_reason,
    stop_round = stop_round,
    theta_engine = theta_engine,
    degree_min = as.double(get_last("degree_min", NA_real_)),
    largest_component_frac = as.double(get_last("largest_component_frac", NA_real_)),
    rms_theta_delta = as.double(get_last("rms_theta_delta", NA_real_)),
    topk_overlap = as.double(get_last("topk_overlap", NA_real_)),
    stop_blocked_by = as.character(get_last("stop_blocked_by", NA_character_)),
    stop_blocked_candidates = as.character(get_last("stop_blocked_candidates", NA_character_))
  )
}


# ---- internal helpers ------------------------------------------------------

.bt_assert_run_like <- function(run) {
  if (!is.list(run)) {
    rlang::abort("`run` must be a list-like run object.")
  }
  invisible(run)
}

.bt_get_run_tbl <- function(run, field) {
  if (is.null(run[[field]])) {
    return(NULL)
  }

  x <- run[[field]]
  if (inherits(x, "tbl_df")) {
    return(x)
  }

  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }

  rlang::abort(sprintf("`run$%s` must be a tibble/data.frame or NULL.", field))
}
