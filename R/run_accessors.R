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
