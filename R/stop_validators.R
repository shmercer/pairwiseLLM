# -------------------------------------------------------------------------
# Stop payload shape validators (internal)
# -------------------------------------------------------------------------

#' Internal: validate stop-metrics tibble schema
#'
#' @param x A tibble expected to contain stop metrics columns.
#'
#' @keywords internal
.validate_stop_metrics_tbl <- function(x) {
  x <- tibble::as_tibble(x)
  if (nrow(x) != 1L) {
    stop("stop metrics must be a one-row tibble.", call. = FALSE)
  }

  required <- c(
    "engine", "n_items", "n_total_items", "theta_sd",
    "se_mean", "se_max", "rel_se_mean", "rel_se_p90",
    "reliability", "sepG", "item_misfit_prop", "judge_misfit_prop",
    "n_matched", "rms_theta_delta", "topk_overlap", "rank_corr"
  )

  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop("stop metrics missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}

#' Internal: validate stop decision payload
#'
#' @param x A list with scalar `stop` and scalar `reason`.
#'
#' @keywords internal
.validate_stop_decision <- function(x) {
  if (!is.list(x) || is.null(x$stop)) {
    stop("stop decision must be a list with element `stop`.", call. = FALSE)
  }
  if (!is.logical(x$stop) || length(x$stop) != 1L || is.na(x$stop)) {
    stop("stop decision `stop` must be a non-missing scalar logical.", call. = FALSE)
  }

  reason <- x$reason %||% NA_character_
  if (!is.character(reason) || length(reason) != 1L) {
    stop("stop decision `reason` must be a scalar character (or NA).", call. = FALSE)
  }
  invisible(TRUE)
}

#' Internal: validate rounds schema
#'
#' @param rounds_tbl A tibble of per-round diagnostics.
#'
#' @keywords internal
.validate_rounds_schema <- function(rounds_tbl) {
  rounds_tbl <- tibble::as_tibble(rounds_tbl)
  req <- c(
    "round", "n_new_pairs_scored", "n_total_results",
    "stop", "stop_reason",
    "stop_blocked_by", "stop_blocked_candidates",
    "degree_min", "largest_component_frac",
    "rms_theta_delta", "topk_overlap"
  )
  missing <- setdiff(req, names(rounds_tbl))
  if (length(missing) > 0L) {
    stop("rounds schema missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.list(rounds_tbl$stop_reason)) {
    stop("rounds `stop_reason` must not be a list column.", call. = FALSE)
  }
  if ("stop_blocked_by" %in% names(rounds_tbl) && is.list(rounds_tbl$stop_blocked_by)) {
    stop("rounds `stop_blocked_by` must not be a list column.", call. = FALSE)
  }
  if ("stop_blocked_candidates" %in% names(rounds_tbl) && is.list(rounds_tbl$stop_blocked_candidates)) {
    stop("rounds `stop_blocked_candidates` must not be a list column.", call. = FALSE)
  }
  invisible(TRUE)
}
