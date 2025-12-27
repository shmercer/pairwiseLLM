#' Compute drift metrics between two theta estimates
#'
#' This helper summarizes how a set of item scores (theta) has changed between two
#' model fits (e.g., across waves/batches when using a core linking set).
#'
#' Inputs can be:
#' \itemize{
#'   \item A list returned by \code{\link{fit_bt_model}} (uses \code{$theta}),
#'   \item A tibble/data frame containing columns \code{ID} and \code{theta}, or
#'   \item A named numeric vector of theta values.
#' }
#'
#' @param current Current theta estimates.
#' @param previous Previous theta estimates.
#' @param ids Optional character vector of IDs to compute drift on (e.g., a core set).
#'   If \code{NULL}, uses the intersection of IDs present in both inputs.
#' @param prefix Optional string prefix to apply to output column names.
#' @param abs_shift_probs Numeric vector of probabilities for absolute-shift quantiles.
#'   Default \code{c(0.9, 0.95)}.
#' @param methods Character vector indicating which correlation(s) to compute.
#'   Supported: \code{"pearson"}, \code{"spearman"}. Default both.
#'
#' @return A one-row tibble with drift summary columns, including:
#' \describe{
#'   \item{<prefix>n}{Number of items used for drift computation.}
#'   \item{<prefix>theta_cor}{Pearson correlation between current and previous theta (if requested).}
#'   \item{<prefix>theta_spearman}{Spearman correlation between current and previous theta (if requested).}
#'   \item{<prefix>mean_abs_shift}{Mean absolute shift in theta.}
#'   \item{<prefix>p90_abs_shift}{90th percentile absolute shift (if requested).}
#'   \item{<prefix>p95_abs_shift}{95th percentile absolute shift (if requested).}
#'   \item{<prefix>max_abs_shift}{Maximum absolute shift.}
#'   \item{<prefix>mean_signed_shift}{Mean signed shift (current - previous).}
#' }
#'
#' @examples
#' cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2))
#' prev <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0.5, 2.5))
#' bt_drift_metrics(cur, prev, prefix = "core_")
#'
#' @import tibble
#' @export
bt_drift_metrics <- function(current,
                             previous,
                             ids = NULL,
                             prefix = "",
                             abs_shift_probs = c(0.9, 0.95),
                             methods = c("pearson", "spearman")) {
  extract_theta <- function(x, arg_name) {
    # Only treat as a "fit" if it's a list that is NOT a data.frame/tibble.
    # (tibbles are lists too, and may have a 'theta' column.)
    if (is.list(x) && !inherits(x, "data.frame") && !is.null(x$theta)) {
      x <- x$theta
    }

    if (is.numeric(x) && !is.null(names(x))) {
      return(tibble::tibble(ID = names(x), theta = as.double(unname(x))))
    }

    x_tbl <- tibble::as_tibble(x)
    if (!all(c("ID", "theta") %in% names(x_tbl))) {
      stop(
        "`", arg_name, "` must be a fit (with `$theta`), a tibble with columns ID/theta, or a named numeric vector.",
        call. = FALSE
      )
    }
    tibble::tibble(ID = as.character(x_tbl$ID), theta = as.double(unname(x_tbl$theta)))
  }

  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("`prefix` must be a single string.", call. = FALSE)
  }

  if (!is.numeric(abs_shift_probs) || length(abs_shift_probs) < 1L || any(!is.finite(abs_shift_probs))) {
    stop("`abs_shift_probs` must be a numeric vector of finite probabilities.", call. = FALSE)
  }

  methods <- unique(as.character(methods))
  ok_methods <- c("pearson", "spearman")
  if (length(methods) < 1L || any(!methods %in% ok_methods)) {
    stop("`methods` must be any of: pearson, spearman.", call. = FALSE)
  }

  cur <- extract_theta(current, "current")
  prev <- extract_theta(previous, "previous")

  if (!is.null(ids)) {
    if (!is.character(ids)) {
      stop("`ids` must be a character vector when provided.", call. = FALSE)
    }
    ids <- unique(ids)
    cur <- cur[cur$ID %in% ids, , drop = FALSE]
    prev <- prev[prev$ID %in% ids, , drop = FALSE]
  }

  joined <- dplyr::inner_join(cur, prev, by = "ID", suffix = c("_cur", "_prev"))

  n <- nrow(joined)
  if (n == 0L) {
    out <- tibble::tibble(
      n = 0L,
      mean_abs_shift = NA_real_,
      max_abs_shift = NA_real_,
      mean_signed_shift = NA_real_
    )
    for (p in abs_shift_probs) {
      nm <- paste0("p", as.integer(round(100 * p)), "_abs_shift")
      out[[nm]] <- NA_real_
    }
    if ("pearson" %in% methods) out$theta_cor <- NA_real_
    if ("spearman" %in% methods) out$theta_spearman <- NA_real_
  } else {
    delta <- as.double(joined$theta_cur - joined$theta_prev)
    abs_delta <- abs(delta)

    safe_q <- function(x, p) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
    }

    out <- tibble::tibble(
      n = n,
      mean_abs_shift = if (all(is.na(abs_delta))) NA_real_ else mean(abs_delta, na.rm = TRUE),
      max_abs_shift = if (all(is.na(abs_delta))) NA_real_ else max(abs_delta, na.rm = TRUE),
      mean_signed_shift = if (all(is.na(delta))) NA_real_ else mean(delta, na.rm = TRUE)
    )

    for (p in abs_shift_probs) {
      nm <- paste0("p", as.integer(round(100 * p)), "_abs_shift")
      out[[nm]] <- safe_q(abs_delta, p)
    }

    if ("pearson" %in% methods) {
      out$theta_cor <- suppressWarnings(stats::cor(joined$theta_cur, joined$theta_prev, use = "pairwise.complete.obs"))
    }
    if ("spearman" %in% methods) {
      out$theta_spearman <- suppressWarnings(stats::cor(joined$theta_cur, joined$theta_prev, use = "pairwise.complete.obs", method = "spearman"))
    }
  }

  names(out) <- paste0(prefix, names(out))
  out
}
