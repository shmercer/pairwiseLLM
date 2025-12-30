# Drift metrics between two theta scales
#
# This is an internal helper used by the linking workflows to decide whether
# to link, and to record diagnostics about drift.

.as_theta_tibble <- function(x, arg_name = "current") {
  # 1) Fit object (list) with $theta
  if (is.list(x) && !inherits(x, "data.frame")) {
    if ("theta" %in% names(x) && is.null(x$theta)) {
      stop(
        "`", arg_name, "$theta` is NULL. ",
        "Fits must contain a `$theta` tibble with columns ID/theta ",
        "(and optional se).",
        call. = FALSE
      )
    }
    if (!is.null(x$theta)) {
      x <- x$theta
    }
  }

  # 2) Named numeric vector
  if (is.numeric(x) && !is.null(names(x))) {
    out <- tibble::tibble(ID = names(x), theta = as.numeric(x))
    return(out)
  }

  # 3) data.frame / tibble
  if (inherits(x, "data.frame")) {
    if (!("ID" %in% names(x)) || !("theta" %in% names(x))) {
      stop(
        "`", arg_name, "` must have columns ID and theta.",
        call. = FALSE
      )
    }
    out <- tibble::as_tibble(x)
    out <- dplyr::select(out, dplyr::any_of(c("ID", "theta", "se")))
    out$ID <- as.character(out$ID)
    out$theta <- as.numeric(out$theta)
    return(out)
  }

  stop(
    "`", arg_name,
    "` must be a fit (with `$theta`), a tibble with columns ID/theta, or a named numeric vector.",
    call. = FALSE
  )
}

#' Compute drift metrics between two theta sets
#'
#' @param current A fit (list with `$theta`), a tibble/data.frame with columns
#'   `ID` and `theta`, or a named numeric vector.
#' @param baseline Same as `current`.
#' @param ids Optional character vector. If provided, metrics are computed on
#'   the intersection of `ids` with the IDs present in both inputs.
#' @param prefix Prefix for column names in the returned tibble.
#'
#' @return A one-row tibble of drift metrics.
#'
#' @keywords internal
bt_drift_metrics <- function(current,
                             baseline = NULL,
                             previous = NULL,
                             ids = NULL,
                             prefix = "") {
  if (is.null(baseline) && !is.null(previous)) baseline <- previous
  if (is.null(baseline)) {
    stop("`baseline` (or `previous`) must be provided.", call. = FALSE)
  }

  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("`prefix` must be a single string.", call. = FALSE)
  }

  .prefixed_row <- function(prefix, values) {
    # Avoid tidy-eval `:=` in package code to prevent R CMD check notes.
    stopifnot(is.character(prefix), length(prefix) == 1L)
    stopifnot(is.list(values), length(values) > 0L)
    names(values) <- paste0(prefix, names(values))
    tibble::as_tibble(values)
  }

  cur <- .as_theta_tibble(current, arg_name = "current")
  base <- .as_theta_tibble(baseline, arg_name = "baseline")

  if (!is.null(ids)) {
    if (!is.character(ids)) {
      stop("`ids` must be a character vector of IDs.", call. = FALSE)
    }
    if (length(ids) < 1L || anyNA(ids) || any(!nzchar(ids))) {
      stop("`ids` must be a non-empty character vector of IDs.", call. = FALSE)
    }
    cur <- cur[cur$ID %in% ids, , drop = FALSE]
    base <- base[base$ID %in% ids, , drop = FALSE]
  }

  overlap <- intersect(cur$ID, base$ID)
  cur <- cur[cur$ID %in% overlap, , drop = FALSE]
  base <- base[base$ID %in% overlap, , drop = FALSE]

  # Align baseline to current ID order
  base <- base[match(cur$ID, base$ID), , drop = FALSE]

  # BT scales are only identifiable up to a global sign flip. If the new fit is
  # effectively the negative of the baseline, naive drift diagnostics will show
  # correlations near -1 and (spuriously) huge shifts. We therefore flip the
  # baseline to make the Pearson correlation non-negative (when defined).
  flip_applied <- FALSE
  cor_raw <- suppressWarnings(stats::cor(cur$theta, base$theta, use = "complete.obs"))
  if (is.finite(cor_raw) && cor_raw < 0) {
    base$theta <- -base$theta
    if (!is.null(base$se)) base$se <- base$se
    flip_applied <- TRUE
  }

  n <- nrow(cur)
  if (n == 0L) {
    return(.prefixed_row(prefix, list(
      n = 0L,
      theta_cor = NA_real_,
      theta_spearman = NA_real_,
      mean_abs_shift = NA_real_,
      p90_abs_shift = NA_real_,
      p95_abs_shift = NA_real_,
      max_abs_shift = NA_real_,
      mean_signed_shift = NA_real_
    )))
  }

  delta <- cur$theta - base$theta
  abs_delta <- abs(delta)

  safe_cor <- function(x, y, method = c("pearson", "spearman")) {
    method <- match.arg(method)
    if (length(x) < 2L || length(y) < 2L) {
      return(NA_real_)
    }
    if (is.na(stats::sd(x, na.rm = TRUE)) || is.na(stats::sd(y, na.rm = TRUE))) {
      return(NA_real_)
    }
    if (stats::sd(x, na.rm = TRUE) == 0 || stats::sd(y, na.rm = TRUE) == 0) {
      return(NA_real_)
    }
    suppressWarnings(stats::cor(x, y, method = method, use = "complete.obs"))
  }

  # Correlations require >= 2 points
  theta_cor <- safe_cor(cur$theta, base$theta, method = "pearson")
  theta_spear <- safe_cor(cur$theta, base$theta, method = "spearman")

  mean_abs_shift <- mean(abs_delta, na.rm = TRUE)
  p90_abs_shift <- as.numeric(stats::quantile(abs_delta, probs = 0.90, na.rm = TRUE, names = FALSE, type = 7))
  p95_abs_shift <- as.numeric(stats::quantile(abs_delta, probs = 0.95, na.rm = TRUE, names = FALSE, type = 7))
  max_abs_shift <- max(abs_delta, na.rm = TRUE)
  mean_signed_shift <- mean(delta, na.rm = TRUE)

  .prefixed_row(prefix, list(
    n = as.integer(n),
    flip_applied = isTRUE(flip_applied),
    theta_cor = as.numeric(theta_cor),
    theta_spearman = as.numeric(theta_spear),
    mean_abs_shift = as.numeric(mean_abs_shift),
    p90_abs_shift = as.numeric(p90_abs_shift),
    p95_abs_shift = as.numeric(p95_abs_shift),
    max_abs_shift = as.numeric(max_abs_shift),
    mean_signed_shift = as.numeric(mean_signed_shift)
  ))
}
