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

  n <- nrow(cur)
  if (n == 0L) {
    return(tibble::tibble(
      !!paste0(prefix, "n") := 0L,
      !!paste0(prefix, "theta_cor") := NA_real_,
      !!paste0(prefix, "theta_spearman") := NA_real_,
      !!paste0(prefix, "mean_abs_shift") := NA_real_,
      !!paste0(prefix, "p90_abs_shift") := NA_real_,
      !!paste0(prefix, "p95_abs_shift") := NA_real_,
      !!paste0(prefix, "max_abs_shift") := NA_real_,
      !!paste0(prefix, "mean_signed_shift") := NA_real_
    ))
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

  tibble::tibble(
    !!paste0(prefix, "n") := as.integer(n),
    !!paste0(prefix, "theta_cor") := as.numeric(theta_cor),
    !!paste0(prefix, "theta_spearman") := as.numeric(theta_spear),
    !!paste0(prefix, "mean_abs_shift") := as.numeric(mean_abs_shift),
    !!paste0(prefix, "p90_abs_shift") := as.numeric(p90_abs_shift),
    !!paste0(prefix, "p95_abs_shift") := as.numeric(p95_abs_shift),
    !!paste0(prefix, "max_abs_shift") := as.numeric(max_abs_shift),
    !!paste0(prefix, "mean_signed_shift") := as.numeric(mean_signed_shift)
  )
}
