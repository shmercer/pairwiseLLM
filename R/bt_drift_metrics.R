# Drift metrics between two theta scales
#
# This is an internal helper used by the linking workflows to decide whether
# to link, and to record diagnostics about drift.

.as_theta_tibble <- function(x, arg_name = "current") {
  normalize_theta_df <- function(df) {
    if (!inherits(df, "data.frame")) {
      return(df)
    }

    nm <- names(df)

    # Normalize ID column
    if (!"ID" %in% nm) {
      id_cands <- c("id", "Id", "iD")
      hit <- id_cands[id_cands %in% nm][1]
      if (!is.na(hit) && length(hit) > 0) {
        df <- dplyr::rename(df, ID = dplyr::all_of(hit))
        nm <- names(df)
      }
    }
    # If `ID` is still missing, try row names (common for matrices/data.frames).
    if (!("ID" %in% names(df))) {
      rn <- rownames(df)
      if (!is.null(rn) && length(rn) == nrow(df)) {
        df$ID <- rn
        rownames(df) <- NULL
      } else if (nrow(df) == 0L) {
        df$ID <- character()
      }
    }


    # Normalize theta column
    if (!"theta" %in% nm) {
      theta_cands <- grep("^theta($|_)", nm, value = TRUE)
      if (length(theta_cands) > 0) {
        pref <- c("theta", "theta_linked", "theta_run", "theta_bt", "theta_rc")
        chosen <- pref[pref %in% theta_cands][1]
        if (is.na(chosen) || length(chosen) == 0) chosen <- theta_cands[[1]]
        df <- dplyr::rename(df, theta = dplyr::all_of(chosen))
        nm <- names(df)
      }
    }

    # Normalize se column (optional)
    if (!"se" %in% nm) {
      se_cands <- grep("^se($|_)", nm, value = TRUE)
      if (length(se_cands) > 0) {
        pref <- c("se", "se_linked", "se_run", "se_bt", "se_rc")
        chosen <- pref[pref %in% se_cands][1]
        if (is.na(chosen) || length(chosen) == 0) chosen <- se_cands[[1]]
        df <- dplyr::rename(df, se = dplyr::all_of(chosen))
      }
    }

    df
  }

  # 1) Fit object (list) with $theta (or a data.frame itself)
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

  # 2) Named numeric vector (ID -> theta)
  if (is.numeric(x) && !is.null(names(x))) {
    return(tibble::tibble(ID = names(x), theta = as.numeric(x)))
  }

  # 3) Data frame / tibble with (ID, theta)
  if (inherits(x, "data.frame")) {
    x <- normalize_theta_df(x)
    if (!all(c("ID", "theta") %in% names(x))) {
      stop("`", arg_name, "` must have columns ID and theta.", call. = FALSE)
    }
    x$ID <- as.character(x$ID)
    return(tibble::as_tibble(x))
  }

  stop(
    "`", arg_name, "` must be a fit, a named numeric vector, or a tibble with columns ID/theta.",
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
bt_drift_metrics <- function(current = NULL,
                             baseline = NULL,
                             previous = NULL,
                             ids = NULL,
                             prefix = "",
                             flip_to_match = TRUE,
                             abs_shift_probs = c(0.9, 0.95),
                             methods = c("pearson", "spearman"),
                             # Backward compatible aliases (used by older tests/helpers)
                             theta = NULL,
                             baseline_theta = NULL,
                             core_ids = NULL,
                             ...) {
  if (is.null(current) && !is.null(theta)) current <- theta
  if (is.null(baseline) && !is.null(baseline_theta)) baseline <- baseline_theta
  if (is.null(ids) && !is.null(core_ids)) ids <- core_ids
  if (is.null(baseline) && !is.null(previous)) baseline <- previous
  legacy_call <- !is.null(theta) || !is.null(baseline_theta) || !is.null(core_ids)
  if (legacy_call && identical(prefix, "")) {
    # Older helpers/tests expect `core_*` column names when using the legacy
    # theta/baseline_theta/core_ids argument names.
    prefix <- "core_"
  }
  if (is.null(baseline)) {
    stop("`baseline` (or `previous`) must be provided.", call. = FALSE)
  }

  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("`prefix` must be a single string.", call. = FALSE)
  }

  if (!is.numeric(abs_shift_probs) || length(abs_shift_probs) < 1L || anyNA(abs_shift_probs) || any(!is.finite(abs_shift_probs))) {
    stop("`abs_shift_probs` must be a numeric vector of finite probabilities.", call. = FALSE)
  }
  if (any(abs_shift_probs <= 0 | abs_shift_probs >= 1)) {
    stop("`abs_shift_probs` entries must be strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.character(methods) || length(methods) < 1L || anyNA(methods)) {
    stop("`methods` must be a non-empty character vector.", call. = FALSE)
  }
  allowed_methods <- c("pearson", "spearman")
  if (any(!methods %in% allowed_methods)) {
    stop("`methods` must be a subset of c('pearson','spearman').", call. = FALSE)
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
  safe_cor <- function(x, y, method = "pearson") {
    ok <- stats::complete.cases(x, y)
    if (sum(ok) < 2L) {
      return(NA_real_)
    }
    tryCatch(
      suppressWarnings(stats::cor(x[ok], y[ok], method = method)),
      error = function(e) NA_real_
    )
  }

  flip_applied <- FALSE
  # Flip decision uses the first requested method (defaults to pearson)
  cor_raw <- safe_cor(cur$theta, base$theta, method = methods[[1]])
  if (is.finite(cor_raw) && cor_raw < 0) {
    base$theta <- -base$theta
    flip_applied <- TRUE
  }

  n <- nrow(cur)

  # Always provide p90/p95 columns for schema stability; abs_shift_probs can request extras.
  probs_all <- sort(unique(c(0.90, 0.95, abs_shift_probs)))
  prob_names <- paste0("p", as.integer(round(100 * probs_all)), "_abs_shift")

  if (n == 0L) {
    shift_cols <- as.list(stats::setNames(rep(NA_real_, length(probs_all)), prob_names))
    return(.prefixed_row(prefix, c(list(
      n = 0L,
      flip_applied = NA,
      theta_cor = NA_real_,
      theta_spearman = NA_real_,
      mean_abs_shift = NA_real_,
      max_abs_shift = NA_real_,
      mean_signed_shift = NA_real_
    ), shift_cols)))
  }

  delta <- cur$theta - base$theta
  abs_delta <- abs(delta)

  # Correlations require >= 2 complete points; we still output both columns.
  theta_cor <- safe_cor(cur$theta, base$theta, method = "pearson")
  theta_spear <- safe_cor(cur$theta, base$theta, method = "spearman")

  mean_abs_shift <- mean(abs_delta, na.rm = TRUE)
  max_abs_shift <- max(abs_delta, na.rm = TRUE)
  mean_signed_shift <- mean(delta, na.rm = TRUE)

  qs <- stats::quantile(abs_delta, probs = probs_all, na.rm = TRUE, names = FALSE, type = 7)
  shift_cols <- as.list(stats::setNames(as.numeric(qs), prob_names))

  .prefixed_row(prefix, c(list(
    n = as.integer(n),
    flip_applied = isTRUE(flip_applied),
    theta_cor = as.numeric(theta_cor),
    theta_spearman = as.numeric(theta_spear),
    mean_abs_shift = as.numeric(mean_abs_shift),
    max_abs_shift = as.numeric(max_abs_shift),
    mean_signed_shift = as.numeric(mean_signed_shift)
  ), shift_cols))
}
