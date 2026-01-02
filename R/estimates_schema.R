# Internal helpers to standardize the final estimates table schema.

#' @noRd
.rank_desc <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- as.numeric(x)
  if (all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  as.integer(rank(-x, ties.method = "min", na.last = "keep"))
}

#' @noRd
.make_estimates_tbl <- function(ids,
                                theta_bt_firth = NULL,
                                se_bt_firth = NULL,
                                theta_rc = NULL,
                                pi_rc = NULL,
                                ...) {
  ids <- as.character(ids)
  n <- length(ids)

  .fill_num <- function(x) {
    if (is.null(x)) {
      return(rep(NA_real_, n))
    }
    x <- as.numeric(x)
    if (length(x) != n) {
      stop("All estimate vectors must match length(ids).", call. = FALSE)
    }
    x
  }

  theta_bt_firth <- .fill_num(theta_bt_firth)
  se_bt_firth <- .fill_num(se_bt_firth)
  theta_rc <- .fill_num(theta_rc)
  pi_rc <- .fill_num(pi_rc)

  extra <- list(...)
  if (length(extra)) {
    bad <- vapply(extra, function(v) length(v) != n, logical(1))
    if (any(bad)) {
      stop("All `...` columns must match length(ids).", call. = FALSE)
    }
  }

  out <- tibble::tibble(
    ID = ids,
    theta_bt_firth = theta_bt_firth,
    se_bt_firth = se_bt_firth,
    rank_bt_firth = .rank_desc(theta_bt_firth),
    pi_rc = pi_rc,
    theta_rc = theta_rc,
    rank_rc = .rank_desc(theta_rc)
  )

  if (length(extra)) {
    for (nm in names(extra)) out[[nm]] <- extra[[nm]]
  }

  out
}

#' Validate that an estimates table contains the standard schema
#'
#' Internal helper used by runners to ensure stable, schema-first outputs.
#' Returns \code{TRUE} invisibly; otherwise errors with a descriptive message.
#'
#' @param x A data.frame or tibble.
#' @param arg_name Name of the argument for error messages.
#' @return Invisible \code{TRUE}.
#' @noRd
.validate_estimates_tbl <- function(x, arg_name = "estimates") {
  if (!inherits(x, "data.frame")) {
    stop(sprintf("`%s` must be a data.frame/tibble.", arg_name), call. = FALSE)
  }
  required <- c(
    "ID",
    "theta_rc", "se_rc", "rank_rc", "pi_rc",
    "theta_bt_firth", "se_bt_firth", "rank_bt_firth",
    "theta_bt_mle", "se_bt_mle", "rank_bt_mle",
    "bt_engine_requested", "bt_engine_used", "bt_status", "bt_failure_reason"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(sprintf("`%s` is missing required columns: %s", arg_name, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}
