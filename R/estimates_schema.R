# ---- estimates schema (internal) ----

#' Internal: rank a numeric vector (descending)
#'
#' @param x Numeric vector.
#'
#' @return Integer ranks, with `NA` where `x` is `NA`.
#' @keywords internal
.rank_desc <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- as.numeric(x)
  if (all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  r <- rank(-x, ties.method = "min", na.last = "keep")
  as.integer(r)
}


#' Internal: standardize the final estimates table schema
#'
#' Creates a tibble with a stable set of estimate columns across engines.
#'
#' Required columns:
#' - `ID`
#' - `theta_bt_firth`, `se_bt_firth`, `rank_bt_firth`
#' - `pi_rc`, `theta_rc`, `rank_rc`
#'
#' Additional per-ID columns can be supplied via `...` as named vectors.
#'
#' @param ids Character vector of IDs (length n).
#' @param theta_bt_firth Numeric vector (length n) or `NULL`.
#' @param se_bt_firth Numeric vector (length n) or `NULL`.
#' @param theta_rc Numeric vector (length n) or `NULL`.
#' @param pi_rc Numeric vector (length n) or `NULL`.
#' @param ... Named vectors (length n) to add as extra columns.
#'
#' @return A tibble.
#' @keywords internal
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

  tibble::tibble(
    ID = ids,
    theta_bt_firth = theta_bt_firth,
    se_bt_firth = se_bt_firth,
    rank_bt_firth = .rank_desc(theta_bt_firth),
    pi_rc = pi_rc,
    theta_rc = theta_rc,
    rank_rc = .rank_desc(theta_rc),
    !!!extra
  )
}
