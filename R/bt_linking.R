#' Link theta estimates to a reference scale using a core anchor set
#'
#' Bradley--Terry (and related IRT) models identify theta only up to an additive
#' constant (and, depending on constraints, sometimes an additional linear scaling).
#' In sequential / multi-wave workflows, a common approach is to link each new fit
#' to a reference fit using a stable core set.
#'
#' This function computes a simple affine transform
#'
#' \deqn{\theta_{linked} = a + b\,\theta_{current}}
#'
#' by matching the mean and SD of the current fit's core thetas to the reference
#' fit's core thetas. The transform is then applied to all items in the current
#' fit. Standard errors are scaled by \eqn{|b|} when available.
#'
#' Inputs can be:
#' \itemize{
#'   \item A list returned by \code{\link{fit_bt_model}} (uses \code{$theta}),
#'   \item A tibble/data frame containing columns \code{ID} and \code{theta} (and optionally \code{se}), or
#'   \item A named numeric vector of theta values.
#' }
#'
#' @param current Current theta estimates.
#' @param reference Reference theta estimates defining the target scale.
#' @param ids Optional character vector of IDs used to estimate the linking
#'   transform (typically a core linking set). If \code{NULL}, uses the
#'   intersection of IDs present in both inputs.
#' @param method Linking method. Currently only \code{"mean_sd"} is supported.
#' @param min_n Minimum number of core IDs required to compute the transform.
#'   If fewer are available, the transform defaults to \code{a=0, b=1}.
#'
#' @return A list with:
#' \describe{
#'   \item{a}{Intercept of the affine transform.}
#'   \item{b}{Scale of the affine transform.}
#'   \item{method}{Linking method used.}
#'   \item{n_core}{Number of core IDs used.}
#'   \item{theta}{A tibble with columns \code{ID}, \code{theta}, \code{se} (if present),
#'     and the linked columns \code{theta_linked} and \code{se_linked}.}
#' }
#'
#' @examples
#' ref <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = 0.1)
#' cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 2, 3), se = 0.2)
#' lk <- bt_link_thetas(cur, ref)
#' lk$a
#' lk$b
#' lk$theta
#'
#' @import tibble
#' @export
bt_link_thetas <- function(current,
                           reference,
                           ids = NULL,
                           method = c("mean_sd"),
                           min_n = 3L) {
  method <- match.arg(method)

  extract_tbl <- function(x, arg_name) {
    # Only treat as a fit if it's a list that is NOT a data.frame/tibble.
    if (is.list(x) && !inherits(x, "data.frame") && !is.null(x$theta)) {
      x <- x$theta
    }

    if (is.numeric(x) && !is.null(names(x))) {
      return(tibble::tibble(ID = names(x), theta = as.double(unname(x)), se = NA_real_))
    }

    x_tbl <- tibble::as_tibble(x)
    if (!all(c("ID", "theta") %in% names(x_tbl))) {
      stop(
        "`", arg_name, "` must be a fit (with `$theta`), a tibble with columns ID/theta, or a named numeric vector.",
        call. = FALSE
      )
    }
    out <- tibble::tibble(
      ID = as.character(x_tbl$ID),
      theta = as.double(unname(x_tbl$theta)),
      se = if ("se" %in% names(x_tbl)) as.double(unname(x_tbl$se)) else NA_real_
    )
    out
  }

  if (!is.null(ids) && !is.character(ids)) {
    stop("`ids` must be a character vector when provided.", call. = FALSE)
  }
  min_n <- as.integer(min_n)
  if (length(min_n) != 1L || is.na(min_n) || min_n < 2L) {
    stop("`min_n` must be a single integer >= 2.", call. = FALSE)
  }

  cur <- extract_tbl(current, "current")
  ref <- extract_tbl(reference, "reference")

  # Determine core IDs used to estimate the transform
  if (is.null(ids)) {
    core_ids <- intersect(cur$ID, ref$ID)
  } else {
    core_ids <- intersect(unique(as.character(ids)), intersect(cur$ID, ref$ID))
  }

  cur_core <- cur[cur$ID %in% core_ids, , drop = FALSE]
  ref_core <- ref[ref$ID %in% core_ids, , drop = FALSE]
  # Align by ID
  joined <- dplyr::inner_join(cur_core, ref_core, by = "ID", suffix = c("_cur", "_ref"))
  n_core <- nrow(joined)

  a <- 0
  b <- 1
  if (method == "mean_sd" && n_core >= min_n) {
    mu_cur <- mean(joined$theta_cur, na.rm = TRUE)
    mu_ref <- mean(joined$theta_ref, na.rm = TRUE)
    sd_cur <- stats::sd(joined$theta_cur, na.rm = TRUE)
    sd_ref <- stats::sd(joined$theta_ref, na.rm = TRUE)

    if (is.finite(sd_cur) && sd_cur > 1e-8 && is.finite(sd_ref)) {
      b <- sd_ref / sd_cur
    } else {
      b <- 1
    }
    if (is.finite(mu_cur) && is.finite(mu_ref)) {
      a <- mu_ref - b * mu_cur
    } else {
      a <- 0
    }
  }

  out_theta <- tibble::tibble(
    ID = cur$ID,
    theta = cur$theta,
    se = cur$se,
    theta_linked = as.double(a + b * cur$theta),
    se_linked = ifelse(is.na(cur$se), NA_real_, abs(b) * cur$se)
  )

  list(
    a = as.double(a),
    b = as.double(b),
    method = method,
    n_core = as.integer(n_core),
    theta = out_theta
  )
}


.bt_should_apply_linking <- function(drift_tbl,
                                     cor_target = 0.98,
                                     p90_abs_shift_target = 0.15,
                                     max_abs_shift_target = 0.30) {
  # drift_tbl is one-row tibble from bt_drift_metrics with prefix core_
  if (is.null(drift_tbl) || nrow(drift_tbl) != 1L) {
    return(FALSE)
  }
  cor_val <- drift_tbl$core_theta_cor
  p90_val <- drift_tbl$core_p90_abs_shift
  max_val <- drift_tbl$core_max_abs_shift

  trig <- FALSE
  if (is.finite(cor_target) && !is.na(cor_val) && is.finite(cor_val)) {
    trig <- trig || (cor_val < cor_target)
  }
  if (is.finite(p90_abs_shift_target) && !is.na(p90_val) && is.finite(p90_val)) {
    trig <- trig || (p90_val > p90_abs_shift_target)
  }
  if (is.finite(max_abs_shift_target) && !is.na(max_val) && is.finite(max_val)) {
    trig <- trig || (max_val > max_abs_shift_target)
  }
  trig
}
