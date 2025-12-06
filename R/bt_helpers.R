#' Summarize a Bradley–Terry model fit
#'
#' This helper takes the object returned by \code{\link{fit_bt_model}} and
#' returns a tibble with one row per object (e.g., writing sample), including:
#' \itemize{
#'   \item \code{ID}: object identifier
#'   \item \code{theta}: estimated ability parameter
#'   \item \code{se}: standard error of \code{theta}
#'   \item \code{rank}: rank order of \code{theta} (1 = highest by default)
#'   \item \code{engine}: modeling engine used ("sirt" or "BradleyTerry2")
#'   \item \code{reliability}: MLE reliability (for \pkg{sirt}) or \code{NA}
#' }
#'
#' @param fit A list returned by \code{\link{fit_bt_model}}.
#' @param decreasing Logical; should higher \code{theta} values receive
#'   lower rank numbers? If \code{TRUE} (default), the highest \code{theta}
#'   gets \code{rank = 1}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ID}{Object identifier.}
#'   \item{theta}{Estimated ability parameter.}
#'   \item{se}{Standard error of \code{theta}.}
#'   \item{rank}{Rank of \code{theta}; 1 = highest
#'   (if \code{decreasing = TRUE}).}
#'   \item{engine}{Modeling engine used ("sirt" or "BradleyTerry2").}
#'   \item{reliability}{MLE reliability (numeric scalar) repeated on each row.}
#' }
#'
#' @import tibble
#' @export
summarize_bt_fit <- function(fit, decreasing = TRUE) {
  # Basic sanity checks
  if (!is.list(fit) || is.null(fit$theta)) {
    stop(
      "`fit` must be a list returned by `fit_bt_model()` and contain a
      `$theta` tibble.",
      call. = FALSE
    )
  }

  # Coerce theta to tibble to ensure data-frame semantics
  theta <- tibble::as_tibble(fit$theta)

  required_cols <- c("ID", "theta", "se")
  if (!all(required_cols %in% names(theta))) {
    stop(
      "`fit$theta` must contain columns: ",
      paste(required_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Order and rank by theta
  ord <- order(theta$theta, decreasing = decreasing, na.last = "keep")
  rank_vec <- rep(NA_integer_, nrow(theta))

  finite_idx <- which(is.finite(theta$theta))
  if (length(finite_idx) > 0L) {
    ord_finite <- ord[ord %in% finite_idx]
    rank_vec[ord_finite] <- seq_along(ord_finite)
  }

  engine <- if (!is.null(fit$engine)) fit$engine else NA_character_
  reliability <- if (!is.null(fit$reliability)) fit$reliability else NA_real_

  # Modify the existing tibble rather than calling tibble()
  theta$rank <- rank_vec
  theta$engine <- engine
  theta$reliability <- reliability

  theta
}
