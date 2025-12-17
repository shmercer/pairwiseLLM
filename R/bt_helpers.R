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
#' @param verbose Logical. If \code{TRUE} (default), emit warnings when coercing.
#'   If \code{FALSE}, suppress coercion warnings during ranking.
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
#' @examples
#' # Example using built-in comparison data
#' data("example_writing_pairs")
#' bt <- build_bt_data(example_writing_pairs)
#'
#' fit1 <- fit_bt_model(bt, engine = "sirt")
#' fit2 <- fit_bt_model(bt, engine = "BradleyTerry2")
#'
#' summarize_bt_fit(fit1)
#' summarize_bt_fit(fit2)
#'
#' @import tibble
#' @export
summarize_bt_fit <- function(fit, decreasing = TRUE, verbose = TRUE) {
  if (!is.list(fit) || is.null(fit$theta)) {
    stop(
      "`fit` must be a list returned by `fit_bt_model()` and contain a `$theta` tibble.",
      call. = FALSE
    )
  }

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

  # Make a *plain* numeric vector for ranking:
  # - drop names/attributes
  # - ensure atomic double
  theta_num <- theta$theta
  theta_num <- unname(theta_num) # removes names attribute (important for fit2)
  theta_num <- as.double(theta_num) # ensures plain numeric

  # If something ever sneaks in as character, coerce (quietly if verbose=FALSE)
  if (!is.numeric(theta_num)) {
    theta_num <- if (isTRUE(verbose)) as.numeric(theta_num) else suppressWarnings(as.numeric(theta_num))
    theta_num <- as.double(unname(theta_num))
  }

  # Order and rank (quietly if verbose = FALSE)
  ord <- if (isTRUE(verbose)) {
    order(theta_num, decreasing = decreasing, na.last = "keep")
  } else {
    suppressWarnings(order(theta_num, decreasing = decreasing, na.last = "keep"))
  }

  rank_vec <- rep(NA_integer_, length(theta_num))

  finite_idx <- which(is.finite(theta_num))
  if (length(finite_idx) > 0L) {
    ord_finite <- ord[ord %in% finite_idx]
    rank_vec[ord_finite] <- seq_along(ord_finite)
  }

  engine <- if (!is.null(fit$engine)) fit$engine else NA_character_
  reliability <- if (!is.null(fit$reliability)) fit$reliability else NA_real_

  theta$rank <- rank_vec
  theta$engine <- engine
  theta$reliability <- reliability

  theta
}
