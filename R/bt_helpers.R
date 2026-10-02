#' Calculate scale-separation reliability
#'
#' Calculate conventional SSR as `1 - mean(se^2) / stats::var(theta)` and
#' expose its variance components. The observed variance uses the sample
#' denominator `n - 1`. Estimates and SEs must use the same scale and item order.
#'
#' Negative SSR and negative estimated true-score variance are retained, not
#' clipped. They indicate that mean squared uncertainty exceeds observed score
#' variance. SSR depends on the estimator, its SE convention, and the estimated
#' score variance; it is not an estimator-free measure of recovery or accuracy.
#'
#' @param theta Real numeric vector of finite item estimates, with at least two
#'   entries and positive sample variance.
#' @param se Real numeric vector of finite, nonnegative standard errors, in the
#'   same order and of the same length as `theta`. Names are not used to align
#'   the vectors.
#' @return A list with `observed_variance`, `mean_squared_se`,
#'   `true_score_variance` (observed variance minus mean squared SE), `ssr`,
#'   `n_items`, `n_finite`, `valid`, and `status`. Successful calculations have
#'   `valid = TRUE` and status `"ok"` or `"negative_true_score_variance"`.
#'   Invalid inputs or nonfinite calculated components raise an error; no items
#'   are dropped and no coefficient is returned for an undefined calculation.
#' @examples
#' scale_separation_reliability(c(-1, 0, 1), c(0.2, 0.3, 0.4))
#' @seealso [fit_bt_model()]
#' @family frequentist models
#' @export
scale_separation_reliability <- function(theta, se) {
  .bt_validate_estimates(theta, se)
  if (length(theta) < 2L) {
    .bt_abort("SSR requires at least two items.")
  }
  observed <- stats::var(theta)
  error <- mean(se^2)
  if (!is.finite(observed) || observed <= 0 || !is.finite(error)) {
    .bt_abort("SSR requires positive finite score variance and finite mean squared SE.")
  }
  true <- observed - error
  ssr <- 1 - error / observed
  if (!is.finite(true) || !is.finite(ssr)) {
    .bt_abort("SSR calculation produced nonfinite variance components or reliability.")
  }
  list(
    observed_variance = observed, mean_squared_se = error,
    true_score_variance = true, ssr = ssr,
    n_items = length(theta), n_finite = length(theta), valid = TRUE,
    status = if (true < 0) "negative_true_score_variance" else "ok"
  )
}

#' Summarize a Bradley–Terry model fit
#'
#' This helper takes the object returned by \code{\link{fit_bt_model}} and
#' returns a tibble with one row per object (e.g., writing sample), including:
#' \itemize{
#'   \item \code{ID}: object identifier
#'   \item \code{theta}: estimated ability parameter
#'   \item \code{se}: standard error of \code{theta}
#'   \item \code{rank}: rank order of \code{theta} (1 = highest by default)
#'   \item \code{engine}: modeling engine used ("sirt", "BradleyTerry2", "brglm2", or "alpha")
#'   \item \code{reliability}: raw sirt reliability, calculated Firth/alpha SSR, or \code{NA}
#' }
#'
#' Standard errors describe model uncertainty; small differences in estimates
#' should not be interpreted without considering that uncertainty. Reliability
#' is a study-level summary repeated on each row, not an item-specific score.
#' The returned rows retain input order; sort explicitly by `rank` or `theta`
#' when preparing a ranked report.
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
#'   \item{engine}{Modeling engine used ("sirt", "BradleyTerry2", "brglm2", or "alpha").}
#'   \item{reliability}{Reliability (numeric scalar, or `NA`) repeated on each row.}
#' }
#'
#' @examples
#' # Example using built-in comparison data
#' data("example_writing_pairs")
#' bt <- build_bt_data(example_writing_pairs)
#'
#' if (requireNamespace("sirt", quietly = TRUE)) {
#'   fit1 <- fit_bt_model(bt, engine = "sirt", verbose = FALSE)
#'   summarize_bt_fit(fit1)
#' }
#' if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
#'   fit2 <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
#'   summarize_bt_fit(fit2)
#' }
#'
#' @import tibble
#' @seealso [build_bt_data()], [fit_bt_model()]
#' @family frequentist models
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

  # If something ever sneaks in as character, coerce (quietly if verbose=FALSE)
  if (!is.numeric(theta_num)) {
    theta_num <- if (isTRUE(verbose)) as.numeric(theta_num) else suppressWarnings(as.numeric(theta_num))
  }
  theta_num <- as.double(unname(theta_num)) # ensures plain numeric

  # Order and rank (quietly if verbose = FALSE)
  ord <- if (isTRUE(verbose)) {
    order(theta_num, decreasing = decreasing, na.last = NA)
  } else {
    suppressWarnings(order(theta_num, decreasing = decreasing, na.last = NA))
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
