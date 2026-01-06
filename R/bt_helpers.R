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

  # Ensure the output `theta` column is numeric as well (useful for downstream
  # metrics and plotting).
  theta$theta <- theta_num

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
# -------------------------------------------------------------------------
# Internal helpers (shared across runners)
# -------------------------------------------------------------------------

#' Extract theta from the last available running fit
#'
#' Internal helper used by runners to guarantee that a compact theta table
#' exists even when final refitting is disabled or unavailable.
#'
#' This function attempts to extract a standardized theta table
#' (ID, theta, se, rank) from the last running fit and align it to `id_vec`.
#'
#' @param final_fit A fit object produced during a run (or `NULL`).
#' @param id_vec Character vector of item IDs to include/order.
#'
#' @return A list with elements:
#' - `theta`: tibble with columns `ID`, `theta`, `se`, `rank`, or `NULL`.
#' - `engine`: character string naming the running engine, or `NA_character_`.
#'
#' @keywords internal
.theta_from_last_running_fit <- function(final_fit, id_vec) {
  if (is.null(final_fit) || is.null(final_fit$theta)) {
    return(list(theta = NULL, engine = NA_character_))
  }

  th <- tibble::as_tibble(final_fit$theta)
  if (!all(c("ID", "theta") %in% names(th))) {
    return(list(theta = NULL, engine = NA_character_))
  }

  # Align coverage and ordering on id_vec (and keep IDs as character)
  id_vec <- as.character(id_vec %||% character(0))
  th$ID <- as.character(th$ID)
  th <- dplyr::right_join(th, tibble::tibble(ID = id_vec), by = "ID")

  theta_num <- suppressWarnings(as.double(th$theta))

  # Determine running engine label.
  engine_running <- NA_character_
  if (!is.null(final_fit$engine_running) && is.character(final_fit$engine_running) && length(final_fit$engine_running) == 1L) {
    engine_running <- as.character(final_fit$engine_running)
  } else if (!is.null(final_fit$engine_used) && is.character(final_fit$engine_used) && length(final_fit$engine_used) == 1L) {
    engine_running <- as.character(final_fit$engine_used)
  } else if (!is.null(final_fit$engine_requested) && is.character(final_fit$engine_requested) && length(final_fit$engine_requested) == 1L) {
    engine_running <- as.character(final_fit$engine_requested)
  } else if (!is.null(final_fit$engine) && is.character(final_fit$engine) && length(final_fit$engine) == 1L) {
    engine_running <- as.character(final_fit$engine)
  }
  if (is.na(engine_running) || !nzchar(engine_running)) engine_running <- "rank_centrality"

  # SE handling: for BT-family engines, retain SE if available; for RC (and
  # unknown), expose NA. (BT-family includes bt_firth, bt_mle, etc.)
  se_out <- rep(NA_real_, nrow(th))
  is_bt_engine <- isTRUE(grepl("^bt", engine_running))
  if (is_bt_engine) {
    if ("se_bt" %in% names(th)) {
      se_out <- suppressWarnings(as.double(th$se_bt))
    } else if ("se_bt_firth" %in% names(th)) {
      se_out <- suppressWarnings(as.double(th$se_bt_firth))
    } else if ("se" %in% names(th)) {
      se_out <- suppressWarnings(as.double(th$se))
    }
  }

  # Rank: prefer an existing rank column if present, otherwise compute.
  rank_out <- NULL
  if ("rank_running" %in% names(th)) {
    rank_out <- suppressWarnings(as.integer(th$rank_running))
  } else if ("rank" %in% names(th)) {
    rank_out <- suppressWarnings(as.integer(th$rank))
  }
  if (is.null(rank_out)) {
    rank_out <- .rank_desc(theta_num)
  }

  out_theta <- tibble::tibble(
    ID = as.character(th$ID),
    theta = as.double(theta_num),
    se = as.double(se_out),
    rank = as.integer(rank_out)
  )

  list(theta = out_theta, engine = engine_running)
}
