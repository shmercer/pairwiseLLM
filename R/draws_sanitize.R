# -------------------------------------------------------------------------
# Draw sanitation helpers (defensive handling for non-finite draws)
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.pairwiseLLM_sanitize_draws_matrix <- function(draws, name = "draws") {
  if (!is.matrix(draws) || !is.numeric(draws)) {
    rlang::abort(paste0("`", name, "` must be a numeric matrix."))
  }
  if (ncol(draws) < 1L || nrow(draws) < 1L) {
    rlang::abort(paste0("`", name, "` must have at least one row and column."))
  }

  bad <- !is.finite(draws)
  if (!any(bad)) {
    return(draws)
  }

  ids <- colnames(draws)
  if (is.null(ids)) {
    ids <- as.character(seq_len(ncol(draws)))
    colnames(draws) <- ids
  }

  col_means <- rep.int(0, ncol(draws))
  for (j in seq_len(ncol(draws))) {
    vals <- draws[, j, drop = TRUE]
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0L) {
      col_means[[j]] <- mean(vals)
    }
  }

  draws_clean <- draws
  for (j in seq_len(ncol(draws_clean))) {
    bad_j <- bad[, j]
    if (any(bad_j)) {
      draws_clean[bad_j, j] <- col_means[[j]]
    }
  }

  n_bad <- sum(bad)
  n_cols <- sum(colSums(bad) > 0)
  rlang::warn(paste0(
    "Non-finite values detected in `", name, "`; replaced ",
    n_bad, " value(s) across ", n_cols, " column(s) with column means."
  ))

  draws_clean
}

#' @keywords internal
#' @noRd
.pairwiseLLM_col_sds <- function(x, center = NULL) {
  if (!is.matrix(x) || !is.numeric(x)) {
    rlang::abort("`x` must be a numeric matrix.")
  }
  n <- nrow(x)
  if (n < 2L || ncol(x) < 1L) {
    return(rep_len(NA_real_, ncol(x)))
  }
  center <- center %||% colMeans(x)
  center <- as.double(center)
  if (length(center) != ncol(x)) {
    rlang::abort("`center` must have one value per column in `x`.")
  }
  ss <- colSums(x * x) - as.double(n) * center^2
  sqrt(pmax(ss / as.double(n - 1L), 0))
}

#' @keywords internal
#' @noRd
.pairwiseLLM_col_quantiles <- function(x, probs, names = FALSE) {
  if (!is.matrix(x) || !is.numeric(x)) {
    rlang::abort("`x` must be a numeric matrix.")
  }
  probs <- as.double(probs)
  vapply(
    seq_len(ncol(x)),
    function(idx) stats::quantile(x[, idx], probs = probs, names = names),
    numeric(length(probs))
  )
}
