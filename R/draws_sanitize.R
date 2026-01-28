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
