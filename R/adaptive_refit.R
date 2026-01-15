# -------------------------------------------------------------------------
# Refit cadence helpers (observed comparisons only)
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
refit_every_batches <- function(CW, batch_size) {
  CW <- as.integer(CW)
  batch_size <- as.integer(batch_size)
  if (is.na(CW) || CW < 1L) {
    rlang::abort("`CW` must be a positive integer.")
  }
  if (is.na(batch_size) || batch_size < 1L) {
    rlang::abort("`batch_size` must be a positive integer.")
  }

  max(1L, ceiling(CW / batch_size))
}

#' @keywords internal
#' @noRd
should_refit <- function(comparisons_observed, last_refit_at, batch_size, CW) {
  comparisons_observed <- as.integer(comparisons_observed)
  last_refit_at <- as.integer(last_refit_at)
  batch_size <- as.integer(batch_size)
  CW <- as.integer(CW)

  if (is.na(comparisons_observed) || comparisons_observed < 0L) {
    rlang::abort("`comparisons_observed` must be a non-negative integer.")
  }
  if (is.na(last_refit_at) || last_refit_at < 0L) {
    rlang::abort("`last_refit_at` must be a non-negative integer.")
  }
  if (is.na(batch_size) || batch_size < 1L) {
    rlang::abort("`batch_size` must be a positive integer.")
  }
  if (is.na(CW) || CW < 1L) {
    rlang::abort("`CW` must be a positive integer.")
  }

  (comparisons_observed - last_refit_at) >= CW
}
