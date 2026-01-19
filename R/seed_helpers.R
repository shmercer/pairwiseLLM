# -------------------------------------------------------------------------
# Internal helper: run code with a temporary RNG seed
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.pairwiseLLM_with_seed <- function(seed, fn) {
  if (is.null(seed)) {
    return(fn())
  }

  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    rlang::abort("`seed` must be a single, non-missing numeric value.")
  }

  withr::with_seed(seed, fn())
}
