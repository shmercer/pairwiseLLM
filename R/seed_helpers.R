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

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL

  on.exit(
    {
      if (!is.null(old_seed)) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (!had_seed && exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)
  fn()
}
