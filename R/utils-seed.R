# Internal helpers for CRAN-safe RNG seeding

#' @keywords internal
.with_seed_restore <- function(seed, f, arg_name = "seed") {
  if (is.null(seed)) {
    return(f())
  }

  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    stop("`", arg_name, "` must be a single integer.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- NULL
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)
  f()
}
