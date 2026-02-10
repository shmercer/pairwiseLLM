# -------------------------------------------------------------------------
# Core/batch sizing helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
detect_physical_cores <- function() {
  cores <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )
  if (is.na(cores) || cores < 1L) {
    cores <- tryCatch(
      parallel::detectCores(logical = TRUE),
      error = function(e) NA_integer_
    )
  }
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  as.integer(cores)
}

#' @keywords internal
#' @noRd
compute_core_budget <- function(core_fraction = 0.8, max_cores = NULL, min_cores = 1) {
  if (!is.numeric(core_fraction) || length(core_fraction) != 1L || core_fraction < 0) {
    rlang::abort("`core_fraction` must be a non-negative number.")
  }
  if (!is.numeric(min_cores) || length(min_cores) != 1L || min_cores < 1) {
    rlang::abort("`min_cores` must be a positive number.")
  }
  cores <- detect_physical_cores()
  budget <- floor(cores * core_fraction)
  if (!is.null(max_cores)) {
    if (!is.numeric(max_cores) || length(max_cores) != 1L || max_cores < 1) {
      rlang::abort("`max_cores` must be a positive number.")
    }
    budget <- min(budget, max_cores)
  }
  if (budget < min_cores) budget <- min_cores
  as.integer(budget)
}

#' @keywords internal
#' @noRd
compute_batch_sizes <- function(N, overrides = list()) {
  if (!is.numeric(N) || length(N) != 1L || N < 1) {
    rlang::abort("`N` must be a positive number.")
  }
  N <- as.integer(N)

  BATCH1 <- min(5000L, max(500L, N))
  BATCH2 <- min(2000L, max(200L, floor(N / 2)))
  BATCH3 <- min(500L, max(100L, floor(BATCH2 / 4)))
  CW <- floor(N / 2)

  sizes <- list(
    BATCH1 = as.integer(BATCH1),
    BATCH2 = as.integer(BATCH2),
    BATCH3 = as.integer(BATCH3),
    CW = as.integer(CW)
  )

  if (length(overrides) > 0L) {
    for (nm in names(overrides)) {
      if (nm %in% names(sizes)) {
        sizes[[nm]] <- as.integer(overrides[[nm]])
      }
    }
  }

  sizes
}
