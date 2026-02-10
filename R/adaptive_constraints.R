# -------------------------------------------------------------------------
# Adaptive constraints and key helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
make_unordered_key <- function(id1, id2) {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  paste(pmin(id1, id2), pmax(id1, id2), sep = ":")
}

#' @keywords internal
#' @noRd
make_ordered_key <- function(A_id, B_id) {
  paste(as.character(A_id), as.character(B_id), sep = ":")
}

.adaptive_stage_seed <- function(seed_base, step_id, stage_id, offset = 0L) {
  seed_base <- as.double(seed_base %||% 1L)
  step_id <- as.double(step_id %||% 1L)
  stage_id <- as.double(stage_id %||% 1L)
  offset <- as.double(offset %||% 0L)
  mod <- .Machine$integer.max
  seed <- (seed_base * 1000003 + step_id * 10007 + stage_id * 101 + offset) %% mod
  seed <- as.integer(max(1, floor(seed)))
  seed
}

.adaptive_validate_seed <- function(seed) {
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    rlang::abort("`seed` must be a non-missing scalar integer.")
  }
  seed
}

.adaptive_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }
  withr::with_seed(seed, eval.parent(substitute(expr)))
}
