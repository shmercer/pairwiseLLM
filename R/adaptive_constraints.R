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

.adaptive_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }
  withr::with_seed(seed, eval.parent(substitute(expr)))
}
