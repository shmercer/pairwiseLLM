# -------------------------------------------------------------------------
# Adaptive TrueSkill utilities
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
compute_u0 <- function(i, j, trueskill_state) {
  p <- trueskill_win_probability(i, j, trueskill_state)
  p * (1 - p)
}

#' @keywords internal
#' @noRd
score_candidates_u0 <- function(candidates, trueskill_state) {
  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  candidates <- tibble::as_tibble(candidates)
  if (!all(c("i", "j") %in% names(candidates))) {
    rlang::abort("`candidates` must include `i` and `j` columns.")
  }

  n_rows <- nrow(candidates)
  if (n_rows == 0L) {
    candidates$u0 <- numeric(0)
    return(candidates)
  }

  i_vals <- candidates$i
  j_vals <- candidates$j
  u0 <- vapply(seq_len(n_rows), function(idx) {
    compute_u0(i_vals[[idx]], j_vals[[idx]], trueskill_state)
  }, numeric(1L))

  candidates$u0 <- as.double(u0)
  candidates
}
