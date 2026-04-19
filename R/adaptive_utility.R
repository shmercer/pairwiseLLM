# -------------------------------------------------------------------------
# Adaptive TrueSkill utilities
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.trueskill_win_probability_vec <- function(i, j, trueskill_state) {
  validate_trueskill_state(trueskill_state)

  i_id <- as.character(i)
  j_id <- as.character(j)
  if (length(i_id) != length(j_id)) {
    rlang::abort("`i` and `j` must have the same length.")
  }
  if (length(i_id) < 1L) {
    return(numeric())
  }
  if (any(i_id == j_id)) {
    rlang::abort("`i` and `j` must be distinct item ids.")
  }

  items <- trueskill_state$items
  item_ids <- as.character(items$item_id)

  i_pos <- match(i_id, item_ids)
  j_pos <- match(j_id, item_ids)
  if (anyNA(i_pos) || anyNA(j_pos)) {
    rlang::abort("`i` and `j` must be present in `trueskill_state$items`.")
  }

  mu_i <- as.double(items$mu[i_pos])
  mu_j <- as.double(items$mu[j_pos])
  sigma_i <- as.double(items$sigma[i_pos])
  sigma_j <- as.double(items$sigma[j_pos])
  s2 <- sigma_i^2 + sigma_j^2 + 2 * trueskill_state$beta^2

  stats::pnorm((mu_i - mu_j) / sqrt(s2))
}

#' @keywords internal
#' @noRd
compute_u0 <- function(i, j, trueskill_state) {
  p <- .trueskill_win_probability_vec(i, j, trueskill_state)
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
    candidates$p <- numeric(0)
    return(candidates)
  }

  p <- .trueskill_win_probability_vec(candidates$i, candidates$j, trueskill_state)
  u0 <- p * (1 - p)

  candidates$p <- as.double(p)
  candidates$u0 <- as.double(u0)
  candidates
}
