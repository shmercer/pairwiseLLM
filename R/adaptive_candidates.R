# -------------------------------------------------------------------------
# Adaptive v2 candidate generation
# -------------------------------------------------------------------------

.validate_rank_mu <- function(rank_mu) {
  if (is.null(rank_mu) || length(rank_mu) == 0L) {
    rlang::abort("`rank_mu` must be a non-empty vector of item ids.")
  }
  if (length(rank_mu) < 2L) {
    rlang::abort("`rank_mu` must contain at least two item ids.")
  }
  if (anyNA(rank_mu)) {
    rlang::abort("`rank_mu` must not contain missing values.")
  }
  if (is.character(rank_mu) && any(rank_mu == "")) {
    rlang::abort("`rank_mu` must not contain empty ids.")
  }
  if (anyDuplicated(rank_mu)) {
    rlang::abort("`rank_mu` must be unique.")
  }
  rank_mu
}

.validate_candidate_cap <- function(C_max) {
  C_max <- as.integer(C_max)
  if (is.na(C_max) || C_max < 1L) {
    rlang::abort("`C_max` must be a positive integer.")
  }
  C_max
}

.validate_candidate_window <- function(W_used, n_ids) {
  W_used <- as.integer(W_used)
  if (is.na(W_used) || W_used < 1L) {
    rlang::abort("`W_used` must be a positive integer.")
  }
  if (!is.integer(n_ids) || length(n_ids) != 1L || is.na(n_ids) || n_ids < 2L) {
    rlang::abort("`n_ids` must be a single integer >= 2.")
  }
  if (W_used == 1L && n_ids > 2L) {
    rlang::abort("`W_used` must be >= 2 when N > 2.")
  }
  W_used
}

.validate_candidate_seed <- function(seed) {
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    rlang::abort("`seed` must be a non-missing scalar integer.")
  }
  seed
}

#' @keywords internal
#' @noRd
generate_candidate_pairs <- function(rank_mu, W_used, C_max = 20000L, seed) {
  rank_mu <- .validate_rank_mu(rank_mu)
  W_used <- .validate_candidate_window(W_used, n_ids = length(rank_mu))
  C_max <- .validate_candidate_cap(C_max)
  seed <- .validate_candidate_seed(seed)

  n_ids <- length(rank_mu)
  # Effective window size per spec: W_eff = 2 * floor(W_used / 2) + 1
  half_window <- floor(W_used / 2)

  if (n_ids == 2L && W_used == 1L) {
    i_id <- rank_mu[[1L]]
    j_id <- rank_mu[[2L]]
    i_out <- pmin(i_id, j_id)
    j_out <- pmax(i_id, j_id)
    return(tibble::tibble(i = i_out, j = j_out))
  }

  i_chunks <- vector("list", n_ids)
  j_chunks <- vector("list", n_ids)

  for (a in seq_len(n_ids)) {
    lower <- max(1L, a - half_window)
    upper <- min(n_ids, a + half_window)
    start <- max(a + 1L, lower)
    if (start > upper) next

    i_id <- rank_mu[[a]]
    j_ids <- rank_mu[start:upper]

    i_chunks[[a]] <- rep(i_id, length(j_ids))
    j_chunks[[a]] <- j_ids
  }

  i_vals <- unlist(i_chunks, use.names = FALSE)
  j_vals <- unlist(j_chunks, use.names = FALSE)

  if (length(i_vals) == 0L) {
    return(tibble::tibble(i = i_vals, j = j_vals))
  }

  i_out <- pmin(i_vals, j_vals)
  j_out <- pmax(i_vals, j_vals)

  unordered_key <- make_unordered_key(i_out, j_out)
  keep <- !duplicated(unordered_key)
  i_vals <- i_out[keep]
  j_vals <- j_out[keep]

  max_pairs <- as.integer(n_ids * (n_ids - 1L) / 2)
  cap <- min(C_max, max_pairs)
  if (length(i_vals) > cap) {
    withr::local_seed(seed)
    idx <- sample.int(length(i_vals), size = cap, replace = FALSE)
    i_vals <- i_vals[idx]
    j_vals <- j_vals[idx]
  }

  tibble::tibble(i = i_vals, j = j_vals)
}

#' @keywords internal
#' @noRd
generate_candidate_pairs_from_state <- function(state, W_used, C_max = 20000L, seed) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  trueskill_state <- state$trueskill_state
  if (is.null(trueskill_state)) {
    rlang::abort("`state$trueskill_state` must be set.")
  }
  validate_trueskill_state(trueskill_state)

  items <- trueskill_state$items
  rank_mu <- items$item_id[order(-items$mu, items$item_id)]

  generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = W_used,
    C_max = C_max,
    seed = seed
  )
}
