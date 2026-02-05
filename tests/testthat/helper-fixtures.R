make_test_items <- function(n) {
  n <- as.integer(n)
  tibble::tibble(item_id = seq_len(n))
}

make_test_trueskill_state <- function(items, mu = NULL, sigma = NULL) {
  items <- tibble::as_tibble(items)
  if (is.null(mu)) {
    mu <- rep(25, nrow(items))
  }
  if (is.null(sigma)) {
    sigma <- rep(25 / 3, nrow(items))
  }
  items$mu <- mu
  items$sigma <- sigma
  pairwiseLLM:::new_trueskill_state(items)
}

make_history <- function(pairs) {
  pairs <- tibble::as_tibble(pairs)
  if (nrow(pairs) == 0L && ncol(pairs) == 0L) {
    return(tibble::tibble(A_id = character(), B_id = character()))
  }
  if (!all(c("A_id", "B_id") %in% names(pairs))) {
    if (all(c("A", "B") %in% names(pairs))) {
      pairs <- dplyr::rename(pairs, A_id = .data$A, B_id = .data$B)
    } else if (all(c("i", "j") %in% names(pairs))) {
      pairs <- dplyr::rename(pairs, A_id = .data$i, B_id = .data$j)
    } else {
      rlang::abort("`pairs` must include A_id/B_id, A/B, or i/j.")
    }
  }
  pairs <- dplyr::mutate(pairs, A_id = as.character(.data$A_id), B_id = as.character(.data$B_id))
  pairs[, c("A_id", "B_id"), drop = FALSE]
}

make_test_state <- function(items, trueskill_state, history = tibble::tibble()) {
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$trueskill_state <- trueskill_state
  state$history_pairs <- make_history(history)
  state
}
