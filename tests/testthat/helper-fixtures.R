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

make_deterministic_judge <- function(always = c("i_wins", "j_wins", "invalid")) {
  always <- match.arg(always)
  force(always)

  function(A, B, state, ...) {
    if (identical(always, "invalid")) {
      return(list(is_valid = FALSE, invalid_reason = "invalid_fixture"))
    }
    if (identical(always, "i_wins")) {
      return(list(is_valid = TRUE, Y = 1L))
    }
    list(is_valid = TRUE, Y = 0L)
  }
}

snapshot_state_core <- function(state) {
  state[c(
    "item_ids",
    "item_index",
    "n_items",
    "items",
    "history_pairs",
    "item_log",
    "trueskill_state",
    "btl_fit",
    "stop_metrics",
    "config",
    "meta"
  )]
}

make_test_btl_fit <- function(ids, draws = NULL, diagnostics = NULL, model_variant = "btl_e_b") {
  ids <- as.character(ids)
  if (is.null(draws)) {
    draws <- matrix(rep(seq_along(ids), each = 10L), nrow = 10L)
  }
  draws <- as.matrix(draws)
  colnames(draws) <- ids

  diagnostics <- diagnostics %||% list(
    divergences = 0L,
    max_rhat = 1.0,
    min_ess_bulk = 1000
  )

  list(
    btl_posterior_draws = draws,
    theta_mean = stats::setNames(as.double(colMeans(draws)), ids),
    theta_sd = stats::setNames(as.double(apply(draws, 2, stats::sd)), ids),
    diagnostics = diagnostics,
    model_variant = model_variant,
    epsilon_mean = NA_real_,
    epsilon_p2.5 = NA_real_,
    epsilon_p5 = NA_real_,
    epsilon_p50 = NA_real_,
    epsilon_p95 = NA_real_,
    epsilon_p97.5 = NA_real_,
    beta_mean = NA_real_,
    beta_p2.5 = NA_real_,
    beta_p5 = NA_real_,
    beta_p50 = NA_real_,
    beta_p95 = NA_real_,
    beta_p97.5 = NA_real_
  )
}

make_deterministic_fit_fn <- function(ids, fit = NULL) {
  env <- new.env(parent = emptyenv())
  env$calls <- 0L
  ids <- as.character(ids)
  fit <- fit %||% make_test_btl_fit(ids)

  fit_fn <- function(state, config) {
    env$calls <- env$calls + 1L
    fit
  }

  list(
    fit_fn = fit_fn,
    get_calls = function() env$calls
  )
}
