bootstrap_data <- function(wins = 6L, total = 8L) {
  data.frame(object1 = rep("a", total), object2 = rep("b", total),
    result = c(rep(1, wins), rep(0, total - wins)))
}

bootstrap_fit <- function(wins = 6L, total = 8L) {
  fit_bt_model(bootstrap_data(wins, total), engine = "alpha", alpha = 1, verbose = FALSE)
}

bootstrap_error <- function(expr) tryCatch(expr, pairwiseLLM_bt_bootstrap_error = identity)

bootstrap_run <- function(..., keep = "full") {
  bootstrap_bt_model(bootstrap_fit(), mode = "fixed", n_rep = 8L, seed = 304L, keep = keep, ...)
}

bootstrap_state <- function(strategy = "trueskill_p50", ids = letters[1:6], ...) {
  adaptive_rank_start(ids, seed = 17L, adaptive_config = list(pairing_strategy = strategy), ...)
}

bootstrap_adaptive <- function(state = bootstrap_state(), n_rep = 3L, budget = 10L, seed = 304L, ...) {
  theta <- stats::setNames(seq(-1, 1, length.out = state$n_items), state$item_ids)
  bootstrap_bt_model(theta, "adaptive", n_rep, seed, initial_state = state, budget = budget,
    estimator = "alpha", estimator_args = list(alpha = 0.5), ...)
}

# Independent closed-form two-item alpha=1 fit for numerical reference tests.
bootstrap_two_item <- function(bt_data, item_ids) {
  wins <- sum((bt_data$object1 == item_ids[1L] & bt_data$result == 1) |
    (bt_data$object2 == item_ids[1L] & bt_data$result == 0))
  delta <- log((wins + 1) / (nrow(bt_data) - wins + 1))
  stats::setNames(c(delta, -delta) / 2, item_ids)
}
environment(bootstrap_two_item) <- baseenv()
