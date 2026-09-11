task07_fixture <- function(pattern = "good", n = 8L) {
  ids <- letters[seq_len(n)]
  truth <- stats::setNames(seq(-1.4, 1.4, length.out = n), ids)
  scores <- if (n == 8L) {
    switch(pattern,
      good = truth,
      noisy = truth[c(1, 3, 2, 5, 4, 6, 8, 7)],
      misplaced = c(truth[1:7], -2),
      local = truth[c(1, 2, 3, 5, 4, 6, 7, 8)],
      reversed = rev(truth)
    )
  } else {
    rev(truth)
  }
  outcomes <- expand.grid(A_id = ids, B_id = ids, stringsAsFactors = FALSE)
  outcomes <- outcomes[outcomes$A_id != outcomes$B_id, ]
  outcomes$Y <- as.integer(truth[outcomes$A_id] > truth[outcomes$B_id])
  list(ids = ids, truth = truth, outcomes = tibble::as_tibble(outcomes),
    prior = pairwiseLLM::make_warm_start_prior(stats::setNames(as.numeric(scores), ids),
      prior_sd = 0.5))
}

task07_start <- function(fixture, mode = "both", strategy = "hybrid", config = list()) {
  pairwiseLLM::adaptive_rank_start(fixture$ids, seed = 71L, warm_start_mode = mode,
    warm_start_prior = if (mode == "cold") NULL else fixture$prior,
    now_fn = function() as.POSIXct("2026-09-11", tz = "UTC"),
    adaptive_config = c(list(pairing_strategy = strategy, dup_max_obs_relaxed = 2L), config))
}

task07_run <- function(fixture, mode = "both", strategy = "hybrid", n_steps = 30L) {
  initial <- task07_start(fixture, mode, strategy)
  trace <- new.env(parent = emptyenv())
  trace$before <- list()
  replay <- pairwiseLLM::make_adaptive_judge_replay(fixture$outcomes, fixture$ids)
  judge <- function(A, B, state, ...) {
    trace$before[[length(trace$before) + 1L]] <- list(
      ts = state$trueskill_state,
      counts = pairwiseLLM:::.adaptive_pair_counts(state$history_pairs, state$item_ids)
    )
    replay(A, B, state, ...)
  }
  state <- pairwiseLLM::adaptive_rank_run_live(initial, judge, n_steps = n_steps,
    btl_config = list(refit_pairs_target = 5000L), progress = "none")
  list(initial = initial, state = state, before = trace$before)
}
