reservoir_fixture <- function() {
  ids <- letters[1:6]
  outcomes <- tibble::tibble(A_id = c("b", "c", "d", "e", "f", "a", "d", "b", "f"),
    B_id = c("a", "b", "c", "d", "e", "f", "a", "e", "c"),
    Y = c(1L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L))
  list(ids = ids, outcomes = outcomes,
    reservoir = make_adaptive_replay_reservoir(outcomes, ids),
    prior = make_warm_start_prior(stats::setNames(c(-1, -0.6, -0.2, 0.2, 0.6, 1), ids)))
}

reservoir_start <- function(f, strategy = "random", mode = "cold", seed = 87L) {
  # This stateless clock must not serialize the development test environment.
  clock <- function() as.POSIXct("2026-09-18", tz = "UTC")
  environment(clock) <- baseenv()
  adaptive_rank_start(f$ids, seed = seed, replay_reservoir = f$reservoir,
    warm_start_mode = mode, warm_start_prior = if (mode == "cold") NULL else f$prior,
    now_fn = clock,
    adaptive_config = list(pairing_strategy = strategy))
}

reservoir_run <- function(state, f, n_steps = 20L) {
  adaptive_rank_run_live(state, make_adaptive_judge_replay(f$reservoir), n_steps = n_steps,
    btl_config = list(refit_pairs_target = 5000L), progress = "none")
}

expect_reservoir_evidence <- function(state, f) {
  committed <- state$step_log[!is.na(state$step_log$pair_id), ]
  keys <- pairwiseLLM:::.adaptive_reservoir_key(committed$A_id, committed$B_id)
  rows <- match(keys, pairwiseLLM:::.adaptive_reservoir_key(f$outcomes$A_id, f$outcomes$B_id))
  expect_false(anyNA(rows))
  expect_identical(anyDuplicated(keys), 0L)
  expect_identical(committed$A_id, f$outcomes$A_id[rows])
  expect_identical(committed$B_id, f$outcomes$B_id[rows])
  expect_identical(committed$Y, f$outcomes$Y[rows])
  expect_identical(state$history_pairs$A_id, committed$A_id)
  expect_identical(state$history_pairs$B_id, committed$B_id)
  expect_true(all(committed$judge_backend == "replay"))
  expect_lte(nrow(committed), nrow(f$outcomes))
}
