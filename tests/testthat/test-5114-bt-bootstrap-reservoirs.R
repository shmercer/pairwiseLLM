test_that("reservoir bootstrap fixes initialization and resimulates only synthetic legal evidence", {
  ids <- letters[1:6]
  pairs <- t(combn(ids, 2))
  edges <- data.frame(A_id = pairs[, 1], B_id = pairs[, 2], Y = rep(0L, nrow(pairs)))
  prior <- make_warm_start_prior(stats::setNames(seq(-1, 1, length.out = 6), ids))
  for (strategy in c("trueskill_p50", "trueskill_pollitt", "hybrid", "random")) {
    state <- bootstrap_state(strategy, replay_reservoir = make_adaptive_replay_reservoir(edges, ids),
      warm_start_prior = prior, warm_start_mode = "both")
    out <- bootstrap_adaptive(state, n_rep = 2L, budget = 9L,
      btl_config = list(refit_pairs_target = 100L), keep = "full")
    expect_identical(out$n_success, 2L)
    expect_true(all(out$replicates$n_repeated_comparisons == 0L))
    for (art in out$artifacts) {
      expect_identical(art$state$warm_start_pairs, state$warm_start_pairs)
      expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(art$state))
      expect_true(all(paste(art$comparisons$object1, art$comparisons$object2) %in% paste(edges$A_id, edges$B_id)))
      expect_identical(art$state$predictive_prior, prior)
    }
    changed <- edges
    changed$Y <- 1L
    other <- bootstrap_state(strategy, replay_reservoir = make_adaptive_replay_reservoir(changed, ids),
      warm_start_prior = prior, warm_start_mode = "both")
    repeated <- bootstrap_adaptive(other, n_rep = 2L, budget = 9L,
      btl_config = list(refit_pairs_target = 100L), keep = "full")
    expect_identical(out$theta, repeated$theta)
    expect_identical(out$replicates, repeated$replicates)
    expect_identical(out$draws, repeated$draws)
    expect_true(any(unlist(lapply(out$artifacts, function(x) x$comparisons$result)) == 1L))
  }
})

test_that("exhaustion is deterministic and cannot substitute a different schedule", {
  ids <- letters[1:3]
  edges <- data.frame(A_id = c("a", "b"), B_id = c("b", "c"), Y = c(0L, 1L))
  s <- bootstrap_state("trueskill_p50", ids, replay_reservoir = make_adaptive_replay_reservoir(edges, ids))
  run <- function() {
    bootstrap_error(bootstrap_adaptive(s, n_rep = 2L, budget = 3L,
      btl_config = list(refit_pairs_target = 100L), keep = "full"))
  }
  e <- run()
  expect_identical(e$result$n_failed, 2L)
  expect_true(all(e$result$replicates$failure_phase == "schedule"))
  expect_true(all(e$result$replicates$n_comparisons == 2L))
  expect_true(all(e$result$replicates$failure_reason == "candidate_starvation"))
  expect_identical(e$result$replicates, run()$result$replicates)
  for (a in e$result$artifacts) {
    expect_identical(a$state$controller$pairing_strategy, "trueskill_p50")
    expect_identical(a$state$step_log$starvation_reason[3], "reservoir_exhausted")
  }
})

test_that("initialization seed metadata survives persistence without changing legacy sessions", {
  ids <- letters[1:3]
  edges <- data.frame(A_id = c("a", "a", "b"), B_id = c("b", "c", "c"), Y = c(0L, 1L, 0L))
  reservoir <- make_adaptive_replay_reservoir(edges, ids)
  old <- bootstrap_state("random", ids, replay_reservoir = reservoir,
    now_fn = pairwiseLLM:::.bt_bootstrap_clock)
  expect_null(old$meta$initialization_seed)
  expected <- pairwiseLLM:::.adaptive_reservoir_bootstrap(old)
  new <- old
  new$meta$initialization_seed <- old$meta$seed
  new$meta$seed <- 2304L
  expect_identical(pairwiseLLM:::.adaptive_reservoir_bootstrap(new), expected)
  judge <- make_adaptive_judge_replay(reservoir)
  new <- adaptive_rank_run_live(new, judge, n_steps = 1L, progress = "none")
  dir <- withr::local_tempdir()
  save_adaptive_session(new, dir, overwrite = TRUE)
  restored <- load_adaptive_session(dir)
  expect_identical(restored$meta$initialization_seed, old$meta$seed)
  a <- adaptive_rank_run_live(new, judge, n_steps = 2L, progress = "none")
  b <- adaptive_rank_run_live(restored, judge, n_steps = 2L, progress = "none")
  expect_identical(a$step_log, b$step_log)
  expect_identical(a$history_pairs[, 1:2], b$history_pairs[, 1:2])
  expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(old))
})
