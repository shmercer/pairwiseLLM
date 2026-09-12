test_that("matched BTL-only and both replay isolate initial TrueSkill state", {
  withr::local_seed(902L)
  rng <- .Random.seed
  f <- task07_fixture("misplaced")
  captured <- new.env(parent = emptyenv())
  captured$fits <- list()
  testthat::local_mocked_bindings(fit_bayes_btl_mcmc = function(results, ids,
      model_variant, cmdstan, warm_start_prior) {
    captured$fits[[length(captured$fits) + 1L]] <- list(results = results,
      ids = ids, prior = warm_start_prior, model_variant = model_variant)
    list(fit = make_test_btl_fit(ids, draws = outer(seq_len(10) * 0.005, seq_along(ids), "+")))
  }, .package = "pairwiseLLM")
  for (strategy in c("hybrid", "trueskill_p50", "trueskill_pollitt")) {
    runs <- lapply(c("btl_only", "both"), function(mode) {
      initial <- task07_start(f, mode, strategy)
      judge <- make_adaptive_judge_replay(f$outcomes, f$ids)
      state <- adaptive_rank_run_live(initial, judge, n_steps = 7L,
        btl_config = list(refit_pairs_target = 7L), progress = "none")
      replayed <- initial$trueskill_state
      for (k in seq_len(7L)) {
        row <- state$step_log[k, ]
        winner <- if (row$Y == 1L) row$A_id else row$B_id
        loser <- if (row$Y == 1L) row$B_id else row$A_id
        replayed <- pairwiseLLM:::update_trueskill_state(replayed, winner, loser)
      }
      expect_identical(state$trueskill_state, replayed)
      expect_true(state$warm_start_done)
      expect_identical(nrow(state$round_log), 1L)
      expect_false(state$meta$stop_decision)
      list(initial = initial, state = state)
    })
    cold_ts <- runs[[1L]]
    warm_ts <- runs[[2L]]
    expect_identical(cold_ts$state$step_log[c("A_id", "B_id", "Y")],
      warm_ts$state$step_log[c("A_id", "B_id", "Y")])
    expect_identical(cold_ts$initial$trueskill_state$items$sigma,
      warm_ts$initial$trueskill_state$items$sigma)
    expect_false(identical(cold_ts$state$trueskill_state, warm_ts$state$trueskill_state))
    expect_identical(cold_ts$state$btl_fit, warm_ts$state$btl_fit)
    fits <- tail(captured$fits, 2L)
    expect_identical(fits[[1L]], fits[[2L]])
    expect_identical(fits[[1L]]$prior, f$prior)
  }
  # Random ordering depends on exposure and the seed, even after TS states diverge.
  btl <- task07_run(f, "btl_only", "random", n_steps = 12L)
  both <- task07_run(f, "both", "random", n_steps = 12L)
  repeat_run <- task07_run(f, "both", "random", n_steps = 12L)
  expect_identical(btl$state$step_log[c("A_id", "B_id", "Y")],
    both$state$step_log[c("A_id", "B_id", "Y")])
  expect_identical(both$state$step_log, repeat_run$state$step_log)
  expect_identical(both$state$trueskill_state, repeat_run$state$trueskill_state)
  expect_identical(.Random.seed, rng)
})

test_that("BTL stopping and identifiability remain separate from predictive TS confidence", {
  f <- task07_fixture(n = 6L)
  fit <- make_test_btl_fit(f$ids,
    draws = outer(seq_len(10) * 0.005, seq_along(f$ids), "+"))
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    state <- task07_start(f, mode, "random", list(max_pairs_after_stop = 3L))
    judge <- make_adaptive_judge_replay(f$outcomes, f$ids)
    failed_fit <- fit
    failed_fit$diagnostics$divergences <- 1L
    failed <- adaptive_rank_run_live(state, make_adaptive_judge_replay(f$outcomes, f$ids),
      n_steps = 6L, fit_fn = function(...) failed_fit,
      btl_config = list(refit_pairs_target = 3L, stability_lag = 1L), progress = "none")
    expect_false(any(failed$round_log$stop_decision))
    expect_false(any(failed$round_log$diagnostics_pass))
    stopped <- adaptive_rank_run_live(state, judge, n_steps = 6L,
      fit_fn = function(...) fit, btl_config = list(refit_pairs_target = 3L, stability_lag = 1L),
      progress = "none")
    expect_identical(stopped$meta$stop_boundary_step_id, 6L)
    expect_identical(stopped$meta$stop_boundary_refit_id, 2L)
    session <- withr::local_tempdir()
    save_adaptive_session(stopped, session)
    resumed <- load_adaptive_session(session)
    expected <- adaptive_rank_run_live(stopped, judge, n_steps = 5L,
      fit_fn = function(...) fit, progress = "none")
    actual <- adaptive_rank_run_live(resumed, make_adaptive_judge_replay(f$outcomes, f$ids),
      n_steps = 5L, fit_fn = function(...) fit, progress = "none")
    expect_identical(actual$meta$stop_boundary_step_id, 6L)
    expect_identical(actual$meta$pairs_committed_after_stop, 3L)
    expect_identical(actual$meta$stop_reason, "max_pairs_after_stop_exhausted")
    expect_identical(actual$step_log, expected$step_log)
    expect_identical(actual$trueskill_state, expected$trueskill_state)
    expect_identical(actual$stop_metrics, expected$stop_metrics)
    expect_identical(actual$round_log, expected$round_log)
  }
  state <- task07_start(f)
  state$btl_fit <- fit
  cfg <- list(global_identified_reliability_min = 0.8, global_identified_rank_corr_min = 0.8)
  # High BTL reliability alone cannot identify a reversed TrueSkill ranking.
  disagree <- pairwiseLLM:::.adaptive_update_identifiability_state(state, cfg)
  expect_gt(disagree$controller$reliability_EAP, 0.8)
  expect_equal(disagree$controller$ts_btl_rank_spearman, -1)
  expect_false(disagree$controller$global_identified)
  state$trueskill_state$items$mu <- seq_along(f$ids)
  agree <- pairwiseLLM:::.adaptive_update_identifiability_state(state, cfg)
  expect_equal(agree$controller$ts_btl_rank_spearman, 1)
  expect_true(agree$controller$global_identified)
  state$btl_fit <- make_test_btl_fit(f$ids,
    draws = outer(seq(-100, 100, length.out = 10), seq_along(f$ids), "+"))
  uncertain <- pairwiseLLM:::.adaptive_update_identifiability_state(state, cfg)
  expect_lt(uncertain$controller$reliability_EAP, 0.8)
  expect_false(uncertain$controller$global_identified)
})
