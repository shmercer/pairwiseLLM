test_that("reservoir sessions resume during bootstrap and adaptive selection", {
  f <- reservoir_fixture()
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    for (split in c(2L, 6L)) {
      initial <- reservoir_start(f, strategy, "both")
      partial <- reservoir_run(initial, f, split)
      dir <- withr::local_tempdir()
      expect_silent(save_adaptive_session(partial, dir, overwrite = TRUE))
      expect_silent(restored <- adaptive_rank_resume(dir))
      expected <- reservoir_run(partial, f, 3L)
      expect_silent(actual <- reservoir_run(restored, f, 3L))
      expect_identical(actual$history_pairs, expected$history_pairs)
      expect_identical(actual$step_log, expected$step_log)
      expect_identical(actual$trueskill_state, expected$trueskill_state)
      expect_identical(actual$warm_start_pairs, initial$warm_start_pairs)
      expect_reservoir_evidence(actual, f)
      expect_no_error(validate_session_dir(dir))
    }
  }
})

test_that("changed reservoir membership, orientation, or Y cannot resume", {
  f <- reservoir_fixture()
  partial <- reservoir_run(reservoir_start(f), f, 2L)
  dir <- withr::local_tempdir()
  expect_silent(save_adaptive_session(partial, dir, overwrite = TRUE))
  expect_silent(restored <- adaptive_rank_resume(dir))
  alternatives <- list(f$outcomes[-1, ], f$outcomes, f$outcomes)
  alternatives[[2]]$A_id[1] <- "a"
  alternatives[[2]]$B_id[1] <- "b"
  alternatives[[3]]$Y[1] <- 1L - alternatives[[3]]$Y[1]
  for (outcomes in alternatives) {
    other <- make_adaptive_replay_reservoir(outcomes, f$ids)
    expect_error(adaptive_rank_run_live(restored, make_adaptive_judge_replay(other), progress = "none"),
      "judge identity mismatch")
  }
  reordered <- make_adaptive_replay_reservoir(f$outcomes[9:1, ], rev(f$ids))
  expect_no_error(adaptive_rank_run_live(restored, make_adaptive_judge_replay(reordered),
    n_steps = 1L, progress = "none"))
  path <- file.path(dir, "step_log.rds")
  log <- readRDS(path)
  log$A_id[1] <- "foreign"
  saveRDS(log, path)
  expect_error(validate_session_dir(dir), "log and history integrity")
  expect_error(adaptive_rank_resume(dir), "integrity|mismatch")
})

test_that("wrapper binds reservoir once and validates any explicit resume input", {
  f <- reservoir_fixture()
  data <- tibble::tibble(ID = f$ids, text = paste("Item", f$ids))
  dir <- withr::local_tempdir()
  first <- adaptive_rank(data, judge = make_adaptive_judge_replay(f$reservoir),
    replay_reservoir = f$reservoir, session_dir = dir, n_steps = 2L, progress = "none",
    adaptive_config = list(pairing_strategy = "random"))
  continued <- adaptive_rank(data, judge = make_adaptive_judge_replay(f$reservoir),
    session_dir = dir, n_steps = 1L, progress = "none")
  expect_equal(nrow(first$state$history_pairs), 2L)
  expect_equal(nrow(continued$state$history_pairs), 3L)
  expect_reservoir_evidence(continued$state, f)
  expect_no_error(adaptive_rank(data, judge = make_adaptive_judge_replay(f$reservoir),
    replay_reservoir = f$reservoir, session_dir = dir, n_steps = 1L, progress = "none"))
  changed <- f$outcomes
  changed$Y[1] <- 1L - changed$Y[1]
  other <- make_adaptive_replay_reservoir(changed, f$ids)
  expect_error(adaptive_rank(data, judge = make_adaptive_judge_replay(other),
    replay_reservoir = other, session_dir = dir, n_steps = 1L, progress = "none"),
    "Cannot change the replay reservoir")
})

test_that("Bayesian refits receive only committed frozen presentations for all variants", {
  withr::local_seed(149)
  f <- reservoir_fixture()
  captured <- new.env(parent = emptyenv())
  captured$fits <- list()
  testthat::local_mocked_bindings(fit_bayes_btl_mcmc = function(results, ids,
      model_variant, cmdstan, warm_start_prior) {
    captured$fits[[length(captured$fits) + 1L]] <- list(results = results,
      ids = ids, prior = warm_start_prior, model_variant = model_variant)
    list(fit = make_test_btl_fit(ids, model_variant = model_variant,
      draws = outer(seq_len(10) * 0.005, seq_along(ids), "+")))
  }, .package = "pairwiseLLM")
  testthat::local_mocked_bindings(req_perform = function(...) stop("network called"), .package = "httr2")
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    for (mode in c("btl_only", "both")) {
      initial <- reservoir_start(f, "random", mode)
      state <- adaptive_rank_run_live(initial, make_adaptive_judge_replay(f$reservoir),
        n_steps = 7L, btl_config = list(refit_pairs_target = 7L, model_variant = variant),
        progress = "none")
      fit <- tail(captured$fits, 1L)[[1L]]
      expect_identical(fit$model_variant, variant)
      expect_identical(fit$prior, f$prior)
      expect_identical(nrow(fit$results), 7L)
      expect_identical(nrow(state$round_log), 1L)
      committed <- pairwiseLLM:::.adaptive_results_from_step_log(state)
      expect_identical(fit$results, committed)
      expect_reservoir_evidence(state, f)
      rebuilt <- pairwiseLLM:::.adaptive_committed_results_rebuild(state)
      expect_identical(rebuilt$A_id, state$history_pairs$A_id)
      expect_identical(rebuilt$B_id, state$history_pairs$B_id)
      expect_identical(rebuilt$Y, state$step_log$Y)
      expect_false("outcomes" %in% names(state$replay_reservoir))
    }
  }
  expect_identical(length(captured$fits), 8L)
})
