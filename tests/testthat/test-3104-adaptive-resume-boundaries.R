resume_state_3104 <- function(strategy = "hybrid", mode = "both") {
  ids <- letters[1:6]
  prior <- if (mode == "cold") {
    NULL
  } else {
    pairwiseLLM::make_warm_start_prior(stats::setNames(seq(-1, 1, length.out = 6), ids))
  }
  pairwiseLLM::adaptive_rank_start(ids, seed = 62L,
    warm_start_prior = prior, warm_start_mode = mode,
    adaptive_config = list(pairing_strategy = strategy))
}

expect_resume_trajectory_3104 <- function(actual, expected) {
  for (field in c("trueskill_state", "warm_start_pairs", "warm_start_idx",
    "warm_start_done", "history_state", "round", "predictive_prior", "btl_fit")) {
    expect_identical(actual[[field]], expected[[field]], info = field)
  }
  expect_identical(actual$history_pairs[, c("A_id", "B_id")],
    expected$history_pairs[, c("A_id", "B_id")])
  fields <- setdiff(names(expected$step_log), "timestamp")
  expect_identical(actual$step_log[, fields], expected$step_log[, fields])
  expect_identical(actual$meta$warm_start_mode, expected$meta$warm_start_mode)
  expect_identical(actual$controller$pairing_strategy, expected$controller$pairing_strategy)
}

expect_resume_refit_3104 <- function(actual, expected) {
  # Derived runtime caches are deliberately dropped on save and reconstructed lazily.
  fields <- setdiff(names(expected$refit_meta), c("committed_results_cache",
    "committed_results_cache_built", "link_cross_edges_by_spoke", "link_cross_edges_cache_built",
    "link_refit_local_memo_env"))
  expect_identical(actual$refit_meta[fields], expected$refit_meta[fields])
}

test_that("resume preserves exact bootstrap and post-bootstrap trajectories", {
  withr::local_seed(193)
  rng_before <- .Random.seed
  testthat::local_mocked_bindings(
    extract_warm_start_features = function(...) stop("Resume must not extract features"),
    load_warm_start_model = function(...) stop("Resume must not load models"),
    predict.pairwiseLLM_warm_model = function(...) stop("Resume must not predict"),
    .warm_start_python_request = function(...) stop("Resume must not invoke Python"),
    .package = "pairwiseLLM")
  modes <- c(hybrid = "cold", random = "btl_only", trueskill_p50 = "trueskill_only",
    trueskill_pollitt = "both")
  judge <- make_deterministic_judge("i_wins")
  for (strategy in names(modes)) {
    state <- resume_state_3104(strategy, modes[[strategy]])
    previous <- 0L
    for (boundary in c(2L, 5L, 7L)) {
      state <- pairwiseLLM::adaptive_rank_run_live(state, judge,
        n_steps = boundary - previous, progress = "none")
      previous <- boundary
      expect_identical(nrow(state$step_log), boundary)
      expect_gte(nrow(state$history_pairs), min(boundary, 6L))
      expect_identical(state$warm_start_done, boundary >= 5L)
      expect_identical(state$config$btl_config$refit_pairs_target, 20L)
      expect_identical(state$config$btl_config$stability_lag, 2L)
      session <- withr::local_tempdir()
      pairwiseLLM::save_adaptive_session(state, session, overwrite = TRUE)
      restored <- pairwiseLLM::adaptive_rank_resume(session)
      expect_resume_trajectory_3104(restored, state)
      expect_resume_refit_3104(restored, state)
      expect_identical(pairwiseLLM:::select_next_pair(restored),
        pairwiseLLM:::select_next_pair(state))
      expected <- pairwiseLLM::adaptive_rank_run_live(state, judge,
        n_steps = 1L, progress = "none")
      actual <- pairwiseLLM::adaptive_rank_run_live(restored, judge,
        n_steps = 1L, progress = "none")
      expect_identical(tail(actual$step_log$status, 1), "ok")
      expect_resume_trajectory_3104(actual, expected)
    }
  }
  expect_identical(.Random.seed, rng_before)
})

test_that("resumed strategies reject conflicts and retain other controller overrides", {
  judge <- make_deterministic_judge("i_wins")
  for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
    state <- pairwiseLLM::adaptive_rank_run_live(resume_state_3104(strategy), judge,
      n_steps = 6L, progress = "none")
    session <- withr::local_tempdir()
    pairwiseLLM::save_adaptive_session(state, session, overwrite = TRUE)
    restored <- pairwiseLLM::adaptive_rank_resume(session)
    before <- readBin(file.path(session, "state.rds"), "raw", n = 1e7)
    for (alternative in setdiff(c("hybrid", "random", "trueskill_p50",
      "trueskill_pollitt"), strategy)) {
      expect_error(pairwiseLLM::adaptive_rank_run_live(restored,
        function(...) stop("Judge must not be called"),
        adaptive_config = list(pairing_strategy = alternative), progress = "none"),
      "Cannot change.*pairing_strategy.*on resume")
    }
    if (strategy != "hybrid") {
      expect_error(pairwiseLLM::adaptive_rank_run_live(restored, judge,
        adaptive_config = list(pairing_strategy = NULL), progress = "none"),
      "Cannot change.*pairing_strategy.*on resume")
    }
    expect_error(pairwiseLLM::adaptive_rank_run_live(restored, judge,
      adaptive_config = list(pairing_strategy = "unknown"), progress = "none"),
    "pairing_strategy.*must be one of")
    expect_identical(readBin(file.path(session, "state.rds"), "raw", n = 1e7), before)
    expected <- pairwiseLLM::adaptive_rank_run_live(restored, judge,
      n_steps = 1L, progress = "none")
    actual <- pairwiseLLM::adaptive_rank_run_live(restored, judge, n_steps = 1L,
      adaptive_config = list(pairing_strategy = strategy), progress = "none")
    expect_resume_trajectory_3104(actual, expected)
    updated <- pairwiseLLM::adaptive_rank_run_live(restored, judge, n_steps = 1L,
      adaptive_config = list(max_pairs_after_stop = 4L), progress = "none")
    expect_identical(updated$controller$pairing_strategy, strategy)
    expect_identical(updated$meta$warm_start_mode, "both")
    expect_identical(updated$controller$max_pairs_after_stop, 4L)
  }
})

test_that("the public resume wrapper preserves the saved strategy and warm inputs", {
  samples <- data.frame(item_id = letters[1:6], text = paste("item", letters[1:6]))
  session <- withr::local_tempdir()
  state <- resume_state_3104("trueskill_pollitt")
  pairwiseLLM::save_adaptive_session(state, session, overwrite = TRUE)
  judge <- make_deterministic_judge("i_wins")
  expect_error(pairwiseLLM::adaptive_rank(samples, judge = judge, session_dir = session,
    adaptive_config = list(pairing_strategy = "random"), progress = "none"),
  "Cannot change.*pairing_strategy.*on resume")
  expect_error(pairwiseLLM::adaptive_rank(samples, judge = judge, session_dir = session,
    warm_start_mode = "both", progress = "none"), "Omit all warm-start")
  expect_error(pairwiseLLM::adaptive_rank(samples, judge = judge, session_dir = session,
    warm_start_prior = state$predictive_prior, progress = "none"), "Omit all warm-start")
  out <- pairwiseLLM::adaptive_rank(samples, judge = judge, session_dir = session,
    adaptive_config = list(pairing_strategy = "trueskill_pollitt", max_pairs_after_stop = 4L),
    n_steps = 1L, progress = "none")
  expect_identical(out$state$controller$pairing_strategy, "trueskill_pollitt")
  expect_identical(out$state$meta$warm_start_mode, "both")
  expect_identical(out$state$predictive_prior, state$predictive_prior)
  expect_identical(out$state$controller$max_pairs_after_stop, 4L)
})

test_that("refit and first-stop boundaries resume the same remaining budget", {
  withr::local_seed(719)
  rng_before <- .Random.seed
  judge <- make_deterministic_judge("i_wins")
  btl_config <- list(refit_pairs_target = 3L, stability_lag = 1L)
  for (strategy in c("hybrid", "trueskill_pollitt")) {
    state <- resume_state_3104(strategy)
    state <- pairwiseLLM:::.adaptive_apply_controller_config(state,
      list(max_pairs_after_stop = 3L))
    fit <- make_test_btl_fit(state$item_ids,
      draws = outer(seq_len(10) * 0.005, seq_len(6), "+"))
    stub <- make_deterministic_fit_fn(state$item_ids, fit)
    state <- pairwiseLLM::adaptive_rank_run_live(state, judge, n_steps = 3L,
      fit_fn = stub$fit_fn, btl_config = btl_config, progress = "none")
    expect_identical(nrow(state$round_log), 1L)
    expect_false(state$round_log$stop_decision[[1]])
    for (boundary in c(3L, 6L)) {
      if (boundary == 6L) {
        state <- pairwiseLLM::adaptive_rank_run_live(state, judge, n_steps = 3L,
          fit_fn = stub$fit_fn, progress = "none")
        expect_identical(state$meta$stop_boundary_step_id, 6L)
        expect_identical(state$meta$stop_boundary_refit_id, 2L)
        expect_false(state$meta$stop_decision)
      }
      session <- withr::local_tempdir()
      pairwiseLLM::save_adaptive_session(state, session, overwrite = TRUE)
      restored <- pairwiseLLM::adaptive_rank_resume(session)
      expect_resume_trajectory_3104(restored, state)
      expect_resume_refit_3104(restored, state)
      expect_identical(restored$stop_metrics, state$stop_metrics)
      expect_identical(restored$round_log, state$round_log)
      n_steps <- if (boundary == 3L) 1L else 5L
      expected <- pairwiseLLM::adaptive_rank_run_live(state, judge,
        n_steps = n_steps, fit_fn = stub$fit_fn, progress = "none")
      actual <- pairwiseLLM::adaptive_rank_run_live(restored, judge,
        n_steps = n_steps, fit_fn = stub$fit_fn, progress = "none")
      expect_identical(actual$config$btl_config, state$config$btl_config)
      expect_identical(expected$config$btl_config, state$config$btl_config)
      expect_resume_trajectory_3104(actual, expected)
      for (field in c("stop_boundary_step_id", "stop_boundary_refit_id",
        "pairs_committed_after_stop", "stop_decision", "stop_reason")) {
        expect_identical(actual$meta[[field]], expected$meta[[field]], info = field)
      }
      if (boundary == 6L) {
        expect_identical(actual$meta$pairs_committed_after_stop, 3L)
        expect_identical(actual$meta$stop_reason, "max_pairs_after_stop_exhausted")
        expect_identical(nrow(actual$history_pairs), 9L)
      } else {
        overridden <- pairwiseLLM::adaptive_rank_run_live(restored, judge, n_steps = 1L,
          fit_fn = stub$fit_fn, btl_config = list(refit_pairs_target = 1L), progress = "none")
        expect_identical(nrow(overridden$round_log), 2L)
        expect_identical(overridden$config$btl_config$refit_pairs_target, 1L)
        expect_identical(overridden$config$btl_config$stability_lag, 2L)
      }
    }
  }
  expect_identical(.Random.seed, rng_before)
})
