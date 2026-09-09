test_that("adaptive_rank_resume restores state and run_live can continue", {
  items <- make_test_items(5)
  judge <- make_deterministic_judge("i_wins")
  session_dir <- withr::local_tempdir()

  state <- adaptive_rank_start(
    items,
    session_dir = session_dir,
    checkpoint_every_steps = 3L
  )
  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 2L,
    session_dir = session_dir,
    progress = "none"
  )

  resumed <- adaptive_rank_resume(session_dir)
  expect_identical(as.integer(resumed$config$checkpoint_every_steps), 3L)
  expect_identical(resumed$refit_meta$phase_a_committed_pairs_by_set, c(`1` = 2L))
  withr::local_seed(2)
  resumed <- adaptive_rank_run_live(
    resumed,
    judge,
    n_steps = 2L,
    session_dir = session_dir,
    progress = "none"
  )

  expect_equal(nrow(resumed$step_log), nrow(state$step_log) + 2L)
  history <- adaptive_results_history(resumed, committed_only = TRUE)
  expect_equal(nrow(history), sum(!is.na(resumed$step_log$pair_id)))
  expect_identical(resumed$refit_meta$phase_a_committed_pairs_by_set, c(`1` = 4L))
  expect_history_state_matches_history(resumed)
})

test_that("predictive assessment resumes live steps using its saved numeric record", {
  items <- make_test_items(5)
  prior <- make_warm_start_prior(stats::setNames(seq_len(5), items$item_id))
  session <- withr::local_tempdir()
  state <- adaptive_rank_start(items, session_dir = session, warm_start_prior = prior)
  judge <- make_deterministic_judge("i_wins")
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")
  testthat::local_mocked_bindings(
    extract_warm_start_features = function(...) stop("Resume must not extract"),
    load_warm_start_model = function(...) stop("Resume must not load models"), .package = "pairwiseLLM")
  resumed <- adaptive_rank_resume(session)
  resumed <- adaptive_rank_run_live(resumed, judge, n_steps = 2L, progress = "none")
  expect_identical(resumed$predictive_prior, state$predictive_prior)
  expect_equal(nrow(resumed$step_log), nrow(state$step_log) + 2L)
  expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
  expect_history_state_matches_history(resumed)
})
