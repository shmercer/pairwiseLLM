testthat::test_that("adaptive_run helper validation and path handling", {
  expect_error(pairwiseLLM:::.adaptive_merge_config(1), "adaptive")
  expect_error(pairwiseLLM:::.adaptive_check_string(NA_character_, "x"), "non-empty")
  expect_error(pairwiseLLM:::.adaptive_check_backend("together", "batch"), "Batch mode")
  expect_error(pairwiseLLM:::.adaptive_sanitize_submission_options(1), "submission")
  expect_error(pairwiseLLM:::.adaptive_prepare_paths(1, list(), "live"), "paths")
  expect_error(
    pairwiseLLM:::.adaptive_prepare_paths(list(output_dir = 1), list(), "batch"),
    "output_dir"
  )
})

testthat::test_that("adaptive_run helpers handle seen results and ingestion", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  env_seen <- new.env(parent = emptyenv())
  env_seen[["x"]] <- TRUE
  state$results_seen <- env_seen
  expect_equal(pairwiseLLM:::.adaptive_results_seen_names(state), "x")

  state <- pairwiseLLM:::.adaptive_results_seen_set(state, c("y", ""))
  expect_true(isTRUE(state$results_seen[["y"]]))

  state$results_seen <- NULL
  state <- pairwiseLLM:::.adaptive_results_seen_set(state, "z")
  expect_true(isTRUE(state$results_seen[["z"]]))

  state$results_seen <- NULL
  state$history_results <- pairwiseLLM:::.adaptive_empty_results_tbl()
  state <- pairwiseLLM:::.adaptive_state_sync_results_seen(state)
  expect_identical(length(state$results_seen), 0L)

  state$history_results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 0L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )
  state$results_seen <- NULL
  state <- pairwiseLLM:::.adaptive_state_sync_results_seen(state)
  expect_true(isTRUE(state$results_seen[["A:B#1"]]))

  one_result <- tibble::tibble(
    pair_uid = NA_character_,
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 0L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  ingest_empty <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, NULL)
  expect_equal(nrow(ingest_empty$new_results), 0L)

  ingest_warn <- NULL
  testthat::expect_warning(
    {
      ingest_warn <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, one_result)
    },
    "Dropping results"
  )
  expect_equal(nrow(ingest_warn$new_results), 0L)
})

testthat::test_that("adaptive_run normalization helper handles already-normalized input", {
  empty_out <- pairwiseLLM:::.adaptive_normalize_submission_output(
    raw = NULL,
    pairs_submitted = NULL,
    backend = "openai",
    model = "gpt-test"
  )
  expect_equal(nrow(empty_out$results), 0L)

  results_tbl <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 0L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  out <- pairwiseLLM:::.adaptive_normalize_submission_output(
    raw = results_tbl,
    pairs_submitted = tibble::tibble(),
    backend = "openai",
    model = "gpt-test"
  )
  expect_equal(nrow(out$results), 1L)

  out_list <- pairwiseLLM:::.adaptive_normalize_submission_output(
    raw = list(results = results_tbl, failed_attempts = list(bad = TRUE)),
    pairs_submitted = tibble::tibble(),
    backend = "openai",
    model = "gpt-test"
  )
  expect_equal(nrow(out_list$results), 1L)
  expect_equal(nrow(out_list$failed_attempts), 0L)

  expect_error(
    pairwiseLLM:::.adaptive_normalize_submission_output(
      raw = list(results = "bad"),
      pairs_submitted = NULL,
      backend = "openai",
      model = "gpt-test"
    ),
    "pairs_submitted"
  )
})

testthat::test_that("adaptive_run scheduling helpers cover edge branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 2)
  adaptive <- list(bins = 2L, mix_struct = 0.7, within_adj_split = 0.5, exploration_frac = 0.1)

  expect_error(pairwiseLLM:::.adaptive_schedule_next_pairs(state, NA, adaptive, seed = 1), "target_pairs")
  empty_out <- pairwiseLLM:::.adaptive_schedule_next_pairs(state, 0L, adaptive, seed = 1)
  expect_equal(nrow(empty_out$pairs), 0L)

  state$phase <- "phase2"
  withr::local_seed(1)
  no_candidates <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive, seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0, 0), c("A", "B", "C")),
          theta_draws = matrix(0, nrow = 1, ncol = 3, dimnames = list(NULL, c("A", "B", "C")))
        )
      )
    },
    compute_ranking_from_theta_mean = function(theta_mean, state) state$ids,
    select_window_size = function(N, phase, near_stop) 1L,
    build_candidate_pairs = function(...) tibble::tibble()
  )
  expect_equal(nrow(no_candidates$pairs), 0L)
})

testthat::test_that("adaptive_run helpers cover target selection and replacements", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 3)
  adaptive <- list(batch_overrides = list(BATCH1 = 2L, BATCH2 = 1L, BATCH3 = 1L))

  state$phase <- "phase3"
  target3 <- pairwiseLLM:::.adaptive_schedule_target(state, adaptive)
  expect_equal(target3$target, 1L)

  state$phase <- "phase2"
  target2 <- pairwiseLLM:::.adaptive_schedule_target(state, adaptive)
  expect_equal(target2$target, 1L)

  state$phase <- "phase1"
  target1 <- pairwiseLLM:::.adaptive_schedule_target(state, adaptive)
  expect_equal(target1$target, 2L)

  expect_equal(pairwiseLLM:::.adaptive_replacement_target(0L, adaptive, 2L), 0L)
  adaptive$max_replacements <- NA_integer_
  expect_equal(pairwiseLLM:::.adaptive_replacement_target(3L, adaptive, 2L), 2L)

  expect_null(pairwiseLLM:::.adaptive_phase_scalar_from_pairs(NULL))
  expect_null(pairwiseLLM:::.adaptive_phase_scalar_from_pairs(tibble::tibble(A_id = "A")))
  expect_null(pairwiseLLM:::.adaptive_phase_scalar_from_pairs(tibble::tibble(phase = NA_character_)))
  expect_error(
    pairwiseLLM:::.adaptive_phase_scalar_from_pairs(tibble::tibble(phase = c("phase1", "phase2"))),
    "single phase"
  )

  expect_error(
    pairwiseLLM:::.adaptive_schedule_replacement_pairs(state, -1L, adaptive, seed = 1, replacement_phase = "phase1"),
    "target_pairs"
  )
  empty_replace <- pairwiseLLM:::.adaptive_schedule_replacement_pairs(
    state,
    0L,
    adaptive,
    seed = 1,
    replacement_phase = "phase1"
  )
  expect_equal(nrow(empty_replace$pairs), 0L)
  expect_error(
    pairwiseLLM:::.adaptive_schedule_replacement_pairs(state, 1L, adaptive, seed = 1, replacement_phase = ""),
    "replacement_phase"
  )

  withr::local_seed(1)
  replacement_non_phase1 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_replacement_pairs(state, 1L, adaptive, seed = 1, replacement_phase = "phase2"),
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    }
  )
  expect_equal(nrow(replacement_non_phase1$pairs), 0L)
})

testthat::test_that("adaptive_run helper error paths cover missing refit state", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 6)
  adaptive <- list(n_draws_fast = 10L)

  expect_error(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.adaptive_get_refit_fit(state, adaptive, batch_size = 1L, seed = 1),
      fit_bayes_btl_fast = function(...) NULL
    ),
    "Fast inference failed"
  )
})

testthat::test_that("adaptive_run helper updates failed attempts metadata", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 7)
  failed_attempts <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    attempted_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test",
    error_code = "timeout",
    error_detail = "timeout"
  )

  updated <- pairwiseLLM:::.adaptive_append_failed_attempts(state, failed_attempts, phase = "phase1", iter = 0L)
  expect_equal(updated$failed_attempts$phase[[1]], "phase1")
  expect_equal(updated$failed_attempts$iter[[1]], 0L)
})

testthat::test_that("adaptive_run next_action returns expected reason", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 8)

  state$comparisons_scheduled <- state$budget_max
  done_budget <- pairwiseLLM:::.adaptive_next_action(state, scheduled_pairs = 1L)
  expect_equal(done_budget$reason, "budget_exhausted")

  state$comparisons_scheduled <- 0L
  state$budget_max <- 10L
  done_none <- pairwiseLLM:::.adaptive_next_action(state, scheduled_pairs = 0L)
  expect_equal(done_none$reason, "no_feasible_pairs")
})
testthat::test_that("adaptive_run replacement loop handles edge cases", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 4)
  adaptive <- list(max_refill_rounds = 0L)

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_replacements_live(
      state = state,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      prompt_template = "template",
      backend = "openai",
      adaptive = adaptive,
      submission = list(),
      missing = 1L,
      seed = 1,
      replacement_phase = "phase1",
      base_batch_size = NA_integer_
    ),
    .adaptive_schedule_replacement_pairs = function(...) {
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    }
  )
  expect_equal(length(out$submissions), 0L)

  out2 <- pairwiseLLM:::.adaptive_run_replacements_live(
    state = state,
    model = "gpt-test",
    trait_name = "quality",
    trait_description = "Which is better?",
    prompt_template = "template",
    backend = "openai",
    adaptive = adaptive,
    submission = list(),
    missing = 0L,
    seed = 1,
    replacement_phase = "phase1",
    base_batch_size = 1L
  )
  expect_equal(length(out2$submissions), 0L)
})

testthat::test_that("adaptive_rank_resume and run_live cover error and branch paths", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 5)

  expect_error(
    adaptive_rank_resume(state = NULL, state_path = NULL, submission_info = list(), mode = "live"),
    "state"
  )

  tmp_state <- tempfile("adaptive_state_", fileext = ".rds")
  saveRDS(state, tmp_state)
  expect_error(
    adaptive_rank_resume(state = NULL, state_path = tmp_state, submission_info = list(), mode = "live"),
    "backend"
  )

  state$config$backend <- "openai"
  state$config$model <- "gpt-test"
  expect_error(
    adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = list(backend = "openai", model = "gpt-test")
    ),
    "trait_name"
  )

  failed_attempts <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    phase = "phase1",
    iter = 0L,
    attempted_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test",
    error_code = "timeout",
    error_detail = "timeout"
  )

  state$config$trait_name <- "quality"
  state$config$trait_description <- "Which is better?"
  state$config$prompt_template <- "template"
  state$config$submission <- list()

  out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        prompt_template = "template",
        failed_attempts = failed_attempts
      )
    ),
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    }
  )
  expect_equal(nrow(out$state$failed_attempts), 1L)

  captured <- new.env(parent = emptyenv())
  testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "batch",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        prompt_template = "template"
      )
    ),
    llm_resume_multi_batches = function(output_dir = NULL, jobs = NULL, ...) {
      captured$output_dir <- output_dir
      list(
        jobs = jobs,
        combined = NULL,
        failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
        batch_failures = tibble::tibble()
      )
    },
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    }
  )
  expect_true(nzchar(captured$output_dir))

  pairs_tbl <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase1",
    iter = 0L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  batch_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "batch",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        prompt_template = "template"
      )
    ),
    llm_resume_multi_batches = function(output_dir = NULL, jobs = NULL, ...) {
      list(
        jobs = jobs,
        combined = NULL,
        failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
        batch_failures = tibble::tibble()
      )
    },
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      list(state = state, pairs = pairs_tbl)
    },
    .adaptive_submit_batch = function(...) {
      list(jobs = list(), registry = tibble::tibble())
    }
  )
  expect_true("output_dir" %in% names(batch_out$submission_info))

  expect_error(
    adaptive_rank_run_live(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      max_iterations = 0
    ),
    "max_iterations"
  )

  live_out <- testthat::with_mocked_bindings(
    adaptive_rank_run_live(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      max_iterations = 2
    ),
    adaptive_rank_start = function(...) {
      list(
        state = list(),
        submission_info = list(),
        next_action = list(action = "resume")
      )
    },
    adaptive_rank_resume = function(...) {
      list(
        state = list(),
        submission_info = list(),
        next_action = list(action = "done")
      )
    }
  )
  expect_equal(live_out$iterations, 2L)
})
