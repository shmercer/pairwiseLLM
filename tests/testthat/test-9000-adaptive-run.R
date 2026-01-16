testthat::test_that("adaptive_rank_start ingests live results and schedules replacements", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 4L,
    budget_max = 6L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 4L, BATCH2 = 2L),
    max_refill_rounds = 1L
  )

  make_results <- function(pairs, backend, model, keep_idx) {
    keep_idx <- as.integer(keep_idx)
    ids1 <- pairs$ID1[keep_idx]
    ids2 <- pairs$ID2[keep_idx]
    unordered_key <- pairwiseLLM:::make_unordered_key(ids1, ids2)
    ordered_key <- pairwiseLLM:::make_ordered_key(ids1, ids2)

    tibble::tibble(
      pair_uid = pairs$pair_uid[keep_idx],
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase[keep_idx]),
      iter = as.integer(pairs$iter[keep_idx]),
      received_at = as.POSIXct("2026-01-10 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
  }

  call_count <- 0L
  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    call_count <<- call_count + 1L
    total <- nrow(pairs)
    if (call_count == 1L) {
      keep <- seq_len(total - 1L)
      missing <- total
    } else {
      keep <- seq_len(total)
      missing <- integer()
    }

    results <- make_results(pairs, backend, model, keep)

    failed_attempts <- pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    if (length(missing) > 0L) {
      ids1 <- pairs$ID1[missing]
      ids2 <- pairs$ID2[missing]
      failed_attempts <- tibble::tibble(
        pair_uid = pairs$pair_uid[missing],
        unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
        ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
        A_id = ids1,
        B_id = ids2,
        phase = as.character(pairs$phase[missing]),
        iter = as.integer(pairs$iter[missing]),
        attempted_at = as.POSIXct("2026-01-10 00:00:00", tz = "UTC"),
        backend = as.character(backend),
        model = as.character(model),
        error_code = "timeout",
        error_detail = "timeout"
      )
    }

    list(
      results = results,
      failed_pairs = tibble::tibble(),
      failed_attempts = failed_attempts
    )
  }

  withr::local_seed(101)
  out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = adaptive,
      seed = 101
    ),
    submit_llm_pairs = mock_submit
  )

  expect_equal(out$state$comparisons_observed, 4L)
  expect_equal(out$state$comparisons_scheduled, 5L)
  expect_equal(nrow(out$state$failed_attempts), 1L)
  expect_equal(length(out$submission_info$live_submissions), 2L)
  expect_true(all(out$submission_info$live_submissions[[2]]$pairs$phase == "phase1"))
  expect_true(all(out$submission_info$live_submissions[[2]]$pairs$iter == 0L))
})

testthat::test_that("adaptive_rank_resume ingests batch results incrementally", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 4L,
    budget_max = 4L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 4L)
  )

  last_pairs <- NULL
  make_results <- function(pairs, backend, model) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    unordered_key <- pairwiseLLM:::make_unordered_key(ids1, ids2)
    ordered_key <- pairwiseLLM:::make_ordered_key(ids1, ids2)
    tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-11 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
  }

  mock_submit_batch <- function(pairs, model, trait_name, trait_description,
                                prompt_template, backend, ...) {
    last_pairs <<- pairs
    list(
      jobs = list(list(
        segment_index = 1L,
        provider = backend,
        model = model,
        batch_id = "batch-1",
        batch_input_path = "in.jsonl",
        batch_output_path = "out.jsonl",
        csv_path = "out.csv",
        pairs = pairs,
        done = FALSE
      )),
      registry = tibble::tibble(
        segment_index = 1L,
        provider = backend,
        model = model,
        batch_id = "batch-1",
        batch_input_path = "in.jsonl",
        batch_output_path = "out.jsonl",
        csv_path = "out.csv",
        done = FALSE
      )
    )
  }

  mock_resume_batch <- function(jobs = NULL, output_dir = NULL, ...) {
    results <- make_results(last_pairs, "openai", "gpt-test")
    list(
      jobs = jobs,
      combined = results,
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
      batch_failures = tibble::tibble()
    )
  }

  withr::local_seed(202)
  start_out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "batch",
      adaptive = adaptive,
      paths = list(output_dir = withr::local_tempdir()),
      seed = 202
    ),
    llm_submit_pairs_multi_batch = mock_submit_batch
  )

  expect_equal(start_out$state$comparisons_observed, 0L)

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = start_out$state,
      mode = "batch",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 202
    ),
    llm_resume_multi_batches = mock_resume_batch,
    llm_submit_pairs_multi_batch = mock_submit_batch
  )

  expect_equal(resume_out$state$comparisons_observed, 4L)

  resume_out2 <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = resume_out$state,
      mode = "batch",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 202
    ),
    llm_resume_multi_batches = mock_resume_batch,
    llm_submit_pairs_multi_batch = mock_submit_batch
  )

  expect_equal(resume_out2$state$comparisons_observed, 4L)
})

testthat::test_that("adaptive_rank_start stores submission options in state and reuse on resume", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 2L,
    budget_max = 2L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 2L)
  )

  captured_submit <- new.env(parent = emptyenv())
  mock_submit_batch <- function(pairs, model, trait_name, trait_description,
                                prompt_template, backend,
                                batch_size = NULL,
                                openai_max_retries = 3,
                                verbose = FALSE,
                                ...) {
    captured_submit$batch_size <- batch_size
    captured_submit$openai_max_retries <- openai_max_retries
    captured_submit$verbose <- verbose
    list(
      jobs = list(list(
        segment_index = 1L,
        provider = backend,
        model = model,
        batch_id = "batch-1",
        batch_input_path = "in.jsonl",
        batch_output_path = "out.jsonl",
        csv_path = "out.csv",
        pairs = pairs,
        done = FALSE
      )),
      registry = tibble::tibble(
        segment_index = 1L,
        provider = backend,
        model = model,
        batch_id = "batch-1",
        batch_input_path = "in.jsonl",
        batch_output_path = "out.jsonl",
        csv_path = "out.csv",
        done = FALSE
      )
    )
  }

  captured_resume <- new.env(parent = emptyenv())
  mock_resume_batch <- function(jobs = NULL,
                                output_dir = NULL,
                                openai_max_retries = 3,
                                verbose = FALSE,
                                ...) {
    captured_resume$openai_max_retries <- openai_max_retries
    captured_resume$verbose <- verbose
    list(
      jobs = jobs,
      combined = NULL,
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
      batch_failures = tibble::tibble()
    )
  }

  out_dir <- withr::local_tempdir()
  withr::local_seed(555)
  start_out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "batch",
      submission = list(batch_size = 2L, openai_max_retries = 7L, verbose = TRUE),
      adaptive = adaptive,
      paths = list(output_dir = out_dir),
      seed = 555
    ),
    llm_submit_pairs_multi_batch = mock_submit_batch
  )

  expect_equal(captured_submit$batch_size, 2L)
  expect_equal(captured_submit$openai_max_retries, 7L)
  expect_true(isTRUE(captured_submit$verbose))
  expect_true(file.exists(start_out$state_path))
  expect_equal(start_out$state$config$submission$openai_max_retries, 7L)
  expect_true(isTRUE(start_out$state$config$submission$verbose))

  loaded_state <- readRDS(start_out$state_path)
  expect_equal(loaded_state$config$submission$openai_max_retries, 7L)
  expect_true(isTRUE(loaded_state$config$submission$verbose))

  testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = loaded_state,
      state_path = start_out$state_path,
      mode = "batch",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 555
    ),
    llm_resume_multi_batches = mock_resume_batch
  )

  expect_equal(captured_resume$openai_max_retries, 7L)
  expect_true(isTRUE(captured_resume$verbose))
})

testthat::test_that("adaptive_rank_resume normalizes raw live results before ingesting", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 1L),
    seed = 777
  )

  scheduled <- pairwiseLLM:::phase1_generate_pairs(
    state = state,
    n_pairs = 1L,
    bins = 2L,
    seed = 777
  )
  state <- scheduled$state
  pairs_submitted <- scheduled$pairs

  raw_results <- tibble::tibble(
    custom_id = pairs_submitted$pair_uid,
    better_sample = "SAMPLE_1"
  )

  resume_out <- adaptive_rank_resume(
    state = state,
    mode = "live",
    submission_info = list(
      backend = "openai",
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      prompt_template = "TEMPLATE",
      pairs_submitted = pairs_submitted,
      results = raw_results
    ),
    seed = 777
  )

  expect_equal(resume_out$state$comparisons_observed, 1L)
  expect_true(all(c("unordered_key", "ordered_key", "winner_pos") %in% names(resume_out$state$history_results)))
})

testthat::test_that("adaptive_rank_resume is deterministic under fixed seed", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 2L,
    budget_max = 6L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 2L, BATCH2 = 2L)
  )

  make_results <- function(pairs, backend, model) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
      ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-12 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
  }

  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    list(
      results = make_results(pairs, backend, model),
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  withr::local_seed(303)
  start_out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = adaptive,
      seed = 303
    ),
    submit_llm_pairs = mock_submit
  )

  captured_pairs <- list()
  mock_submit_capture <- function(pairs, model, trait_name, trait_description,
                                  prompt_template, backend, ...) {
    captured_pairs[[length(captured_pairs) + 1L]] <<- pairs
    list(
      results = make_results(pairs, backend, model),
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  state_a <- start_out$state
  state_b <- start_out$state

  withr::local_seed(404)
  testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state_a,
      mode = "live",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 404
    ),
    submit_llm_pairs = mock_submit_capture
  )

  withr::local_seed(404)
  testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state_b,
      mode = "live",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 404
    ),
    submit_llm_pairs = mock_submit_capture
  )

  expect_equal(captured_pairs[[1]], captured_pairs[[2]])
})

testthat::test_that("adaptive stopping checks confirm MCMC and finalize summaries", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )

  add_results <- function(state, n_pairs) {
    ids <- state$ids
    A_id <- rep(ids[1], n_pairs)
    B_id <- rep(ids[2], n_pairs)
    unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
    ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
    pair_uid <- paste0(unordered_key, "#", seq_len(n_pairs))

    state$history_pairs <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = rep(state$texts[[ids[1]]], n_pairs),
      B_text = rep(state$texts[[ids[2]]], n_pairs),
      phase = "phase2",
      iter = 1L,
      created_at = as.POSIXct(rep("2026-01-01 00:00:00", n_pairs), tz = "UTC")
    )

    state$history_results <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      better_id = A_id,
      winner_pos = 1L,
      phase = "phase2",
      iter = 1L,
      received_at = as.POSIXct(rep("2026-01-02 00:00:00", n_pairs), tz = "UTC"),
      backend = "openai",
      model = "gpt-test"
    )

    state$comparisons_scheduled <- as.integer(n_pairs)
    state$comparisons_observed <- as.integer(n_pairs)
    state
  }

  mock_mcmc <- function(results, ids, cmdstan = list()) {
    draws <- matrix(rep(c(2, 1, 0), times = 4), nrow = 4, byrow = TRUE)
    colnames(draws) <- ids
    list(theta_draws = draws, fit_meta = list(converged = TRUE))
  }

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 1L, M1_target = 1L, budget_max = 10L),
    seed = 1
  )
  state$phase <- "phase2"
  state$config$CW <- 1L
  state$U0 <- 1

  theta_mean <- stats::setNames(c(2, 1, 0), state$ids)
  draws <- matrix(rep(theta_mean, each = 4), nrow = 4, byrow = FALSE)
  colnames(draws) <- state$ids
  state$fast_fit <- list(theta_mean = theta_mean, theta_draws = draws)

  state <- add_results(state, 2L)
  state$config$last_refit_at <- as.integer(state$comparisons_observed)
  out1 <- pairwiseLLM:::.adaptive_run_stopping_checks(state, adaptive = list(exploration_frac = 0.05))

  state2 <- add_results(out1$state, 3L)
  state2$config$last_refit_at <- as.integer(state2$comparisons_observed)
  out2 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_stopping_checks(
      state2,
      adaptive = list(exploration_frac = 0.05)
    ),
    fit_bayes_btl_mcmc = mock_mcmc
  )

  expect_true(isTRUE(out2$state$config$stop_confirmed))
  expect_true(is.list(out2$state$config$final_summary))
  expect_true(all(c("win_prob", "win_prob_btl") %in% names(out2$state$config$final_summary$adjacent_win_probs)))
})
