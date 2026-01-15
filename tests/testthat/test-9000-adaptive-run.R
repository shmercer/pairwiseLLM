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
