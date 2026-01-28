epsilon_summary_fixture <- function(mean = 0.1) {
  tibble::tibble(
    epsilon_mean = mean,
    epsilon_p2.5 = 0.01,
    epsilon_p5 = 0.02,
    epsilon_p50 = mean,
    epsilon_p95 = 0.2,
    epsilon_p97.5 = 0.21
  )
}

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

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary_fixture(),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        min_ess_tail = 1000
      ),
      model_variant = model_variant
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
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  expect_equal(out$state$comparisons_observed, 3L)
  expect_equal(out$state$comparisons_scheduled, 3L)
  expect_equal(nrow(out$state$failed_attempts), 1L)
  expect_equal(length(out$submission_info$live_submissions), 1L)
})

testthat::test_that("adaptive_rank_start saves live state when state_path is provided", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 2L,
    budget_max = 6L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 2L)
  )

  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    results <- tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
      ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-10 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )

    list(
      results = results,
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary_fixture(),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        min_ess_tail = 1000
      ),
      model_variant = model_variant
    )
  }

  out_dir <- withr::local_tempdir()
  state_path <- file.path(out_dir, "adaptive_state.rds")
  withr::local_seed(202)
  out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = adaptive,
      paths = list(output_dir = out_dir, state_path = state_path),
      seed = 202
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  expect_true(file.exists(state_path))
  expect_identical(out$state$config$state_path, state_path)
})

testthat::test_that("adaptive_rank_resume skips rollback when live results are absent", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L),
    seed = 1L
  )

  pairs_submitted <- tibble::tibble(
    pair_uid = "A:B#1",
    A_id = "A",
    B_id = "B"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      adaptive_rank_resume(
        state = state,
        mode = "live",
        submission_info = list(
          backend = "openai",
          model = "gpt-test",
          trait_name = "quality",
          trait_description = "Which is better?",
          prompt_template = "template",
          pairs_submitted = pairs_submitted
        ),
        adaptive = list(d1 = 2L),
        seed = 1L
      ),
      .adaptive_run_stopping_checks = function(state, adaptive, seed) {
        list(state = state)
      },
      .adaptive_schedule_target = function(state, adaptive) {
        list(state = state, target = 0L)
      }
    ),
    NA
  )
})

testthat::test_that("adaptive_rank_resume saves live state when configured", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L),
    seed = 1L
  )
  out_dir <- withr::local_tempdir()
  save_path <- file.path(out_dir, "adaptive_state.rds")
  state$config$state_path <- save_path

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
        pairs_submitted = pairwiseLLM:::.adaptive_empty_pairs_tbl()
      ),
      adaptive = list(d1 = 2L),
      seed = 1L
    ),
    .adaptive_run_stopping_checks = function(state, adaptive, seed) {
      list(state = state)
    },
    .adaptive_schedule_target = function(state, adaptive) {
      list(state = state, target = 0L)
    }
  )

  expect_true(file.exists(save_path))
  expect_identical(out$state_path, save_path)
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

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary_fixture(),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        min_ess_tail = 1000
      ),
      model_variant = model_variant
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
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
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
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
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
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  expect_equal(resume_out2$state$comparisons_observed, 4L)
})

testthat::test_that("adaptive_rank_resume filters batch polling args to formals", {
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

  called <- list()
  mock_resume_batch <- function(
      jobs = NULL,
      output_dir = NULL,
      interval_seconds = 60,
      per_job_delay = 2,
      write_results_csv = FALSE,
      keep_jsonl = TRUE,
      write_registry = FALSE,
      tag_prefix = "<BETTER_SAMPLE>",
      tag_suffix = "</BETTER_SAMPLE>",
      verbose = FALSE,
      write_combined_csv = FALSE,
      combined_csv_path = NULL,
      openai_max_retries = 3
  ) {
    called <<- list(
      interval_seconds = interval_seconds,
      per_job_delay = per_job_delay,
      verbose = verbose
    )
    results <- make_results(last_pairs, "openai", "gpt-test")
    list(
      jobs = jobs,
      combined = results,
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
      batch_failures = tibble::tibble()
    )
  }

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary_fixture(),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        min_ess_tail = 1000
      ),
      model_variant = model_variant
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
      mode = "batch",
      submission = list(
        n_segments = 2L,
        progress = TRUE,
        interval_seconds = 5,
        per_job_delay = 1,
        verbose = TRUE
      ),
      adaptive = adaptive,
      paths = list(output_dir = withr::local_tempdir()),
      seed = 303
    ),
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = start_out$state,
      mode = "batch",
      submission_info = start_out$submission_info,
      submission = list(
        interval_seconds = 12,
        per_job_delay = 7,
        verbose = TRUE
      ),
      adaptive = adaptive,
      seed = 303
    ),
    llm_resume_multi_batches = mock_resume_batch,
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  expect_equal(resume_out$state$comparisons_observed, 4L)
  expect_equal(called$interval_seconds, 12)
  expect_equal(called$per_job_delay, 7)
  expect_true(called$verbose)
})

testthat::test_that("adaptive_rank_resume submits when scheduled pairs exist", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L),
    seed = 101
  )

  pairs_tbl <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  empty_results <- pairwiseLLM:::.adaptive_empty_results_tbl()
  empty_failed <- pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()

  out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
      prompt_template = "template"
    ),
    seed = 101
  ),
    .adaptive_run_stopping_checks = function(state, adaptive, seed) {
      list(state = state)
    },
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      state <- pairwiseLLM:::record_presentation(state, "A", "B")
      state$history_pairs <- dplyr::bind_rows(state$history_pairs, pairs_tbl)
      state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
      list(state = state, pairs = pairs_tbl)
    },
    .adaptive_submit_live = function(...) list(results = tibble::tibble()),
    .adaptive_normalize_submission_output = function(...) {
      list(results = empty_results, failed_attempts = empty_failed)
    },
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
  )

  expect_equal(nrow(out$submission_info$pairs_submitted), 1L)
})

testthat::test_that("adaptive_rank_start stores submission options in state and reuse on resume", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 2L,
    budget_max = 4L,
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
    llm_submit_pairs_multi_batch = mock_submit_batch,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
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
    llm_submit_pairs_multi_batch = mock_submit_batch,
    llm_resume_multi_batches = mock_resume_batch,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
  )

  expect_equal(captured_resume$openai_max_retries, 7L)
  expect_true(isTRUE(captured_resume$verbose))
})

testthat::test_that("adaptive_rank_resume normalizes raw live results before ingesting", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 10L),
    seed = 777
  )

  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  scheduled <- pairwiseLLM:::.adaptive_schedule_warm_start(state, state$config$v3)
  state <- scheduled$state
  pairs_submitted <- scheduled$pairs

  raw_results <- tibble::tibble(
    custom_id = pairs_submitted$pair_uid,
    better_sample = "SAMPLE_1"
  )

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
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
    ),
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
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
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
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
    submit_llm_pairs = mock_submit_capture,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
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
    submit_llm_pairs = mock_submit_capture,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      force(config)
      force(seed)
      ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
      theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
      model_variant <- config$model_variant %||% "btl_e_b"
      epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
        rep(0.1, nrow(theta_draws))
      } else {
        NULL
      }
      beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
        rep(0, nrow(theta_draws))
      } else {
        NULL
      }
      list(
        draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
        epsilon_summary = epsilon_summary_fixture(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          min_ess_tail = 1000
        ),
        model_variant = model_variant
      )
    }
  )

  expect_true(length(captured_pairs) %in% c(0L, 2L))
  if (length(captured_pairs) == 2L) {
    expect_equal(captured_pairs[[1]], captured_pairs[[2]])
  }
})
