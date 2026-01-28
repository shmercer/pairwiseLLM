run_with_adaptive_mocks <- function(expr, mock_start = NULL, mock_resume = NULL) {
  expr <- substitute(expr)
  ns_env <- asNamespace("pairwiseLLM")
  global_env <- .GlobalEnv
  orig_start <- get("adaptive_rank_start", envir = ns_env)
  orig_resume <- get("adaptive_rank_resume", envir = ns_env)
  orig_start_global <- get("adaptive_rank_start", envir = global_env)
  orig_resume_global <- get("adaptive_rank_resume", envir = global_env)
  start_locked <- bindingIsLocked("adaptive_rank_start", ns_env)
  resume_locked <- bindingIsLocked("adaptive_rank_resume", ns_env)
  on.exit({
    if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
    if (resume_locked) unlockBinding("adaptive_rank_resume", ns_env)
    assign("adaptive_rank_start", orig_start, envir = ns_env)
    assign("adaptive_rank_resume", orig_resume, envir = ns_env)
    if (start_locked) lockBinding("adaptive_rank_start", ns_env)
    if (resume_locked) lockBinding("adaptive_rank_resume", ns_env)
    assign("adaptive_rank_start", orig_start_global, envir = global_env)
    assign("adaptive_rank_resume", orig_resume_global, envir = global_env)
  }, add = TRUE)
  if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
  if (resume_locked) unlockBinding("adaptive_rank_resume", ns_env)
  if (!is.null(mock_start)) assign("adaptive_rank_start", mock_start, envir = ns_env)
  if (!is.null(mock_resume)) assign("adaptive_rank_resume", mock_resume, envir = ns_env)
  if (start_locked) lockBinding("adaptive_rank_start", ns_env)
  if (resume_locked) lockBinding("adaptive_rank_resume", ns_env)
  eval(expr, parent.frame())
}

testthat::test_that("adaptive_rank_run_batch filters polling args and loops until stop", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  capture_env <- new.env(parent = emptyenv())
  mock_start <- function(samples,
                         model,
                         trait_name,
                         trait_description,
                         prompt_template,
                         backend,
                         mode,
                         submission,
                         adaptive,
                         paths,
                         seed) {
    capture_env$start_submission <- submission
    list(
      state = list(step = 1L),
      submission_info = list(token = "job-1"),
      next_action = list(action = "resume")
    )
  }

  mock_resume <- function(state, mode, submission_info, submission, adaptive, seed) {
    capture_env$resume_submission <- submission
    list(
      state = list(step = 2L),
      submission_info = submission_info,
      next_action = list(action = "stop")
    )
  }

  run_with_mocks <- function(expr) {
    expr <- substitute(expr)
    ns_env <- asNamespace("pairwiseLLM")
    global_env <- .GlobalEnv
    orig_start <- get("adaptive_rank_start", envir = ns_env)
    orig_resume <- get("adaptive_rank_resume", envir = ns_env)
    orig_start_global <- get("adaptive_rank_start", envir = global_env)
    orig_resume_global <- get("adaptive_rank_resume", envir = global_env)
    start_locked <- bindingIsLocked("adaptive_rank_start", ns_env)
    resume_locked <- bindingIsLocked("adaptive_rank_resume", ns_env)
    on.exit({
      if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
      if (resume_locked) unlockBinding("adaptive_rank_resume", ns_env)
      assign("adaptive_rank_start", orig_start, envir = ns_env)
      assign("adaptive_rank_resume", orig_resume, envir = ns_env)
      if (start_locked) lockBinding("adaptive_rank_start", ns_env)
      if (resume_locked) lockBinding("adaptive_rank_resume", ns_env)
      assign("adaptive_rank_start", orig_start_global, envir = global_env)
      assign("adaptive_rank_resume", orig_resume_global, envir = global_env)
    }, add = TRUE)
    if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
    if (resume_locked) unlockBinding("adaptive_rank_resume", ns_env)
    assign("adaptive_rank_start", mock_start, envir = ns_env)
    assign("adaptive_rank_resume", mock_resume, envir = ns_env)
    if (start_locked) lockBinding("adaptive_rank_start", ns_env)
    if (resume_locked) lockBinding("adaptive_rank_resume", ns_env)
    eval(expr, parent.frame())
  }

  withr::local_seed(101)
  out <- run_with_mocks(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "gemini",
      submission = list(
        n_segments = 5L,
        progress = TRUE,
        interval_seconds = 10,
        per_job_delay = 1,
        verbose = TRUE
      ),
      max_iterations = 3L,
      seed = 101
    )
  )

  expect_equal(out$iterations, 2L)
  expect_equal(out$state$step, 2L)
  expect_equal(capture_env$start_submission$n_segments, 1L)
  expect_null(capture_env$resume_submission$n_segments)
  expect_null(capture_env$resume_submission$progress)
  expect_equal(capture_env$resume_submission$interval_seconds, 10)
  expect_equal(capture_env$resume_submission$per_job_delay, 1)
  expect_true(capture_env$resume_submission$verbose)
})

testthat::test_that("adaptive_rank_run_batch coerces non-list submission to list", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  capture_env <- new.env(parent = emptyenv())
  mock_start <- function(samples,
                         model,
                         trait_name,
                         trait_description,
                         prompt_template,
                         backend,
                         mode,
                         submission,
                         adaptive,
                         paths,
                         seed) {
    capture_env$start_submission <- submission
    list(
      state = list(step = 1L),
      submission_info = list(token = "job-1"),
      next_action = list(action = "stop")
    )
  }

  run_with_mocks <- function(expr) {
    expr <- substitute(expr)
    ns_env <- asNamespace("pairwiseLLM")
    global_env <- .GlobalEnv
    orig_start <- get("adaptive_rank_start", envir = ns_env)
    orig_start_global <- get("adaptive_rank_start", envir = global_env)
    start_locked <- bindingIsLocked("adaptive_rank_start", ns_env)
    on.exit({
      if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
      assign("adaptive_rank_start", orig_start, envir = ns_env)
      if (start_locked) lockBinding("adaptive_rank_start", ns_env)
      assign("adaptive_rank_start", orig_start_global, envir = global_env)
    }, add = TRUE)
    if (start_locked) unlockBinding("adaptive_rank_start", ns_env)
    assign("adaptive_rank_start", mock_start, envir = ns_env)
    if (start_locked) lockBinding("adaptive_rank_start", ns_env)
    eval(expr, parent.frame())
  }

  withr::local_seed(202)
  out <- run_with_mocks(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "gemini",
      submission = "nope",
      max_iterations = 1L,
      seed = 202
    )
  )

  expect_equal(out$iterations, 1L)
  expect_equal(capture_env$start_submission, list(n_segments = 1L))
})

testthat::test_that("adaptive_rank_run_batch validates max_iterations", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  testthat::expect_error(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "gemini",
      max_iterations = 0L
    ),
    "`max_iterations` must be a positive integer\\."
  )
})

testthat::test_that("adaptive_rank_run_batch accepts Inf max_iterations", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  out <- run_with_adaptive_mocks(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "gemini",
      max_iterations = Inf
    ),
    mock_start = function(...) {
      list(
        state = list(step = 1L),
        submission_info = list(),
        next_action = list(action = "resume")
      )
    },
    mock_resume = function(...) {
      list(
        state = list(step = 2L),
        submission_info = list(),
        next_action = list(action = "stop")
      )
    }
  )

  expect_equal(out$iterations, 2L)
  expect_equal(out$state$step, 2L)
})

testthat::test_that("adaptive_rank_run_batch resumes from checkpoint and continues log numbering", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  withr::local_tempdir()
  output_dir <- file.path(tempdir(), "adaptive-run-batch")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path(output_dir, "adaptive_state.rds")

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  batch_row <- pairwiseLLM:::.adaptive_batch_log_defaults()
  batch_row$iter <- 1L
  batch_row$phase <- "phase1"
  batch_row$mode <- "warm_start"
  batch_row$created_at <- as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
  state$batch_log <- dplyr::bind_rows(state$batch_log, batch_row)

  round_row <- pairwiseLLM:::.adaptive_round_log_defaults()
  round_row$iter <- 1L
  round_row$phase <- "phase1"
  round_row$created_at <- as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
  state$config$round_log <- dplyr::bind_rows(state$config$round_log, round_row)

  state$config$output_dir <- output_dir
  state$config$state_path <- state_path
  pairwiseLLM:::adaptive_state_save(state, state_path)

  mock_start <- function(...) {
    testthat::fail("adaptive_rank_start should not be called when checkpoint exists.")
  }

  checkpoint_path <- state_path
  mock_resume <- function(state = NULL, state_path = NULL, mode, submission_info, submission, adaptive, seed) {
    testthat::expect_equal(mode, "batch")
    testthat::expect_equal(state_path, checkpoint_path)
    testthat::expect_equal(submission_info$output_dir, output_dir)

    st0 <- pairwiseLLM:::adaptive_state_load(state_path)
    new_batch <- pairwiseLLM:::.adaptive_batch_log_defaults()
    new_batch$iter <- max(st0$batch_log$iter, na.rm = TRUE) + 1L
    new_batch$phase <- "phase2"
    new_batch$mode <- "adaptive"
    new_batch$created_at <- as.POSIXct("2026-01-03 00:00:00", tz = "UTC")
    st0$batch_log <- dplyr::bind_rows(st0$batch_log, new_batch)

    new_round <- pairwiseLLM:::.adaptive_round_log_defaults()
    new_round$iter <- max(st0$config$round_log$iter, na.rm = TRUE) + 1L
    new_round$phase <- "phase2"
    new_round$created_at <- as.POSIXct("2026-01-03 00:00:00", tz = "UTC")
    st0$config$round_log <- dplyr::bind_rows(st0$config$round_log, new_round)

    list(
      state = st0,
      submission_info = submission_info,
      next_action = list(action = "stop")
    )
  }

  out <- run_with_adaptive_mocks(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      paths = list(state_path = state_path, output_dir = output_dir),
      max_iterations = Inf
    ),
    mock_start = mock_start,
    mock_resume = mock_resume
  )

  expect_equal(out$iterations, 1L)
  expect_equal(out$state$batch_log$iter, c(1L, 2L))
  expect_equal(out$state$config$round_log$iter, c(1L, 2L))
})

testthat::test_that("adaptive_rank_run_batch refuses incompatible checkpoints", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  withr::local_tempdir()
  output_dir <- file.path(tempdir(), "adaptive-run-batch")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path(output_dir, "adaptive_state.rds")

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state$config$output_dir <- output_dir
  state$config$state_path <- state_path
  pairwiseLLM:::adaptive_state_save(state, state_path)

  bad_samples <- tibble::tibble(
    ID = c("B", "A"),
    text = c("bravo", "alpha")
  )

  testthat::expect_error(
    run_with_adaptive_mocks(
      pairwiseLLM::adaptive_rank_run_batch(
        samples = bad_samples,
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        backend = "openai",
        paths = list(state_path = state_path, output_dir = output_dir)
      ),
      mock_start = function(...) {
        testthat::fail("adaptive_rank_start should not be called on mismatch.")
      },
      mock_resume = function(...) {
        testthat::fail("adaptive_rank_resume should not be called on mismatch.")
      }
    ),
    "samples\\$ID"
  )
})
