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
