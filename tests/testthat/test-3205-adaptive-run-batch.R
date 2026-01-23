testthat::test_that("adaptive_rank_run_batch filters polling args and loops until stop", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  captured <- list()
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
    captured$start_submission <<- submission
    list(
      state = list(step = 1L),
      submission_info = list(token = "job-1"),
      next_action = list(action = "resume")
    )
  }

  mock_resume <- function(state, mode, submission_info, submission, adaptive, seed) {
    captured$resume_submission <<- submission
    list(
      state = list(step = 2L),
      submission_info = submission_info,
      next_action = list(action = "stop")
    )
  }

  withr::local_seed(101)
  out <- testthat::with_mocked_bindings(
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
    ),
    adaptive_rank_start = mock_start,
    adaptive_rank_resume = mock_resume,
    .env = asNamespace("pairwiseLLM")
  )

  expect_equal(out$iterations, 2L)
  expect_equal(out$state$step, 2L)
  expect_equal(captured$start_submission$n_segments, 1L)
  expect_null(captured$resume_submission$n_segments)
  expect_null(captured$resume_submission$progress)
  expect_equal(captured$resume_submission$interval_seconds, 10)
  expect_equal(captured$resume_submission$per_job_delay, 1)
  expect_true(captured$resume_submission$verbose)
})

testthat::test_that("adaptive_rank_run_batch coerces non-list submission to list", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )

  captured <- list()
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
    captured$start_submission <<- submission
    list(
      state = list(step = 1L),
      submission_info = list(token = "job-1"),
      next_action = list(action = "stop")
    )
  }

  withr::local_seed(202)
  out <- testthat::with_mocked_bindings(
    pairwiseLLM::adaptive_rank_run_batch(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "gemini",
      submission = "nope",
      max_iterations = 1L,
      seed = 202
    ),
    adaptive_rank_start = mock_start,
    .env = asNamespace("pairwiseLLM")
  )

  expect_equal(out$iterations, 1L)
  expect_equal(captured$start_submission, list(n_segments = 1L))
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
