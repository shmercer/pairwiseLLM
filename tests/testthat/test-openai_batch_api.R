testthat::test_that("openai_upload_batch_file errors on missing file", {
  nonexistent <- tempfile(fileext = ".jsonl")
  testthat::expect_false(file.exists(nonexistent))

  testthat::expect_error(
    openai_upload_batch_file(nonexistent),
    "File does not exist"
  )
})

testthat::test_that("openai_download_batch_output errors if no output_file_id", {
  fake_batch <- list(
    id             = "batch_123",
    status         = "completed",
    output_file_id = NULL
  )

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) fake_batch,
    {
      tf <- tempfile(fileext = ".jsonl")
      testthat::expect_error(
        openai_download_batch_output("batch_123", tf),
        "has no output_file_id"
      )
    }
  )
})

testthat::test_that("openai_poll_batch_until_complete succeeds after several polls", {
  fake_batches <- list(
    list(id = "batch_123", status = "in_progress"),
    list(id = "batch_123", status = "in_progress"),
    list(id = "batch_123", status = "completed", output_file_id = "file_out_123")
  )
  i <- 0L

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) {
      i <<- i + 1L
      fake_batches[[i]]
    },
    {
      res <- openai_poll_batch_until_complete(
        batch_id         = "batch_123",
        interval_seconds = 0,   # no sleep in tests
        timeout_seconds  = 60,
        max_attempts     = 5,
        verbose          = FALSE
      )
      testthat::expect_equal(res$status, "completed")
      testthat::expect_equal(i, 3L)
    }
  )
})

testthat::test_that("openai_poll_batch_until_complete stops at max_attempts", {
  fake_batch <- list(id = "batch_123", status = "in_progress")
  i <- 0L

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) {
      i <<- i + 1L
      fake_batch
    },
    {
      testthat::expect_error(
        openai_poll_batch_until_complete(
          batch_id         = "batch_123",
          interval_seconds = 0,   # avoid sleeping in tests
          timeout_seconds  = 60,
          max_attempts     = 3,
          verbose          = FALSE
        ),
        "Reached max_attempts"
      )
      testthat::expect_equal(i, 3L)
    }
  )
})
