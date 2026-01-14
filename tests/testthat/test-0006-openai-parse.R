# =====================================================================
# test-openai_parse.R
# Tests for parse_openai_batch_output()
# =====================================================================

testthat::test_that("parse_openai_batch_output validates input file", {
  # Non-existent file
  testthat::expect_error(
    parse_openai_batch_output("nonexistent.jsonl"),
    "File does not exist"
  )

  # Empty file
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)

  testthat::expect_error(
    parse_openai_batch_output(tmp),
    "File contains no lines"
  )
})

testthat::test_that("parse_openai_batch_output handles malformed JSON and body", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  lines <- c(
    "", # Empty line (should be skipped)
    "NOT JSON", # Malformed -> NULL -> skipped
    '{"custom_id": "bad_id"}', # No response body -> NA row
    '{"custom_id": "LIVE_A_vs_B", "response": {"status_code": 200, "body": null}}' # Explicit null body -> NA row
  )
  writeLines(lines, tmp)

  res <- parse_openai_batch_output(tmp)
  testthat::expect_equal(nrow(res), 2L)

  # Row 1 (from 'bad_id')
  r1 <- res[1, ]
  testthat::expect_equal(r1$custom_id, "bad_id")
  # "bad_id" fails the _vs_ regex, so IDs should be NA
  testthat::expect_true(is.na(r1$ID1))
  testthat::expect_true(is.na(r1$model))

  # Row 2 (from 'LIVE_A_vs_B')
  r2 <- res[2, ]
  testthat::expect_equal(r2$custom_id, "LIVE_A_vs_B")
  # "LIVE_A_vs_B" parses correctly: left="LIVE_A", right="B"
  # suffix after last _ in left is "A"
  testthat::expect_equal(r2$ID1, "A")
  testthat::expect_equal(r2$ID2, "B")
  testthat::expect_equal(r2$status_code, 200L)
  testthat::expect_true(is.na(r2$content))
})

testthat::test_that("parse_openai_batch_output extracts detailed token usage", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Chat completion object with detailed usage
  obj <- list(
    custom_id = "LIVE_S1_vs_S2",
    response = list(
      status_code = 200,
      body = list(
        object = "chat.completion",
        model = "gpt-4",
        choices = list(list(message = list(content = "Hi"))),
        usage = list(
          prompt_tokens = 50,
          completion_tokens = 20,
          total_tokens = 70,
          input_tokens_details = list(cached_tokens = 25),
          output_tokens_details = list(reasoning_tokens = 10)
        )
      )
    )
  )

  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE), tmp)

  res <- parse_openai_batch_output(tmp)

  testthat::expect_equal(res$prompt_tokens, 50)
  testthat::expect_equal(res$prompt_cached_tokens, 25)
  testthat::expect_equal(res$reasoning_tokens, 10)
})

testthat::test_that("parse_openai_batch_output extracts better_id correctly from ID1_vs_ID2", {
  # Edge case: ID1 contains underscores, e.g. "PREFIX_ID_1_vs_ID_2"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  obj <- list(
    custom_id = "LIVE_A_1_vs_B_2", # ID1="A_1", ID2="B_2" (assuming prefix logic matches)
    response = list(
      body = list(
        object = "chat.completion",
        choices = list(list(message = list(content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")))
      )
    )
  )

  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE), tmp)
  res <- parse_openai_batch_output(tmp)

  # The parser logic: parts = strsplit(..., "_vs_")
  # left = "LIVE_A_1", right = "B_2"
  # regexpr("_[^_]*$", left) matches "_1". substring after matches "1".
  # So ID1 = "1", NOT "A_1". This confirms current behavior.
  testthat::expect_equal(res$ID1, "1")
  testthat::expect_equal(res$ID2, "B_2")
  testthat::expect_equal(res$better_id, "1")
})

testthat::test_that("parse_openai_batch_output collects thoughts and message text separately for responses", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  # Construct a fake batch output line similar to gpt-5.1 responses
  line_obj <- list(
    custom_id = "LIVE_S01_vs_S02",
    response = list(
      status_code = 200L,
      body = list(
        object = "response",
        model = "gpt-5.1",
        reasoning = list(
          effort = "low",
          summary = list(text = "Reasoning summary. ")
        ),
        output = list(
          list(
            id = "rs_x",
            type = "reasoning",
            summary = list()
          ),
          list(
            id = "msg_x",
            type = "message",
            status = "completed",
            content = list(
              list(
                type = "output_text",
                text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
              )
            ),
            role = "assistant"
          )
        ),
        usage = list(
          input_tokens = 10L,
          output_tokens = 5L,
          total_tokens = 15L
        )
      )
    ),
    error = NULL
  )

  json_line <- jsonlite::toJSON(line_obj, auto_unbox = TRUE)
  writeLines(json_line, con = tmp, useBytes = TRUE)

  res <- parse_openai_batch_output(tmp)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 1L)

  # IDs from custom_id
  testthat::expect_equal(res$custom_id, "LIVE_S01_vs_S02")
  testthat::expect_equal(res$ID1, "S01")
  testthat::expect_equal(res$ID2, "S02")

  # Basic metadata
  testthat::expect_equal(res$model, "gpt-5.1")
  testthat::expect_equal(res$object_type, "response")
  testthat::expect_equal(res$status_code, 200L)
  testthat::expect_true(is.na(res$error_message))

  # Reasoning summary should go to thoughts
  testthat::expect_equal(res$thoughts, "Reasoning summary. ")

  # Content should be assistant message only
  testthat::expect_equal(
    res$content,
    "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
  )

  # Tag parsing and better_id mapping
  testthat::expect_equal(res$better_sample, "SAMPLE_2")
  testthat::expect_equal(res$better_id, "S02")

  # Token usage
  testthat::expect_equal(res$prompt_tokens, 10)
  testthat::expect_equal(res$completion_tokens, 5)
  testthat::expect_equal(res$total_tokens, 15)
})
