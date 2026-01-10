# =====================================================================
# test-5625-openai_compare_pair_live_reasoning_parsing.R
#
# IMPORTANT:
# This file exists to overwrite an earlier, brittle version that called
# `openai_compare_pair_live()` directly.
#
# In this repo's test suite, `openai_compare_pair_live()` is sometimes
# mocked to throw ("Sequential Internal Crash") when validating wrapper
# behavior. Direct-calling tests are therefore not deterministic across the
# full suite.
#
# These tests instead target the internal parser that implements the branchy
# logic needed for coverage: `.openai_live_extract_thoughts_content()`.
# =====================================================================

testthat::test_that(".openai_live_extract_thoughts_content parses dataframe reasoning summaries", {
  body <- list(
    object = "response",
    reasoning_summary = data.frame(text = "DF Summary", stringsAsFactors = FALSE),
    output = list(
      list(
        type = "message",
        content = list(list(type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"))
      )
    )
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_equal(parsed$thoughts, "DF Summary")
  testthat::expect_match(parsed$content, "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
})

testthat::test_that(".openai_live_extract_thoughts_content parses list reasoning summaries", {
  body <- list(
    object = "response",
    reasoning_summary = list(text = "List Summary"),
    output = list(
      list(
        type = "message",
        content = list(list(type = "output_text", text = "Hello"))
      )
    )
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_equal(parsed$thoughts, "List Summary")
  testthat::expect_equal(parsed$content, "Hello")
})

testthat::test_that(".openai_live_extract_thoughts_content falls back to legacy reasoning fields", {
  body <- list(
    object = "response",
    reasoning = list(summary = list(text = "Legacy summary.")),
    output_text = "Fallback content"
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_equal(parsed$thoughts, "Legacy summary.")
  testthat::expect_equal(parsed$content, "Fallback content")
})

testthat::test_that(".openai_live_extract_thoughts_content handles response message items without explicit type", {
  body <- list(
    object = "response",
    output = list(
      list(
        # Some payloads omit `type`; treat content with output_text as a message.
        content = list(
          list(type = "output_text", text = "visible "),
          list(type = "output_text", text = "text")
        )
      )
    )
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_true(is.na(parsed$thoughts))
  testthat::expect_equal(parsed$content, "visible text")
})

testthat::test_that(".openai_live_extract_thoughts_content reads reasoning summaries from output reasoning items", {
  body <- list(
    object = "response",
    output = list(
      list(
        type = "reasoning",
        summary = list(
          list(type = "summary_text", text = "Reasoning sentence 1."),
          list(type = "summary_text", text = "Reasoning sentence 2.")
        )
      ),
      list(
        type = "message",
        content = list(list(type = "output_text", text = "visible"))
      )
    )
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_match(parsed$thoughts, "Reasoning sentence 1\\.")
  testthat::expect_match(parsed$thoughts, "Reasoning sentence 2\\.")
  testthat::expect_equal(parsed$content, "visible")
})

testthat::test_that(".openai_live_extract_thoughts_content reads dataframe reasoning summaries from output items", {
  body <- list(
    object = "response",
    output = list(
      list(
        type = "reasoning",
        summary = data.frame(text = "DF Summary", stringsAsFactors = FALSE)
      ),
      list(
        type = "message",
        content = list(list(type = "output_text", text = "visible"))
      )
    )
  )

  parsed <- pairwiseLLM:::.openai_live_extract_thoughts_content(body)
  testthat::expect_equal(parsed$thoughts, "DF Summary")
  testthat::expect_equal(parsed$content, "visible")
})
