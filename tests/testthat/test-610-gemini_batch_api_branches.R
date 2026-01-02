test_that(".parse_gemini_pair_response handles empty data-frame candidates", {
  resp <- list(
    candidates = data.frame(),
    usageMetadata = list(promptTokenCount = 1, candidatesTokenCount = 2)
  )

  row <- pairwiseLLM:::.parse_gemini_pair_response(
    custom_id = "c1",
    ID1 = "A",
    ID2 = "B",
    response = resp,
    include_thoughts = FALSE
  )

  expect_s3_class(row, "tbl_df")
  expect_true(row$result_type %in% c("succeeded", "ok"))
  expect_true(is.na(row$content))
  expect_true(is.na(row$better_id))
})

test_that(".parse_gemini_pair_response: error-shaped response + include_thoughts split", {
  # Error-shaped response
  resp_err <- list(error = list(message = "boom"))
  row_err <- pairwiseLLM:::.parse_gemini_pair_response(
    custom_id = "c2",
    ID1 = "A",
    ID2 = "B",
    response = resp_err
  )
  expect_identical(row_err$result_type, "errored")
  expect_match(row_err$error_message, "boom")

  # Two-part response; when include_thoughts=TRUE, first part is thoughts
  resp_ok <- list(
    response = list(
      list(
        candidates = list(
          list(
            model = "gemini",
            content = list(
              parts = list(
                list(text = "THINK", thoughtSignature = "sig"),
                list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"),
                list(text = "X")
              )
            )
          )
        ),
        usageMetadata = list(promptTokenCount = 3, candidatesTokenCount = 4, thoughtsTokenCount = 5)
      )
    )
  )

  row_ok <- pairwiseLLM:::.parse_gemini_pair_response(
    custom_id = "c3",
    ID1 = "A",
    ID2 = "B",
    response = resp_ok,
    include_thoughts = TRUE
  )

  expect_identical(row_ok$thought_signature, "sig")
  expect_identical(row_ok$thoughts, "THINK")
  expect_match(row_ok$content, "<BETTER_SAMPLE>")
  expect_identical(row_ok$better_sample, "SAMPLE_1")
  expect_identical(row_ok$better_id, "A")
  expect_identical(row_ok$prompt_tokens, 3)
  expect_identical(row_ok$completion_tokens, 4)
})
