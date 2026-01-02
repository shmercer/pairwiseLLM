testthat::test_that("parse_gemini_batch_output handles succeeded + processing and detects include_thoughts from request", {
  req_tbl <- tibble::tibble(
    custom_id = c("A_vs_B", "C_vs_D"),
    ID1 = c("A", "C"),
    ID2 = c("B", "D"),
    request = list(
      list(generationConfig = list(thinkingConfig = list(includeThoughts = TRUE))),
      list(generationConfig = list(thinkingConfig = list(includeThoughts = FALSE)))
    )
  )

  succeeded_obj <- list(
    custom_id = "A_vs_B",
    result = list(
      type = "succeeded",
      response = list(
        candidates = list(
          list(content = list(parts = list(
            list(text = "THOUGHTS", thoughtSignature = "sig"),
            list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
          )))
        ),
        usageMetadata = list(
          promptTokenCount = 10,
          candidatesTokenCount = 5,
          totalTokenCount = 15,
          cachedContentTokenCount = 2
        )
      )
    )
  )

  processing_obj <- list(
    custom_id = "C_vs_D",
    result = list(type = "processing")
  )

  tmp <- tempfile(fileext = ".jsonl")
  writeLines(c(
    jsonlite::toJSON(succeeded_obj, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(processing_obj, auto_unbox = TRUE, null = "null")
  ), tmp)

  out <- pairwiseLLM::parse_gemini_batch_output(tmp, req_tbl)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 2)

  row1 <- out[out$custom_id == "A_vs_B", , drop = FALSE]
  testthat::expect_equal(row1$result_type, "succeeded")
  testthat::expect_equal(row1$better_sample, "SAMPLE_2")
  testthat::expect_equal(row1$better_id, "B")
  testthat::expect_equal(row1$thoughts, "THOUGHTS")
  testthat::expect_equal(row1$content, "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")

  row2 <- out[out$custom_id == "C_vs_D", , drop = FALSE]
  testthat::expect_equal(row2$result_type, "processing")
  testthat::expect_true(is.na(row2$better_sample))
  testthat::expect_true(is.na(row2$content))
})
