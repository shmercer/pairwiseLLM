test_that(".openai_live_extract_thoughts_content returns a stable list", {
  body1 <- list(
    object = "chat.completion",
    choices = list(list(message = list(content = "hi")))
  )
  out1 <- pairwiseLLM:::.openai_live_extract_thoughts_content(body1)
  expect_named(out1, c("thoughts", "content"))
  expect_true(is.list(out1))
  expect_true(is.na(out1$thoughts))
  expect_equal(out1$content, "hi")

  body2 <- list(
    object = "response",
    output = list(
      list(
        type = "message",
        content = list(
          list(type = "output_text", text = "a"),
          list(type = "output_text", text = "b")
        )
      )
    )
  )
  out2 <- pairwiseLLM:::.openai_live_extract_thoughts_content(body2)
  expect_true(is.na(out2$thoughts))
  expect_equal(out2$content, "ab")

  body3 <- list(object = "response")
  out3 <- pairwiseLLM:::.openai_live_extract_thoughts_content(body3)
  expect_true(is.na(out3$thoughts))
  expect_true(is.na(out3$content))
})

test_that("submit_openai_pairs_live handles empty inputs deterministically", {
  pairs0 <- tibble::tibble(
    ID1 = character(),
    text1 = character(),
    ID2 = character(),
    text2 = character()
  )

  out <- pairwiseLLM::submit_openai_pairs_live(
    pairs = pairs0,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    api_key = "k",
    include_raw = TRUE,
    verbose = FALSE,
    progress = FALSE
  )

  expect_true(is.list(out))
  expect_true(all(c("results", "failed_pairs") %in% names(out)))
  expect_s3_class(out$results, "tbl_df")
  expect_equal(nrow(out$results), 0L)
  expect_true("raw_response" %in% names(out$results))
  expect_equal(nrow(out$failed_pairs), 0L)
})

test_that("submit_openai_pairs_live runs sequentially with a local mock", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "t1",
    ID2 = "B",
    text2 = "t2"
  )

  fake_row <- tibble::tibble(
    model = "m",
    object_type = "chat.completion",
    status_code = 200L,
    error_message = NA_character_,
    thoughts = NA_character_,
    content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample = "SAMPLE_1",
    better_id = "A",
    prompt_tokens = 1,
    completion_tokens = 1,
    total_tokens = 2
  )

  testthat::local_mocked_bindings(
    openai_compare_pair_live = function(...) fake_row,
    .env = asNamespace("pairwiseLLM")
  )

  out <- pairwiseLLM::submit_openai_pairs_live(
    pairs = pairs,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    api_key = "k",
    verbose = FALSE,
    progress = FALSE
  )

  expect_equal(nrow(out$results), 1L)
  expect_equal(out$results$better_id[[1]], "A")
  expect_true(is.character(out$results$custom_id[[1]]))
})
