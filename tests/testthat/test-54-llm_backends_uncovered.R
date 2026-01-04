test_that("54-01 llm_compare_pair normalizes empty api_key to NULL", {
  captured_key <- "__unset__"

  out <- testthat::with_mocked_bindings(
    pairwiseLLM::llm_compare_pair(
      ID1 = "a",
      text1 = "A",
      ID2 = "b",
      text2 = "B",
      trait_name = "overall",
      trait_description = "overall quality",
      backend = "openai",
      api_key = "",
      endpoint = "responses",
      model = "gpt-4o-mini",
      max_retries = 0
    ),
    openai_compare_pair_live = function(api_key, ...) {
      captured_key <<- api_key
      tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")
    }
  )

  expect_null(captured_key)
  expect_equal(out$better_id, "a")
})

test_that("54-02 submit_llm_pairs normalizes empty api_key to NULL", {
  captured_key <- "__unset__"

  pairs <- tibble::tibble(
    ID1 = c("a", "a"),
    ID2 = c("b", "c"),
    text1 = c("A", "A"),
    text2 = c("B", "C")
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM::submit_llm_pairs(
      pairs = pairs,
      trait_name = "overall",
      trait_description = "overall quality",
      backend = "openai",
      api_key = "",
      endpoint = "chat.completions",
      model = "gpt-4o-mini",
      max_retries = 0,
      max_concurrent = 1
    ),
    submit_openai_pairs_live = function(api_key, ...) {
      captured_key <<- api_key
      list(
        results = tibble::tibble(
          ID1 = pairs$ID1,
          ID2 = pairs$ID2,
          better_id = pairs$ID1
        ),
        failed_pairs = tibble::tibble(),
        raw_responses = tibble::tibble()
      )
    }
  )

  expect_null(captured_key)
  expect_true(is.list(out))
  expect_equal(nrow(out$results), nrow(pairs))
})

test_that("54-03 llm_compare_pair errors for an unimplemented backend", {
  testthat::with_mocked_bindings(
    .normalize_backend_arg = function(...) "not_implemented",
    .package = "pairwiseLLM",
    {
      expect_error(
        pairwiseLLM::llm_compare_pair(
          ID1 = "a",
          text1 = "A",
          ID2 = "b",
          text2 = "B",
          trait_name = "overall",
          trait_description = "overall quality",
          backend = "openai",
          api_key = "x",
          model = "gpt-4o-mini"
        ),
        "Backend `not_implemented` is not implemented yet.",
        fixed = TRUE
      )
    }
  )
})

test_that("54-04 submit_llm_pairs errors for an unimplemented backend", {
  pairs <- tibble::tibble(ID1 = "a", ID2 = "b", text1 = "A", text2 = "B")

  testthat::with_mocked_bindings(
    .normalize_backend_arg = function(...) "not_implemented",
    .package = "pairwiseLLM",
    {
      expect_error(
        pairwiseLLM::submit_llm_pairs(
          pairs = pairs,
          trait_name = "overall",
          trait_description = "overall quality",
          backend = "openai",
          api_key = "x",
          model = "gpt-4o-mini"
        ),
        "Backend `not_implemented` is not implemented yet.",
        fixed = TRUE
      )
    }
  )
})
