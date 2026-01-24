# =====================================================================
# test-4004-llm-submit-pairs-batch-openai-gpt5.R
# OpenAI batch endpoint selection for GPT-5 series
# =====================================================================

testthat::test_that("llm_submit_pairs_batch selects responses for GPT-5 minimal", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(..., endpoint) {
      list(endpoint = endpoint, results = NULL)
    },
    .env = asNamespace("pairwiseLLM"),
    {
      out <- pairwiseLLM::llm_submit_pairs_batch(
        pairs = pairs,
        backend = "openai",
        model = "gpt-5",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        include_thoughts = FALSE,
        reasoning = "none"
      )
      testthat::expect_equal(out$endpoint, "responses")
    }
  )
})

testthat::test_that("llm_submit_pairs_batch keeps chat.completions for GPT-5.1 none", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(..., endpoint) {
      list(endpoint = endpoint, results = NULL)
    },
    .env = asNamespace("pairwiseLLM"),
    {
      out <- pairwiseLLM::llm_submit_pairs_batch(
        pairs = pairs,
        backend = "openai",
        model = "gpt-5.1-2025-12-11",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        include_thoughts = FALSE,
        reasoning = "none"
      )
      testthat::expect_equal(out$endpoint, "chat.completions")
    }
  )
})

testthat::test_that("llm_submit_pairs_batch selects responses when thoughts requested", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(..., endpoint) {
      list(endpoint = endpoint, results = NULL)
    },
    .env = asNamespace("pairwiseLLM"),
    {
      out <- pairwiseLLM::llm_submit_pairs_batch(
        pairs = pairs,
        backend = "openai",
        model = "gpt-5.1-2025-12-11",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        include_thoughts = TRUE
      )
      testthat::expect_equal(out$endpoint, "responses")
    }
  )
})
