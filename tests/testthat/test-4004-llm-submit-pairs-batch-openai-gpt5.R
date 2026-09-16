# =====================================================================
# test-4004-llm-submit-pairs-batch-openai-gpt5.R
# OpenAI batch endpoint selection for GPT-5 series
# =====================================================================

testthat::test_that("OpenAI batch wrappers forward controls into real JSONL requests", {
  pairs <- tibble::tibble(ID1 = c("A", "C"), text1 = "First", ID2 = c("B", "D"), text2 = "Second")
  directory <- withr::local_tempdir()
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    openai_upload_batch_file = function(path, api_key) {
      captured$requests <- lapply(readLines(path), jsonlite::fromJSON)
      list(id = "file-synthetic")
    },
    openai_create_batch = function(input_file_id, endpoint, ...) {
      testthat::expect_identical(input_file_id, "file-synthetic")
      captured$endpoint <- endpoint
      list(id = "batch-synthetic", status = "validating")
    },
    .package = "pairwiseLLM"
  )
  for (generic in c(FALSE, TRUE)) {
    for (endpoint in c("chat.completions", "responses")) {
      for (store in c(FALSE, TRUE)) {
        args <- list(pairs = pairs, model = "gpt-5.6-terra", trait_name = "Cohesion",
                     trait_description = "Connections", endpoint = endpoint, reasoning = "none",
                     store = store, poll = FALSE, batch_input_path = file.path(directory, "input.jsonl"))
        if (endpoint == "responses") args$max_output_tokens <- 64
        if (generic) args$backend <- "openai"
        fun <- if (generic) pairwiseLLM::llm_submit_pairs_batch else pairwiseLLM::run_openai_batch_pipeline
        result <- do.call(fun, args)
        testthat::expect_identical(result$batch$id, "batch-synthetic")
        expected_endpoint <- if (endpoint == "responses") "/v1/responses" else "/v1/chat/completions"
        testthat::expect_identical(captured$endpoint, expected_endpoint)
        testthat::expect_length(captured$requests, 2L)
        for (request in captured$requests) {
          testthat::expect_identical(request$body$store, store)
          testthat::expect_identical(request$url, expected_endpoint)
          if (endpoint == "responses") {
            testthat::expect_identical(request$body$max_output_tokens, 64L)
            testthat::expect_identical(request$body$reasoning$effort, "none")
          } else {
            testthat::expect_false("max_output_tokens" %in% names(request$body))
          }
        }
      }
    }
  }
})

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
    .package = "pairwiseLLM",
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
    .package = "pairwiseLLM",
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
    .package = "pairwiseLLM",
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

testthat::test_that("llm_submit_pairs_batch selects responses for GPT-5.6 named tiers", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()
  captured <- rlang::env(seen = character())

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(model, ..., endpoint) {
      captured$seen <- c(captured$seen, model)
      list(endpoint = endpoint, results = NULL)
    },
    .package = "pairwiseLLM",
    {
      for (model in c("gpt-5.6-sol", "gpt-5.6-terra", "gpt-5.6-luna")) {
        out <- pairwiseLLM::llm_submit_pairs_batch(
          pairs = pairs,
          backend = "openai",
          model = model,
          trait_name = td$name,
          trait_description = td$description,
          prompt_template = tmpl,
          include_thoughts = TRUE
        )
        testthat::expect_equal(out$endpoint, "responses")
      }
    }
  )

  testthat::expect_equal(captured$seen, c("gpt-5.6-sol", "gpt-5.6-terra", "gpt-5.6-luna"))
})
