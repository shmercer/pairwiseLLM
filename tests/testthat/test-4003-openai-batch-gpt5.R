# =====================================================================
# test-4003-openai-batch-gpt5.R
# OpenAI batch GPT-5 request composition
# =====================================================================

testthat::test_that("build_openai_batch_requests maps GPT-5 none to minimal", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  out <- pairwiseLLM::build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none"
  )

  body <- out$body[[1]]
  testthat::expect_equal(body$reasoning$effort, "minimal")
})

testthat::test_that("build_openai_batch_requests drops sampling for GPT-5 minimal", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  out <- pairwiseLLM::build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none",
    temperature = 0.2,
    top_p = 0.9,
    logprobs = TRUE
  )

  body <- out$body[[1]]
  testthat::expect_false("temperature" %in% names(body))
  testthat::expect_false("top_p" %in% names(body))
  testthat::expect_false("logprobs" %in% names(body))
})

testthat::test_that("build_openai_batch_requests handles service tier inclusion", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  out_flex <- pairwiseLLM::build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5-mini",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none",
    service_tier = "flex"
  )

  out_standard <- pairwiseLLM::build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5-mini",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none",
    service_tier = "standard"
  )

  body_flex <- out_flex$body[[1]]
  body_standard <- out_standard$body[[1]]

  testthat::expect_equal(body_flex$service_tier, "flex")
  testthat::expect_false("service_tier" %in% names(body_standard))
})

testthat::test_that("build_openai_batch_requests warns on thoughts for non GPT-5", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  testthat::expect_warning(
    pairwiseLLM::build_openai_batch_requests(
      pairs = pairs,
      model = "gpt-4.1",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      include_thoughts = TRUE
    ),
    "include_thoughts"
  )
})

testthat::test_that("run_openai_batch_pipeline auto-selects responses for GPT-5 minimal", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  td <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  withr::local_tempdir()

  testthat::with_mocked_bindings(
    openai_upload_batch_file = function(path, purpose = "batch", api_key = NULL) {
      list(id = "file_1")
    },
    openai_create_batch = function(input_file_id,
                                   endpoint,
                                   completion_window = "24h",
                                   metadata = NULL,
                                   api_key = NULL) {
      list(id = "batch_1", endpoint = endpoint, status = "created")
    },
    .env = asNamespace("pairwiseLLM"),
    {
      out <- pairwiseLLM::run_openai_batch_pipeline(
        pairs = pairs,
        model = "gpt-5",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        include_thoughts = FALSE,
        endpoint = NULL,
        poll = FALSE,
        reasoning = "none"
      )
      testthat::expect_equal(out$batch$endpoint, "/v1/responses")
    }
  )
})
