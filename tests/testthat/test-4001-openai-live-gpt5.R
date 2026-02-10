# =====================================================================
# test-4001-openai-live-gpt5.R
# GPT-5 live payload rules
# =====================================================================

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template

testthat::test_that("gpt-5 reasoning none maps to minimal and drops sampling", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  fake_body <- list(object = "response", model = "gpt-5", output = list())

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      pairwiseLLM::openai_compare_pair_live(
        ID1 = "A", text1 = "Text A",
        ID2 = "B", text2 = "Text B",
        model = "gpt-5",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        temperature = 0.7,
        top_p = 0.5,
        logprobs = TRUE
      )

      testthat::expect_equal(captured_body$reasoning$effort, "minimal")
      testthat::expect_null(captured_body$temperature)
      testthat::expect_null(captured_body$top_p)
      testthat::expect_null(captured_body$logprobs)
    }
  )
})

testthat::test_that("gpt-5 reasoning low rejects sampling params", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  testthat::expect_error(
    pairwiseLLM::openai_compare_pair_live(
      ID1 = "A", text1 = "Text A",
      ID2 = "B", text2 = "Text B",
      model = "gpt-5",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      reasoning = "low",
      temperature = 0
    ),
    "gpt-5/gpt-5-mini/gpt-5-nano"
  )
})

testthat::test_that("gpt-5.2 service_tier includes flex/priority and omits standard", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  fake_body <- list(object = "response", model = "gpt-5.2", output = list())

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      pairwiseLLM::openai_compare_pair_live(
        ID1 = "A", text1 = "Text A",
        ID2 = "B", text2 = "Text B",
        model = "gpt-5.2-2025-12-11",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        service_tier = "flex"
      )
      testthat::expect_equal(captured_body$service_tier, "flex")

      pairwiseLLM::openai_compare_pair_live(
        ID1 = "A", text1 = "Text A",
        ID2 = "B", text2 = "Text B",
        model = "gpt-5.2-2025-12-11",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        service_tier = "priority"
      )
      testthat::expect_equal(captured_body$service_tier, "priority")

      pairwiseLLM::openai_compare_pair_live(
        ID1 = "A", text1 = "Text A",
        ID2 = "B", text2 = "Text B",
        model = "gpt-5.2-2025-12-11",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        service_tier = "standard"
      )
      testthat::expect_true(is.null(captured_body$service_tier))
    }
  )
})
