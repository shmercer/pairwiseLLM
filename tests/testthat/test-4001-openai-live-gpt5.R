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

testthat::test_that("Responses requests forward and validate max_output_tokens", {
  td <- trait_description("overall_quality")
  captured_body <- NULL
  fake_body <- list(object = "response", model = "gpt-5.6-luna", output = list())

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
        model = "gpt-5.6-luna",
        trait_name = td$name,
        trait_description = td$description,
        endpoint = "responses",
        reasoning = "none",
        max_output_tokens = 256
      )
      testthat::expect_identical(captured_body$max_output_tokens, 256L)
    }
  )

  testthat::expect_error(
    pairwiseLLM::openai_compare_pair_live(
      ID1 = "A", text1 = "Text A",
      ID2 = "B", text2 = "Text B",
      model = "gpt-5.6-luna",
      trait_name = td$name,
      trait_description = td$description,
      endpoint = "responses",
      max_output_tokens = 0
    ),
    "positive integer"
  )
})

testthat::test_that("live endpoints preserve explicit service tiers in JSON", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .openai_api_key = function(...) "fixture-key",
    .openai_req_perform = function(req) {
      captured <<- do.call(jsonlite::toJSON, c(list(x = req$body$data), req$body$params))
      list()
    },
    .openai_resp_status = function(...) 200L,
    .openai_resp_body_json = function(...) list(object = "response", output = list()),
    .package = "pairwiseLLM"
  )
  cases <- list(list(), list(service_tier = NULL), list(service_tier = "standard"),
    list(service_tier = "default"), list(service_tier = "auto"),
    list(service_tier = "flex"), list(service_tier = "priority"))
  expected <- list(NULL, NULL, "default", "default", "auto", "flex", "priority")
  for (endpoint in c("responses", "chat.completions")) {
    for (model in c("gpt-4.1", "gpt-5.2-2025-12-11", "gpt-5.6-sol")) {
      args <- list(ID1 = "A", text1 = "one", ID2 = "B", text2 = "two",
        model = model, trait_name = "clarity", trait_description = "Which is clearer?",
        endpoint = endpoint)
      for (i in seq_along(cases)) {
        do.call(pairwiseLLM::openai_compare_pair_live, c(args, cases[[i]]))
        body <- jsonlite::fromJSON(captured, simplifyVector = FALSE)
        if (is.null(expected[[i]])) {
          testthat::expect_false("service_tier" %in% names(body))
        } else {
          testthat::expect_identical(body$service_tier, expected[[i]])
        }
      }
    }
  }
})

testthat::test_that("live endpoints reject invalid service tiers before credentials or transport", {
  calls <- 0L
  forbidden <- function(...) {
    calls <<- calls + 1L
    stop("unexpected request")
  }
  testthat::local_mocked_bindings(.openai_api_key = forbidden,
    .openai_req_perform = forbidden, .package = "pairwiseLLM")
  for (endpoint in c("responses", "chat.completions")) {
    for (value in list(NA_character_, "gold", "", 1, TRUE, character(),
                       c("default", "flex"), list("default"))) {
      testthat::expect_error(pairwiseLLM::openai_compare_pair_live(
        "A", "one", "B", "two", "gpt-4.1", "clarity", "Which is clearer?",
        endpoint = endpoint, service_tier = value), "`service_tier`", fixed = TRUE)
    }
  }
  testthat::expect_identical(calls, 0L)
})

testthat::test_that("gpt-5.4-mini keeps flex tier and model-default sampling when reasoning is none", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  fake_body <- list(object = "response", model = "gpt-5.4-mini", output = list())

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
        model = "gpt-5.4-mini",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        service_tier = "flex",
        temperature = NULL,
        top_p = NULL
      )

      testthat::expect_equal(captured_body$service_tier, "flex")
      testthat::expect_equal(captured_body$reasoning$effort, "none")
      testthat::expect_false("temperature" %in% names(captured_body))
      testthat::expect_false("top_p" %in% names(captured_body))
    }
  )
})

testthat::test_that("gpt-5.6 named tiers keep reasoning and service tier semantics", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured <- rlang::env(body = NULL)

  fake_body <- list(object = "response", model = "gpt-5.6-sol", output = list())

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured$body <- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      pairwiseLLM::openai_compare_pair_live(
        ID1 = "A", text1 = "Text A",
        ID2 = "B", text2 = "Text B",
        model = "gpt-5.6-sol",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        include_thoughts = TRUE,
        service_tier = "flex"
      )

      testthat::expect_equal(captured$body$model, "gpt-5.6-sol")
      testthat::expect_equal(captured$body$reasoning$effort, "low")
      testthat::expect_equal(captured$body$reasoning$summary, "auto")
      testthat::expect_equal(captured$body$service_tier, "flex")
      testthat::expect_false("temperature" %in% names(captured$body))
    }
  )
})


test_that("openai live store preserves booleans and omission through JSON encoding", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .openai_api_key = function(...) "fixture-key",
    .openai_req_perform = function(req) {
      captured <<- do.call(jsonlite::toJSON, c(list(x = req$body$data), req$body$params))
      list()
    },
    .openai_resp_status = function(...) 200L,
    .openai_resp_body_json = function(...) list(object = "response", model = "gpt-4.1", output = list()),
    .package = "pairwiseLLM"
  )
  for (endpoint in c("responses", "chat.completions")) {
    args <- list(ID1 = "A", text1 = "one", ID2 = "B", text2 = "two",
      model = "gpt-4.1", trait_name = "clarity", trait_description = "Which is clearer?",
      endpoint = endpoint, include_raw = TRUE)
    for (extra in list(list(), list(store = NULL), list(store = FALSE), list(store = TRUE))) {
      do.call(pairwiseLLM::openai_compare_pair_live, c(args, extra))
      body <- jsonlite::fromJSON(captured, simplifyVector = FALSE)
      if (is.null(extra$store)) {
        expect_false("store" %in% names(body))
      } else {
        expect_identical(body$store, extra$store)
        expect_match(captured, paste0('"store":', tolower(as.character(extra$store))), fixed = TRUE)
      }
    }
  }
})

test_that("openai live rejects invalid store before credentials or transport", {
  calls <- 0L
  forbidden <- function(...) {
    calls <<- calls + 1L
    stop("unexpected request")
  }
  testthat::local_mocked_bindings(.openai_api_key = forbidden,
    .openai_req_perform = forbidden, .package = "pairwiseLLM")
  for (endpoint in c("responses", "chat.completions")) {
    for (value in list(NA, "false", 0, 1L, logical(), c(TRUE, FALSE),
                       list(FALSE), matrix(FALSE), array(TRUE, 1L))) {
      expect_error(pairwiseLLM::openai_compare_pair_live(
        "A", "one", "B", "two", "gpt-4.1", "clarity", "Which is clearer?",
        endpoint = endpoint, store = value), "`store` must be TRUE, FALSE, or NULL.", fixed = TRUE)
    }
  }
  expect_identical(calls, 0L)
})
