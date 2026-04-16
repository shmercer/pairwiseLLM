# tests/testthat/test-2021-vertex-live.R

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template
vertex_compare_pair_live <- pairwiseLLM::vertex_compare_pair_live
submit_vertex_pairs_live <- pairwiseLLM::submit_vertex_pairs_live
normalize_vertex_service_tier <- pairwiseLLM:::normalize_vertex_service_tier

testthat::test_that("normalize_vertex_service_tier validates and maps public tiers", {
  testthat::expect_null(normalize_vertex_service_tier(NULL))
  testthat::expect_null(normalize_vertex_service_tier("standard"))
  testthat::expect_equal(normalize_vertex_service_tier("flex"), "shared")
  testthat::expect_equal(normalize_vertex_service_tier("priority"), "dedicated")

  testthat::expect_error(
    normalize_vertex_service_tier(1),
    "service_tier"
  )
  testthat::expect_error(
    normalize_vertex_service_tier("gold"),
    "service_tier"
  )
})

testthat::test_that(".vertex_model_resource normalizes short model names and rejects non-Google publishers", {
  testthat::expect_equal(
    pairwiseLLM:::.vertex_model_resource("gemini-2.5-flash"),
    "publishers/google/models/gemini-2.5-flash"
  )
  testthat::expect_equal(
    pairwiseLLM:::.vertex_model_resource("publishers/google/models/gemini-2.5-pro"),
    "publishers/google/models/gemini-2.5-pro"
  )
  testthat::expect_error(
    pairwiseLLM:::.vertex_model_resource("publishers/anthropic/models/claude"),
    "Google publisher model"
  )
})

testthat::test_that(".vertex_is_gemini_3_model detects Gemini 3 resources", {
  testthat::expect_true(
    pairwiseLLM:::.vertex_is_gemini_3_model("publishers/google/models/gemini-3-flash")
  )
  testthat::expect_false(
    pairwiseLLM:::.vertex_is_gemini_3_model("publishers/google/models/gemini-2.5-flash")
  )
})

testthat::test_that(".vertex_request builds the Vertex express-mode URL and headers", {
  req <- pairwiseLLM:::.vertex_request(
    path = "/v1/publishers/google/models/gemini-2.5-flash:generateContent",
    api_key = "VERTEX_TEST_KEY",
    service_tier = "flex"
  )

  req_data <- unclass(req)
  testthat::expect_equal(
    req_data$url,
    paste0(
      "https://aiplatform.googleapis.com/",
      "v1/publishers/google/models/gemini-2.5-flash:generateContent",
      "?key=VERTEX_TEST_KEY"
    )
  )
  testthat::expect_equal(req_data$headers[["Content-Type"]], "application/json")
  testthat::expect_equal(
    req_data$headers[["X-Vertex-AI-LLM-Request-Type"]],
    "shared"
  )

  req_standard <- pairwiseLLM:::.vertex_request(
    path = "/v1/publishers/google/models/gemini-2.5-flash:generateContent",
    api_key = "VERTEX_TEST_KEY",
    service_tier = "standard"
  )
  testthat::expect_null(unclass(req_standard)$headers[["X-Vertex-AI-LLM-Request-Type"]])
})

testthat::test_that("vertex_compare_pair_live parses a successful response", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  fake_resp <- structure(list(), class = "httr2_response")
  fake_body <- list(
    modelVersion = "publishers/google/models/gemini-2.5-flash",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
          )
        )
      )
    ),
    usageMetadata = list(
      promptTokenCount = 11L,
      candidatesTokenCount = 5L,
      totalTokenCount = 16L
    )
  )

  testthat::local_mocked_bindings(
    .vertex_api_key = function(api_key = NULL) "VERTEX_TEST_KEY",
    .vertex_req_perform = function(req) fake_resp,
    .vertex_resp_status = function(resp) 200L,
    .vertex_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- vertex_compare_pair_live(
    ID1 = "S01",
    text1 = "Sample 1 text.",
    ID2 = "S02",
    text2 = "Sample 2 text.",
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    include_raw = TRUE
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(res$model, "publishers/google/models/gemini-2.5-flash")
  testthat::expect_equal(res$object_type, "generateContent")
  testthat::expect_equal(res$status_code, 200L)
  testthat::expect_equal(res$better_sample, "SAMPLE_2")
  testthat::expect_equal(res$better_id, "S02")
  testthat::expect_equal(res$prompt_tokens, 11)
  testthat::expect_equal(res$completion_tokens, 5)
  testthat::expect_equal(res$total_tokens, 16)
  testthat::expect_identical(res$raw_response[[1]], fake_body)
})

testthat::test_that("vertex_compare_pair_live captures thoughts when requested", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  fake_resp <- structure(list(), class = "httr2_response")
  fake_body <- list(
    model = "publishers/google/models/gemini-2.5-pro",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = "Reasoning trace"),
            list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
          )
        )
      )
    ),
    usageMetadata = list()
  )

  testthat::local_mocked_bindings(
    .vertex_api_key = function(api_key = NULL) "VERTEX_TEST_KEY",
    .vertex_req_perform = function(req) fake_resp,
    .vertex_resp_status = function(resp) 200L,
    .vertex_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- vertex_compare_pair_live(
    ID1 = "S01",
    text1 = "Sample 1 text.",
    ID2 = "S02",
    text2 = "Sample 2 text.",
    model = "gemini-2.5-pro",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    include_thoughts = TRUE
  )

  testthat::expect_equal(res$thoughts, "Reasoning trace")
  testthat::expect_equal(res$content, "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  testthat::expect_equal(res$better_id, "S01")
  testthat::expect_true(is.na(res$prompt_tokens))
})

testthat::test_that("vertex_compare_pair_live constructs the request body and tier header inputs", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  captured <- new.env(parent = emptyenv())
  captured$request <- NULL
  captured$body <- NULL

  testthat::local_mocked_bindings(
    .vertex_api_key = function(api_key = NULL) "VERTEX_TEST_KEY",
    .vertex_request = function(path, api_key = NULL, service_tier = "standard") {
      captured$request <- list(
        path = path,
        api_key = api_key,
        service_tier = service_tier
      )
      structure(list(), class = "httr2_request")
    },
    .vertex_req_body_json = function(req, body) {
      captured$body <- body
      req
    },
    .vertex_req_perform = function(...) structure(list(), class = "httr2_response"),
    .vertex_resp_status = function(...) 200L,
    .vertex_resp_body_json = function(...) list(candidates = list()),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  vertex_compare_pair_live(
    ID1 = "A",
    text1 = "A",
    ID2 = "B",
    text2 = "B",
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    temperature = 0.4,
    top_p = 0.8,
    top_k = 32,
    max_output_tokens = 120,
    thinking_budget = 64,
    include_thoughts = TRUE,
    service_tier = "priority"
  )

  testthat::expect_equal(
    captured$request$path,
    "/v1/publishers/google/models/gemini-2.5-flash:generateContent"
  )
  testthat::expect_equal(captured$request$api_key, NULL)
  testthat::expect_equal(captured$request$service_tier, "priority")
  testthat::expect_equal(captured$body$generationConfig$temperature, 0.4)
  testthat::expect_equal(captured$body$generationConfig$topP, 0.8)
  testthat::expect_equal(captured$body$generationConfig$topK, 32)
  testthat::expect_equal(captured$body$generationConfig$maxOutputTokens, 120)
  testthat::expect_true(captured$body$generationConfig$thinkingConfig$includeThoughts)
  testthat::expect_equal(captured$body$generationConfig$thinkingConfig$thinkingBudget, 64)
})

testthat::test_that("vertex_compare_pair_live supports Gemini 3 thinking_level and rejects invalid combinations", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  captured <- new.env(parent = emptyenv())
  captured$body <- NULL

  testthat::local_mocked_bindings(
    .vertex_api_key = function(api_key = NULL) "VERTEX_TEST_KEY",
    .vertex_request = function(...) structure(list(), class = "httr2_request"),
    .vertex_req_body_json = function(req, body) {
      captured$body <- body
      req
    },
    .vertex_req_perform = function(...) structure(list(), class = "httr2_response"),
    .vertex_resp_status = function(...) 200L,
    .vertex_resp_body_json = function(...) list(candidates = list()),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  vertex_compare_pair_live(
    ID1 = "A",
    text1 = "A",
    ID2 = "B",
    text2 = "B",
    model = "gemini-3-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    thinking_level = "medium"
  )

  testthat::expect_equal(
    captured$body$generationConfig$thinkingConfig$thinkingLevel,
    "MEDIUM"
  )

  testthat::expect_error(
    vertex_compare_pair_live(
      ID1 = "A",
      text1 = "A",
      ID2 = "B",
      text2 = "B",
      model = "gemini-3-flash",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      thinking_level = "low",
      thinking_budget = 64
    ),
    "Do not supply both `thinking_level` and `thinking_budget`"
  )

  testthat::expect_error(
    vertex_compare_pair_live(
      ID1 = "A",
      text1 = "A",
      ID2 = "B",
      text2 = "B",
      model = "gemini-2.5-flash",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      thinking_level = "low"
    ),
    "`thinking_level` is only supported for Gemini 3"
  )
})

testthat::test_that("vertex_compare_pair_live returns an error row on request failure", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  testthat::local_mocked_bindings(
    .vertex_api_key = function(api_key = NULL) "VERTEX_TEST_KEY",
    .vertex_req_perform = function(req) stop("HTTP 500 Internal Server Error"),
    .vertex_resp_status = function(resp) 500L,
    .vertex_resp_body_json = function(resp, ...) NULL,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- vertex_compare_pair_live(
    ID1 = "S01",
    text1 = "Sample 1 text.",
    ID2 = "S02",
    text2 = "Sample 2 text.",
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_true(is.na(res$object_type))
  testthat::expect_match(res$error_message, "HTTP 500", fixed = FALSE)
})

testthat::test_that("vertex_compare_pair_live handles httr2_http errors and extracts body", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  err_body_text <- "{\"error\": \"Invalid request\"}"
  fake_err_resp <- httr2::response(
    status_code = 400,
    body = charToRaw(err_body_text),
    headers = list("Content-Type" = "application/json")
  )
  cnd <- structure(
    list(message = "HTTP 400 Bad Request", resp = fake_err_resp),
    class = c("httr2_http", "error", "condition")
  )

  testthat::local_mocked_bindings(
    .vertex_api_key = function(...) "VERTEX_TEST_KEY",
    .vertex_request = function(...) structure(list(), class = "httr2_request"),
    .vertex_req_body_json = function(req, body) req,
    .vertex_req_perform = function(...) stop(cnd),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- vertex_compare_pair_live(
    ID1 = "A",
    text1 = "A",
    ID2 = "B",
    text2 = "B",
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_equal(res$status_code, 400L)
  testthat::expect_match(res$error_message, "HTTP 400 Bad Request", fixed = TRUE)
  testthat::expect_match(res$error_message, err_body_text, fixed = TRUE)
})

testthat::test_that("vertex_compare_pair_live handles empty and malformed candidates gracefully", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")
  empty_candidates <- list(candidates = list())
  empty_parts <- list(candidates = list(list(content = list(parts = list()))))
  one_part <- list(candidates = list(list(content = list(parts = list(
    list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  )))))

  testthat::local_mocked_bindings(
    .vertex_api_key = function(...) "VERTEX_TEST_KEY",
    .vertex_request = function(...) structure(list(), class = "httr2_request"),
    .vertex_req_body_json = function(req, ...) req,
    .vertex_req_perform = function(...) structure(list(), class = "httr2_response"),
    .vertex_resp_status = function(...) 200L,
    .vertex_resp_body_json = function(...) empty_candidates,
    .env = ns
  )

  res1 <- vertex_compare_pair_live("A", "a", "B", "b", "gemini-2.5-flash", "t", "d")
  testthat::expect_true(is.na(res1$content))
  testthat::expect_true(is.na(res1$thoughts))

  testthat::local_mocked_bindings(
    .vertex_resp_body_json = function(...) empty_parts,
    .env = ns
  )
  res2 <- vertex_compare_pair_live("A", "a", "B", "b", "gemini-2.5-flash", "t", "d")
  testthat::expect_true(is.na(res2$content))

  testthat::local_mocked_bindings(
    .vertex_resp_body_json = function(...) one_part,
    .env = ns
  )
  res3 <- vertex_compare_pair_live(
    "A",
    "a",
    "B",
    "b",
    "gemini-2.5-flash",
    "t",
    "d",
    include_thoughts = TRUE
  )
  testthat::expect_true(is.na(res3$thoughts))
  testthat::expect_equal(res3$content, "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  testthat::expect_equal(res3$better_sample, "SAMPLE_1")
})

testthat::test_that("submit_vertex_pairs_live returns list structure for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0),
    text1 = character(0),
    ID2 = character(0),
    text2 = character(0)
  )

  res <- submit_vertex_pairs_live(
    pairs = empty_pairs,
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_vertex_pairs_live runs correctly and forwards service_tier", {
  ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  calls <- list()

  pairs <- tibble::tibble(
    ID1 = c("S01", "S02"),
    text1 = c("T1", "T2"),
    ID2 = c("S03", "S04"),
    text2 = c("T3", "T4")
  )

  testthat::local_mocked_bindings(
    vertex_compare_pair_live = function(ID1, ID2, ...) {
      calls <<- append(calls, list(list(ID1 = ID1, ID2 = ID2, dots = list(...))))
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
        ID1 = ID1,
        ID2 = ID2,
        model = "vertex-model",
        status_code = 200L,
        error_message = NA_character_,
        better_sample = "SAMPLE_1",
        better_id = ID1
      )
    },
    .env = ns
  )

  res <- submit_vertex_pairs_live(
    pairs = pairs,
    model = "gemini-2.5-flash",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    service_tier = "flex",
    verbose = FALSE
  )

  testthat::expect_equal(nrow(res$results), 2L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
  testthat::expect_equal(length(calls), 2L)
  testthat::expect_equal(calls[[1]]$dots$service_tier, "flex")
})

testthat::test_that("submit_vertex_pairs_live separates failed pairs", {
  ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  pairs <- tibble::tibble(
    ID1 = c("S01", "FailMe"),
    text1 = "A",
    ID2 = "B",
    text2 = "C"
  )

  testthat::with_mocked_bindings(
    vertex_compare_pair_live = function(ID1, ...) {
      if (ID1 == "FailMe") {
        return(tibble::tibble(
          custom_id = "LIVE_FailMe_vs_B",
          ID1 = ID1,
          ID2 = "B",
          model = "vertex-model",
          status_code = 500L,
          error_message = "Mock API Error",
          better_id = NA_character_
        ))
      }

      tibble::tibble(
        custom_id = "LIVE_S01_vs_B",
        ID1 = ID1,
        ID2 = "B",
        model = "vertex-model",
        status_code = 200L,
        error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = ns,
    {
      res <- submit_vertex_pairs_live(
        pairs,
        "gemini-2.5-flash",
        td$name,
        td$description,
        verbose = FALSE
      )

      testthat::expect_equal(nrow(res$results), 1L)
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(nrow(res$failed_attempts), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "FailMe")
      testthat::expect_equal(res$failed_pairs$error_message, "Mock API Error")
    }
  )
})

testthat::test_that("submit_vertex_pairs_live respects save_path resume logic", {
  testthat::skip_if_not_installed("readr")

  ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmp_csv <- tempfile(fileext = ".csv")

  existing_data <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01",
    ID2 = "S02",
    model = "vertex-model",
    status_code = 200L,
    error_message = NA_character_,
    better_id = "S01"
  )
  readr::write_csv(existing_data, tmp_csv)

  pairs <- tibble::tibble(
    ID1 = c("S01", "S03"),
    text1 = c("A", "B"),
    ID2 = c("S02", "S04"),
    text2 = c("C", "D")
  )

  call_count <- 0

  testthat::with_mocked_bindings(
    vertex_compare_pair_live = function(...) {
      call_count <<- call_count + 1
      tibble::tibble(
        custom_id = "LIVE_S03_vs_S04",
        ID1 = "S03",
        ID2 = "S04",
        model = "vertex-model",
        status_code = 200L,
        error_message = NA_character_,
        better_id = "S03"
      )
    },
    .env = ns,
    {
      res <- submit_vertex_pairs_live(
        pairs = pairs,
        model = "gemini-2.5-flash",
        trait_name = td$name,
        trait_description = td$description,
        save_path = tmp_csv,
        verbose = FALSE
      )

      testthat::expect_equal(call_count, 1L)
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_setequal(res$results$ID1, c("S01", "S03"))
    }
  )

  unlink(tmp_csv)
})

testthat::test_that("submit_vertex_pairs_live validates inputs", {
  td <- trait_description("overall_quality")

  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_vertex_pairs_live(bad_pairs, "gemini-2.5-flash", td$name, td$description),
    "must contain columns"
  )

  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    submit_vertex_pairs_live(
      good_pairs,
      "gemini-2.5-flash",
      td$name,
      td$description,
      status_every = 0
    ),
    "positive integer"
  )
})
