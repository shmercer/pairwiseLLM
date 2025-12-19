# tests/testthat/test-gemini_live.R

test_that("gemini_compare_pair_live parses a successful response without
          thoughts", {
            skip_if_not_installed("httr2")

            ns <- asNamespace("pairwiseLLM")

            fake_resp <- structure(list(), class = "httr2_response")

            fake_body <- list(
              model = "models/gemini-3-pro-preview",
              candidates = list(
                list(
                  content = list(
                    parts = list(
                      list(text = "Some internal text that we treat as content. "),
                      list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
                    )
                  )
                )
              ),
              usageMetadata = list(
                promptTokenCount     = 42L,
                candidatesTokenCount = 7L,
                totalTokenCount      = 49L
              )
            )

            testthat::local_mocked_bindings(
              # Avoid hitting .get_api_key() / env
              .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
              .gemini_req_perform = function(req) fake_resp,
              .gemini_resp_status = function(resp) 200L,
              .gemini_resp_body_json = function(resp, ...) fake_body,
              .env = ns
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            res <- gemini_compare_pair_live(
              ID1               = "S01",
              text1             = "Sample 1 text.",
              ID2               = "S02",
              text2             = "Sample 2 text.",
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl,
              thinking_level    = "low",
              include_thoughts  = FALSE,
              include_raw       = TRUE
            )

            expect_s3_class(res, "tbl_df")
            expect_equal(nrow(res), 1L)

            expect_equal(res$ID1, "S01")
            expect_equal(res$ID2, "S02")
            expect_equal(res$model, "models/gemini-3-pro-preview")
            expect_equal(res$object_type, "generateContent")
            expect_equal(res$status_code, 200L)
            expect_true(is.na(res$error_message) || identical(res$error_message, ""))

            # Without include_thoughts, everything is collapsed into content
            expect_true(is.na(res$thoughts))
            expect_true(grepl("Some internal text", res$content, fixed = TRUE))
            expect_true(grepl("<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>", res$content,
                              fixed = TRUE
            ))

            expect_equal(res$better_sample, "SAMPLE_1")
            expect_equal(res$better_id, "S01")

            expect_equal(res$prompt_tokens, 42)
            expect_equal(res$completion_tokens, 7)
            expect_equal(res$total_tokens, 49)

            expect_true("raw_response" %in% names(res))
            expect_type(res$raw_response, "list")
            expect_identical(res$raw_response[[1]], fake_body)
          })

test_that("gemini_compare_pair_live parses thoughts and content when
          include_thoughts = TRUE", {
            skip_if_not_installed("httr2")

            ns <- asNamespace("pairwiseLLM")

            fake_resp <- structure(list(), class = "httr2_response")

            fake_body <- list(
              model = "models/gemini-3-pro-preview",
              candidates = list(
                list(
                  content = list(
                    parts = list(
                      list(text = "These are my detailed thoughts..."),
                      list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
                    )
                  )
                )
              ),
              usageMetadata = list()
            )

            testthat::local_mocked_bindings(
              .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
              .gemini_req_perform = function(req) fake_resp,
              .gemini_resp_status = function(resp) 200L,
              .gemini_resp_body_json = function(resp, ...) fake_body,
              .env = ns
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            res <- gemini_compare_pair_live(
              ID1               = "S01",
              text1             = "Sample 1 text.",
              ID2               = "S02",
              text2             = "Sample 2 text.",
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl,
              thinking_level    = "high",
              include_thoughts  = TRUE,
              include_raw       = FALSE
            )

            expect_s3_class(res, "tbl_df")
            expect_equal(nrow(res), 1L)

            # thoughts should come from first part, content from subsequent parts
            expect_equal(res$thoughts, "These are my detailed thoughts...")
            expect_equal(res$content, "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")

            expect_equal(res$better_sample, "SAMPLE_2")
            expect_equal(res$better_id, "S02")

            # usageMetadata missing -> NAs
            expect_true(is.na(res$prompt_tokens))
            expect_true(is.na(res$completion_tokens))
            expect_true(is.na(res$total_tokens))
          })

test_that("gemini_compare_pair_live handles responses without
          <BETTER_SAMPLE> tag", {
            skip_if_not_installed("httr2")

            ns <- asNamespace("pairwiseLLM")

            fake_resp <- structure(list(), class = "httr2_response")

            fake_body <- list(
              model = "models/gemini-3-pro-preview",
              candidates = list(
                list(
                  content = list(
                    parts = list(
                      list(text = "I forgot to include the tag, sorry.")
                    )
                  )
                )
              )
            )

            testthat::local_mocked_bindings(
              .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
              .gemini_req_perform = function(req) fake_resp,
              .gemini_resp_status = function(resp) 200L,
              .gemini_resp_body_json = function(resp, ...) fake_body,
              .env = ns
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            res <- gemini_compare_pair_live(
              ID1               = "S01",
              text1             = "Sample 1 text.",
              ID2               = "S02",
              text2             = "Sample 2 text.",
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl,
              thinking_level    = "low",
              include_thoughts  = FALSE
            )

            expect_true(is.na(res$better_sample))
            expect_true(is.na(res$better_id))
          })

test_that("gemini_compare_pair_live returns an error row when
          request fails", {
            skip_if_not_installed("httr2")

            ns <- asNamespace("pairwiseLLM")

            # Simulate a generic error thrown by .gemini_req_perform
            testthat::local_mocked_bindings(
              .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
              .gemini_req_perform = function(req) stop("HTTP 500 Internal Server Error"),
              .gemini_resp_status = function(resp) 500L,
              .gemini_resp_body_json = function(resp, ...) NULL,
              .env = ns
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            res <- gemini_compare_pair_live(
              ID1               = "S01",
              text1             = "Sample 1 text.",
              ID2               = "S02",
              text2             = "Sample 2 text.",
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl
            )

            expect_equal(nrow(res), 1L)
            expect_equal(res$ID1, "S01")
            expect_equal(res$ID2, "S02")

            expect_true(is.na(res$object_type))
            expect_true(is.na(res$content))
            expect_true(is.na(res$thoughts))
            expect_true(is.na(res$better_sample))
            expect_true(is.na(res$better_id))

            expect_match(res$error_message, "HTTP 500", fixed = FALSE)
          })

test_that("gemini_compare_pair_live handles httr2_http errors and extracts body", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  # Create a real httr2 response with a body
  # Note: httr2::response() requires httr2 >= 0.2.0
  err_body_text <- "{\"error\": \"Invalid argument details\"}"
  fake_err_resp <- httr2::response(
    status_code = 400,
    body = charToRaw(err_body_text),
    headers = list("Content-Type" = "application/json")
  )

  # Create the condition object that httr2 throws
  cnd <- structure(
    list(message = "HTTP 400 Bad Request", resp = fake_err_resp),
    class = c("httr2_http", "error", "condition")
  )

  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "TEST_KEY",
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, body) req,
    .gemini_req_perform = function(...) stop(cnd),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
    model = "gemini-3-pro-preview",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  expect_equal(res$status_code, 400L)
  # The error message should contain the original message AND the body
  expect_match(res$error_message, "HTTP 400 Bad Request", fixed = TRUE)
  expect_match(res$error_message, err_body_text, fixed = TRUE)
})

test_that("gemini_compare_pair_live validates model and maps thinking_level
          medium to High", {
            skip_if_not_installed("httr2")

            ns <- asNamespace("pairwiseLLM")

            fake_resp <- structure(list(), class = "httr2_response")
            fake_body <- list(candidates = list())

            testthat::local_mocked_bindings(
              .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
              .gemini_req_perform = function(req) fake_resp,
              .gemini_resp_status = function(resp) 200L,
              .gemini_resp_body_json = function(resp, ...) fake_body,
              .env = ns
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            # Invalid model
            expect_error(
              gemini_compare_pair_live(
                ID1               = "S01",
                text1             = "Sample 1 text.",
                ID2               = "S02",
                text2             = "Sample 2 text.",
                model             = "",
                trait_name        = td$name,
                trait_description = td$description,
                prompt_template   = tmpl
              ),
              "`model` must be a non-empty character scalar"
            )

            # thinking_level = "medium" should warn but still work
            expect_warning(
              {
                res <- gemini_compare_pair_live(
                  ID1               = "S01",
                  text1             = "Sample 1 text.",
                  ID2               = "S02",
                  text2             = "Sample 2 text.",
                  model             = "gemini-3-pro-preview",
                  trait_name        = td$name,
                  trait_description = td$description,
                  prompt_template   = tmpl,
                  thinking_level    = "medium"
                )
                # fake_body has empty candidates, so NA results
                expect_true(is.na(res$better_sample))
              },
              "mapping to \"High\" internally",
              fixed = FALSE
            )

            # thinking_budget in ... should be ignored (no error, but warning)
            expect_warning(
              {
                res <- gemini_compare_pair_live(
                  ID1               = "S01",
                  text1             = "Sample 1 text.",
                  ID2               = "S02",
                  text2             = "Sample 2 text.",
                  model             = "gemini-3-pro-preview",
                  trait_name        = td$name,
                  trait_description = td$description,
                  prompt_template   = tmpl,
                  thinking_level    = "low",
                  thinking_budget   = 2048 # should be ignored
                )
              },
              "`thinking_budget` is ignored for Gemini 3",
              fixed = FALSE
            )
          })

test_that("gemini_compare_pair_live correctly constructs request body with config", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  captured_body <- NULL

  # Mock internal helpers to capture request body
  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "TEST_KEY",
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .gemini_req_perform = function(...) structure(list(), class = "httr2_response"),
    .gemini_resp_status = function(...) 200L,
    .gemini_resp_body_json = function(...) list(candidates = list()),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Call with all config options
  res <- gemini_compare_pair_live(
    ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
    model = "gemini-3-pro-preview",
    trait_name = td$name, trait_description = td$description,
    prompt_template = tmpl,
    temperature = 0.7,
    top_p = 0.9,
    top_k = 40,
    max_output_tokens = 100,
    thinking_level = "low",
    include_thoughts = TRUE
  )

  expect_type(captured_body, "list")
  gc <- captured_body$generationConfig

  expect_equal(gc$temperature, 0.7)
  expect_equal(gc$topP, 0.9)
  expect_equal(gc$topK, 40)
  expect_equal(gc$maxOutputTokens, 100)

  # Check thinking config
  expect_equal(gc$thinkingConfig$thinkingLevel, "Low")
  expect_true(gc$thinkingConfig$includeThoughts)
})

test_that("gemini_compare_pair_live handles empty/malformed candidates gracefully", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  # Scenario 1: candidates list is empty
  empty_candidates <- list(candidates = list())

  # Scenario 2: parts list is empty (content exists but has no parts)
  empty_parts <- list(candidates = list(list(content = list(parts = list()))))

  # Scenario 3: include_thoughts=TRUE but only 1 part (fallback)
  one_part <- list(candidates = list(list(content = list(parts = list(
    list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  )))))

  # 1. Empty candidates
  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "K",
    .gemini_req_perform = function(...) structure(list(), class="httr2_response"),
    .gemini_resp_status = function(...) 200L,
    .gemini_resp_body_json = function(...) empty_candidates,
    .gemini_request = function(...) structure(list(), class="httr2_request"),
    .gemini_req_body_json = function(req, ...) req,
    .env = ns
  )

  res1 <- gemini_compare_pair_live("A","a","B","b","m","t","d")
  expect_true(is.na(res1$content))
  expect_true(is.na(res1$thoughts))

  # 2. Empty parts (update mock)
  testthat::local_mocked_bindings(
    .gemini_resp_body_json = function(...) empty_parts,
    .env = ns
  )
  res2 <- gemini_compare_pair_live("A","a","B","b","m","t","d")
  expect_true(is.na(res2$content))

  # 3. One part + include_thoughts=TRUE
  testthat::local_mocked_bindings(
    .gemini_resp_body_json = function(...) one_part,
    .env = ns
  )
  res3 <- gemini_compare_pair_live("A","a","B","b","m","t","d", include_thoughts = TRUE)

  # Should fallback: content gets the text, thoughts remains NA
  expect_true(is.na(res3$thoughts))
  expect_equal(res3$content, "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  expect_equal(res3$better_sample, "SAMPLE_1")
})

test_that("submit_gemini_pairs_live validates inputs and handles
          zero-row pairs", {
            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            # Missing columns
            bad_pairs <- tibble::tibble(
              ID1   = "S01",
              text1 = "Sample 1"
              # no ID2/text2
            )

            expect_error(
              submit_gemini_pairs_live(
                pairs             = bad_pairs,
                model             = "gemini-3-pro-preview",
                trait_name        = td$name,
                trait_description = td$description,
                prompt_template   = tmpl
              ),
              "`pairs` must contain columns"
            )

            # Zero rows
            empty_pairs <- tibble::tibble(
              ID1   = character(0),
              text1 = character(0),
              ID2   = character(0),
              text2 = character(0)
            )

            res_empty <- submit_gemini_pairs_live(
              pairs             = empty_pairs,
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl,
              include_thoughts  = TRUE
            )

            expect_s3_class(res_empty, "tbl_df")
            expect_equal(nrow(res_empty), 0L)
            expect_setequal(
              names(res_empty),
              c(
                "custom_id", "ID1", "ID2", "model", "object_type",
                "status_code", "error_message", "thoughts", "content",
                "better_sample", "better_id",
                "prompt_tokens", "completion_tokens", "total_tokens"
              )
            )
          })

test_that("submit_gemini_pairs_live validates status_every", {
  pairs <- tibble::tibble(ID1="A", text1="a", ID2="B", text2="b")
  td <- trait_description("overall_quality")

  expect_error(
    submit_gemini_pairs_live(pairs, "m", td$name, td$description, status_every = 0),
    "single positive integer"
  )
  expect_error(
    submit_gemini_pairs_live(pairs, "m", td$name, td$description, status_every = "1"),
    "single positive integer"
  )
})

test_that("submit_gemini_pairs_live calls gemini_compare_pair_live for
          each row and passes include_thoughts", {
            ns <- asNamespace("pairwiseLLM")

            pairs <- tibble::tibble(
              ID1   = c("S01", "S02"),
              text1 = c("Text 1a", "Text 2a"),
              ID2   = c("S03", "S04"),
              text2 = c("Text 1b", "Text 2b")
            )

            td <- trait_description("overall_quality")
            tmpl <- set_prompt_template()

            calls <- list()

            fake_gemini_compare <- function(
    ID1, text1, ID2, text2, model, trait_name, trait_description,
    prompt_template, api_key, thinking_level, temperature,
    top_p, top_k, max_output_tokens, api_version,
    include_raw, include_thoughts, ...
            ) {
              calls <<- append(calls, list(
                list(
                  ID1              = ID1,
                  ID2              = ID2,
                  model            = model,
                  thinking_level   = thinking_level,
                  include_thoughts = include_thoughts
                )
              ))

              tibble::tibble(
                custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
                ID1 = ID1,
                ID2 = ID2,
                model = model,
                object_type = "generateContent",
                status_code = 200L,
                error_message = NA_character_,
                thoughts = if (isTRUE(include_thoughts)) {
                  "fake thoughts"
                } else {
                  NA_character_
                },
                content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
                better_sample = "SAMPLE_1",
                better_id = ID1,
                prompt_tokens = 10,
                completion_tokens = 2,
                total_tokens = 12
              )
            }

            testthat::local_mocked_bindings(
              gemini_compare_pair_live = fake_gemini_compare,
              .env = ns
            )

            res <- submit_gemini_pairs_live(
              pairs             = pairs,
              model             = "gemini-3-pro-preview",
              trait_name        = td$name,
              trait_description = td$description,
              prompt_template   = tmpl,
              thinking_level    = "low",
              include_thoughts  = TRUE,
              include_raw       = FALSE,
              verbose           = FALSE,
              progress          = FALSE
            )

            expect_equal(nrow(res), 2L)
            expect_equal(length(calls), 2L)

            expect_equal(calls[[1]]$ID1, "S01")
            expect_equal(calls[[1]]$ID2, "S03")
            expect_equal(calls[[1]]$model, "gemini-3-pro-preview")
            expect_equal(calls[[1]]$thinking_level, "low")
            expect_true(calls[[1]]$include_thoughts)

            expect_equal(calls[[2]]$ID1, "S02")
            expect_equal(calls[[2]]$ID2, "S04")
            expect_true(all(res$better_sample == "SAMPLE_1"))
            expect_true(all(res$thoughts == "fake thoughts"))
          })

test_that("submit_gemini_pairs_live catches unexpected errors from inner function", {
  ns <- asNamespace("pairwiseLLM")

  pairs <- tibble::tibble(ID1="A", text1="a", ID2="B", text2="b")
  td <- trait_description("overall_quality")

  # Mock gemini_compare_pair_live to throw a generic R error
  testthat::local_mocked_bindings(
    gemini_compare_pair_live = function(...) stop("Unexpected crash"),
    .env = ns
  )

  res <- submit_gemini_pairs_live(pairs, "m", td$name, td$description, verbose = FALSE)

  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$status_code))
  expect_match(res$error_message, "Error during Gemini comparison: Unexpected crash")
})
