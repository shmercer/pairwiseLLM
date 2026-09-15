# =====================================================================
# test-4002-submit-llm-pairs-openai-service-tier.R
# submit_llm_pairs forwarding for OpenAI service_tier
# =====================================================================

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template
submit_llm_pairs <- pairwiseLLM::submit_llm_pairs

testthat::test_that("submit_llm_pairs forwards service_tier to openai live", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "Text A",
    ID2 = "B",
    text2 = "Text B"
  )

  captured <- new.env(parent = emptyenv())
  captured$service_tier <- NULL

  testthat::with_mocked_bindings(
    submit_openai_pairs_live = function(
      pairs,
      model,
      trait_name,
      trait_description,
      prompt_template,
      endpoint,
      api_key,
      verbose,
      status_every,
      progress,
      include_raw,
      save_path,
      parallel,
      workers,
      ...
    ) {
      dots <- list(...)
      captured$service_tier <- if (!is.null(dots$service_tier)) dots$service_tier else NULL
      list(
        results = tibble::tibble(),
        failed_pairs = tibble::tibble(),
        failed_attempts = tibble::tibble()
      )
    },
    .package = "pairwiseLLM",
    {
      submit_llm_pairs(
        pairs = pairs,
        model = "gpt-5",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        backend = "openai",
        endpoint = "responses",
        service_tier = "flex",
        verbose = FALSE,
        progress = FALSE
      )
    }
  )

  testthat::expect_equal(captured$service_tier, "flex")
})

testthat::test_that("generic live wrappers preserve service tiers through request serialization", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .openai_api_key = function(...) "fixture-key",
    .openai_req_perform = function(req) {
      captured <<- do.call(jsonlite::toJSON, c(list(x = req$body$data), req$body$params))
      list()
    },
    .openai_resp_status = function(...) 200L,
    .openai_resp_body_json = function(...) {
      list(object = "chat.completion", model = "gpt-4.1",
        choices = list(list(message = list(content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"))))
    },
    .package = "pairwiseLLM"
  )
  pairs <- tibble::tibble(ID1 = "A", text1 = "one", ID2 = "B", text2 = "two")
  cases <- list(list(), list(service_tier = NULL), list(service_tier = "standard"),
    list(service_tier = "default"), list(service_tier = "auto"),
    list(service_tier = "flex"), list(service_tier = "priority"))
  expected <- list(NULL, NULL, "default", "default", "auto", "flex", "priority")
  for (endpoint in c("responses", "chat.completions")) {
    common <- list(model = "gpt-4.1", trait_name = "clarity",
      trait_description = "Which is clearer?", backend = "openai", endpoint = endpoint)
    for (i in seq_along(cases)) {
      single <- do.call(pairwiseLLM::llm_compare_pair, c(as.list(pairs), common, cases[[i]]))
      testthat::expect_identical(single$better_id, "A")
      single_body <- jsonlite::fromJSON(captured, simplifyVector = FALSE)
      submitted <- do.call(pairwiseLLM::submit_llm_pairs, c(list(pairs = pairs,
        verbose = FALSE, progress = FALSE, parallel = FALSE), common, cases[[i]]))
      testthat::expect_identical(submitted$results$better_id, "A")
      testthat::expect_equal(nrow(submitted$failed_pairs), 0L)
      submitted_body <- jsonlite::fromJSON(captured, simplifyVector = FALSE)
      for (body in list(single_body, submitted_body)) {
        if (is.null(expected[[i]])) {
          testthat::expect_false("service_tier" %in% names(body))
        } else {
          testthat::expect_identical(body$service_tier, expected[[i]])
        }
      }
    }
  }
})
