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
    .env = asNamespace("pairwiseLLM"),
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
