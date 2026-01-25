# =====================================================================
# test-4000-openai-params-normalization.R
# Helpers for OpenAI parameter normalization
# =====================================================================

is_gpt5_series_model <- pairwiseLLM:::is_gpt5_series_model
normalize_openai_reasoning <- pairwiseLLM:::normalize_openai_reasoning
normalize_openai_sampling <- pairwiseLLM:::normalize_openai_sampling
normalize_openai_service_tier <- pairwiseLLM:::normalize_openai_service_tier

testthat::test_that("is_gpt5_series_model identifies GPT-5 series models", {
  positives <- c(
    "gpt-5",
    "gpt-5-mini",
    "gpt-5-nano",
    "gpt-5.1",
    "gpt-5.1-2025-12-11",
    "gpt-5.2",
    "gpt-5.2-2025-12-11"
  )

  testthat::expect_true(all(vapply(positives, is_gpt5_series_model, logical(1))))
  testthat::expect_false(is_gpt5_series_model("gpt-4.1"))
  testthat::expect_false(is_gpt5_series_model("gpt-5.3"))
  testthat::expect_false(is_gpt5_series_model(NA_character_))
  testthat::expect_false(is_gpt5_series_model(NULL))
})

testthat::test_that("normalize_openai_reasoning maps GPT-5 none to minimal", {
  testthat::expect_equal(
    normalize_openai_reasoning("gpt-5", "none", include_thoughts = FALSE),
    "minimal"
  )
  testthat::expect_equal(
    normalize_openai_reasoning("gpt-5-mini", "none", include_thoughts = FALSE),
    "minimal"
  )
})

testthat::test_that("normalize_openai_reasoning defaults low for thoughts on GPT-5 series", {
  testthat::expect_equal(
    normalize_openai_reasoning("gpt-5", NULL, include_thoughts = TRUE),
    "low"
  )
  testthat::expect_equal(
    normalize_openai_reasoning("gpt-5.1", NULL, include_thoughts = TRUE),
    "low"
  )
})

testthat::test_that("normalize_openai_reasoning preserves GPT-5.1 none", {
  testthat::expect_equal(
    normalize_openai_reasoning("gpt-5.1", "none", include_thoughts = FALSE),
    "none"
  )
})

testthat::test_that("normalize_openai_reasoning validates inputs", {
  testthat::expect_error(
    normalize_openai_reasoning(1, "low", include_thoughts = FALSE),
    "model"
  )
  testthat::expect_error(
    normalize_openai_reasoning("gpt-5", 1, include_thoughts = FALSE),
    "reasoning"
  )
  testthat::expect_error(
    normalize_openai_reasoning("gpt-5", "low", include_thoughts = NA),
    "include_thoughts"
  )
})

testthat::test_that("normalize_openai_sampling enforces GPT-5.1/5.2 restrictions", {
  testthat::expect_error(
    normalize_openai_sampling(
      model = "gpt-5.1",
      endpoint = "responses",
      reasoning_effort = "low",
      temperature = 0,
      top_p = NULL,
      logprobs = NULL
    ),
    "gpt-5.1/5.2"
  )
})

testthat::test_that("normalize_openai_sampling drops sampling for GPT-5 minimal", {
  res <- normalize_openai_sampling(
    model = "gpt-5",
    endpoint = "responses",
    reasoning_effort = "minimal",
    temperature = 0.2,
    top_p = 0.9,
    logprobs = TRUE
  )

  testthat::expect_null(res$temperature)
  testthat::expect_null(res$top_p)
  testthat::expect_null(res$logprobs)
})

testthat::test_that("normalize_openai_sampling errors for GPT-5 non-minimal reasoning", {
  testthat::expect_error(
    normalize_openai_sampling(
      model = "gpt-5",
      endpoint = "responses",
      reasoning_effort = "low",
      temperature = 0,
      top_p = NULL,
      logprobs = NULL
    ),
    "gpt-5/gpt-5-mini/gpt-5-nano"
  )
})

testthat::test_that("normalize_openai_sampling keeps sampling when reasoning inactive", {
  res <- normalize_openai_sampling(
    model = "gpt-5.1",
    endpoint = "responses",
    reasoning_effort = "none",
    temperature = 0,
    top_p = 0.9,
    logprobs = TRUE
  )

  testthat::expect_equal(res$temperature, 0)
  testthat::expect_equal(res$top_p, 0.9)
  testthat::expect_equal(res$logprobs, TRUE)
})

testthat::test_that("normalize_openai_sampling does not enforce for chat.completions", {
  res <- normalize_openai_sampling(
    model = "gpt-5",
    endpoint = "chat.completions",
    reasoning_effort = "minimal",
    temperature = 0.2,
    top_p = 0.9,
    logprobs = TRUE
  )

  testthat::expect_equal(res$temperature, 0.2)
  testthat::expect_equal(res$top_p, 0.9)
  testthat::expect_equal(res$logprobs, TRUE)
})

testthat::test_that("normalize_openai_sampling validates endpoint", {
  testthat::expect_error(
    normalize_openai_sampling(
      model = 1,
      endpoint = "responses",
      reasoning_effort = NULL,
      temperature = NULL,
      top_p = NULL,
      logprobs = NULL
    ),
    "model"
  )
  testthat::expect_error(
    normalize_openai_sampling(
      model = "gpt-5",
      endpoint = "responses",
      reasoning_effort = 1,
      temperature = NULL,
      top_p = NULL,
      logprobs = NULL
    ),
    "reasoning_effort"
  )
  testthat::expect_error(
    normalize_openai_sampling(
      model = "gpt-5",
      endpoint = NA_character_,
      reasoning_effort = NULL,
      temperature = NULL,
      top_p = NULL,
      logprobs = NULL
    ),
    "endpoint"
  )
  testthat::expect_error(
    normalize_openai_sampling(
      model = "gpt-5",
      endpoint = "invalid",
      reasoning_effort = NULL,
      temperature = NULL,
      top_p = NULL,
      logprobs = NULL
    ),
    "endpoint"
  )
})

testthat::test_that("normalize_openai_service_tier maps and validates tiers", {
  testthat::expect_null(normalize_openai_service_tier(NULL))
  testthat::expect_equal(normalize_openai_service_tier("standard"), "default")
  testthat::expect_equal(normalize_openai_service_tier("default"), "default")
  testthat::expect_equal(normalize_openai_service_tier("flex"), "flex")
  testthat::expect_equal(normalize_openai_service_tier("priority"), "priority")
  testthat::expect_equal(normalize_openai_service_tier("auto"), "auto")

  testthat::expect_error(
    normalize_openai_service_tier(1),
    "service_tier"
  )
  testthat::expect_error(
    normalize_openai_service_tier("gold"),
    "service_tier"
  )
})
