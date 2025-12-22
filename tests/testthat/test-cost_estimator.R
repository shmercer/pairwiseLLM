testthat::test_that("estimate_llm_pairs_cost returns expected and budget cost with batch discount", {
  pairs <- tibble::tibble(
    ID1 = paste0("S", sprintf("%02d", 1:10)),
    text1 = rep("AAAAA", 10),
    ID2 = paste0("T", sprintf("%02d", 1:10)),
    text2 = vapply(1:10, function(i) paste(rep("B", i), collapse = ""), character(1))
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_submit <- function(pairs, model, trait_name, trait_description, prompt_template,
                          backend, endpoint, ...) {
    # Create a deterministic relationship between prompt bytes and prompt tokens
    bytes <- pairwiseLLM:::`.prompt_bytes_for_pairs`(
      template = prompt_template,
      trait_name = trait_name,
      trait_desc = trait_description,
      text1 = pairs$text1,
      text2 = pairs$text2
    )
    prompt_tokens <- as.integer(round(5 + 0.1 * bytes))

    # Pilot completion tokens chosen so budget > expected
    completion_tokens <- c(10L, 30L)[seq_len(nrow(pairs))]

    tibble::tibble(
      custom_id = paste0("X", seq_len(nrow(pairs))),
      status_code = 200L,
      prompt_tokens = prompt_tokens,
      completion_tokens = completion_tokens,
      total_tokens = prompt_tokens + completion_tokens
    )
  }

  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4.1-mini",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    mode = "batch",
    batch_discount = 0.5,
    n_test = 2,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    verbose = FALSE,
    progress = FALSE,
    .submit_fun = fake_submit
  )

  testthat::expect_s3_class(est, "pairwiseLLM_cost_estimate")

  s <- est$summary
  testthat::expect_equal(s$n_total, 10)
  testthat::expect_equal(s$n_test, 2)
  testthat::expect_equal(s$n_remaining, 8)
  testthat::expect_true("pilot" %in% names(est))
  testthat::expect_true("remaining_pairs" %in% names(est))

  # Budget should be higher than expected because pilot completion tokens are 10 and 30
  testthat::expect_gt(s$budget_cost_total, s$expected_cost_total)
})

testthat::test_that("estimate_llm_pairs_cost handles missing output tokens in pilot", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    text1 = c("t1", "t2", "t3"),
    ID2 = c("D", "E", "F"),
    text2 = c("u1", "u2", "u3")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_submit <- function(pairs, ...) {
    tibble::tibble(
      status_code = 200L,
      prompt_tokens = c(100L, 120L),
      completion_tokens = c(50L, NA_integer_),
      total_tokens = c(150L, NA_integer_)
    )
  }

  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4.1-mini",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    mode = "live",
    n_test = 2,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    verbose = FALSE,
    progress = FALSE,
    .submit_fun = fake_submit
  )

  s <- est$summary

  # Pilot output totals should only include non-missing completion tokens
  testthat::expect_equal(s$pilot_completion_tokens, 50)

  # Estimator should still produce output estimates using available pilot outputs
  testthat::expect_false(is.na(s$est_remaining_completion_tokens_expected))
  testthat::expect_false(is.na(s$est_remaining_completion_tokens_budget))
})
