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
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      status_code = 200L,
      prompt_tokens = prompt_tokens,
      completion_tokens = completion_tokens,
      total_tokens = prompt_tokens + completion_tokens
    )
  }

  est <- suppressWarnings(estimate_llm_pairs_cost(
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
  ))

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
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
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

testthat::test_that("estimate_llm_pairs_cost validates inputs", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  td <- trait_description("overall_quality")

  # Backend ollama
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = "t", trait_description = "d",
      backend = "ollama", cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "backend = \"ollama\""
  )

  # Missing columns
  testthat::expect_error(
    estimate_llm_pairs_cost(tibble::tibble(x = 1),
      model = "m", trait_name = "t", trait_description = "d",
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "must contain columns: ID1"
  )

  # Invalid numerics
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = "t", trait_description = "d",
      cost_per_million_input = "bad", cost_per_million_output = 1
    ),
    "cost_per_million_input"
  )

  # Empty pairs
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs[0, ],
      model = "m", trait_name = "t", trait_description = "d",
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "pairs` has 0 rows"
  )

  # Invalid n_test
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = "t", trait_description = "d",
      n_test = -1, cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "non-negative integer"
  )
})

testthat::test_that("estimate_llm_pairs_cost handles 0 pilot pairs (fallback calibration) and 1 pilot pair", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B"), text1 = c("Short", "Longer text here"),
    ID2 = c("C", "D"), text2 = c("Short", "Longer text here")
  )
  td <- trait_description("overall_quality")

  # Case 1: n_test = 0 -> Fallback calibration (slope 0.25, intercept 0)
  est_0 <- estimate_llm_pairs_cost(
    pairs = pairs, model = "gpt-4", trait_name = td$name, trait_description = td$description,
    n_test = 0,
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = function(...) stop("Should not be called")
  )

  # Check calibration fallback values (intercept 0, slope 0.25)
  cal_0 <- est_0$calibration$coefficients
  testthat::expect_equal(unname(cal_0["slope"]), 0.25)
  testthat::expect_equal(est_0$summary$pilot_prompt_tokens, 0)

  # Case 2: n_test = 1 -> Single point calibration
  fake_submit_1 <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      status_code = 200,
      prompt_tokens = 100,
      completion_tokens = 10
    )
  }

  est_1 <- estimate_llm_pairs_cost(
    pairs = pairs, model = "gpt-4", trait_name = td$name, trait_description = td$description,
    n_test = 1, test_strategy = "random", seed = 42,
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = fake_submit_1
  )

  # Check single point logic
  cal_1 <- est_1$calibration
  testthat::expect_equal(cal_1$n_used, 1)
  # Slope should be tokens / bytes. Intercept 0.
  testthat::expect_equal(unname(cal_1$coefficients["intercept"]), 0)
  testthat::expect_true(unname(cal_1$coefficients["slope"]) > 0)
})

testthat::test_that("estimate_llm_pairs_cost handles stratification edge cases", {
  td <- trait_description("overall_quality")

  # Edge Case: 1 total row (cannot stratify, forces n_test=1)
  pairs_1 <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  fake_submit <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      status_code = 200,
      prompt_tokens = 10,
      completion_tokens = 10
    )
  }

  est_single <- estimate_llm_pairs_cost(
    pairs = pairs_1, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 5, test_strategy = "stratified_prompt_bytes", # n_test > n_total
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = fake_submit
  )
  testthat::expect_equal(est_single$summary$n_test, 1)

  # Edge Case: Low variance (all texts identical) -> breaks < 2 -> fallback to random
  pairs_same <- tibble::tibble(
    ID1 = letters[1:10], text1 = rep("ABC", 10),
    ID2 = LETTERS[1:10], text2 = rep("DEF", 10)
  )

  # Suppress warnings about "essentially perfect fit" from lm() on identical data
  est_low_var <- suppressWarnings(estimate_llm_pairs_cost(
    pairs = pairs_same, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 4, test_strategy = "stratified_prompt_bytes",
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = function(pairs, ...) {
      tibble::tibble(
        ID1 = pairs$ID1,
        ID2 = pairs$ID2,
        better_id = pairs$ID1,
        status_code = 200,
        prompt_tokens = 10,
        completion_tokens = 10
      )
    }
  ))
  testthat::expect_equal(est_low_var$summary$n_test, 4)
  # Ensure it ran and didn't crash on cut()
  testthat::expect_s3_class(est_low_var, "pairwiseLLM_cost_estimate")

  # Edge Case: Topping up logic
  pairs_mix <- tibble::tibble(
    ID1 = 1:20,
    text1 = c(rep("a", 10), rep(paste(rep("a", 100), collapse = ""), 10)), # Clear separation
    ID2 = 1:20, text2 = "b"
  )

  # Suppress warnings about perfect fit
  est_strat <- suppressWarnings(estimate_llm_pairs_cost(
    pairs = pairs_mix, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 7, test_strategy = "stratified_prompt_bytes",
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = function(pairs, ...) {
      tibble::tibble(
        ID1 = pairs$ID1,
        ID2 = pairs$ID2,
        better_id = pairs$ID1,
        status_code = 200,
        prompt_tokens = 10,
        completion_tokens = 10
      )
    }
  ))
  testthat::expect_equal(est_strat$summary$n_test, 7)
})

testthat::test_that("estimate_llm_pairs_cost handles complex submitter returns (list with failures)", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  td <- trait_description("overall_quality")

  fake_submit_list <- function(...) {
    list(
      results = tibble::tibble(
        ID1 = "A",
        ID2 = "B",
        better_id = "A",
        status_code = 200,
        prompt_tokens = 50,
        completion_tokens = 50
      ),
      failed_pairs = tibble::tibble(ID1 = "Fail", error = "Msg")
    )
  }

  est <- estimate_llm_pairs_cost(
    pairs = pairs, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 1,
    cost_per_million_input = 1, cost_per_million_output = 1,
    return_test_results = FALSE,
    return_remaining_pairs = FALSE,
    .submit_fun = fake_submit_list
  )

  # Check list normalization worked
  testthat::expect_equal(est$summary$pilot_prompt_tokens, 50)
  # Check failed pairs are preserved
  testthat::expect_true("pilot_failed_pairs" %in% names(est))
  # Check toggle logic
  testthat::expect_false("pilot" %in% names(est))
  testthat::expect_false("remaining_pairs" %in% names(est))
})

testthat::test_that("print.pairwiseLLM_cost_estimate prints correctly", {
  # Mock object to avoid running full estimation
  x <- list(
    summary = tibble::tibble(
      backend = "openai", model = "gpt-4", endpoint = "chat.completions",
      n_total = 100, n_test = 10, n_remaining = 90,
      mode = "batch", batch_discount = 0.5,
      expected_total_prompt_tokens = 1000,
      expected_total_completion_tokens = 500,
      budget_total_completion_tokens = 600,
      expected_cost_total = 1.234567,
      budget_cost_total = 1.500000
    )
  )
  class(x) <- "pairwiseLLM_cost_estimate"

  # Capture output
  out <- capture.output(print(x))

  testthat::expect_true(any(grepl("Backend: openai", out)))
  testthat::expect_true(any(grepl("Batch discount: 0.5", out)))
  testthat::expect_true(any(grepl("Expected cost: 1.23457", out))) # Default formatting checks

  # Test empty summary edge case
  x_empty <- list(summary = tibble::tibble())
  class(x_empty) <- "pairwiseLLM_cost_estimate"
  out_empty <- capture.output(print(x_empty))
  testthat::expect_true(any(grepl("No summary available", out_empty)))
})

testthat::test_that("Internal helpers function correctly", {
  # These tests cover the hidden helper functions at the bottom of the file
  # which are not strictly called by the main function's inline logic,
  # but need coverage if they exist in the package namespace.

  # .count_fixed_occurrences
  txt <- "A {X} B {X}"
  testthat::expect_equal(pairwiseLLM:::.count_fixed_occurrences(txt, "{X}"), 2)
  testthat::expect_equal(pairwiseLLM:::.count_fixed_occurrences(txt, "{Y}"), 0)

  # .calibrate_prompt_tokens
  # Case: 0 inputs
  res0 <- pairwiseLLM:::.calibrate_prompt_tokens(numeric(0), numeric(0))
  testthat::expect_equal(res0$method, "fallback_bytes_per_token")

  # Case: 1 input
  res1 <- pairwiseLLM:::.calibrate_prompt_tokens(100, 25) # 25 tokens for 100 bytes
  testthat::expect_equal(res1$method, "single_point_ratio")
  testthat::expect_equal(res1$coefficients[["slope"]], 0.25)

  # Case: >1 input (linear model)
  res2 <- pairwiseLLM:::.calibrate_prompt_tokens(c(100, 200), c(10, 20))
  testthat::expect_equal(res2$method, "lm")

  # .predict_prompt_tokens
  calib <- list(coefficients = c(intercept = 5, slope = 0.1))
  preds <- pairwiseLLM:::.predict_prompt_tokens(calib, c(100, 200))
  testthat::expect_equal(preds, c(15, 25))

  # Predict negative clamping
  calib_neg <- list(coefficients = c(intercept = -100, slope = 0))
  testthat::expect_equal(pairwiseLLM:::.predict_prompt_tokens(calib_neg, 10), 0)
})

testthat::test_that("estimate_llm_pairs_cost validates detailed inputs", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  td <- trait_description("overall_quality")

  # 1. Validation: model must be character
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = 1, trait_name = td$name, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "model.*must be a single character"
  )

  # 2. Validation: trait_name must be character
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = 1, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "trait_name.*must be a single character"
  )

  # 3. Validation: trait_description must be character
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = td$name, trait_description = 1,
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "trait_description.*must be a single character"
  )

  # 4. Validation: cost_per_million_output must be numeric
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = td$name, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = "bad"
    ),
    "cost_per_million_output.*must be a single numeric"
  )

  # 5. Validation: batch_discount > 0
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = td$name, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = 1,
      mode = "batch", batch_discount = 0
    ),
    "batch_discount.*must be a single numeric value > 0"
  )

  # 6. Validation: budget_quantile in (0, 1)
  testthat::expect_error(
    estimate_llm_pairs_cost(pairs,
      model = "m", trait_name = td$name, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = 1,
      budget_quantile = 1.1
    ),
    "budget_quantile.*must be a single numeric value in"
  )

  # 7. Validation: Missing columns (constructs the error message)
  testthat::expect_error(
    estimate_llm_pairs_cost(tibble::tibble(x = 1),
      model = "m", trait_name = td$name, trait_description = td$description,
      cost_per_million_input = 1, cost_per_million_output = 1
    ),
    "must contain columns: ID1, text1, ID2, text2"
  )
})

testthat::test_that("estimate_llm_pairs_cost triggers stratified sampling top-up logic", {
  # Setup: Create a scenario where stratified allocation asks for more items
  # from a stratum than exist, forcing the 'top up' logic (lines 248-254) to run.
  # We use 3 items with byte lengths that create distinct strata.
  # If we have 2 strata and n_test=3: allocation might be c(2, 1).
  # If the first stratum only has 1 item, we get 1+1=2 items total.
  # We need 3, so top-up triggers for the last item.

  pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    text1 = c("a", "b", paste(rep("c", 1000), collapse="")), # Distinct lengths
    ID2 = c("A", "B", "C"),
    text2 = c("a", "b", "c")
  )
  td <- trait_description("overall_quality")

  fake_submit <- function(pairs, ...) {
    # Return dummy results for however many pairs are requested
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      status_code = 200,
      prompt_tokens = rep(10, nrow(pairs)),
      completion_tokens = rep(10, nrow(pairs))
    )
  }

  # Suppress warnings about "perfect fit" during calibration
  est <- suppressWarnings(estimate_llm_pairs_cost(
    pairs = pairs, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 3, test_strategy = "stratified_prompt_bytes",
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = fake_submit
  ))

  # Verify that we ended up with the requested number of tests
  # (proving the top-up logic successfully filled the gap)
  testthat::expect_equal(est$summary$n_test, 3)
  testthat::expect_equal(nrow(est$test_pairs), 3)
})

testthat::test_that("estimate_llm_pairs_cost stratifies skewed prompt bytes and tops up", {
  pairs <- tibble::tibble(
    ID1 = letters[1:5],
    text1 = c("a", "bb", strrep("c", 100), strrep("d", 100), strrep("e", 100)),
    ID2 = LETTERS[1:5],
    text2 = "z"
  )
  td <- trait_description("overall_quality")

  fake_submit <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      status_code = 200,
      prompt_tokens = rep(10L, nrow(pairs)),
      completion_tokens = rep(10L, nrow(pairs))
    )
  }

  withr::local_seed(123)
  est <- suppressWarnings(estimate_llm_pairs_cost(
    pairs = pairs, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 4, test_strategy = "stratified_prompt_bytes",
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = fake_submit
  ))

  testthat::expect_equal(est$summary$n_test, 4)
  testthat::expect_equal(nrow(est$test_pairs), 4)
})

testthat::test_that("estimate_llm_pairs_cost handles pilot mismatch and first strategy", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  td <- trait_description("overall_quality")

  # Trigger 1: Strategy "first" (line 213)
  # Trigger 2: Mismatch between requested n_test and pilot results row count (line 313 recycling)
  fake_submit_mismatch <- function(pairs, ...) {
    # Return 2 rows even though 1 pair passed
    tibble::tibble(
      ID1 = rep(pairs$ID1[1], 2),
      ID2 = rep(pairs$ID2[1], 2),
      better_id = rep(pairs$ID1[1], 2),
      status_code = 200,
      prompt_tokens = c(10, 20),
      completion_tokens = c(5, 5)
    )
  }

  est <- estimate_llm_pairs_cost(
    pairs = pairs, model = "m", trait_name = td$name, trait_description = td$description,
    n_test = 1, test_strategy = "first",
    cost_per_million_input = 1, cost_per_million_output = 1,
    .submit_fun = fake_submit_mismatch
  )

  # Check that "first" strategy was used (implied by execution path)
  testthat::expect_equal(est$summary$n_test, 1)

  # Check that mismatch logic summed the tokens correctly (10+20=30)
  testthat::expect_equal(est$summary$pilot_prompt_tokens, 30)
})

testthat::test_that("estimate_llm_pairs_cost recycles pilot bytes when results are invalid", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    text1 = c("short", "longer"),
    ID2 = c("C", "D"),
    text2 = c("x", "y")
  )
  td <- trait_description("overall_quality")

  fake_submit_invalid <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = "NOT_A_PAIR_ID",
      status_code = 200
    )
  }

  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4.1-mini",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = set_prompt_template(),
    mode = "live",
    n_test = 1,
    test_strategy = "first",
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    .submit_fun = fake_submit_invalid
  )

  testthat::expect_equal(est$summary$n_test, 1)
  testthat::expect_equal(est$summary$pilot_prompt_tokens, 0)
})

testthat::test_that("estimate_llm_pairs_cost falls back to default submitter", {
  # This tests the line: if (is.null(submit_fun)) submit_fun <- submit_llm_pairs
  # We set n_test = 0 so that 'submit_fun' is resolved but never CALLED.
  # This covers the resolution logic without requiring API keys or network.

  pairs <- tibble::tibble(ID1 = "A", text1 = "A", ID2 = "B", text2 = "B")
  td <- trait_description("overall_quality")

  est <- estimate_llm_pairs_cost(
    pairs = pairs, model = "gpt-4", trait_name = td$name, trait_description = td$description,
    backend = "openai", n_test = 0,
    cost_per_million_input = 1, cost_per_million_output = 1
    # No .submit_fun passed, triggers fallback
  )

  testthat::expect_s3_class(est, "pairwiseLLM_cost_estimate")
  testthat::expect_equal(est$summary$n_test, 0)
})
