testthat::test_that("[6001] live submit_*_pairs_live returns existing results in return_mode = 'all' on full resume", {
  testthat::skip_if_not_installed("readr")

  td <- withr::local_tempdir()
  save_path <- file.path(td, "live_checkpoint.csv")

  existing <- tibble::tibble(
    custom_id = "MANUAL_1",
    ID1 = "S1",
    ID2 = "S2",
    model = "dummy",
    object_type = "dummy",
    status_code = 200L,
    error_message = NA_character_,
    thoughts = NA_character_,
    content = "ok",
    better_sample = "SAMPLE_1",
    better_id = "S1",
    prompt_tokens = 1,
    completion_tokens = 2,
    total_tokens = 3
  )
  readr::write_csv(existing, save_path)

  # Explicit custom_id should be preserved and used for resumability.
  pairs <- tibble::tibble(
    custom_id = "MANUAL_1",
    ID1 = "S1",
    text1 = "A",
    ID2 = "S2",
    text2 = "B"
  )

  # OpenAI
  out_openai_all <- submit_openai_pairs_live(
    pairs = pairs,
    model = "gpt-4o-mini",
    trait_name = "quality",
    trait_description = "overall quality",
    save_path = save_path,
    return_mode = "all",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out_openai_all$results), 1L)
  testthat::expect_equal(out_openai_all$results$custom_id[[1L]], "MANUAL_1")

  # Anthropic
  out_anthropic_all <- submit_anthropic_pairs_live(
    pairs = pairs,
    model = "claude-3-5-haiku-20241022",
    trait_name = "quality",
    trait_description = "overall quality",
    save_path = save_path,
    return_mode = "all",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out_anthropic_all$results), 1L)
  testthat::expect_equal(out_anthropic_all$results$custom_id[[1L]], "MANUAL_1")

  # Gemini
  out_gemini_all <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "gemini-1.5-flash",
    trait_name = "quality",
    trait_description = "overall quality",
    save_path = save_path,
    return_mode = "all",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out_gemini_all$results), 1L)
  testthat::expect_equal(out_gemini_all$results$custom_id[[1L]], "MANUAL_1")

  # Together
  out_together_all <- submit_together_pairs_live(
    pairs = pairs,
    model = "meta-llama/Llama-3.1-8B-Instruct-Turbo",
    trait_name = "quality",
    trait_description = "overall quality",
    save_path = save_path,
    return_mode = "all",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out_together_all$results), 1L)
  testthat::expect_equal(out_together_all$results$custom_id[[1L]], "MANUAL_1")

  # Ollama
  out_ollama_all <- submit_ollama_pairs_live(
    pairs = pairs,
    model = "llama3",
    trait_name = "quality",
    trait_description = "overall quality",
    save_path = save_path,
    return_mode = "all",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out_ollama_all$results), 1L)
  testthat::expect_equal(out_ollama_all$results$custom_id[[1L]], "MANUAL_1")
})
