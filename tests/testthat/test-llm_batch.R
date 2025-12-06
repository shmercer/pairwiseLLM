# tests/testthat/test-llm-batch.R

test_that("llm_submit_pairs_batch validates pairs and model", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bad_pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Sample 1"
    # missing ID2, text2
  )

  expect_error(
    llm_submit_pairs_batch(
      pairs             = bad_pairs,
      backend           = "openai",
      model             = "gpt-4o-mini",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "`pairs` must contain columns",
    fixed = FALSE
  )

  good_pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Sample 1",
    ID2   = "S02",
    text2 = "Sample 2"
  )

  expect_error(
    llm_submit_pairs_batch(
      pairs             = good_pairs,
      backend           = "openai",
      model             = "",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "`model` must be a non-empty character scalar",
    fixed = TRUE
  )
})

test_that("llm_submit_pairs_batch dispatches to the correct backend
          pipelines", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S02"),
    text1 = c("Text 1a", "Text 2a"),
    ID2   = c("S03", "S04"),
    text2 = c("Text 1b", "Text 2b")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  openai_calls <- list()
  anthropic_calls <- list()
  gemini_calls <- list()

  fake_batch_return <- function(backend_name) {
    input_path <- tempfile(
      pattern = paste0("input_", backend_name, "_"),
      fileext = ".jsonl"
    )
    output_path <- tempfile(
      pattern = paste0("output_", backend_name, "_"),
      fileext = ".jsonl"
    )

    # Create the files so file.exists() expectations pass
    file.create(input_path)
    file.create(output_path)

    list(
      backend = backend_name,
      batch_input_path = input_path,
      batch_output_path = output_path,
      batch = list(id = paste0("batch_", backend_name)),
      results = tibble::tibble(
        custom_id         = "BATCH_S01_vs_S02",
        ID1               = "S01",
        ID2               = "S02",
        model             = paste0("model_", backend_name),
        object_type       = "batch",
        status_code       = 200L,
        error_message     = NA_character_,
        thoughts          = NA_character_,
        content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
        better_sample     = "SAMPLE_1",
        better_id         = "S01",
        prompt_tokens     = 10L,
        completion_tokens = 2L,
        total_tokens      = 12L
      )
    )
  }

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(pairs,
                                         model,
                                         trait_name,
                                         trait_description,
                                         prompt_template,
                                         endpoint = c("chat.completions", "responses"),
                                         batch_input_path = tempfile("openai_batch_input_", fileext = ".jsonl"),
                                         batch_output_path = tempfile("openai_batch_output_", fileext = ".jsonl"),
                                         poll = TRUE,
                                         interval_seconds = 5,
                                         timeout_seconds = 600,
                                         max_attempts = Inf,
                                         metadata = NULL,
                                         api_key = Sys.getenv("OPENAI_API_KEY"),
                                         include_thoughts = FALSE,
                                         include_raw = FALSE,
                                         ...) {
      openai_calls <<- append(openai_calls, list(
        list(
          model             = model,
          trait_name        = trait_name,
          trait_description = trait_description,
          include_thoughts  = include_thoughts,
          include_raw       = include_raw
        )
      ))
      fake_batch_return("openai")
    },
    run_anthropic_batch_pipeline = function(pairs,
                                            model,
                                            trait_name,
                                            trait_description,
                                            prompt_template,
                                            include_thoughts = FALSE,
                                            include_raw = FALSE,
                                            ...) {
      anthropic_calls <<- append(anthropic_calls, list(
        list(
          model             = model,
          trait_name        = trait_name,
          trait_description = trait_description,
          include_thoughts  = include_thoughts,
          include_raw       = include_raw
        )
      ))
      fake_batch_return("anthropic")
    },
    run_gemini_batch_pipeline = function(pairs,
                                         model,
                                         trait_name,
                                         trait_description,
                                         prompt_template,
                                         include_thoughts = FALSE,
                                         include_raw = FALSE,
                                         ...) {
      gemini_calls <<- append(gemini_calls, list(
        list(
          model             = model,
          trait_name        = trait_name,
          trait_description = trait_description,
          include_thoughts  = include_thoughts,
          include_raw       = include_raw
        )
      ))
      fake_batch_return("gemini")
    },
    {
      # OpenAI
      batch_openai <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = "openai",
        model             = "gpt-4o-mini",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = FALSE,
        include_raw       = TRUE
      )

      expect_s3_class(batch_openai, "pairwiseLLM_batch")
      expect_equal(batch_openai$backend, "openai")
      expect_equal(length(openai_calls), 1L)
      expect_true(file.exists(batch_openai$batch_input_path))
      expect_true(file.exists(batch_openai$batch_output_path))

      # Anthropic
      batch_anthropic <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = "anthropic",
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = TRUE,
        include_raw       = FALSE
      )

      expect_s3_class(batch_anthropic, "pairwiseLLM_batch")
      expect_equal(batch_anthropic$backend, "anthropic")
      expect_equal(length(anthropic_calls), 1L)

      # Gemini
      batch_gemini <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = "gemini",
        model             = "gemini-3-pro-preview",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = TRUE,
        include_raw       = FALSE
      )

      expect_s3_class(batch_gemini, "pairwiseLLM_batch")
      expect_equal(batch_gemini$backend, "gemini")
      expect_equal(length(gemini_calls), 1L)
    }
  )
})

test_that("llm_submit_pairs_batch chooses OpenAI responses endpoint for
          gpt-5.1 with thoughts or reasoning", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1a",
    ID2   = "S02",
    text2 = "Text 1b"
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  endpoints <- list()

  fake_batch_return <- function(endpoint_value) {
    input_path <- tempfile(
      pattern = paste0("input_", endpoint_value, "_"),
      fileext = ".jsonl"
    )
    output_path <- tempfile(
      pattern = paste0("output_", endpoint_value, "_"),
      fileext = ".jsonl"
    )
    file.create(input_path)
    file.create(output_path)
    list(
      batch_input_path = input_path,
      batch_output_path = output_path,
      batch = list(id = paste0("batch_", endpoint_value)),
      results = tibble::tibble(
        custom_id         = "BATCH_S01_vs_S02",
        ID1               = "S01",
        ID2               = "S02",
        model             = "gpt-5.1-mini",
        object_type       = "batch",
        status_code       = 200L,
        error_message     = NA_character_,
        thoughts          = NA_character_,
        content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
        better_sample     = "SAMPLE_1",
        better_id         = "S01",
        prompt_tokens     = 10L,
        completion_tokens = 2L,
        total_tokens      = 12L
      )
    )
  }

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(pairs,
                                         model,
                                         trait_name,
                                         trait_description,
                                         prompt_template,
                                         endpoint = c("chat.completions", "responses"),
                                         batch_input_path = tempfile("openai_batch_input_", fileext = ".jsonl"),
                                         batch_output_path = tempfile("openai_batch_output_", fileext = ".jsonl"),
                                         poll = TRUE,
                                         interval_seconds = 5,
                                         timeout_seconds = 600,
                                         max_attempts = Inf,
                                         metadata = NULL,
                                         api_key = Sys.getenv("OPENAI_API_KEY"),
                                         include_thoughts = FALSE,
                                         include_raw = FALSE,
                                         ...) {
      endpoints <<- append(endpoints, list(endpoint))
      fake_batch_return(endpoint)
    },
    {
      # 1) gpt-5.1 with include_thoughts = TRUE -> responses endpoint
      batch_resp <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = "openai",
        model             = "gpt-5.1-mini",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = TRUE
      )

      expect_s3_class(batch_resp, "pairwiseLLM_batch")

      # 2) gpt-5.1 with include_thoughts = FALSE and reasoning = "none" ->
      # chat.completions
      batch_chat <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = "openai",
        model             = "gpt-5.1-mini",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = FALSE,
        reasoning         = "none"
      )

      expect_s3_class(batch_chat, "pairwiseLLM_batch")

      expect_equal(length(endpoints), 2L)
      # First call (with thoughts) should use responses
      expect_equal(endpoints[[1]], "responses")
      # Second call (no thoughts, reasoning = "none") should use
      # chat.completions
      expect_equal(endpoints[[2]], "chat.completions")
    }
  )
})

test_that("llm_download_batch_results extracts results tibble", {
  fake_batch <- list(
    backend = "openai",
    batch_input_path = "input.jsonl",
    batch_output_path = "output.jsonl",
    results = tibble::tibble(
      custom_id     = "BATCH_S01_vs_S02",
      ID1           = "S01",
      ID2           = "S02",
      model         = "model_openai",
      better_sample = "SAMPLE_1",
      better_id     = "S01"
    )
  )
  class(fake_batch) <- c("pairwiseLLM_batch", class(fake_batch))

  res <- llm_download_batch_results(fake_batch)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(res$ID1, "S01")
  expect_equal(res$better_sample, "SAMPLE_1")

  # Non classed, but list with results should still work
  res2 <- llm_download_batch_results(unclass(fake_batch))
  expect_equal(res2$ID2, "S02")

  # Invalid input
  expect_error(
    llm_download_batch_results(list(foo = "bar")),
    "Unsupported input to `llm_download_batch_results",
    fixed = FALSE
  )
})
