# tests/testthat/test-gemini_batch_api.R

# ==============================================================================
# build_gemini_batch_requests
# ==============================================================================

testthat::test_that("build_gemini_batch_requests builds valid requests (Happy Path)", {
  data("example_writing_samples", package = "pairwiseLLM")

  # Setup: Create a small pair set
  pairs <- make_pairs(example_writing_samples)[1:2, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_gemini_batch_requests(
    pairs             = pairs,
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    thinking_level    = "low"
  )

  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 2L)
  testthat::expect_true(all(c("custom_id", "request") %in% names(batch)))

  # Check request structure
  r1 <- batch$request[[1]]
  testthat::expect_true(is.list(r1$contents))
  testthat::expect_true(is.list(r1$generationConfig))

  # Verify prompt content insertion
  text_block <- r1$contents[[1]]$parts[[1]]$text
  testthat::expect_true(grepl("SAMPLE_1", text_block, fixed = TRUE))
  testthat::expect_true(grepl("SAMPLE_2", text_block, fixed = TRUE))

  # Verify thinking level mapping (low -> Low)
  testthat::expect_equal(r1$generationConfig$thinkingConfig$thinkingLevel, "Low")
})

testthat::test_that("build_gemini_batch_requests validates inputs and parameters", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")

  # 1. Error on missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "txt")
  testthat::expect_error(
    build_gemini_batch_requests(bad_pairs, "gemini-model", td$name, td$description),
    "must contain columns"
  )

  # 2. Error on invalid model string
  testthat::expect_error(
    build_gemini_batch_requests(pairs, "", td$name, td$description),
    "model.*must be a non-empty character"
  )

  # 3. Warning: thinking_budget ignored
  testthat::expect_warning(
    build_gemini_batch_requests(
      pairs, "m", td$name, td$description,
      thinking_budget = 1000
    ),
    "thinking_budget.*is ignored"
  )

  # 4. Warning: thinking_level = "medium" -> mapped to "High"
  testthat::expect_warning(
    req <- build_gemini_batch_requests(
      pairs, "m", td$name, td$description,
      thinking_level = "medium"
    ),
    "mapping to \"High\" internally"
  )
  testthat::expect_equal(req$request[[1]]$generationConfig$thinkingConfig$thinkingLevel, "High")

  # 5. Parameter passthrough (temperature, top_p, etc.)
  batch <- build_gemini_batch_requests(
    pairs, "m", td$name, td$description,
    temperature = 0.7, top_p = 0.9, include_thoughts = TRUE
  )
  config <- batch$request[[1]]$generationConfig
  testthat::expect_equal(config$temperature, 0.7)
  testthat::expect_equal(config$topP, 0.9)
  testthat::expect_true(config$thinkingConfig$includeThoughts)
})

# ==============================================================================
# .parse_gemini_pair_response (Internal Helper)
# ==============================================================================

testthat::test_that(".parse_gemini_pair_response handles structural edge cases", {
  # 1. Nested response wrappers (batch API quirk)
  # Sometimes the batch API wraps the result in `response` -> list -> `candidates`
  cand_obj <- list(content = list(parts = list(list(text = "Deep Content"))))
  nested_resp <- list(response = list(list(candidates = list(cand_obj))))

  res <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", nested_resp)
  testthat::expect_equal(res$content, "Deep Content")

  # 2. Candidates as data.frame (httr2 json parsing variation)
  content_obj <- list(parts = list(list(text = "DF Candidate")))
  df_cand <- data.frame(dummy = 1)
  df_cand$content <- list(content_obj)
  resp_df <- list(candidates = df_cand)

  res_df <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_df)
  testthat::expect_equal(res_df$content, "DF Candidate")

  # 3. Parts as data.frame
  parts_df <- data.frame(text = "DF Part", stringsAsFactors = FALSE)
  content_df_parts <- list(parts = parts_df)
  resp_df_parts <- list(candidates = list(list(content = content_df_parts)))

  res_df_parts <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_df_parts)
  testthat::expect_equal(res_df_parts$content, "DF Part")

  # 4. Content as data.frame (direct text, no parts list)
  parts_direct_df <- data.frame(text = "Direct DF", stringsAsFactors = FALSE)
  resp_direct_df <- list(candidates = list(list(content = parts_direct_df)))

  res_direct <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_direct_df)
  testthat::expect_equal(res_direct$content, "Direct DF")
})

testthat::test_that(".parse_gemini_pair_response handles metadata extraction", {
  # 1. Model preference: modelVersion > model
  resp_models <- list(
    candidates = list(list(content = list(parts = list(list(text = "x"))))),
    model = "v1",
    modelVersion = "v2"
  )
  res <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_models)
  testthat::expect_equal(res$model, "v2")

  # 2. Missing usage metadata -> NAs
  resp_no_usage <- list(candidates = list(list(content = list(parts = list(list(text = "x"))))))
  res_nu <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_no_usage)
  testthat::expect_true(is.na(res_nu$total_tokens))

  # 3. thoughtSignature extraction
  resp_sig <- list(
    candidates = list(list(content = list(parts = list(list(
      text = "T",
      thoughtSignature = "SIG123"
    )))))
  )
  res_sig <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_sig)
  testthat::expect_equal(res_sig$thought_signature, "SIG123")
})

testthat::test_that(".parse_gemini_pair_response handles explicit thoughts logic", {
  # 1. Error response handling
  err_resp <- list(error = list(message = "Blocked"))
  res_err <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", err_resp)
  testthat::expect_equal(res_err$result_type, "errored")
  testthat::expect_equal(res_err$error_message, "Blocked")

  # 2. Explicit thoughts: include_thoughts=TRUE, 2 parts
  resp_thoughts <- list(
    candidates = list(list(content = list(parts = list(
      list(text = "Thinking..."),
      list(text = "Answer")
    ))))
  )
  res_t <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_thoughts, include_thoughts = TRUE)
  testthat::expect_equal(res_t$thoughts, "Thinking...")
  testthat::expect_equal(res_t$content, "Answer")

  # 3. Fallback: include_thoughts=TRUE but only 1 part
  resp_single <- list(
    candidates = list(list(content = list(parts = list(
      list(text = "Just answer")
    ))))
  )
  res_s <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_single, include_thoughts = TRUE)
  testthat::expect_true(is.na(res_s$thoughts))
  testthat::expect_equal(res_s$content, "Just answer")
})

# ==============================================================================
# gemini_create_batch & gemini_get_batch
# ==============================================================================

testthat::test_that("gemini_create_batch validates inputs", {
  testthat::expect_error(
    gemini_create_batch(list(), "model"),
    "must be a non-empty list"
  )
  testthat::expect_error(
    gemini_create_batch(list(a = 1), ""),
    "model.*must be a non-empty character"
  )
})

testthat::test_that("gemini_get_batch validates inputs", {
  testthat::expect_error(
    gemini_get_batch(""),
    "batch_name.*must be a non-empty"
  )
})

# ==============================================================================
# gemini_poll_batch_until_complete
# ==============================================================================

testthat::test_that("gemini_poll_batch_until_complete handles validation and timeouts", {
  testthat::expect_error(
    gemini_poll_batch_until_complete(""),
    "batch_name.*non-empty"
  )

  # Mock to always return RUNNING state to trigger timeout
  testthat::with_mocked_bindings(
    gemini_get_batch = function(...) list(name = "b", metadata = list(state = "RUNNING")),
    {
      testthat::expect_warning(
        res <- gemini_poll_batch_until_complete(
          "b",
          interval_seconds = 0, timeout_seconds = 0, verbose = TRUE # Verbose must be TRUE for warning
        ),
        "Timeout reached"
      )
      testthat::expect_equal(res$metadata$state, "RUNNING")
    }
  )
})

# ==============================================================================
# gemini_download_batch_results
# ==============================================================================

testthat::test_that("gemini_download_batch_results validation and error handling", {
  tmp <- tempfile()
  reqs <- tibble::tibble(custom_id = "1")

  # 1. Missing inlinedResponses
  batch_bad <- list(response = list())
  testthat::expect_error(
    gemini_download_batch_results(batch_bad, reqs, tmp),
    "Batch does not contain response\\$inlinedResponses"
  )

  # 2. Unsupported structure (not a data frame)
  batch_weird <- list(response = list(inlinedResponses = list(something = 1)))
  testthat::expect_error(
    gemini_download_batch_results(batch_weird, reqs, tmp),
    "Unsupported structure"
  )

  # 3. Request vs Response count mismatch
  # Mock a structure where response is a DF with 2 rows, but we pass 3 requests
  inner_df <- data.frame(row_id = 1:2)
  # The helper expects inlined$response to be a DF or inlined itself to be a DF
  inner_df$response <- data.frame(candidates = I(list(list(), list())))
  batch_mismatch <- list(response = list(inlinedResponses = inner_df))

  reqs_mismatch <- tibble::tibble(custom_id = c("1", "2", "3"))

  testthat::with_mocked_bindings(
    gemini_get_batch = function(...) batch_mismatch,
    {
      testthat::expect_warning(
        gemini_download_batch_results("b", reqs_mismatch, tmp),
        "Number of inlined responses.*does not match"
      )
      testthat::expect_true(file.exists(tmp))
    }
  )
})

testthat::test_that("gemini_download_batch_results handles nested inlinedResponses", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Mock structure: response$inlinedResponses$inlinedResponses
  inner_df <- data.frame(dummy = 1)
  inner_df$response <- data.frame(candidates = I(list(list()))) # Valid inner structure

  batch_nested <- list(
    response = list(
      inlinedResponses = list(
        inlinedResponses = inner_df
      )
    )
  )
  reqs <- tibble::tibble(custom_id = "1")

  testthat::with_mocked_bindings(
    gemini_get_batch = function(...) stop("Should not be called"),
    {
      out <- gemini_download_batch_results(batch_nested, reqs, tmp)
      testthat::expect_true(file.exists(out))
      lines <- readLines(out)
      testthat::expect_equal(length(lines), 1L)
    }
  )
})

# ==============================================================================
# parse_gemini_batch_output
# ==============================================================================

testthat::test_that("parse_gemini_batch_output handles standard success and error lines", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  # 1. Success line
  succ_resp <- list(
    model = "gemini-pro",
    candidates = list(list(content = list(parts = list(list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"))))),
    usageMetadata = list(promptTokenCount = 10, candidatesTokenCount = 5, totalTokenCount = 15)
  )
  line_ok <- list(custom_id = "GEM_S01_vs_S02", result = list(type = "succeeded", response = succ_resp))

  # 2. Error line
  line_err <- list(
    custom_id = "GEM_S03_vs_S04",
    result = list(type = "errored", error = list(message = "Validation error"))
  )

  writeLines(c(jsonlite::toJSON(line_ok, auto_unbox = TRUE), jsonlite::toJSON(line_err, auto_unbox = TRUE)), tmp)

  reqs <- tibble::tibble(
    custom_id = c("GEM_S01_vs_S02", "GEM_S03_vs_S04"),
    ID1       = c("S01", "S03"),
    ID2       = c("S02", "S04")
  )

  res <- parse_gemini_batch_output(tmp, reqs)

  # Check Success
  r1 <- res[1, ]
  testthat::expect_equal(r1$result_type, "succeeded")
  testthat::expect_equal(r1$better_sample, "SAMPLE_2")
  testthat::expect_equal(r1$total_tokens, 15)

  # Check Error
  r2 <- res[2, ]
  testthat::expect_equal(r2$result_type, "errored")
  testthat::expect_match(r2$error_message, "Validation error")
  testthat::expect_true(is.na(r2$content))
})

testthat::test_that("parse_gemini_batch_output handles empty files, unknown IDs, invalid JSON", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  reqs <- tibble::tibble(custom_id = "A", ID1 = "1", ID2 = "2")

  # 1. Empty File
  file.create(tmp)
  res_empty <- parse_gemini_batch_output(tmp, reqs)
  testthat::expect_equal(nrow(res_empty), 0L)

  # 2. Unknown ID in file
  line_unk <- list(custom_id = "UNKNOWN", result = list(type = "succeeded"))
  writeLines(jsonlite::toJSON(line_unk, auto_unbox = TRUE), tmp)
  res_unk <- parse_gemini_batch_output(tmp, reqs)
  testthat::expect_equal(res_unk$custom_id, "UNKNOWN")
  testthat::expect_true(is.na(res_unk$ID1)) # No match in reqs

  # 3. Invalid JSON content
  writeLines("NOT_JSON", tmp)
  res_inv <- parse_gemini_batch_output(tmp, reqs)
  testthat::expect_match(res_inv$error_message, "Failed to parse JSON line")
})

testthat::test_that("parse_gemini_batch_output detects include_thoughts from request column", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Result has 2 parts
  resp_data <- list(candidates = list(list(content = list(parts = list(
    list(text = "Thought"), list(text = "Answer")
  )))))
  line <- list(custom_id = "CID", result = list(type = "succeeded", response = resp_data))
  writeLines(jsonlite::toJSON(line, auto_unbox = TRUE), tmp)

  # Case A: Request has includeThoughts = TRUE
  req_tbl_true <- tibble::tibble(
    custom_id = "CID", ID1 = "A", ID2 = "B",
    request = list(list(generationConfig = list(thinkingConfig = list(includeThoughts = TRUE))))
  )
  res_true <- parse_gemini_batch_output(tmp, req_tbl_true)
  testthat::expect_equal(res_true$thoughts, "Thought")
  testthat::expect_equal(res_true$content, "Answer")

  # Case B: Request has includeThoughts = FALSE (or missing)
  req_tbl_false <- tibble::tibble(
    custom_id = "CID", ID1 = "A", ID2 = "B",
    request = list(list(generationConfig = list(thinkingConfig = list(includeThoughts = FALSE))))
  )
  res_false <- parse_gemini_batch_output(tmp, req_tbl_false)
  testthat::expect_true(is.na(res_false$thoughts))
  testthat::expect_equal(res_false$content, "ThoughtAnswer")
})

# ==============================================================================
# run_gemini_batch_pipeline
# ==============================================================================

testthat::test_that("run_gemini_batch_pipeline runs full mocked cycle", {
  pairs <- tibble::tibble(ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B")
  td <- trait_description("overall_quality") # Valid trait name
  tmpl <- set_prompt_template()

  # Mock returns
  fake_req_tbl <- tibble::tibble(custom_id = "1", ID1 = "S1", ID2 = "S2", request = list(list()))
  fake_batch_initial <- list(name = "b1", metadata = list(state = "RUNNING"))
  fake_batch_final <- list(name = "b1", metadata = list(state = "SUCCEEDED"))
  fake_results <- tibble::tibble(custom_id="1", ID1="S1", ID2="S2", result_type="succeeded", better_id="S1")

  testthat::with_mocked_bindings(
    build_gemini_batch_requests = function(...) fake_req_tbl,
    gemini_create_batch = function(...) fake_batch_initial,
    gemini_poll_batch_until_complete = function(...) fake_batch_final,
    gemini_download_batch_results = function(batch, requests_tbl, output_path, ...) {
      writeLines('{"dummy": true}', output_path) # Create dummy file using correct arg name
      invisible(output_path)
    },
    parse_gemini_batch_output = function(...) fake_results,
    {
      res <- run_gemini_batch_pipeline(
        pairs, "m", td$name, td$description, tmpl,
        interval_seconds = 0, timeout_seconds = 0, verbose = FALSE
      )

      testthat::expect_equal(res$batch$metadata$state, "SUCCEEDED")
      testthat::expect_equal(res$results$better_id, "S1")
      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_true(file.exists(res$batch_output_path))
    }
  )
})

testthat::test_that("run_gemini_batch_pipeline respects poll=FALSE", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  td <- trait_description("overall_quality") # Valid trait name

  fake_batch <- list(name = "b", metadata = list(state = "RUNNING"))

  testthat::with_mocked_bindings(
    build_gemini_batch_requests = function(...) tibble::tibble(custom_id = "1", ID1 = "A", ID2 = "B", request = list(list())),
    gemini_create_batch = function(...) fake_batch,
    gemini_poll_batch_until_complete = function(...) stop("Should not poll"),
    {
      res <- run_gemini_batch_pipeline(
        pairs, "m", td$name, td$description,
        poll = FALSE, verbose = FALSE
      )
      testthat::expect_null(res$results)
      testthat::expect_null(res$batch_output_path)
      testthat::expect_equal(res$batch$metadata$state, "RUNNING")
    }
  )
})

testthat::test_that("build_gemini_batch_requests passes top_k and max_output_tokens", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  td <- trait_description("overall_quality")

  batch <- build_gemini_batch_requests(
    pairs, "gemini-model", td$name, td$description,
    top_k = 40,
    max_output_tokens = 1024
  )

  config <- batch$request[[1]]$generationConfig
  testthat::expect_equal(config$topK, 40)
  testthat::expect_equal(config$maxOutputTokens, 1024)
})

testthat::test_that("gemini_create_batch generates default display_name if missing", {
  captured_body <- NULL

  testthat::with_mocked_bindings(
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .gemini_req_perform = function(...) structure(list(), class = "httr2_response"),
    .gemini_resp_body_json = function(...) list(name = "b"),
    {
      # 1. No display name provided
      gemini_create_batch(
        requests = list(list(x = 1)),
        model = "gemini-pro"
      )

      testthat::expect_true(!is.null(captured_body$batch$display_name))
      testthat::expect_true(grepl("pairwiseLLM-gemini-batch-", captured_body$batch$display_name))

      # 2. Empty string provided (should trigger default)
      gemini_create_batch(
        requests = list(list(x = 1)),
        model = "gemini-pro",
        display_name = ""
      )
      testthat::expect_true(grepl("pairwiseLLM-gemini-batch-", captured_body$batch$display_name))
    }
  )
})

testthat::test_that("gemini_poll_batch_until_complete loops until terminal state", {
  # Mock returning RUNNING twice, then SUCCEEDED
  states <- c("BATCH_STATE_RUNNING", "BATCH_STATE_RUNNING", "BATCH_STATE_SUCCEEDED")
  call_count <- 0

  testthat::with_mocked_bindings(
    gemini_get_batch = function(...) {
      call_count <<- call_count + 1
      # Return current state based on call count
      s <- if (call_count <= length(states)) states[call_count] else "BATCH_STATE_SUCCEEDED"
      list(name = "b", metadata = list(state = s))
    },
    {
      # We rely on the short interval_seconds=0.01 to make this fast enough without mocking Sys.sleep
      res <- gemini_poll_batch_until_complete(
        "b", interval_seconds = 0.01, timeout_seconds = 10, verbose = FALSE
      )

      # Should have called get_batch 3 times
      testthat::expect_equal(call_count, 3)
      testthat::expect_equal(res$metadata$state, "BATCH_STATE_SUCCEEDED")
    }
  )
})

testthat::test_that(".parse_gemini_pair_response finds deeply nested usage metadata", {
  # Test the recursive logic of the internal find_named helper
  # Construct a response where usageMetadata keys are hidden inside a wrapper list
  resp_nested_usage <- list(
    candidates = list(list(content = list(parts = list(list(text = "A"))))),
    usageMetadata = list(
      wrapper = list(
        nested = list(
          totalTokenCount = 999
        )
      )
    )
  )

  res <- pairwiseLLM:::.parse_gemini_pair_response("id", "A", "B", resp_nested_usage)
  testthat::expect_equal(res$total_tokens, 999)
})

testthat::test_that("gemini_download_batch_results accepts batch object input directly", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Valid batch object with inline responses
  batch_obj <- list(
    response = list(
      inlinedResponses = data.frame(
        row_id = 1,
        response = I(data.frame(candidates = I(list(list()))))
      )
    )
  )

  reqs <- tibble::tibble(custom_id = "1")

  # Ensure gemini_get_batch is NOT called (mock it to fail)
  testthat::with_mocked_bindings(
    gemini_get_batch = function(...) stop("Should not fetch when object provided"),
    {
      out <- gemini_download_batch_results(batch_obj, reqs, tmp)
      testthat::expect_true(file.exists(out))
    }
  )
})

testthat::test_that("build_gemini_batch_requests uses pair_uid for custom_id", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-xyz"
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_gemini_batch_requests(
    pairs = pairs,
    model = "gemini-model",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    thinking_level = "low"
  )

  testthat::expect_equal(batch$custom_id[1], "pair-xyz")
})

testthat::test_that("gemini_get_batch returns parsed response", {
  captured_path <- NULL

  testthat::with_mocked_bindings(
    .gemini_request = function(path, api_key) {
      captured_path <<- path
      structure(list(path = path), class = "httr2_request")
    },
    .gemini_req_perform = function(req) structure(list(), class = "httr2_response"),
    .gemini_resp_body_json = function(resp, simplifyVector = TRUE) list(name = "batches/1"),
    {
      res <- gemini_get_batch("batches/1", api_key = "key", api_version = "v1beta")
      testthat::expect_equal(res$name, "batches/1")
      testthat::expect_equal(captured_path, "/v1beta/batches/1")
    }
  )
})

testthat::test_that("parse_gemini_batch_output handles non-errored terminal result types", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  line <- list(custom_id = "CID", result = list(type = "expired"))
  writeLines(jsonlite::toJSON(line, auto_unbox = TRUE), tmp)

  reqs <- tibble::tibble(custom_id = "CID", ID1 = "A", ID2 = "B")
  res <- parse_gemini_batch_output(tmp, reqs)

  testthat::expect_equal(res$result_type, "expired")
  testthat::expect_true(is.na(res$error_message))
})

testthat::test_that("run_gemini_batch_pipeline errors when batch name is missing", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  testthat::with_mocked_bindings(
    build_gemini_batch_requests = function(...) tibble::tibble(custom_id = "1", ID1 = "A", ID2 = "B", request = list(list())),
    gemini_create_batch = function(...) list(),
    {
      testthat::expect_error(
        run_gemini_batch_pipeline(
          pairs = pairs,
          model = "gemini-model",
          trait_name = td$name,
          trait_description = td$description,
          prompt_template = tmpl,
          poll = TRUE,
          verbose = FALSE
        ),
        "did not contain a `name` field"
      )
    }
  )
})
