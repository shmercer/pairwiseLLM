# =====================================================================
#   test-ollama_live.R
#   Tests for ollama_compare_pair_live() and submit_ollama_pairs_live()
# =====================================================================

# ---------------------------------------------------------------------
# ollama_compare_pair_live: happy path
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live parses successful response
                    correctly", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_body <- list(
    model = "mistral-small3.2:24b",
    response =
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation.",
    prompt_eval_count = 10L,
    eval_count = 5L
  )

  captured_body <- NULL

  testthat::with_mocked_bindings(
    # Mock the retry wrapper so no real HTTP requests are made
    .retry_httr2_request = function(req) {
      structure(list(), class = "fake_resp")
    },
    # We only need to intercept these; request/req_url_path_append/req_error
    # can remain as in the real code.
    req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        num_ctx           = 8192L,
        think             = FALSE,
        include_raw       = TRUE
      )

      # Basic shape
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$custom_id, sprintf("LIVE_%s_vs_%s", ID1, ID2))
      testthat::expect_equal(res$ID1, ID1)
      testthat::expect_equal(res$ID2, ID2)

      testthat::expect_equal(res$model, "mistral-small3.2:24b")
      testthat::expect_equal(res$object_type, "ollama.generate")
      testthat::expect_equal(res$status_code, 200L)
      testthat::expect_true(is.na(res$error_message))

      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation."
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)

      # Token counts from prompt_eval_count + eval_count
      testthat::expect_equal(res$prompt_tokens, 10)
      testthat::expect_equal(res$completion_tokens, 5)
      testthat::expect_equal(res$total_tokens, 15)

      # raw_response
      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_type(res$raw_response, "list")
      testthat::expect_equal(
        res$raw_response[[1]]$model, "mistral-small3.2:24b"
      )

      # Request body sanity checks
      testthat::expect_type(captured_body, "list")
      testthat::expect_equal(captured_body$model, "mistral-small3.2:24b")
      testthat::expect_false(isTRUE(captured_body$stream))

      # Default context window + temperature for non-Qwen models
      testthat::expect_equal(captured_body$options$num_ctx, 8192L)
      testthat::expect_equal(captured_body$options$temperature, 0)
    }
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: Input Validation
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live validates scalar arguments", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # ID1 must be scalar character
  expect_error(
    ollama_compare_pair_live(
      ID1               = c("S01", "S02"),
      text1             = "A",
      ID2               = "S02",
      text2             = "B",
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "`ID1` must be a single character."
  )

  # ID2 must be scalar character
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = "A",
      ID2               = c("S02", "S03"),
      text2             = "B",
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "`ID2` must be a single character."
  )

  # text1
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = character(0),
      ID2               = "S02",
      text2             = "B",
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description
    ),
    "`text1` must be a single character."
  )

  # text2
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = "A",
      ID2               = "S02",
      text2             = c("B", "C"),
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description
    ),
    "`text2` must be a single character."
  )

  # model
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = "A",
      ID2               = "S02",
      text2             = "B",
      model             = 123,
      trait_name        = td$name,
      trait_description = td$description
    ),
    "`model` must be a single character."
  )

  # host must be non-empty
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = "A",
      ID2               = "S02",
      text2             = "B",
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description,
      host              = ""
    ),
    "`host` must be a non-empty character scalar."
  )

  # num_ctx must be positive scalar
  expect_error(
    ollama_compare_pair_live(
      ID1               = "S01",
      text1             = "A",
      ID2               = "S02",
      text2             = "B",
      model             = "m",
      trait_name        = td$name,
      trait_description = td$description,
      num_ctx           = 0
    ),
    "`num_ctx` must be a single positive number."
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: Qwen + think = TRUE → temperature = 0.6
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live sets Qwen temperature
                    when think = TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    model             = "qwen3:32b",
    response          = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Explanation.",
    prompt_eval_count = 4L,
    eval_count        = 6L
  )

  captured_body <- NULL

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) {
      structure(list(), class = "fake_resp")
    },
    req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "qwen3:32b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        think             = TRUE,
        num_ctx           = 4096L,
        include_raw       = FALSE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)

      # Temperature logic: Qwen + think = TRUE → 0.6
      testthat::expect_type(captured_body, "list")
      testthat::expect_equal(captured_body$model, "qwen3:32b")
      testthat::expect_equal(captured_body$options$num_ctx, 4096L)
      testthat::expect_equal(captured_body$options$temperature, 0.6)
    }
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: Response parsing robustness
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live handles missing tags and tokens", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_body <- list(
    model = "m",
    response = "I am undecided and provide no tags.",
    # prompt_eval_count / eval_count intentionally missing
    done = TRUE
  )

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) structure(list(), class = "fake_resp"),
    req_body_json = function(req, body) req,
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1 = "S01", text1 = "A", ID2 = "S02", text2 = "B", model = "m",
        trait_name = td$name, trait_description = td$description
      )

      # Should result in NA for decision and tokens
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.na(res$better_id))
      testthat::expect_true(is.na(res$prompt_tokens))
      testthat::expect_true(is.na(res$total_tokens))
    }
  )
})

testthat::test_that("ollama_compare_pair_live respects custom tag prefixes", {
  fake_body <- list(model = "m", response = "<WIN>SAMPLE_1</WIN> won.")

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) structure(list(), class = "fake_resp"),
    req_body_json = function(req, body) req,
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1 = "S01", text1 = "A", ID2 = "S02", text2 = "B", model = "m",
        trait_name = "t", trait_description = "d",
        tag_prefix = "<WIN>", tag_suffix = "</WIN>"
      )
      testthat::expect_equal(res$better_sample, "SAMPLE_1")
    }
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: JSON parse failure
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live returns error row on
                    JSON parse failure", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) {
      structure(list(), class = "fake_resp")
    },
    req_body_json = function(req, body) req,
    resp_body_json = function(resp, simplifyVector = FALSE) stop("boom"),
    resp_status = function(resp) 500L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = "X",
        ID2               = ID2,
        text2             = "Y",
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        include_raw       = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(
        res$error_message,
        "Failed to parse response body as JSON."
      )
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.na(res$better_id))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: zero rows
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live returns list with empty tibbles
                    for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res <- submit_ollama_pairs_live(
    pairs             = empty_pairs,
    model             = "mistral-small3.2:24b",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    verbose           = FALSE,
    progress          = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_ollama_pairs_live reports no new pairs with include_raw", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  msgs <- testthat::capture_messages(
    res <- submit_ollama_pairs_live(
      pairs             = empty_pairs,
      model             = "mistral-small3.2:24b",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      verbose           = TRUE,
      progress          = FALSE,
      include_raw       = TRUE
    )
  )

  testthat::expect_true(any(grepl("No new pairs to process", msgs)))
  testthat::expect_true("raw_response" %in% names(res$results))
})


# ---------------------------------------------------------------------
# submit_ollama_pairs_live: row-wise calling of ollama_compare_pair_live
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live calls ollama_compare_pair_live
                    row-wise and returns list", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = "mistral-small3.2:24b",
      object_type       = "ollama.generate",
      status_code       = 200L,
      error_message     = NA_character_,
      thoughts          = NA_character_,
      content           = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample     = chosen,
      better_id         = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens     = 10,
      completion_tokens = 5,
      total_tokens      = 15
    )
  }

  calls <- list()

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(ID1, ID2, ...) {
      calls <<- append(calls, list(list(ID1 = ID1, ID2 = ID2)))
      if (ID1 == "S01") {
        fake_result(ID1, ID2, "SAMPLE_1")
      } else {
        fake_result(ID1, ID2, "SAMPLE_2")
      }
    },
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = FALSE,
        progress          = FALSE
      )

      testthat::expect_equal(length(calls), 2L)

      # Check list structure
      testthat::expect_type(res, "list")
      testthat::expect_s3_class(res$results, "tbl_df")
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_equal(res$results$better_id, c("S01", "S04"))
      testthat::expect_equal(nrow(res$failed_pairs), 0L)
    }
  )
})

testthat::test_that("submit_ollama_pairs_live captures errors in failed_pairs", {
  pairs <- tibble::tibble(ID1 = "a", text1 = "a", ID2 = "b", text2 = "b")

  testthat::with_mocked_bindings(
    # Mock inner function to throw an error
    ollama_compare_pair_live = function(...) stop("Inner crash detected"),
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs,
        model = "m", trait_name = "t", trait_description = "d", verbose = FALSE
      )

      # The errored row appears in results (with error info) AND in failed_pairs
      testthat::expect_equal(nrow(res$results), 0L)

      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "a")
      testthat::expect_match(res$failed_pairs$error_message, "Inner crash detected")
    }
  )
})

testthat::test_that("submit_ollama_pairs_live logs errors and strips raw_response on save", {
  testthat::skip_if_not_installed("readr")
  pairs <- tibble::tibble(ID1 = "a", text1 = "a", ID2 = "b", text2 = "b")
  save_path <- withr::local_tempfile(fileext = ".csv")

  msgs <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      ollama_compare_pair_live = function(...) stop("Inner crash detected"),
      .package = "pairwiseLLM",
      {
        testthat::with_mocked_bindings(
          write_csv = function(x, ...) {
            testthat::expect_false("raw_response" %in% names(x))
          },
          .package = "readr",
          {
            submit_ollama_pairs_live(
              pairs,
              model = "m", trait_name = "t", trait_description = "d",
              include_raw = TRUE, save_path = save_path, verbose = TRUE, progress = FALSE
            )
          }
        )
      }
    )
  )

  testthat::expect_true(any(grepl("ERROR: Ollama comparison failed", msgs)))
})

testthat::test_that("submit_ollama_pairs_live catches errors thrown by inner function", {
  pairs <- tibble::tibble(ID1 = "a", text1 = "a", ID2 = "b", text2 = "b")

  testthat::with_mocked_bindings(
    # Mock inner function to throw an error (simulating network crash, etc.)
    ollama_compare_pair_live = function(...) stop("Inner crash detected"),
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs,
        model = "m", trait_name = "t", trait_description = "d", verbose = FALSE
      )

      # The result is now a list. The errored row should be in $results and $failed_pairs
      testthat::expect_type(res, "list")

      # Check $results
      testthat::expect_equal(nrow(res$results), 0L)


      # No successful results; error is captured in failed_pairs

      # Check $failed_pairs
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_match(res$failed_pairs$error_message, "Inner crash detected")
    }
  )
})

testthat::test_that("ollama_compare_pair_live exposes thinking only when think = TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    model             = "qwen3:32b",
    response          = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Explanation.",
    prompt_eval_count = 4L,
    eval_count        = 6L,
    thinking          = "Internal reasoning trace"
  )

  captured_body <- NULL

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) {
      structure(list(), class = "fake_resp")
    },
    req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "qwen3:32b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        think             = TRUE,
        num_ctx           = 4096L,
        include_raw       = FALSE
      )

      testthat::expect_equal(res$thoughts, "Internal reasoning trace")
      testthat::expect_equal(captured_body$options$temperature, 0.6)
    }
  )
})

testthat::test_that("ollama_compare_pair_live uses error field when status != 200", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_body <- list(
    model             = "mistral-small3.2:24b",
    response          = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    prompt_eval_count = 1L,
    eval_count        = 1L,
    error             = "OOM in backend"
  )

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) structure(list(), class = "fake_resp"),
    req_body_json = function(req, body) req,
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 500L,
    {
      res <- ollama_compare_pair_live(
        ID1               = "S01",
        text1             = "A",
        ID2               = "S02",
        text2             = "B",
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434"
      )

      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(res$error_message, "OOM in backend")
    }
  )
})

testthat::test_that("ollama_compare_pair_live falls back to generic error message", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_body <- list(
    model             = "mistral-small3.2:24b",
    response          = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    prompt_eval_count = 1L,
    eval_count        = 1L
    # no error or message field
  )

  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) structure(list(), class = "fake_resp"),
    req_body_json = function(req, body) req,
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 503L,
    {
      res <- ollama_compare_pair_live(
        ID1               = "S01",
        text1             = "A",
        ID2               = "S02",
        text2             = "B",
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434"
      )

      testthat::expect_equal(res$status_code, 503L)
      testthat::expect_match(
        res$error_message,
        "Ollama request failed with status 503",
        fixed = FALSE
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live validates required columns", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bad_pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "A"
    # missing ID2, text2
  )

  expect_error(
    submit_ollama_pairs_live(
      pairs             = bad_pairs,
      model             = "mistral-small3.2:24b",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      verbose           = FALSE,
      progress          = FALSE
    ),
    "`pairs` must contain columns:",
    fixed = TRUE
  )
})

testthat::test_that("submit_ollama_pairs_live validates status_every", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "A",
    ID2   = "S02",
    text2 = "B"
  )

  expect_error(
    submit_ollama_pairs_live(
      pairs             = pairs,
      model             = "mistral-small3.2:24b",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      status_every      = 0,
      verbose           = FALSE,
      progress          = FALSE
    ),
    "`status_every` must be a single positive integer.",
    fixed = TRUE
  )
})

testthat::test_that("submit_ollama_pairs_live emits warning for error rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "A",
    ID2   = "S02",
    text2 = "B"
  )

  fake_row <- tibble::tibble(
    custom_id         = "LIVE_S01_vs_S02",
    ID1               = "S01",
    ID2               = "S02",
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 500L,
    error_message     = "backend failure",
    thoughts          = NA_character_,
    content           = NA_character_,
    better_sample     = NA_character_,
    better_id         = NA_character_,
    prompt_tokens     = NA_real_,
    completion_tokens = NA_real_,
    total_tokens      = NA_real_
  )

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) fake_row,
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = TRUE,
        status_every      = 1,
        progress          = TRUE, # exercises txtProgressBar path
        include_raw       = FALSE,
        think             = FALSE,
        num_ctx           = 8192L
      )

      # Access $results
      testthat::expect_equal(res$failed_pairs$error_message, "backend failure")
      testthat::expect_equal(res$failed_pairs$status_code, 500L)
    }
  )
})

testthat::test_that("submit_ollama_pairs_live computes timing when show_status is TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "A",
    ID2   = "S02",
    text2 = "B"
  )

  ok_row <- tibble::tibble(
    custom_id         = "LIVE_S01_vs_S02",
    ID1               = "S01",
    ID2               = "S02",
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = "S01",
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) ok_row,
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = TRUE,
        status_every      = 1,
        progress          = FALSE,
        include_raw       = FALSE
      )

      # Access $results
      testthat::expect_equal(res$results$better_id, "S01")
      testthat::expect_true(all(is.na(res$results$error_message)))
    }
  )
})

# ---------------------------------------------------------------------
# ensure_only_ollama_model_loaded: system / parsing behaviour
# ---------------------------------------------------------------------

testthat::test_that(
  "ensure_only_ollama_model_loaded validates model argument",
  {
    testthat::expect_error(
      ensure_only_ollama_model_loaded(character()),
      "`model` must be a non-empty character scalar.",
      fixed = TRUE
    )

    testthat::expect_error(
      ensure_only_ollama_model_loaded(c("a", "b")),
      "`model` must be a non-empty character scalar.",
      fixed = TRUE
    )

    testthat::expect_error(
      ensure_only_ollama_model_loaded(""),
      "`model` must be a non-empty character scalar.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "ensure_only_ollama_model_loaded handles ollama ps failure",
  {
    testthat::with_mocked_bindings(
      .ollama_system2 = function(command, args, stdout = TRUE, stderr = TRUE, ...) {
        stop("simulated failure from ollama ps")
      },
      {
        res <- ensure_only_ollama_model_loaded("mistral-small3.2:24b", verbose = TRUE)

        # Should return an empty character vector, invisibly
        testthat::expect_type(res, "character")
        testthat::expect_length(res, 0L)
      }
    )
  }
)

testthat::test_that(
  "ensure_only_ollama_model_loaded handles empty and header-only output",
  {
    # Case 1: completely empty output but status == 0
    testthat::with_mocked_bindings(
      .ollama_system2 = function(command, args, stdout = TRUE, stderr = TRUE, ...) {
        structure(character(0), status = 0L)
      },
      {
        res_empty <- ensure_only_ollama_model_loaded("qwen3:32b", verbose = FALSE)
        testthat::expect_type(res_empty, "character")
        testthat::expect_length(res_empty, 0L)
      }
    )

    # Case 2: header line only
    header_only <- structure(
      "NAME            ID              STATUS",
      status = 0L
    )

    testthat::with_mocked_bindings(
      .ollama_system2 = function(command, args, stdout = TRUE, stderr = TRUE, ...) {
        header_only
      },
      {
        res_header <- ensure_only_ollama_model_loaded("qwen3:32b", verbose = FALSE)
        testthat::expect_type(res_header, "character")
        testthat::expect_length(res_header, 0L)
      }
    )
  }
)

testthat::test_that(
  "ensure_only_ollama_model_loaded parses models and unloads others",
  {
    calls <- list()

    fake_ps_output <- structure(
      c(
        "NAME            ID              STATUS",
        "mistral-small3.2:24b   abc123   running",
        "qwen3:32b              def456   running",
        "gemma3:27b             ghi789   running"
      ),
      status = 0L
    )

    fake_ollama_system2 <- function(command, args, stdout = TRUE, stderr = TRUE, ...) {
      if (identical(args, "ps")) {
        # First call: simulate `ollama ps`
        fake_ps_output
      } else {
        # Subsequent calls should be of the form c("stop", <model>)
        calls <<- append(calls, list(list(command = command, args = args)))
        invisible(NULL)
      }
    }

    testthat::with_mocked_bindings(
      .ollama_system2 = fake_ollama_system2,
      {
        keep_model <- "qwen3:32b"

        res <- ensure_only_ollama_model_loaded(keep_model, verbose = FALSE)

        # Should request unloading of all models except the one we keep
        testthat::expect_setequal(
          res,
          c("mistral-small3.2:24b", "gemma3:27b")
        )

        # We should have issued stop commands for the same set
        stopped_models <- vapply(
          calls,
          function(x) x$args[2],
          character(1)
        )

        testthat::expect_setequal(
          stopped_models,
          c("mistral-small3.2:24b", "gemma3:27b")
        )

        # All stop commands should target the `ollama` binary
        testthat::expect_true(all(vapply(calls, function(x) x$command, "") == "ollama"))
      }
    )
  }
)

testthat::test_that("ensure_only_ollama_model_loaded continues if stop fails", {
  # Mock ps output
  fake_ps <- structure(c(
    "NAME    ID",
    "m1      1",
    "m2      2",
    "keep    3"
  ), status = 0L)

  calls <- list()

  fake_sys <- function(command, args, ...) {
    if (identical(args, "ps")) {
      return(fake_ps)
    }
    # Record stop calls
    calls <<- append(calls, list(args))
    # Fail on m1, succeed on m2
    if ("m1" %in% args) stop("Cannot stop m1")
    invisible(NULL)
  }

  testthat::with_mocked_bindings(
    .ollama_system2 = fake_sys,
    {
      # Should not throw
      res <- ensure_only_ollama_model_loaded("keep", verbose = FALSE)

      # Should still return the list of models it *attempted* to unload
      # (even though m1 failed, it was identified as a target)
      testthat::expect_setequal(res, c("m1", "m2"))

      # Verify both were attempted
      stopped <- vapply(calls, function(x) x[2], character(1))
      testthat::expect_setequal(stopped, c("m1", "m2"))
    }
  )
})

testthat::test_that("ensure_only_ollama_model_loaded handles empty or weird CLI output", {
  # Scenario 1: ollama ps returns only header (variant)
  testthat::with_mocked_bindings(
    .ollama_system2 = function(...) {
      structure("NAME       ID       SIZE   PROCESSOR  UNTIL", status = 0L)
    },
    {
      # Should return empty character invisibly (no models found to unload)
      res <- ensure_only_ollama_model_loaded("mistral", verbose = FALSE)
      testthat::expect_length(res, 0L)
    }
  )

  # Scenario 2: ollama ps returns header and the target model only
  testthat::with_mocked_bindings(
    .ollama_system2 = function(...) {
      structure(c(
        "NAME       ID       SIZE   PROCESSOR  UNTIL",
        "mistral    abc      4GB    gpu        4m"
      ), status = 0L)
    },
    {
      # Should return empty character (target matches, nothing to unload)
      res <- ensure_only_ollama_model_loaded("mistral", verbose = FALSE)
      testthat::expect_length(res, 0L)
    }
  )
})

testthat::test_that("ensure_only_ollama_model_loaded emits verbose status messages", {
  msgs_empty <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      .ollama_system2 = function(...) structure(character(0), status = 0L),
      .package = "pairwiseLLM",
      {
        ensure_only_ollama_model_loaded("mistral", verbose = TRUE)
      }
    )
  )
  testthat::expect_true(any(grepl("failed or returned no output", msgs_empty)))

  msgs_header <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      .ollama_system2 = function(...) structure("NAME ID", status = 0L),
      .package = "pairwiseLLM",
      {
        ensure_only_ollama_model_loaded("mistral", verbose = TRUE)
      }
    )
  )
  testthat::expect_true(any(grepl("No active Ollama models", msgs_header)))

  msgs_only_target <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      .ollama_system2 = function(...) {
        structure(c("NAME ID", "mistral abc"), status = 0L)
      },
      .package = "pairwiseLLM",
      {
        ensure_only_ollama_model_loaded("mistral", verbose = TRUE)
      }
    )
  )
  testthat::expect_true(any(grepl("Active Ollama models", msgs_only_target)))
  testthat::expect_true(any(grepl("No models to unload", msgs_only_target)))
})

testthat::test_that("ensure_only_ollama_model_loaded reports unload messages when verbose", {
  calls <- list()
  fake_ps <- structure(c("NAME ID", "keep 1", "drop 2"), status = 0L)

  msgs <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      .ollama_system2 = function(command, args, ...) {
        if (identical(args, "ps")) return(fake_ps)
        calls <<- append(calls, list(args))
        invisible(NULL)
      },
      .package = "pairwiseLLM",
      {
        res <- ensure_only_ollama_model_loaded("keep", verbose = TRUE)
        testthat::expect_equal(res, "drop")
      }
    )
  )

  testthat::expect_true(any(grepl("Unloading Ollama model: drop", msgs)))
  testthat::expect_true(length(calls) == 1L)
})

testthat::test_that(".ollama_system2 delegates to system2", {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!file.exists(rscript)) {
    testthat::skip("Rscript is not available on this system.")
  }

  out <- pairwiseLLM:::.ollama_system2(
    rscript,
    args = c("-e", "cat('ok')"),
    stdout = TRUE,
    stderr = TRUE
  )

  testthat::expect_true(any(grepl("ok", out)))
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: Zero rows (Updated return structure)
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live returns list with empty tibbles
                    for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res <- submit_ollama_pairs_live(
    pairs             = empty_pairs,
    model             = "mistral-small3.2:24b",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    verbose           = FALSE,
    progress          = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: Row-wise execution & List return
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live calls ollama_compare_pair_live
                    row-wise and returns list", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = "mistral-small3.2:24b",
      object_type       = "ollama.generate",
      status_code       = 200L,
      error_message     = NA_character_,
      thoughts          = NA_character_,
      content           = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample     = chosen,
      better_id         = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens     = 10,
      completion_tokens = 5,
      total_tokens      = 15
    )
  }

  calls <- list()

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(ID1, ID2, ...) {
      calls <<- append(calls, list(list(ID1 = ID1, ID2 = ID2)))
      if (ID1 == "S01") {
        fake_result(ID1, ID2, "SAMPLE_1")
      } else {
        fake_result(ID1, ID2, "SAMPLE_2")
      }
    },
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = FALSE,
        progress          = FALSE
      )

      testthat::expect_equal(length(calls), 2L)

      # Check list structure
      testthat::expect_type(res, "list")
      testthat::expect_s3_class(res$results, "tbl_df")
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_equal(res$results$better_id, c("S01", "S04"))
      testthat::expect_equal(nrow(res$failed_pairs), 0L)
    }
  )
})

testthat::test_that("submit_ollama_pairs_live captures errors in failed_pairs", {
  pairs <- tibble::tibble(ID1 = "a", text1 = "a", ID2 = "b", text2 = "b")

  testthat::with_mocked_bindings(
    # Mock inner function to throw an error
    ollama_compare_pair_live = function(...) stop("Inner crash detected"),
    {
      res <- submit_ollama_pairs_live(
        pairs,
        model = "m", trait_name = "t", trait_description = "d", verbose = FALSE
      )

      # The errored row appears in results (with error info) AND in failed_pairs
      testthat::expect_equal(nrow(res$results), 0L)

      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "a")
      testthat::expect_match(res$failed_pairs$error_message, "Inner crash detected")
    }
  )
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: Saving and Parallel Support
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live supports incremental saving (sequential)", {
  testthat::skip_if_not_installed("readr")
  pairs <- tibble::tibble(
    ID1 = c("A", "C"), text1 = c("a", "c"),
    ID2 = c("B", "D"), text2 = c("b", "d")
  )

  write_calls <- list()
  fake_row <- tibble::tibble(custom_id = "cid", error_message = NA_character_)

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) fake_row,
    .package = "pairwiseLLM",
    {
      testthat::with_mocked_bindings(
        write_csv = function(x, file, append = FALSE, col_names = !append, ...) {
          write_calls <<- append(write_calls, list(list(file = file, append = append)))
        },
        .package = "readr",
        {
          submit_ollama_pairs_live(
            pairs,
            model = "m", trait_name = "t", trait_description = "d",
            verbose = FALSE, save_path = "out.csv"
          )
        }
      )
    }
  )

  # Should call write_csv twice (once per pair)
  testthat::expect_equal(length(write_calls), 2L)
  testthat::expect_equal(write_calls[[1]]$file, "out.csv")
})

testthat::test_that("submit_ollama_pairs_live resumes from existing file", {
  testthat::skip_if_not_installed("readr")
  pairs <- tibble::tibble(
    ID1 = c("A", "C"), text1 = c("a", "c"),
    ID2 = c("B", "D"), text2 = c("b", "d")
  )

  # Mock existing results: A vs B already done
  existing <- tibble::tibble(
    custom_id = "LIVE_A_vs_B",
    ID1 = "A", ID2 = "B", model = "m",
    error_message = NA_character_, status_code = 200L
  )

  processed <- list()

  # Mock file existence
  testthat::with_mocked_bindings(
    file.exists = function(x) x == "saved.csv",
    .package = "base",
    {
      testthat::with_mocked_bindings(
        read_csv = function(...) existing,
        # Mock write_csv to suppress vroom dependency warnings during test
        write_csv = function(...) NULL,
        .package = "readr",
        {
          testthat::with_mocked_bindings(
            ollama_compare_pair_live = function(ID1, ID2, ...) {
              processed <<- append(processed, paste(ID1, ID2, sep = "_"))
              tibble::tibble(
                custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
                ID1 = ID1, ID2 = ID2, model = "m", error_message = NA_character_, status_code = 200L
              )
            },
            .package = "pairwiseLLM",
            {
              res <- submit_ollama_pairs_live(
                pairs,
                model = "m", trait_name = "t", trait_description = "d",
                save_path = "saved.csv", verbose = FALSE
              )

              # Should only process C vs D (A vs B skipped)
              testthat::expect_equal(processed, list("C_D"))

              # Result should contain both (existing + new)
              testthat::expect_equal(nrow(res$results), 2L)
              testthat::expect_setequal(res$results$custom_id, c("LIVE_A_vs_B", "LIVE_C_vs_D"))
            }
          )
        }
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live emits resume messages when verbose", {
  testthat::skip_if_not_installed("readr")
  pairs <- tibble::tibble(
    ID1 = c("A", "C"), text1 = c("a", "c"),
    ID2 = c("B", "D"), text2 = c("b", "d")
  )

  existing <- tibble::tibble(
    custom_id = "LIVE_A_vs_B",
    ID1 = "A", ID2 = "B", model = "m",
    error_message = NA_character_, status_code = 200L
  )

  msgs <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      file.exists = function(x) x == "saved.csv",
      .package = "base",
      {
        testthat::with_mocked_bindings(
          read_csv = function(...) existing,
          write_csv = function(...) NULL,
          .package = "readr",
          {
            testthat::with_mocked_bindings(
              ollama_compare_pair_live = function(ID1, ID2, ...) {
                tibble::tibble(
                  custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
                  ID1 = ID1,
                  ID2 = ID2,
                  model = "m",
                  status_code = 200L,
                  error_message = NA_character_,
                  better_sample = "SAMPLE_1",
                  better_id = ID1,
                  prompt_tokens = 1,
                  completion_tokens = 1,
                  total_tokens = 2
                )
              },
              .package = "pairwiseLLM",
              {
                submit_ollama_pairs_live(
                  pairs,
                  model = "m", trait_name = "t", trait_description = "d",
                  save_path = "saved.csv", verbose = TRUE, progress = FALSE
                )
              }
            )
          }
        )
      }
    )
  )

  testthat::expect_true(any(grepl("Found existing file", msgs)))
  testthat::expect_true(any(grepl("Skipping 1 pairs", msgs)))
})

testthat::test_that("submit_ollama_pairs_live supports parallel processing", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")

  pairs <- tibble::tibble(
    ID1 = c("A", "C"), text1 = c("a", "c"),
    ID2 = c("B", "D"), text2 = c("b", "d")
  )

  plan_called <- FALSE
  lapply_called <- FALSE

  fake_chunk <- list(
    tibble::tibble(custom_id = "1", error_message = NA),
    tibble::tibble(custom_id = "2", error_message = NA)
  )

  testthat::with_mocked_bindings(
    # Mock future::plan
    plan = function(...) {
      plan_called <<- TRUE
      "old"
    },
    .package = "future",
    {
      testthat::with_mocked_bindings(
        # Mock future.apply::future_lapply
        future_lapply = function(...) {
          lapply_called <<- TRUE
          fake_chunk
        },
        .package = "future.apply",
        {
          res <- submit_ollama_pairs_live(
            pairs,
            model = "m", trait_name = "t", trait_description = "d",
            verbose = FALSE, parallel = TRUE, workers = 2
          )

          testthat::expect_type(res, "list")
        }
      )
    }
  )

testthat::expect_true(plan_called)
testthat::expect_true(lapply_called)
})

testthat::test_that("submit_ollama_pairs_live parallel path logs messages and strips raw_response", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")

  pairs <- tibble::tibble(
    ID1 = c("A", "C"), text1 = c("a", "c"),
    ID2 = c("B", "D"), text2 = c("b", "d")
  )

  save_path <- withr::local_tempfile(fileext = ".csv")
  write_calls <- list()

  msgs <- testthat::capture_messages(
    testthat::with_mocked_bindings(
      plan = function(...) "old",
      .package = "future",
      {
        testthat::with_mocked_bindings(
          future_lapply = function(X, FUN, ...) lapply(X, FUN),
          .package = "future.apply",
          {
            testthat::with_mocked_bindings(
              ollama_compare_pair_live = function(...) {
                tibble::tibble(custom_id = "id", error_message = NA_character_, raw_response = list(list(ok = TRUE)))
              },
              .package = "pairwiseLLM",
              {
                testthat::with_mocked_bindings(
                  write_csv = function(x, ...) {
                    write_calls <<- append(write_calls, list(names(x)))
                  },
                  .package = "readr",
                  {
                    submit_ollama_pairs_live(
                      pairs,
                      model = "m", trait_name = "t", trait_description = "d",
                      verbose = TRUE, progress = FALSE,
                      include_raw = TRUE, save_path = save_path,
                      parallel = TRUE, workers = 2
                    )
                  }
                )
              }
            )
          }
        )
      }
    )
  )

  testthat::expect_true(any(grepl("Setting up parallel plan", msgs)))
  testthat::expect_true(any(grepl("Processing .* PARALLEL", msgs)))
  testthat::expect_true(length(write_calls) > 0L)
  testthat::expect_true(all(vapply(write_calls, function(nms) !"raw_response" %in% nms, logical(1))))
})

# ---------------------------------------------------------------------
# Update old tests to access res$results
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live emits warning for error rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1   = "S01", text1 = "A",
    ID2   = "S02", text2 = "B"
  )

  fake_row <- tibble::tibble(
    custom_id         = "LIVE_S01_vs_S02",
    ID1               = "S01",
    ID2               = "S02",
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 500L,
    error_message     = "backend failure",
    thoughts          = NA_character_,
    content           = NA_character_,
    better_sample     = NA_character_,
    better_id         = NA_character_,
    prompt_tokens     = NA_real_,
    completion_tokens = NA_real_,
    total_tokens      = NA_real_
  )

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) fake_row,
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = TRUE,
        status_every      = 1,
        progress          = TRUE,
        include_raw       = FALSE,
        think             = FALSE,
        num_ctx           = 8192L
      )

      testthat::expect_equal(res$failed_pairs$error_message, "backend failure")
      testthat::expect_equal(res$failed_pairs$status_code, 500L)
    }
  )
})

testthat::test_that("submit_ollama_pairs_live computes timing when show_status is TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1   = "S01", text1 = "A",
    ID2   = "S02", text2 = "B"
  )

  ok_row <- tibble::tibble(
    custom_id         = "LIVE_S01_vs_S02",
    ID1               = "S01",
    ID2               = "S02",
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = "S01",
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) ok_row,
    .package = "pairwiseLLM",
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        verbose           = TRUE,
        status_every      = 1,
        progress          = FALSE,
        include_raw       = FALSE
      )

      testthat::expect_equal(res$results$better_id, "S01")
      testthat::expect_true(all(is.na(res$results$error_message)))
    }
  )
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: Additional Coverage (Save, Resume, Parallel Errors)
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live errors if readr missing when save_path used", {
  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base",
    {
      testthat::expect_error(
        submit_ollama_pairs_live(
          tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b"),
          "model", "t", "d",
          save_path = "out.csv"
        ),
        "package is required for incremental saving"
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live creates directory for save_path", {
  testthat::skip_if_not_installed("readr")
  temp_dir <- file.path(tempdir(), "ollama_subdir")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  save_path <- file.path(temp_dir, "out.csv")

  fake_row <- tibble::tibble(
    custom_id = "id",
    ID1 = "A",
    ID2 = "B",
    status_code = 200L,
    error_message = NA_character_,
    better_sample = "SAMPLE_1",
    better_id = "A",
    prompt_tokens = 1,
    completion_tokens = 1,
    total_tokens = 2
  )

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) fake_row,
    .package = "pairwiseLLM",
    {
      msgs <- testthat::capture_messages(
        submit_ollama_pairs_live(
          tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b"),
          "m", "t", "d",
          save_path = save_path, verbose = TRUE, progress = FALSE
        )
      )
      testthat::expect_true(any(grepl("Creating output directory", msgs)))
    }
  )

  testthat::expect_true(dir.exists(temp_dir))
  testthat::expect_true(file.exists(save_path))
})

testthat::test_that("submit_ollama_pairs_live warns if resume file unreadable", {
  testthat::skip_if_not_installed("readr")

  testthat::with_mocked_bindings(
    file.exists = function(...) TRUE,
    .package = "base",
    {
      testthat::with_mocked_bindings(
        read_csv = function(...) stop("Corrupt file"),
        .package = "readr",
        {
          testthat::with_mocked_bindings(
            ollama_compare_pair_live = function(...) tibble::tibble(custom_id = "1", error_message = NA),
            .package = "pairwiseLLM",
            {
              testthat::expect_warning(
                submit_ollama_pairs_live(
                  tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b"),
                  "m", "t", "d",
                  save_path = "bad.csv", verbose = FALSE
                ),
                "Could not read existing save file"
              )
            }
          )
        }
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live errors if parallel packages missing", {
  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package %in% c("future", "future.apply")) FALSE else TRUE
    },
    .package = "base",
    {
      testthat::expect_error(
        submit_ollama_pairs_live(
          tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b"),
          "m", "t", "d",
          parallel = TRUE, workers = 2
        ),
        "Packages 'future' and 'future.apply' are required"
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live parallel handles worker errors", {
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("future")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  # Mock future.apply::future_lapply to run sequentially via lapply
  # We do NOT pass ... to lapply because work_fn takes no extra args,
  # but future_lapply receives future.seed in ..., which causes the error if forwarded.
  testthat::with_mocked_bindings(
    plan = function(...) "old",
    .package = "future",
    {
      testthat::with_mocked_bindings(
        future_lapply = function(X, FUN, ...) lapply(X, FUN),
        .package = "future.apply",
        {
          testthat::with_mocked_bindings(
            ollama_compare_pair_live = function(...) stop("Worker Failed"),
            .package = "pairwiseLLM",
            {
              res <- submit_ollama_pairs_live(
                pairs, "m", "t", "d",
                parallel = TRUE, workers = 2, verbose = FALSE
              )

              testthat::expect_equal(nrow(res$results), 0L)
              testthat::expect_equal(nrow(res$failed_pairs), 1L)
              testthat::expect_match(res$failed_pairs$error_message, "Worker Failed")
            }
          )
        }
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live parallel warns on save failure", {
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("future")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  fake_row <- tibble::tibble(custom_id = "id", error_message = NA_character_)

  testthat::with_mocked_bindings(
    plan = function(...) "old",
    .package = "future",
    {
      testthat::with_mocked_bindings(
        future_lapply = function(X, FUN, ...) lapply(X, FUN),
        .package = "future.apply",
        {
          testthat::with_mocked_bindings(
            ollama_compare_pair_live = function(...) fake_row,
            .package = "pairwiseLLM",
            {
              testthat::with_mocked_bindings(
                write_csv = function(...) stop("Disk Full"),
                .package = "readr",
                {
                  testthat::expect_warning(
                    submit_ollama_pairs_live(
                      pairs, "m", "t", "d",
                      parallel = TRUE, workers = 2,
                      save_path = "out.csv", verbose = FALSE
                    ),
                    "Failed to save incremental results"
                  )
                }
              )
            }
          )
        }
      )
    }
  )
})

testthat::test_that("submit_ollama_pairs_live sequential warns on save failure", {
  testthat::skip_if_not_installed("readr")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  fake_row <- tibble::tibble(custom_id = "id", error_message = NA_character_)

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(...) fake_row,
    .package = "pairwiseLLM",
    {
      testthat::with_mocked_bindings(
        write_csv = function(...) stop("Permission Denied"),
        .package = "readr",
        {
          testthat::expect_warning(
            submit_ollama_pairs_live(
              pairs, "m", "t", "d",
              save_path = "out.csv", verbose = FALSE
            ),
            "Failed to save incremental result"
          )
        }
      )
    }
  )
})
