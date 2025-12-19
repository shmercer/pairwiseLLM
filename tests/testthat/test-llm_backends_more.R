# Tests for backend-agnostic live comparison helpers and retry logic

test_that("llm_compare_pair dispatches to the correct backend helper", {
  # Create dummy values for a single pair
  ID1 <- "id1"
  text1 <- "text one"
  ID2 <- "id2"
  text2 <- "text two"
  model <- "fake-model"
  trait_name <- "Trait"
  trait_description <- "A trait description"
  tmpl <- set_prompt_template()
  # Prepare return objects for each backend
  openai_ret <- tibble::tibble(backend = "openai")
  anthropic_ret <- tibble::tibble(backend = "anthropic")
  gemini_ret <- tibble::tibble(backend = "gemini")
  together_ret <- tibble::tibble(backend = "together")
  ollama_ret <- tibble::tibble(backend = "ollama")
  # Helper to record the last call arguments
  last_args <- list()
  # Use with_mocked_bindings to stub each backend function
  res <- with_mocked_bindings(
    openai_compare_pair_live = function(...) {
      last_args <<- list(fn = "openai", args = list(...))
      openai_ret
    },
    anthropic_compare_pair_live = function(...) {
      last_args <<- list(fn = "anthropic", args = list(...))
      anthropic_ret
    },
    gemini_compare_pair_live = function(...) {
      last_args <<- list(fn = "gemini", args = list(...))
      gemini_ret
    },
    together_compare_pair_live = function(...) {
      last_args <<- list(fn = "together", args = list(...))
      together_ret
    },
    ollama_compare_pair_live = function(...) {
      last_args <<- list(fn = "ollama", args = list(...))
      ollama_ret
    },
    {
      # Test each backend dispatch
      # OpenAI with explicit endpoint
      res_openai <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "openai", endpoint = "chat.completions",
                                     temperature = 0.7)
      expect_equal(res_openai$backend, "openai")
      # Ensure the correct backend helper was called and temperature forwarded
      expect_identical(last_args$fn, "openai")
      expect_true("temperature" %in% names(last_args$args))
      # Anthropic
      res_anthropic <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                        trait_name, trait_description,
                                        prompt_template = tmpl,
                                        backend = "anthropic", reasoning = "low")
      expect_equal(res_anthropic$backend, "anthropic")
      expect_identical(last_args$fn, "anthropic")
      expect_true("reasoning" %in% names(last_args$args))
      # Gemini
      res_gemini <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "gemini", include_thoughts = TRUE)
      expect_equal(res_gemini$backend, "gemini")
      expect_identical(last_args$fn, "gemini")
      expect_true("include_thoughts" %in% names(last_args$args))
      # Together
      res_together <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                       trait_name, trait_description,
                                       prompt_template = tmpl,
                                       backend = "together", api_key = "key", include_raw = TRUE)
      expect_equal(res_together$backend, "together")
      expect_identical(last_args$fn, "together")
      expect_true("api_key" %in% names(last_args$args))
      expect_true(last_args$args$include_raw)
      # Ollama
      res_ollama <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "ollama", host = "http://localhost:11434")
      expect_equal(res_ollama$backend, "ollama")
      expect_identical(last_args$fn, "ollama")
      expect_true("host" %in% names(last_args$args))
      # Unsupported backend should error
      expect_error(
        llm_compare_pair(ID1, text1, ID2, text2, model,
                         trait_name, trait_description,
                         prompt_template = tmpl,
                         backend = "invalid"),
        "should be one of \"openai\", \"anthropic\", \"gemini\", \"together\", \"ollama\""
      )
    }
  )
})

test_that("submit_llm_pairs dispatches to correct backend helper", {
  # Create a simple pairs tibble
  pairs_tbl <- tibble::tibble(ID1 = c("a","b"), text1 = c("a1","b1"),
                              ID2 = c("c","d"), text2 = c("c1","d1"))
  trait_name <- "Quality"
  trait_description <- "Description"
  model <- "modelX"
  tmpl <- set_prompt_template()
  last_call <- list()
  # Stubs for backend-specific submit helpers
  with_mocked_bindings(
    submit_openai_pairs_live = function(...) {
      last_call <<- list(fn = "openai", args = list(...))
      tibble::tibble(provider = "openai")
    },
    submit_anthropic_pairs_live = function(...) {
      last_call <<- list(fn = "anthropic", args = list(...))
      tibble::tibble(provider = "anthropic")
    },
    submit_gemini_pairs_live = function(...) {
      last_call <<- list(fn = "gemini", args = list(...))
      tibble::tibble(provider = "gemini")
    },
    submit_together_pairs_live = function(...) {
      last_call <<- list(fn = "together", args = list(...))
      tibble::tibble(provider = "together")
    },
    submit_ollama_pairs_live = function(...) {
      last_call <<- list(fn = "ollama", args = list(...))
      tibble::tibble(provider = "ollama")
    },
    {
      # Test each backend
      res_openai <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "openai",
                                     endpoint = "responses", verbose = FALSE, status_every = 1,
                                     include_raw = TRUE)
      expect_equal(res_openai$provider, "openai")
      expect_equal(last_call$fn, "openai")
      expect_true(last_call$args$include_raw)
      # Anthropic
      res_anthropic <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                        prompt_template = tmpl, backend = "anthropic",
                                        verbose = FALSE)
      expect_equal(res_anthropic$provider, "anthropic")
      expect_equal(last_call$fn, "anthropic")
      # Gemini
      res_gemini <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "gemini",
                                     verbose = FALSE)
      expect_equal(res_gemini$provider, "gemini")
      expect_equal(last_call$fn, "gemini")
      # Together
      res_together <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                       prompt_template = tmpl, backend = "together",
                                       verbose = FALSE, status_every = 1)
      expect_equal(res_together$provider, "together")
      expect_equal(last_call$fn, "together")
      # Ollama
      res_ollama <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "ollama",
                                     verbose = FALSE, status_every = 1)
      expect_equal(res_ollama$provider, "ollama")
      expect_equal(last_call$fn, "ollama")
      # Unsupported backend
      expect_error(
        submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                         prompt_template = tmpl, backend = "fake"),
        "should be one of \"openai\", \"anthropic\", \"gemini\", \"together\", \"ollama\""
      )
    }
  )
})

test_that(".retry_httr2_request retries on transient statuses and returns success", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L

  # FIX: Create a request that does NOT throw on error (is_error returns FALSE),
  # ensuring req_perform returns the response object (500) instead of throwing.
  # This matches the behavior tested by the original mock.
  dummy_req <- httr2::request("http://example.com") |>
    httr2::req_error(is_error = function(resp) FALSE)

  mk_resp <- function(status) {
    httr2::response(status_code = status)
  }

  # Mock: 1st attempt -> 500, 2nd attempt -> 200
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    if (attempt_env$count == 1L) mk_resp(500L) else mk_resp(200L)
  }

  httr2::local_mocked_responses(mock_callback)

  out <- pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 3L, base_delay = 0)

  expect_equal(attempt_env$count, 2L)
  expect_s3_class(out, "httr2_response")
  expect_equal(out$status_code, 200L)
})

test_that(".retry_httr2_request handles httr2_http errors and rethrows when non-transient", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L
  # Standard request that throws on error (default)
  dummy_req <- httr2::request("http://example.com")

  mk_error <- function(status) {
    resp <- httr2::response(status_code = status)
    structure(list(message = paste0("HTTP ", status), response = resp),
              class = c("httr2_http", "error", "condition"))
  }

  # Mock: Throw 400 immediately
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    stop(mk_error(400L))
  }

  httr2::local_mocked_responses(mock_callback)

  expect_error(
    pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 2L, base_delay = 0),
    class = "httr2_http"
  )
  expect_equal(attempt_env$count, 1L)
})

test_that(".retry_httr2_request retries on httr2_http transient errors and eventually succeeds", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L
  dummy_req <- httr2::request("http://example.com")

  mk_error <- function(status) {
    resp <- httr2::response(status_code = status)
    structure(list(message = paste0("HTTP ", status), response = resp),
              class = c("httr2_http", "error", "condition"))
  }

  # Mock: Throw 503 (transient) twice, then return 200
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    if (attempt_env$count <= 2L) {
      stop(mk_error(503L))
    } else {
      httr2::response(status_code = 200L)
    }
  }

  httr2::local_mocked_responses(mock_callback)

  out <- pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 3L, base_delay = 0)

  expect_equal(attempt_env$count, 3L)
  expect_s3_class(out, "httr2_response")
  expect_equal(out$status_code, 200L)
})
