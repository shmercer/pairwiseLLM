test_that("both public entry points send the frozen scientific request as raw JSON", {
  captured <- list()
  captured_auth <- character()
  response <- decision_333_response()
  response$usage$input_tokens_details <- list(cached_tokens = 2, cache_write_tokens = 3)
  response$usage$output_tokens_details <- list(reasoning_tokens = 0)
  httr2::local_mocked_responses(function(req) {
    # Resolve the synthetic header during the request's lifetime: httr2 1.3
    # keeps sensitive values in weak references that expire after the call.
    headers <- if (inherits(req$headers, "httr2_headers")) {
      getFromNamespace("headers_flatten", "httr2")(req$headers, redact = FALSE)
    } else req$headers
    captured_auth <<- c(captured_auth, headers$Authorization)
    captured[[length(captured) + 1L]] <<- req
    decision_334_response(response)
  })
  local_mocked_bindings(set_prompt_template = function(...) stop("generative default was evaluated"),
                         .package = "pairwiseLLM")
  rows <- lapply(list(openai_compare_pair_live, llm_compare_pair), function(fn) {
    do.call(fn, decision_334_args())
  })
  expect_identical(rows[[1]], rows[[2]])
  expect_length(captured, 2L)
  expect_identical(captured_auth, rep("Bearer synthetic-fixture-key", 2L))
  for (req in captured) {
    expect_identical(req$url, "https://api.openai.com/v1/decisions")
    expect_identical(req$body$type, "raw")
    expect_identical(req$body$content_type, "application/json")
    wire <- jsonlite::parse_json(rawToChar(req$body$data))
    expect_identical(wire, decision_333_fixture("openai-request.json"))
    expect_identical(req$body$data, charToRaw(pairwiseLLM:::.decision_json(wire)))
  }
  row <- rows[[1]]
  expect_identical(c(row$p_sample_1, row$p_sample_2, row$p_chosen, row$decision_confidence), c(.3, .7, .7, .2))
  expect_identical(row$better_sample, "SAMPLE_2")
  expect_identical(row$better_id, "B:02")
  expect_identical(row$winner_pos, 2L)
  expect_identical(row$pair_uid, "live-pair-334")
  expect_identical(row$custom_id, row$pair_uid)
  expect_identical(row$provider_request_id, "provider-334")
  expect_identical(row$requested_model, "gpt-6-luna")
  expect_identical(row$resolved_model, response$model)
  expect_identical(c(row$prompt_tokens, row$completion_tokens, row$total_tokens,
                     row$prompt_cached_tokens, row$prompt_cache_write_tokens, row$reasoning_tokens),
                   c(12, 0, 12, 2, 3, 0))
  expect_false("raw_response" %in% names(row))
  controls <- jsonlite::parse_json(row$request_controls_json)
  expect_null(controls$prompt_cache_options)
  expect_identical(controls$effective_prompt_cache_options, list(mode = "implicit", ttl = "30m"))
})

test_that("explicit custom instructions, cache controls and exact text survive serialization", {
  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- jsonlite::parse_json(rawToChar(req$body$data))
    decision_334_response()
  })
  template <- "Judge {TRAIT_NAME}.\nComplete guidance:\n{TRAIT_DESCRIPTION}"
  args <- decision_334_args(text1 = "\u00e9\r\n\"SAMPLE_2\" literal", text2 = "",
                            decision_template = template, include_raw = TRUE)
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    for (options in list(NULL, list(mode = "implicit"), list(mode = "explicit"),
                         list(ttl = "30m"), list(mode = NULL, ttl = "30m"),
                         list(mode = "explicit", ttl = "30m"))) {
      row <- do.call(fn, c(args, list(prompt_cache_options = options, prompt_template = NULL)))
      expect_identical(captured$prompt_cache_options, options)
      evidence <- jsonlite::parse_json(captured$input)
      expect_identical(evidence, list(SAMPLE_1 = args$text1, SAMPLE_2 = args$text2))
      expect_identical(captured$questions[[1]]$instructions,
                       paste0("Judge Organization.\nComplete guidance:\n", args$trait_description))
      expect_match(row$decision_template_version, "^custom-sha256-")
      expect_identical(jsonlite::parse_json(row$request_controls_json)$prompt_cache_options, options)
      expect_true("raw_response" %in% names(row))
    }
  }
  without_uid <- decision_334_args()
  without_uid$pair_uid <- NULL
  expect_identical(do.call(openai_compare_pair_live, without_uid)$pair_uid,
                   "LIVE_A_with_vs_delimiter_vs_B:02")
})

test_that("httr2 sends the validated bytes after preparing the actual curl payload", {
  captured <- list()
  # Intercept the final transfer function, after httr2 has applied the method,
  # headers and body to curl options. No connection is opened by this mock.
  local_mocked_bindings(req_perform1 = function(req, req_prep, ...) {
    captured[[length(captured) + 1L]] <<- req_prep
    decision_334_response()
  }, .package = "httr2")
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    row <- do.call(fn, decision_334_args(prompt_cache_options = list(mode = "explicit")))
    expect_identical(row$better_sample, "SAMPLE_2")
  }
  expect_length(captured, 2L)
  expected <- decision_333_fixture("openai-request.json")
  expected$prompt_cache_options <- list(mode = "explicit")
  for (req in captured) {
    expect_identical(req$url, "https://api.openai.com/v1/decisions")
    expect_true(req$options$post)
    expect_identical(req$headers$`Content-Type`, "application/json")
    expect_identical(req$options$postfields, charToRaw(pairwiseLLM:::.decision_json(expected)))
    expect_identical(jsonlite::parse_json(rawToChar(req$options$postfields)), expected)
  }
})

test_that("unsupported settings and malformed controls fail before credentials or transport", {
  calls <- 0L
  forbidden <- function(...) { calls <<- calls + 1L; stop("unexpected IO") }
  local_mocked_bindings(.openai_api_key = forbidden, .openai_request = forbidden,
                         .pairwiseLLM_req_perform = forbidden, .package = "pairwiseLLM")
  controls <- list(temperature = 0, top_p = NULL, logprobs = FALSE, reasoning = "none",
                   max_output_tokens = 100, include_thoughts = FALSE, tag_prefix = "<BETTER_SAMPLE>",
                   tag_suffix = "</BETTER_SAMPLE>", store = FALSE, service_tier = NULL,
                   prompt_caching = "disabled", prompt_cache_key = "key", unknown_option = TRUE,
                   prompt_template = set_prompt_template())
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    for (name in names(controls)) {
      expect_error(do.call(fn, c(decision_334_args(), controls[name])), "supported|Generative")
    }
    for (options in list(list(), list(mode = "disabled"), list(mode = NA_character_),
                         list(mode = TRUE), list(ttl = "24h"), list(ttl = NULL),
                         list(extra = "value"), list(mode = "implicit", mode = "explicit"),
                         c(mode = "implicit"))) {
      expect_error(do.call(fn, c(decision_334_args(), list(prompt_cache_options = options))), "prompt_cache_options")
    }
    expect_error(do.call(fn, c(decision_334_args(), list(decision_template = "missing placeholders"))), "decision_template")
    expect_error(do.call(fn, decision_334_args(model = "unverified-model")), "gpt-6-luna")
    expect_error(do.call(fn, decision_334_args(include_raw = NA)), "include_raw")
    expect_error(do.call(fn, c(decision_334_args(), list(temperature = 0, temperature = 1))), "uniquely named")
    expect_error(do.call(fn, c(decision_334_args(), list(123))), "Generative")
  }
  expect_error(pairwiseLLM:::.openai_decision_controls(list(123)), "uniquely named")
  expect_identical(calls, 0L)
})

test_that("altered constructed requests are rejected by both entry points before IO", {
  calls <- 0L
  original <- decision_333_request(pair_uid = "live-pair-334")
  other_identities <- list(decision_333_request("typesafe"),
                            decision_333_request(model = "another-model"))
  altered <- original
  forbidden <- function(...) { calls <<- calls + 1L; stop("unexpected IO") }
  local_mocked_bindings(.build_decision_request = function(...) altered,
                         .openai_api_key = forbidden, .openai_request = forbidden,
                         .pairwiseLLM_req_perform = forbidden, .package = "pairwiseLLM")
  for (mutate in decision_334_mutations()) {
    altered <- mutate(original)
    for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
      expect_error(do.call(fn, decision_334_args()), class = "pairwiseLLM_decision_request_error")
    }
  }
  for (r in other_identities) {
    altered <- r
    for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
      expect_error(do.call(fn, decision_334_args()), class = "pairwiseLLM_decision_request_error")
    }
  }
  expect_identical(calls, 0L)
})

test_that("post-formatting payload changes fail before the HTTP performer", {
  attach <- httr2::req_body_raw
  calls <- 0L
  local_mocked_bindings(req_body_raw = function(req, body, type) {
    body <- sub('"gpt-6-luna"', '"altered-model"', rawToChar(body), fixed = TRUE)
    attach(req, charToRaw(body), type)
  }, .package = "httr2")
  local_mocked_bindings(.openai_req_perform = function(...) { calls <<- calls + 1L; stop("unexpected HTTP") },
                         .package = "pairwiseLLM")
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    expect_error(do.call(fn, decision_334_args()), class = "pairwiseLLM_decision_request_error")
  }
  expect_identical(calls, 0L)
})

test_that("serialization cannot change scientific fields or approved transport controls", {
  encode <- pairwiseLLM:::.decision_json
  mutation <- "scientific"
  calls <- 0L
  local_mocked_bindings(.decision_json = function(x) {
    if (is.list(x) && all(c("model", "input", "questions") %in% names(x))) {
      if (mutation == "scientific") x$questions[[1]]$instructions <- "serializer changed instructions"
      else x$prompt_cache_options <- list(mode = "implicit")
    }
    encode(x)
  }, .openai_request = function(...) { calls <<- calls + 1L; stop("unexpected IO") },
  .package = "pairwiseLLM")
  for (mutation in c("scientific", "controls")) {
    for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
      expect_error(do.call(fn, decision_334_args(prompt_cache_options = list(mode = "explicit"))),
                   class = "pairwiseLLM_decision_request_error")
    }
  }
  expect_identical(calls, 0L)
})

test_that("transient retries preserve the exact final payload and failed-attempt records", {
  captured <- list()
  httr2::local_mocked_responses(function(req) {
    captured[[length(captured) + 1L]] <<- req$body$data
    if (length(captured) == 1L) return(decision_334_response(list(error = list(message = "busy")), 503L))
    decision_334_response()
  })
  row <- suppressMessages(do.call(llm_compare_pair, decision_334_args(prompt_cache_options = list(mode = "explicit"))))
  expect_length(captured, 2L)
  expect_identical(captured[[1]], captured[[2]])
  expect_identical(jsonlite::parse_json(rawToChar(captured[[2]]))$prompt_cache_options, list(mode = "explicit"))
  expect_identical(nrow(row$retry_failures[[1]]), 1L)
  expect_identical(row$retry_failures[[1]]$error_code, "http_error")
  request <- decision_333_request(pair_uid = row$pair_uid)
  normalized <- pairwiseLLM:::.normalize_llm_results(row, decision_333_pairs(list(request)), "openai", "gpt-6-luna")
  expect_equal(nrow(normalized$results), 1L)
  expect_equal(nrow(normalized$failed_attempts), 1L)
})

test_that("the retry preflight runs before each attempt and integrity failures never retry", {
  preflights <- calls <- 0L
  local_mocked_bindings(.pairwiseLLM_req_perform = function(req) {
    calls <<- calls + 1L
    decision_334_response(list(error = list(message = "busy")), 503L)
  }, .package = "pairwiseLLM")
  validate <- function(req) {
    preflights <<- preflights + 1L
    if (preflights == 2L) pairwiseLLM:::.decision_request_error()
  }
  expect_error(suppressMessages(pairwiseLLM:::.retry_httr2_request(
    list(), base_delay = 0, validate_request = validate
  )), class = "pairwiseLLM_decision_request_error")
  expect_identical(preflights, 2L)
  expect_identical(calls, 1L)
})

test_that("live parsing preserves probability boundaries, selection and positional identity", {
  response <- decision_333_response()
  httr2::local_mocked_responses(function(req) decision_334_response(response))
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    for (case in list(list(p = c(0, 1), choice = "SAMPLE_2"), list(p = c(1, 0), choice = "SAMPLE_1"),
                       list(p = c(.5, .5), choice = "SAMPLE_1"), list(p = c(.5, .5), choice = "SAMPLE_2"))) {
      response <- do.call(decision_333_response, case)
      forward <- do.call(fn, decision_334_args())
      original <- decision_334_args()
      reversed <- do.call(fn, decision_334_args(ID1 = original$ID2, text1 = original$text2,
                                                ID2 = original$ID1, text2 = original$text1, pair_uid = "reversed-334"))
      expect_identical(c(forward$p_sample_1, forward$p_sample_2), case$p)
      expect_identical(forward$better_sample, case$choice)
      expect_identical(reversed$better_sample, case$choice)
      expect_false(identical(forward$better_id, reversed$better_id))
      expect_false(identical(forward$request_fingerprint, reversed$request_fingerprint))
    }
  }
})

test_that("malformed responses, refusals and inconsistent choices remain diagnostic failures", {
  cases <- list(
    list(body = "not JSON", code = "malformed_decision"),
    list(body = list(answers = list(list(type = "refusal", name = "writing_trait"))), code = "decision_refusal"),
    list(body = list(answers = list(list(type = "choice", name = "writing_trait", choice = "SAMPLE_2", confidence = .2))), code = "invalid_probabilities"),
    list(body = decision_333_response(p = c(.8, .2)), code = "choice_probability_mismatch")
  )
  current <- cases[[1]]$body
  calls <- 0L
  httr2::local_mocked_responses(function(req) { calls <<- calls + 1L; decision_334_response(current) })
  for (fn in list(openai_compare_pair_live, llm_compare_pair)) {
    for (case in cases) {
      current <- case$body
      before <- calls
      row <- do.call(fn, decision_334_args())
      expect_identical(calls, before + 1L)
      expect_identical(row$decision_error_code, case$code)
      expect_true("raw_response" %in% names(row))
      normalized <- pairwiseLLM:::.normalize_llm_results(row,
        decision_333_pairs(list(decision_333_request(pair_uid = row$pair_uid))), "openai", "gpt-6-luna")
      expect_equal(nrow(normalized$results), 0L)
      expect_identical(normalized$failed_attempts$error_code, case$code)
      if (case$code == "choice_probability_mismatch") {
        expect_identical(row$better_sample, "SAMPLE_2")
        expect_identical(row$p_chosen, .2)
      }
    }
  }
})

test_that("provider errors retain existing HTTP and retry condition contracts", {
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    decision_334_response(list(error = list(message = "synthetic invalid request")), 400L)
  })
  expect_error(do.call(openai_compare_pair_live, decision_334_args()), class = "httr2_http_400")
  expect_identical(calls, 1L)
})

test_that("timeout exhaustion preserves failed attempts without changing the payload", {
  captured <- list()
  retry <- pairwiseLLM:::.retry_httr2_request
  local_mocked_bindings(
    .retry_httr2_request = function(req, validate_request = NULL) {
      retry(req, max_attempts = 2L, base_delay = 0, validate_request = validate_request)
    },
    .pairwiseLLM_req_perform = function(req) {
      captured[[length(captured) + 1L]] <<- req$body$data
      rlang::abort("Synthetic timeout", class = "httr2_timeout")
    }, .package = "pairwiseLLM"
  )
  err <- tryCatch(suppressMessages(do.call(openai_compare_pair_live, decision_334_args())), error = identity)
  expect_s3_class(err, "pairwiseLLM_retry_error")
  expect_identical(attr(err, "retry_failures")$error_code, rep("timeout", 2L))
  expect_length(captured, 2L)
  expect_identical(captured[[1]], captured[[2]])
})

test_that("Decisions cannot enter managed Batch or deferred bulk execution", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "one", ID2 = "B", text2 = "two")
  for (fn in list(llm_submit_pairs_batch, submit_llm_pairs, submit_openai_pairs_live)) {
    expect_error(fn(pairs, model = "gpt-6-luna", trait_name = "Organization",
                    trait_description = "Logical sequencing", endpoint = "decisions"),
                 "Batch|Bulk")
  }
})
