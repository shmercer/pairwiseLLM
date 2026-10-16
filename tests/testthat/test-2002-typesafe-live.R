test_that("both TypeSafe entry points transmit the frozen JSON and retain result evidence", {
  captured <- list()
  auth <- character()
  httr2::local_mocked_responses(function(req) {
    headers <- if (inherits(req$headers, "httr2_headers")) {
      getFromNamespace("headers_flatten", "httr2")(req$headers, redact = FALSE)
    } else req$headers
    auth <<- c(auth, headers$Authorization)
    captured[[length(captured) + 1L]] <<- req
    typesafe_335_response()
  })
  local_mocked_bindings(set_prompt_template = function(...) stop("generative default evaluated"),
                         .package = "pairwiseLLM")
  direct <- do.call(typesafe_compare_pair_live, typesafe_335_args())
  routed <- do.call(llm_compare_pair, c(typesafe_335_args(), list(backend = "typesafe")))
  expect_identical(direct, routed)
  expect_identical(auth, rep("Bearer synthetic-fixture-key", 2L))
  for (req in captured) {
    expect_identical(req$url, "https://api.typesafe.ai/v1/systemone")
    expect_identical(req$body$type, "raw")
    expect_identical(req$body$content_type, "application/json")
    wire <- jsonlite::parse_json(rawToChar(req$body$data))
    expect_identical(wire, decision_333_fixture("typesafe-request.json"))
  }
  expect_identical(c(direct$p_sample_1, direct$p_sample_2, direct$p_chosen,
                     direct$decision_confidence), c(.3, .7, .7, .2))
  expect_identical(direct$better_sample, "SAMPLE_2")
  expect_identical(direct$better_id, "B:02")
  expect_identical(direct$winner_pos, 2L)
  expect_identical(direct$custom_id, "live-pair-335")
  expect_identical(direct$pair_uid, direct$custom_id)
  expect_identical(direct$provider_request_id, "provider-335")
  expect_identical(direct$endpoint, "/v1/systemone")
  expect_identical(direct$requested_model, "jev-1.13.0")
  expect_identical(direct$resolved_model, "jev-1.13.0")
  expect_identical(c(direct$prompt_tokens, direct$completion_tokens, direct$total_tokens), c(12, 3, NA_real_))
  expect_identical(direct$object_type, "decision")
  expect_true(is.na(direct$error_message))
  expect_false("raw_response" %in% names(direct))
  expect_identical(jsonlite::parse_json(direct$request_provenance_json)$provider, "typesafe")
})

test_that("actual prepared curl bytes preserve custom instructions and reversed evidence", {
  captured <- list()
  local_mocked_bindings(req_perform1 = function(req, req_prep, ...) {
    captured[[length(captured) + 1L]] <<- req_prep
    typesafe_335_response()
  }, .package = "httr2")
  template <- "Judge {TRAIT_NAME}.\nFull guidance:\n{TRAIT_DESCRIPTION}"
  args <- typesafe_335_args(text1 = "\u00e9\r\n\"SAMPLE_2\" literal", text2 = "",
                            decision_template = template, include_raw = TRUE)
  forward <- do.call(typesafe_compare_pair_live, args)
  reversed_args <- args
  reversed_args[c("ID1", "text1", "ID2", "text2")] <- args[c("ID2", "text2", "ID1", "text1")]
  reversed_args$pair_uid <- "reversed-335"
  reversed <- do.call(llm_compare_pair, c(reversed_args, list(backend = "typesafe", endpoint = "systemone")))
  expect_length(captured, 2L)
  for (i in seq_along(captured)) {
    req <- captured[[i]]
    expected_args <- if (i == 1L) args else reversed_args
    expect_identical(req$url, "https://api.typesafe.ai/v1/systemone")
    expect_true(req$options$post)
    expect_identical(req$headers$`Content-Type`, "application/json")
    wire <- jsonlite::parse_json(rawToChar(req$options$postfields))
    expect_identical(jsonlite::parse_json(wire$state),
                     list(SAMPLE_1 = expected_args$text1, SAMPLE_2 = expected_args$text2))
    expect_identical(wire$questions$writing_trait$instructions,
                     paste0("Judge Organization.\nFull guidance:\n", args$trait_description))
    expect_identical(req$options$postfields, charToRaw(pairwiseLLM:::.decision_json(wire)))
  }
  expect_match(forward$decision_template_version, "^custom-sha256-")
  expect_true("raw_response" %in% names(forward))
  expect_false(identical(forward$better_id, reversed$better_id))
  expect_false(identical(forward$request_fingerprint, reversed$request_fingerprint))
})

test_that("TypeSafe defaults and model overrides never substitute or infer resolved versions", {
  sent <- character()
  response <- decision_333_response("typesafe")
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, jsonlite::parse_json(rawToChar(req$body$data))$model)
    typesafe_335_response(response)
  })
  for (backend in c("direct", "typesafe")) {
    fn <- if (backend == "direct") typesafe_compare_pair_live else llm_compare_pair
    args <- typesafe_335_args()
    if (backend == "typesafe") args$backend <- backend
    args$model <- args$pair_uid <- NULL
    row <- do.call(fn, args)
    expect_identical(tail(sent, 1), "jev-1.13.0")
    expect_identical(row$pair_uid, "LIVE_A_with_vs_delimiter_vs_B:02")
    for (model in c("jev-1.14.0", "jev-27.3.15", "jev-latest", "jev-preview")) {
      args$model <- model
      for (resolved in list("jev-1.14.0", NULL)) {
        response$model <- resolved
        row <- do.call(fn, args)
        expect_identical(tail(sent, 1), model)
        expect_identical(row$requested_model, model)
        expect_identical(row$resolved_model, resolved %||% NA_character_)
        expect_identical(row$model, resolved %||% model)
      }
    }
  }
})

test_that("unsupported controls and invalid inputs fail before any credential or provider operation", {
  calls <- 0L
  forbidden <- function(...) { calls <<- calls + 1L; stop("unexpected IO") }
  local_mocked_bindings(.typesafe_api_key = forbidden, .typesafe_request = forbidden,
                         .pairwiseLLM_req_perform = forbidden, .package = "pairwiseLLM")
  controls <- list(temperature = 0, top_p = NULL, logprobs = FALSE, reasoning = "none",
                   max_output_tokens = 1, include_thoughts = FALSE, tag_prefix = "<TAG>",
                   store = FALSE, service_tier = NULL, prompt_cache_options = NULL,
                   long_context = TRUE, headers = list(), base_url = "https://example.com",
                   unknown = TRUE, prompt_template = set_prompt_template())
  for (backend in c("direct", "typesafe")) {
    fn <- if (backend == "direct") typesafe_compare_pair_live else llm_compare_pair
    args <- typesafe_335_args()
    if (backend == "typesafe") args$backend <- backend
    for (name in names(controls)) {
      expect_error(do.call(fn, c(args, controls[name])), "supported|Generative")
    }
    for (model in list(NULL, NA_character_, "", 1, c("jev-1.13.0", "jev-latest"),
                       matrix("jev-latest"), "jev", "jev-1.13", "gpt-6-luna", "jev-1.13.0\n",
                       "jev-unknown", "jev-1.13.0/extra", " jev-latest")) {
      args["model"] <- list(model)
      expect_error(do.call(fn, args), "model")
    }
    args$model <- "jev-1.13.0"
    for (value in list(NA, NULL, 1, c(TRUE, FALSE), matrix(TRUE))) {
      expect_error(do.call(fn, c(args, list(include_raw = value))), "include_raw")
    }
    expect_error(do.call(fn, c(args, list(decision_template = "missing placeholders"))), "decision_template")
    expect_error(do.call(fn, c(args, list(pair_uid = "a", pair_uid = "b"))), "uniquely named")
    unnamed_args <- c(args, list(prompt_template = NULL, include_raw = FALSE))
    if (backend == "typesafe") unnamed_args$endpoint <- "systemone"
    expect_error(do.call(fn, c(unnamed_args, list(TRUE))), "uniquely named")
  }
  args <- typesafe_335_args()
  for (endpoint in list(NULL, "decisions", "responses", c("systemone", "decisions"), NA_character_)) {
    expect_error(do.call(llm_compare_pair, c(args, list(backend = "typesafe", endpoint = endpoint))), "endpoint")
  }
  expect_identical(calls, 0L)
})

test_that("every altered TypeSafe scientific request fails unchanged before provider operations", {
  original <- decision_333_request("typesafe", pair_uid = "live-pair-335")
  altered <- original
  calls <- 0L
  forbidden <- function(...) { calls <<- calls + 1L; stop("unexpected IO") }
  local_mocked_bindings(.build_decision_request = function(...) altered,
                         .typesafe_api_key = forbidden, .typesafe_request = forbidden,
                         .pairwiseLLM_req_perform = forbidden, .package = "pairwiseLLM")
  for (mutate in typesafe_335_mutations()) {
    altered <- mutate(original)
    before <- serialize(altered, NULL)
    for (backend in c("direct", "typesafe")) {
      args <- typesafe_335_args()
      fn <- typesafe_compare_pair_live
      if (backend == "typesafe") { args$backend <- backend; fn <- llm_compare_pair }
      err <- tryCatch(do.call(fn, args), error = identity)
      expect_s3_class(err, "pairwiseLLM_decision_request_error")
      expect_false(grepl("Synthetic|Changed|Altered|fixture-key", conditionMessage(err)))
      expect_identical(serialize(altered, NULL), before)
    }
  }
  expect_identical(calls, 0L)
})

test_that("serialization and request attachment cannot change validated content", {
  encode <- pairwiseLLM:::.decision_json
  attach <- httr2::req_body_raw
  mode <- "serialize"
  calls <- 0L
  local_mocked_bindings(.decision_json = function(x) {
    if (mode == "serialize" && is.list(x) && all(c("model", "state", "questions") %in% names(x))) {
      x$questions$writing_trait$instructions <- "Changed during serialization"
    }
    encode(x)
  }, .pairwiseLLM_req_perform = function(...) { calls <<- calls + 1L; stop("unexpected HTTP") },
  .package = "pairwiseLLM")
  local_mocked_bindings(req_body_raw = function(req, body, type) {
    req <- attach(req, body, type)
    if (mode == "bytes") req$body$data <- charToRaw("{}")
    if (mode == "url") req$url <- "https://example.com/v1/systemone"
    if (mode == "content_type") req$body$content_type <- "text/plain"
    if (mode == "body_type") req$body$type <- "json"
    req
  }, .package = "httr2")
  for (mode in c("serialize", "bytes", "url", "content_type", "body_type")) {
    for (backend in c("direct", "typesafe")) {
      args <- typesafe_335_args()
      fn <- typesafe_compare_pair_live
      if (backend == "typesafe") { args$backend <- backend; fn <- llm_compare_pair }
      expect_error(do.call(fn, args), class = "pairwiseLLM_decision_request_error")
    }
  }
  expect_identical(calls, 0L)
})

test_that("TypeSafe transient statuses retry identical bytes with retained attempts", {
  retry <- pairwiseLLM:::.retry_httr2_request
  local_mocked_bindings(.retry_httr2_request = function(req, validate_request, transient_status) {
    retry(req, base_delay = 0, validate_request = validate_request, transient_status = transient_status)
  }, .package = "pairwiseLLM")
  captured <- list()
  transient <- 429L
  httr2::local_mocked_responses(function(req) {
    captured[[length(captured) + 1L]] <<- req$body$data
    typesafe_335_response(status = if (length(captured) == 1L) transient else 200L)
  })
  for (transient in c(408L, 429L, 500L, 501L, 503L, 529L, 599L)) {
    captured <- list()
    row <- suppressMessages(do.call(typesafe_compare_pair_live, typesafe_335_args()))
    expect_length(captured, 2L)
    expect_identical(captured[[1]], captured[[2]])
    expect_identical(row$retry_failures[[1]]$error_detail, paste0("HTTP ", transient))
    expect_true(is.na(row$decision_error_code))
  }
})

test_that("retry preflights detect altered bytes or scientific bodies before a second HTTP attempt", {
  retry <- pairwiseLLM:::.retry_httr2_request
  calls <- preflights <- 0L
  mode <- "bytes"
  local_mocked_bindings(.retry_httr2_request = function(req, validate_request, transient_status) {
    retry(req, base_delay = 0, transient_status = transient_status, validate_request = function(r) {
      preflights <<- preflights + 1L
      if (preflights == 2L) {
        if (mode == "bytes") r$body$data <- charToRaw("{}") else {
          env <- environment(validate_request)
          env$request$body$questions$writing_trait$instructions <- "Altered between attempts"
        }
      }
      validate_request(r)
    })
  }, .pairwiseLLM_req_perform = function(req) {
    calls <<- calls + 1L
    typesafe_335_response(status = 529L)
  }, .package = "pairwiseLLM")
  for (mode in c("bytes", "scientific")) {
    calls <- preflights <- 0L
    expect_error(suppressMessages(do.call(typesafe_compare_pair_live, typesafe_335_args())),
                 class = "pairwiseLLM_decision_request_error")
    expect_identical(calls, 1L)
    expect_identical(preflights, 2L)
  }
})

test_that("HTTP failures have bounded retries, actionable diagnostics and no provider payloads", {
  retry <- pairwiseLLM:::.retry_httr2_request
  local_mocked_bindings(.retry_httr2_request = function(req, validate_request, transient_status) {
    retry(req, base_delay = 0, validate_request = validate_request, transient_status = transient_status)
  }, .package = "pairwiseLLM")
  status <- 401L
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    typesafe_335_response(list(error = list(message = "PRIVATE SAMPLE synthetic-fixture-key")), status)
  })
  for (status in c(400L, 401L, 403L, 404L, 413L, 422L, 429L, 529L)) {
    calls <- 0L
    args <- typesafe_335_args(model = "jev-27.3.15")
    err <- tryCatch(suppressMessages(do.call(typesafe_compare_pair_live, args)), error = identity)
    expect_s3_class(err, "pairwiseLLM_typesafe_http_error")
    expect_identical(err$status_code, status)
    expect_identical(calls, if (status %in% c(429L, 529L)) 3L else 1L)
    expect_identical(nrow(attr(err, "retry_failures")), calls)
    expect_false(grepl("PRIVATE|fixture-key", paste(capture.output(str(err)), collapse = " ")))
    expect_match(conditionMessage(err), if (status %in% c(401L, 403L)) "TYPESAFE_API_KEY" else "model|limits")
  }
})

test_that("timeouts and other network failures retain safe diagnostics without retrying invalid requests", {
  retry <- pairwiseLLM:::.retry_httr2_request
  calls <- 0L
  timeout <- TRUE
  local_mocked_bindings(.retry_httr2_request = function(req, validate_request, transient_status) {
    retry(req, base_delay = 0, validate_request = validate_request, transient_status = transient_status)
  }, .pairwiseLLM_req_perform = function(req) {
    calls <<- calls + 1L
    rlang::abort("PRIVATE SAMPLE synthetic-fixture-key",
                  class = if (timeout) "httr2_timeout" else "synthetic_connection_error")
  }, .package = "pairwiseLLM")
  for (timeout in c(TRUE, FALSE)) {
    calls <- 0L
    err <- tryCatch(suppressMessages(do.call(typesafe_compare_pair_live, typesafe_335_args())), error = identity)
    expect_s3_class(err, "pairwiseLLM_typesafe_transport_error")
    expect_identical(calls, if (timeout) 3L else 1L)
    expect_identical(nrow(err$retry_failures), calls)
    expect_identical(attr(err, "retry_failures"), err$retry_failures)
    expect_false(grepl("PRIVATE|fixture-key", paste(capture.output(str(err)), collapse = " ")))
  }
})

test_that("probability boundaries, near ties and normalization retain binary winner semantics", {
  response <- decision_333_response("typesafe")
  httr2::local_mocked_responses(function(req) typesafe_335_response(response))
  for (case in list(list(p = c(0, 1), choice = "SAMPLE_2"), list(p = c(1, 0), choice = "SAMPLE_1"),
                     list(p = c(.5, .5), choice = "SAMPLE_1"), list(p = c(.5, .5), choice = "SAMPLE_2"),
                     list(p = c(.500000001, .499999999), choice = "SAMPLE_2"))) {
    response <- do.call(decision_333_response, c(list(provider = "typesafe"), case))
    forward <- do.call(typesafe_compare_pair_live, typesafe_335_args())
    original <- typesafe_335_args()
    reversed <- do.call(llm_compare_pair, c(typesafe_335_args(
      ID1 = original$ID2, text1 = original$text2, ID2 = original$ID1, text2 = original$text1,
      pair_uid = "reversed-335"), list(backend = "typesafe")))
    expect_identical(c(forward$p_sample_1, forward$p_sample_2), case$p)
    expect_identical(forward$better_sample, case$choice)
    expect_false(identical(forward$better_id, reversed$better_id))
    requests <- list(decision_333_request("typesafe", pair_uid = "live-pair-335"),
                      decision_333_request("typesafe", ID1 = original$ID2, text1 = original$text2,
                                            ID2 = original$ID1, text2 = original$text1, pair_uid = "reversed-335"))
    normalized <- pairwiseLLM:::.normalize_llm_results(dplyr::bind_rows(forward, reversed),
      decision_333_pairs(requests), "typesafe", "jev-1.13.0")
    expect_equal(nrow(normalized$results), 2L)
    bt <- build_bt_data(normalized$results)
    expect_equal(nrow(bt), 2L)
    expect_true(all(bt$result %in% c(0, 1)))
    expect_identical(bt, build_bt_data(dplyr::bind_rows(forward, reversed)))
  }
})

test_that("invalid responses retain diagnostics and never produce additional provider calls", {
  base <- decision_333_response("typesafe")
  no_confidence <- base
  no_confidence$answers$writing_trait$confidence <- NULL
  no_usage <- base
  no_usage$usage <- no_usage$model <- NULL
  bad_usage <- base
  bad_usage$usage$input_tokens <- -1
  bad_map <- base
  bad_map$answers <- unname(bad_map$answers)
  unknown_type <- base
  unknown_type$answers$writing_trait$type <- "refusal"
  cases <- list(
    list(body = "not JSON", code = "malformed_decision"),
    list(body = no_confidence, code = "invalid_confidence"),
    list(body = bad_usage, code = "malformed_decision"),
    list(body = bad_map, code = "malformed_decision"),
    list(body = unknown_type, code = "malformed_decision"),
    list(body = decision_333_response("typesafe", p = c(.2, .2)), code = "invalid_probabilities"),
    list(body = decision_333_response("typesafe", p = c(.8, .2)), code = "choice_probability_mismatch")
  )
  current <- base
  calls <- 0L
  httr2::local_mocked_responses(function(req) { calls <<- calls + 1L; typesafe_335_response(current) })
  request <- decision_333_request("typesafe", pair_uid = "live-pair-335")
  for (case in cases) {
    current <- case$body
    before <- calls
    row <- do.call(typesafe_compare_pair_live, typesafe_335_args())
    expect_identical(calls, before + 1L)
    expect_identical(row$decision_error_code, case$code)
    expect_true("raw_response" %in% names(row))
    normalized <- pairwiseLLM:::.normalize_llm_results(row, decision_333_pairs(list(request)), "typesafe", "jev-1.13.0")
    expect_equal(nrow(normalized$results), 0L)
    expect_equal(nrow(normalized$failed_pairs), 1L)
  }
  current <- no_usage
  row <- do.call(typesafe_compare_pair_live, typesafe_335_args())
  expect_true(all(is.na(c(row$prompt_tokens, row$completion_tokens, row$total_tokens, row$resolved_model))))
})

test_that("TypeSafe cannot enter deferred live bulk or managed Batch paths", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  calls <- 0L
  local_mocked_bindings(.typesafe_request = function(...) { calls <<- calls + 1L; stop("unexpected IO") },
                         .package = "pairwiseLLM")
  for (fn in list(submit_llm_pairs, llm_submit_pairs_batch, llm_submit_pairs_multi_batch)) {
    expect_error(fn(pairs, model = "jev-1.13.0", backend = "typesafe",
                    trait_name = "Organization", trait_description = "Logical sequencing"))
  }
  expect_identical(calls, 0L)
})

test_that("the TypeSafe dispatcher resolves environment credentials without evaluating generative defaults", {
  withr::local_envvar(c(TYPESAFE_API_KEY = "SYNTHETIC_ENV_KEY"))
  auth <- NULL
  httr2::local_mocked_responses(function(req) {
    headers <- if (inherits(req$headers, "httr2_headers")) {
      getFromNamespace("headers_flatten", "httr2")(req$headers, redact = FALSE)
    } else req$headers
    auth <<- headers$Authorization
    typesafe_335_response()
  })
  local_mocked_bindings(set_prompt_template = function(...) stop("unexpected generative default"),
                         .package = "pairwiseLLM")
  row <- do.call(llm_compare_pair, c(typesafe_335_args(api_key = ""), list(backend = "typesafe")))
  expect_identical(auth, "Bearer SYNTHETIC_ENV_KEY")
  expect_identical(row$better_sample, "SAMPLE_2")
})
