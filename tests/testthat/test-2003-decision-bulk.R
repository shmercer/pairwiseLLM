test_that("bulk dispatch and direct wrappers retain identical binary scientific evidence", {
  decision_bulk_clock()
  for (provider in c("openai", "typesafe")) {
    captured <- list()
    local_mocked_bindings(.decision_sync_submit = function(req, spec) {
      pairwiseLLM:::.decision_live_validate(spec, req)
      captured[[length(captured) + 1L]] <<- req$body$data
      decision_bulk_response(provider)
    }, .package = "pairwiseLLM")
    pairs <- decision_bulk_pairs()
    pairs[2, c("ID1", "text1", "ID2", "text2")] <- pairs[1, c("ID2", "text2", "ID1", "text1")]
    args <- decision_bulk_args(provider, pairs = pairs)
    generic <- do.call(submit_llm_pairs, args)
    args$backend <- NULL
    direct <- do.call(if (provider == "openai") submit_openai_pairs_live else submit_typesafe_pairs_live, args)
    expect_identical(generic, direct)
    expect_equal(nrow(generic$results), 4L)
    expect_equal(nrow(generic$failed_pairs), 0L)
    expect_equal(nrow(generic$failed_attempts), 0L)
    expect_identical(generic$results$pair_uid, pairs$pair_uid)
    expect_identical(generic$results$p_sample_1, rep(.3, 4))
    expect_identical(generic$results$p_sample_2, rep(.7, 4))
    expect_identical(generic$results$decision_confidence, rep(.2, 4))
    expect_identical(generic$results$better_id, pairs$ID2)
    expect_identical(generic$results$winner_pos, rep(2L, 4))
    expect_identical(build_bt_data(generic$results)$result, rep(0, 4))
    expect_false(identical(captured[[1]], captured[[2]]))
    expect_identical(captured[[1]], captured[[3]])
    expect_true(all(nchar(generic$results$wire_body_sha256) == 64))
  }
})

test_that("generated occurrence IDs distinguish repetition and reversed presentation", {
  local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_response(), .package = "pairwiseLLM")
  pairs <- decision_bulk_pairs(3)
  pairs$pair_uid <- NULL
  pairs[2, c("ID1", "text1", "ID2", "text2")] <- pairs[1, c("ID2", "text2", "ID1", "text1")]
  out <- do.call(submit_llm_pairs, decision_bulk_args(pairs = pairs))
  expect_equal(length(unique(out$results$pair_uid)), 3L)
  expect_identical(out$results$request_fingerprint[c(1, 3)], rep(out$results$request_fingerprint[1], 2))
  expect_false(identical(out$results$request_fingerprint[1], out$results$request_fingerprint[2]))
  expect_match(out$results$pair_uid[3], "-2$")
})

test_that("rolling promises obey worker bounds and permit out-of-order completion", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  started <- finished <- character()
  active <- peak <- 0L
  resolve_first <- NULL
  local_mocked_bindings(.decision_async_submit = function(req, spec, pool, success, failure) {
    pairwiseLLM:::.decision_live_validate(spec, req)
    uid <- spec$request$pair_uid
    started <<- c(started, uid)
    active <<- active + 1L; peak <<- max(peak, active)
    promise <- promises::promise(function(resolve, reject) {
      done <- function() {
        finished <<- c(finished, uid)
        active <<- active - 1L
        resolve(decision_bulk_response())
      }
      if (uid == "occurrence-1") resolve_first <<- done else {
        later::later(function() {
          done()
          if (uid == "occurrence-3") resolve_first()
        }, delay = 0)
      }
    })
    promises::then(promise, onFulfilled = success, onRejected = failure)
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(parallel = TRUE, workers = 2))
  expect_identical(peak, 2L)
  expect_identical(active, 0L)
  expect_identical(finished[1L], "occurrence-2")
  expect_setequal(started, decision_bulk_pairs()$pair_uid)
  expect_identical(out$results$pair_uid, decision_bulk_pairs()$pair_uid)
})

test_that("the public async adapter retains validated bytes and handles transport rejection", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    decision_bulk_response("typesafe")
  })
  out <- do.call(submit_llm_pairs, decision_bulk_args("typesafe", parallel = TRUE, workers = 2))
  expect_equal(calls, 4L)
  expect_equal(nrow(out$results), 4L)
  local_mocked_bindings(.decision_async_submit = function(req, spec, pool, success, failure) {
    promises::then(promises::promise_reject(rlang::error_cnd("synthetic_failure", message = "PRIVATE")),
                    onFulfilled = success, onRejected = failure)
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args("typesafe", parallel = TRUE, workers = 2))
  expect_equal(nrow(out$failed_pairs), 4L)
  expect_equal(nrow(out$failed_attempts), 4L)
  expect_false(any(grepl("PRIVATE", out$failed_attempts$error_detail)))
})

test_that("request and token admission, retry-after and shared cooldowns are explicit", {
  clock <- decision_bulk_clock()
  times <- numeric(); bytes <- list()
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    times <<- c(times, clock$now); bytes[[length(bytes) + 1L]] <<- req$body$data
    decision_bulk_response(status = if (length(times) == 1L) 429L else 200L,
                           headers = list(`Retry-After` = "7"))
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs(2),
    execution_control = list(requests_per_minute = 2, tokens_per_minute = 10, token_estimates = 5)))
  expect_identical(times, c(1000, 1007, 1060))
  expect_identical(bytes[[1]], bytes[[3]])
  expect_equal(nrow(out$results), 2L)
  expect_equal(nrow(out$failed_attempts), 1L)
  expect_identical(out$failed_attempts$attempt_reason, "initial")
})

test_that("provider transient policies and timeout retries preserve payloads and strict budgets", {
  decision_bulk_clock()
  for (provider in c("openai", "typesafe")) {
    for (status in c(408L, 409L, 429L, 500L, 529L, 599L)) {
      calls <- list()
      local_mocked_bindings(.decision_sync_submit = function(req, spec) {
        calls[[length(calls) + 1L]] <<- req$body$data
        decision_bulk_response(provider, status)
      }, .package = "pairwiseLLM")
      out <- do.call(submit_llm_pairs, decision_bulk_args(provider, pairs = decision_bulk_pairs(1)))
      expected <- if (provider == "typesafe" && status == 409L) 1L else 3L
      expect_length(calls, expected)
      expect_equal(nrow(out$failed_attempts), expected)
      expect_equal(nrow(out$results), 0L)
      expect_true(all(vapply(calls, identical, logical(1L), calls[[1]])))
    }
  }
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L
    rlang::abort("PRIVATE timeout with key", class = "curl_error_operation_timedout")
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs(1)))
  expect_identical(calls, 3L)
  expect_identical(out$failed_attempts$error_code, rep("timeout", 3))
  expect_true(all(out$failed_attempts$possible_duplicate_charge))
})

test_that("permanent and invalid probability outcomes never become complete or retry automatically", {
  for (provider in c("openai", "typesafe")) {
    calls <- 0L
    local_mocked_bindings(.decision_sync_submit = function(req, spec) {
      calls <<- calls + 1L
      if (spec$request$pair_uid == "occurrence-1") return(decision_bulk_response(provider))
      if (spec$request$pair_uid == "occurrence-2") return(decision_bulk_response(provider, 401L))
      decision_bulk_response(provider, body = decision_333_response(provider, p = c(.2, .2)))
    }, .package = "pairwiseLLM")
    out <- do.call(submit_llm_pairs, decision_bulk_args(provider))
    expect_identical(calls, 4L)
    expect_equal(nrow(out$results), 1L)
    expect_equal(nrow(out$failed_pairs), 3L)
    expect_equal(nrow(out$failed_attempts), 3L)
    expect_identical(out$failed_pairs$error_code, c("http_error", "invalid_probabilities", "invalid_probabilities"))
  }
})

test_that("invalid controls and missing optional dependencies fail before requests", {
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(...) { calls <<- calls + 1L; stop("unexpected IO") }, .package = "pairwiseLLM")
  cases <- list(list(unknown = 1), list(max_attempts = 0), list(timeout_seconds = Inf),
                list(lock_timeout_ms = -1), list(retry_ambiguous = NA), list(requests_per_minute = 0),
                list(tokens_per_minute = 10), list(token_estimates = c(1, 2)),
                list(tokens_per_minute = 10, token_estimates = 11), list(token_estimates = NA_real_))
  for (control in cases) expect_error(do.call(submit_llm_pairs, decision_bulk_args(execution_control = control)))
  for (extra in list(list(workers = NA), list(status_every = 0), list(parallel = NA),
                     list(include_raw = NA), list(save_path = ""), list(pair_uid = "ambiguous"),
                     list(prompt_template = "generative"), list(temperature = NULL))) {
    args <- decision_bulk_args(); args[names(extra)] <- extra
    expect_error(do.call(submit_llm_pairs, args))
  }
  pairs <- decision_bulk_pairs(); pairs$pair_uid <- "duplicate"
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(pairs = pairs)), "unique")
  pairs <- decision_bulk_pairs(); pairs$text1[1] <- NA_character_
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(pairs = pairs)), "nonmissing")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(pairs = data.frame(ID1 = "A"))), "must contain")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args("typesafe", endpoint = "responses")), "systemone")
  expect_identical(calls, 0L)
  local_mocked_bindings(.decision_optional_available = function(package) FALSE, .package = "pairwiseLLM")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(parallel = TRUE, workers = 2)), "install.packages.*promises")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(save_path = tempfile())), "install.packages.*filelock")
  local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_response(), .package = "pairwiseLLM")
  expect_equal(nrow(do.call(submit_llm_pairs, decision_bulk_args())$results), 4L)
  expect_equal(nrow(do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs()[0, ]))$results), 0L)
})

test_that("post-attachment or pre-retry payload changes fail without extra transmissions", {
  decision_bulk_clock()
  perform <- pairwiseLLM:::.decision_sync_submit
  attach <- httr2::req_body_raw
  calls <- 0L
  local_mocked_bindings(.pairwiseLLM_req_perform = function(req, ...) {
    calls <<- calls + 1L; decision_bulk_response(status = 503L)
  }, .decision_sync_submit = function(req, spec) {
    if (calls == 1L) req$body$data <- charToRaw("{}")
    perform(req, spec)
  }, .package = "pairwiseLLM")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs(1))),
               class = "pairwiseLLM_decision_request_error")
  expect_identical(calls, 1L)
  local_mocked_bindings(req_body_raw = function(req, body, type) attach(req, charToRaw("{}"), type), .package = "httr2")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args()), class = "pairwiseLLM_decision_request_error")
  expect_identical(calls, 1L)
})

test_that("async fatal errors cancel session handles and do not start more work", {
  skip_if_not_installed("promises"); skip_if_not_installed("later")
  submitted <- cancelled <- 0L
  owned_pool <- NULL
  local_mocked_bindings(.decision_async_submit = function(req, spec, pool, success, failure) {
    submitted <<- submitted + 1L; owned_pool <<- pool
    promises::then(promises::promise_resolve(decision_bulk_response()), onFulfilled = success, onRejected = failure)
  }, .decision_checkpoint_commit = function(journal, uid, state, include_raw) {
    if (state$status == "completed") pairwiseLLM:::.decision_checkpoint_error("synthetic disk failure")
    assign(uid, state, journal$states)
  }, .package = "pairwiseLLM")
  local_mocked_bindings(multi_list = function(pool) {
    expect_identical(pool, owned_pool); list("session-owned-handle")
  }, multi_cancel = function(handle) {
    expect_identical(handle, "session-owned-handle"); cancelled <<- cancelled + 1L
  }, .package = "curl")
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(parallel = TRUE, workers = 2)), "synthetic disk failure")
  expect_identical(submitted, 2L)
  expect_identical(cancelled, 1L)
})

test_that("async curl preparation retains exact scientific JSON and safe transport settings", {
  skip_if_not_installed("promises"); skip_if_not_installed("later")
  prepare_handle <- getFromNamespace("req_handle", "httr2")
  captured <- list()
  provider <- "openai"
  local_mocked_bindings(req_handle = function(req) {
    captured[[length(captured) + 1L]] <<- req
    prepare_handle(req)
  }, .package = "httr2")
  local_mocked_bindings(multi_add = function(handle, done, fail, pool, data) {
    done(list(method = "POST", url = if (provider == "openai") "https://api.openai.com/v1/decisions" else "https://api.typesafe.ai/v1/systemone",
              status_code = 200L, headers = charToRaw("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nx-request-id: synthetic-wire-id\r\n\r\n"),
              content = charToRaw(pairwiseLLM:::.decision_json(decision_333_response(provider))), times = numeric()))
    invisible(handle)
  }, .package = "curl")
  for (provider in c("openai", "typesafe")) {
    captured <- list()
    args <- decision_bulk_args(provider, pairs = decision_bulk_pairs(2), parallel = TRUE, workers = 2)
    if (provider == "openai") args$prompt_cache_options <- list(mode = "explicit")
    out <- do.call(submit_llm_pairs, args)
    expect_equal(nrow(out$results), 2L)
    expect_identical(out$results$provider_request_id, rep("synthetic-wire-id", 2))
    for (req in captured) {
      body <- req$options$postfields
      expect_true(is.raw(body))
      wire <- jsonlite::parse_json(rawToChar(body))
      evidence <- jsonlite::parse_json(wire$input %||% wire$state)
      expect_identical(evidence, list(SAMPLE_1 = "Synthetic sample A.", SAMPLE_2 = "Synthetic sample B."))
      expect_identical(wire$model, args$model)
      if (provider == "openai") expect_identical(wire$prompt_cache_options, list(mode = "explicit"))
      expect_false(isTRUE(req$options$verbose))
      expect_identical(out$results$wire_body_sha256[1], pairwiseLLM:::.decision_sha256(rawToChar(body)))
    }
  }
})

test_that("repeated async callbacks cannot create additional outcomes or attempts", {
  skip_if_not_installed("promises"); skip_if_not_installed("later")
  calls <- 0L
  local_mocked_bindings(.decision_async_submit = function(req, spec, pool, success, failure) {
    calls <<- calls + 1L
    promises::then(promises::promise_resolve(decision_bulk_response()), onFulfilled = function(resp) {
      success(resp); success(resp)
    }, onRejected = failure)
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(parallel = TRUE, workers = 2))
  expect_identical(calls, 4L)
  expect_equal(nrow(out$results), 4L)
  expect_equal(nrow(out$failed_attempts), 0L)
  expect_identical(out$results$attempt_count, rep(1L, 4))
})
