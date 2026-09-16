# Mock below req_perform(): httr2_mock bypasses the native HTTP retry loop.
batch_retry_response <- function(status = 200L, body = "{}", headers = list()) {
  httr2::response(status, headers = c(list(`Content-Type` = "application/json"), headers),
    body = charToRaw(body))
}

batch_retry_metadata <- function(provider) {
  switch(provider,
    openai = '{"id":"b1","status":"completed","output_file_id":"out","error_file_id":"err"}',
    anthropic = '{"id":"b1","processing_status":"ended","results_url":"https://example.com/results"}',
    gemini = paste0('{"name":"batches/b1","metadata":{"state":"BATCH_STATE_SUCCEEDED"},',
      '"response":{"inlinedResponses":[{"response":{"text":"answer"}}]}}'))
}

batch_retry_job <- function(provider, directory) {
  input <- file.path(directory, paste0(provider, "-input.json"))
  jsonlite::write_json(list(requests = list(list(custom_id = "c1", ID1 = "A", ID2 = "B",
    request = list()))), input, auto_unbox = TRUE)
  list(segment_index = 1L, provider = provider, model = "fixture", batch_id = "batches/b1",
    batch_input_path = input, batch_output_path = file.path(directory, paste0(provider, "-output.jsonl")),
    csv_path = file.path(directory, paste0(provider, ".csv")),
    pairs = tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b"), done = FALSE, results = NULL)
}

test_that("retry delays honor valid headers and bound exponential jitter", {
  local_mocked_bindings(.batch_now = function() as.POSIXct("2026-01-01", tz = "UTC"),
    .package = "pairwiseLLM")
  after <- pairwiseLLM:::.batch_retry_after
  for (value in c("0", "7", " 12 ")) {
    expect_equal(after(batch_retry_response(headers = list(`Retry-After` = value))), as.numeric(value))
  }
  for (value in c("garbage", "-2", "Inf", "1.5", paste(rep("9", 400), collapse = ""))) {
    expect_true(is.na(after(batch_retry_response(headers = list(`Retry-After` = value)))))
  }
  expect_true(is.na(after(batch_retry_response())))
  expect_equal(after(batch_retry_response(headers = list(Date = "Wed, 01 Jan 2020 00:00:00 GMT",
    `Retry-After` = "Wed, 01 Jan 2020 00:00:09 GMT"))), 9)
  expect_equal(after(batch_retry_response(headers = list(Date = "Wed, 01 Jan 2020 00:00:10 GMT",
    `Retry-After` = "Wed, 01 Jan 2020 00:00:09 GMT"))), 0)
  # An unusable Date header uses the local clock; this past retry date needs no wait.
  expect_equal(after(batch_retry_response(headers = list(Date = "invalid",
    `Retry-After` = "Wed, 01 Jan 2020 00:00:00 GMT"))), 0)
  withr::local_seed(251)
  delays <- vapply(1:12, pairwiseLLM:::.batch_retry_backoff, numeric(1))
  expect_true(all(delays >= pmin(30, 0.5 * 2^(0:11))))
  expect_true(all(delays <= pmin(30, 0.5 * 2^(0:11) + 0.25)))
})

test_that("native retries recover every selected HTTP status and respect waits", {
  withr::local_seed(251)
  calls <- 0L
  waits <- numeric()
  status <- 503L
  headers <- list(`Retry-After` = "7")
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) batch_retry_response(status, headers = headers) else batch_retry_response()
    },
    sys_sleep = function(seconds, ...) {
      if (seconds > 0) waits <<- c(waits, seconds)
    },
    .package = "httr2"
  )
  req <- httr2::request("https://example.com/batch")
  for (code in c(408L, 429L, 500L, 501L, 502L, 503L, 504L, 529L, 599L)) {
    calls <- 0L
    waits <- numeric()
    status <- code
    expect_equal(httr2::resp_status(pairwiseLLM:::.batch_req_perform(req)), 200L)
    expect_equal(calls, 2L)
    expect_equal(waits, 7)
  }
  for (value in c("invalid", "0", "Wed, 01 Jan 2020 00:00:09 GMT")) {
    calls <- 0L
    waits <- numeric()
    headers <- list(`Retry-After` = value, Date = "Wed, 01 Jan 2020 00:00:00 GMT")
    pairwiseLLM:::.batch_req_perform(req)
    if (value == "invalid") expect_true(waits >= 0.5 && waits <= 0.75)
    if (value == "0") expect_length(waits, 0L)
    if (startsWith(value, "Wed")) expect_equal(waits, 9)
  }
})

test_that("permanent HTTP errors fail once and transient exhaustion preserves error classes", {
  withr::local_seed(251)
  calls <- 0L
  status <- 401L
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      batch_retry_response(status)
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  req <- httr2::request("https://example.com/batch")
  for (code in c(400L, 401L, 403L, 404L, 422L)) {
    calls <- 0L
    status <- code
    err <- tryCatch(pairwiseLLM:::.batch_req_perform(req), error = identity)
    expect_s3_class(err, paste0("httr2_http_", code))
    expect_false(inherits(err, "pairwiseLLM_batch_retry_exhausted"))
    expect_equal(calls, 1L)
  }
  status <- 503L
  for (budget in c(1L, 2L, 3L, 4L)) {
    calls <- 0L
    err <- tryCatch(pairwiseLLM:::.batch_req_perform(req, budget), error = identity)
    expect_s3_class(err, "pairwiseLLM_batch_retry_exhausted")
    expect_s3_class(err, "httr2_http_503")
    expect_equal(calls, budget)
  }
  expect_error(pairwiseLLM:::.batch_req_perform(req, 0L), "max_tries")
  expect_error(pairwiseLLM:::.batch_req_perform(httr2::req_method(req, "POST")), "require a GET")
  expect_error(pairwiseLLM:::.batch_req_perform(httr2::req_body_json(req, list(x = 1))), "require a GET")
})

test_that("wrapped transport failures recover or exhaust without hiding local errors", {
  withr::local_seed(251)
  calls <- 0L
  recover <- TRUE
  transport <- rlang::error_cnd("httr2_failure", message = "connection reset",
    parent = rlang::error_cnd("curl_error_recv_error", message = "reset"))
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      if (recover && calls == 2L) batch_retry_response() else transport
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  req <- httr2::request("https://example.com/batch")
  expect_equal(httr2::resp_status(pairwiseLLM:::.batch_req_perform(req)), 200L)
  expect_equal(calls, 2L)
  recover <- FALSE
  calls <- 0L
  err <- tryCatch(pairwiseLLM:::.batch_req_perform(req), error = identity)
  expect_s3_class(err, "pairwiseLLM_batch_retry_exhausted")
  expect_s3_class(err$parent, "curl_error_recv_error")
  expect_equal(calls, 3L)
  expect_error(pairwiseLLM:::.batch_retrieval_try(stop("local failure"), "OpenAI", "b1", "Read", FALSE),
    "local failure")
})

test_that("all public pollers recover on the same batch without submission", {
  withr::local_seed(251)
  calls <- 0L
  provider <- "openai"
  urls <- character()
  local_mocked_bindings(
    req_perform1 = function(req, ...) {
      calls <<- calls + 1L
      urls <<- c(urls, req$url)
      expect_identical(req$method %||% "GET", "GET")
      if (calls == 1L) batch_retry_response(503L) else batch_retry_response(body = batch_retry_metadata(provider))
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  local_mocked_bindings(
    openai_create_batch = function(...) stop("must not submit"),
    openai_upload_batch_file = function(...) stop("must not upload"),
    anthropic_create_batch = function(...) stop("must not submit"),
    gemini_create_batch = function(...) stop("must not submit"), .package = "pairwiseLLM"
  )
  for (p in c("openai", "anthropic", "gemini")) {
    provider <- p
    calls <- 0L
    urls <- character()
    poll <- getExportedValue("pairwiseLLM", paste0(p, "_poll_batch_until_complete"))
    args <- list("batches/b1", api_key = "fixture-key", interval_seconds = 0, timeout_seconds = 0, verbose = FALSE)
    if (p == "openai") args$max_attempts <- 1L
    result <- do.call(poll, args)
    expect_type(result, "list")
    expect_equal(calls, 2L)
    expect_length(unique(urls), 1L)
    expect_match(urls[1], "batches/b1", fixed = TRUE)
  }
})

test_that("poll limits count status polls and include retry elapsed time", {
  withr::local_seed(251)
  calls <- 0L
  elapsed <- 0
  start <- as.POSIXct("2026-01-01", tz = "UTC")
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      if (calls %% 2L == 1L) {
        batch_retry_response(503L)
      } else {
        elapsed <<- elapsed + 10
        batch_retry_response(body = '{"status":"finalizing"}')
      }
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  local_mocked_bindings(.batch_now = function() start + elapsed, .package = "pairwiseLLM")
  expect_error(pairwiseLLM::openai_poll_batch_until_complete("b1", api_key = "fixture", interval_seconds = 0,
    max_attempts = 2L, timeout_seconds = 100, verbose = FALSE), "max_attempts")
  expect_equal(calls, 4L)
  calls <- 0L
  expect_error(pairwiseLLM::openai_poll_batch_until_complete("b1", api_key = "fixture", interval_seconds = 0,
    timeout_seconds = 1, verbose = FALSE), "Timeout")
  expect_equal(calls, 2L)
})

test_that("downloads retry metadata and content and preserve files on exhaustion", {
  withr::local_seed(251)
  directory <- withr::local_tempdir()
  provider <- "openai"
  fail_content <- FALSE
  counts <- new.env(parent = emptyenv())
  payload <- '{"custom_id":"c1","error":{"message":"fixture"}}\n'
  local_mocked_bindings(
    req_perform1 = function(req, ...) {
      expect_identical(req$method %||% "GET", "GET")
      key <- if (grepl("/content$|/results$", req$url)) "content" else "metadata"
      counts[[key]] <- (counts[[key]] %||% 0L) + 1L
      if (counts[[key]] == 1L || (fail_content && key == "content")) return(batch_retry_response(503L))
      batch_retry_response(body = if (key == "content") payload else batch_retry_metadata(provider))
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  for (kind in c("openai_output", "openai_errors", "anthropic", "gemini")) {
    provider <- if (startsWith(kind, "openai")) "openai" else kind
    counts <- new.env(parent = emptyenv())
    path <- file.path(directory, paste0(kind, ".jsonl"))
    download <- switch(kind,
      openai_output = function() pairwiseLLM::openai_download_batch_output("b1", path, api_key = "fixture"),
      openai_errors = function() pairwiseLLM::openai_download_batch_errors("b1", path, api_key = "fixture"),
      anthropic = function() pairwiseLLM::anthropic_download_batch_results("b1", path, api_key = "fixture"),
      gemini = function() {
        pairwiseLLM::gemini_download_batch_results("batches/b1",
          tibble::tibble(custom_id = "c1"), path, api_key = "fixture")
      })
    download()
    expect_equal(counts$metadata, 2L)
    expect_true(file.exists(path))
    if (kind != "gemini") {
      expect_equal(counts$content, 2L)
      expect_identical(readBin(path, "raw", n = 1000), charToRaw(payload))
      writeLines("existing", path)
      counts <- new.env(parent = emptyenv())
      fail_content <- TRUE
      expect_error(download(), class = "pairwiseLLM_batch_retry_exhausted")
      expect_equal(counts$content, 3L)
      expect_identical(readLines(path), "existing")
      fail_content <- FALSE
    }
  }
})

test_that("multi-batch GET retries recover without nested budgets or scientific failures", {
  withr::local_seed(251)
  withr::local_envvar(c(OPENAI_API_KEY = "fixture", ANTHROPIC_API_KEY = "fixture", GEMINI_API_KEY = "fixture"))
  directory <- withr::local_tempdir()
  provider <- "openai"
  counts <- new.env(parent = emptyenv())
  # First metadata cycle and first download cycle exhaust. The next round succeeds.
  local_mocked_bindings(
    req_perform1 = function(req, ...) {
      expect_identical(req$method %||% "GET", "GET")
      key <- if (grepl("/content$|/results$", req$url)) "content" else "metadata"
      counts[[key]] <- (counts[[key]] %||% 0L) + 1L
      limit <- if (key == "metadata") 3L else if (provider == "openai") 2L else 3L
      if (counts[[key]] <= limit) return(batch_retry_response(503L))
      batch_retry_response(body = if (key == "metadata") batch_retry_metadata(provider) else "{}\n")
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  parsed <- tibble::tibble(custom_id = "c1", ID1 = "A", ID2 = "B", better_id = "A",
    result_type = "succeeded", error_message = NA_character_)
  local_mocked_bindings(
    parse_openai_batch_output = function(...) parsed,
    parse_anthropic_batch_output = function(...) parsed,
    parse_gemini_batch_output = function(...) parsed,
    .package = "pairwiseLLM"
  )
  for (p in c("openai", "anthropic", "gemini")) {
    provider <- p
    counts <- new.env(parent = emptyenv())
    job <- batch_retry_job(provider, directory)
    result <- pairwiseLLM::llm_resume_multi_batches(list(job), interval_seconds = 0, per_job_delay = 0,
      openai_max_retries = 2L, verbose = FALSE)
    expect_true(result$jobs[[1]]$done)
    expect_identical(result$jobs[[1]]$batch_id, job$batch_id)
    expect_equal(nrow(result$combined), 1L)
    expect_equal(nrow(result$jobs[[1]]$failed_attempts), 0L)
    expect_equal(nrow(result$batch_failures), 0L)
    if (p != "gemini") {
      expect_equal(counts$content, if (p == "openai") 3L else 4L)
      expect_equal(counts$metadata, 7L) # three failed + two polls + two download lookups
    } else {
      expect_equal(counts$metadata, 5L)
    }
  }
})

test_that("multi-batch runners immediately surface permanent retrieval and local errors", {
  withr::local_envvar(c(OPENAI_API_KEY = "fixture", ANTHROPIC_API_KEY = "fixture", GEMINI_API_KEY = "fixture"))
  directory <- withr::local_tempdir()
  calls <- 0L
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      batch_retry_response(401L)
    },
    sys_sleep = function(seconds, ...) {
      if (seconds > 0) stop("permanent errors must not sleep")
    }, .package = "httr2"
  )
  for (provider in c("openai", "anthropic", "gemini")) {
    calls <- 0L
    expect_error(pairwiseLLM::llm_resume_multi_batches(list(batch_retry_job(provider, directory)),
      interval_seconds = 0, per_job_delay = 0), class = "httr2_http_401")
    expect_equal(calls, 1L)
  }
})

test_that("download metadata exhaustion is deferred with the correct per-GET budget", {
  withr::local_seed(251)
  withr::local_envvar(c(OPENAI_API_KEY = "fixture", ANTHROPIC_API_KEY = "fixture", GEMINI_API_KEY = "fixture"))
  directory <- withr::local_tempdir()
  provider <- "openai"
  metadata_calls <- 0L
  limit <- 2L
  local_mocked_bindings(
    req_perform1 = function(req, ...) {
      if (grepl("/content$|/results$", req$url)) return(batch_retry_response(body = "{}\n"))
      metadata_calls <<- metadata_calls + 1L
      if (metadata_calls > 1L && metadata_calls <= limit + 1L) return(batch_retry_response(503L))
      batch_retry_response(body = batch_retry_metadata(provider))
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  parsed <- tibble::tibble(custom_id = "c1", ID1 = "A", ID2 = "B", better_id = "A",
    result_type = "succeeded", error_message = NA_character_)
  local_mocked_bindings(
    parse_openai_batch_output = function(...) parsed,
    parse_anthropic_batch_output = function(...) parsed,
    parse_gemini_batch_output = function(...) parsed, .package = "pairwiseLLM"
  )
  for (p in c("openai", "anthropic", "gemini")) {
    provider <- p
    metadata_calls <- 0L
    limit <- if (p == "openai") 2L else 3L
    result <- pairwiseLLM::llm_resume_multi_batches(list(batch_retry_job(p, directory)),
      interval_seconds = 0, per_job_delay = 0, openai_max_retries = 2L)
    expect_equal(metadata_calls, limit + 3L)
    expect_true(result$jobs[[1]]$done)
    expect_equal(nrow(result$failed_attempts), 0L)
  }
})

test_that("Anthropic and Gemini preserve soft timeout returns after HTTP recovery", {
  withr::local_seed(251)
  now <- as.POSIXct("2026-01-01", tz = "UTC")
  calls <- 0L
  local_mocked_bindings(
    req_perform1 = function(...) {
      calls <<- calls + 1L
      now <<- now + 10
      if (calls == 1L) batch_retry_response(503L) else batch_retry_response(body =
        '{"processing_status":"in_progress","metadata":{"state":"BATCH_STATE_RUNNING"}}')
    },
    sys_sleep = function(...) NULL, .package = "httr2"
  )
  local_mocked_bindings(.batch_now = function() now, .anthropic_now = function() now,
    .package = "pairwiseLLM")
  for (provider in c("anthropic", "gemini")) {
    calls <- 0L
    poll <- getExportedValue("pairwiseLLM", paste0(provider, "_poll_batch_until_complete"))
    result <- poll("batches/b1", api_key = "fixture", timeout_seconds = 1, verbose = FALSE)
    expect_equal(calls, 2L)
    expect_identical(result$processing_status, "in_progress")
  }
})
