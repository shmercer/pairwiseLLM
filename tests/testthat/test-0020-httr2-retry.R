.retry_httr2_request <- pairwiseLLM:::.retry_httr2_request
.pairwiseLLM_retry_backoff <- pairwiseLLM:::.pairwiseLLM_retry_backoff
.pairwiseLLM_retry_is_transient <- pairwiseLLM:::.pairwiseLLM_retry_is_transient
.pairwiseLLM_req_perform <- pairwiseLLM:::.pairwiseLLM_req_perform
.pairwiseLLM_resp_status <- pairwiseLLM:::.pairwiseLLM_resp_status

test_that("retry helper retries transient status codes and records failures", {
  call_count <- 0L
  fake_resp <- function(status) {
    structure(list(status = status), class = "httr2_response")
  }

  with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) {
      call_count <<- call_count + 1L
      if (call_count < 3L) {
        return(fake_resp(500L))
      }
      fake_resp(200L)
    },
    `.pairwiseLLM_resp_status` = function(resp) resp$status,
    {
      res <- .retry_httr2_request(list(), max_attempts = 3L, base_delay = 0, jitter = 0)
      failures <- attr(res, "retry_failures")
      expect_equal(call_count, 3L)
      expect_s3_class(res, "httr2_response")
      expect_equal(nrow(failures), 2L)
      expect_true(all(failures$error_code == "http_error"))
    }
  )
})

test_that("retry helper retries timeouts and surfaces retry failures on abort", {
  call_count <- 0L
  timeout_err <- structure(
    list(message = "Timeout"),
    class = c("httr2_timeout", "error", "condition")
  )

  with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) {
      call_count <<- call_count + 1L
      stop(timeout_err)
    },
    {
      err <- tryCatch(
        .retry_httr2_request(list(), max_attempts = 2L, base_delay = 0, jitter = 0),
        error = function(e) e
      )
      expect_s3_class(err, "pairwiseLLM_retry_error")
      expect_equal(call_count, 2L)
      failures <- attr(err, "retry_failures")
      expect_equal(nrow(failures), 2L)
      expect_true(all(failures$error_code == "timeout"))
    }
  )
})

test_that("retry backoff retries transient errors and returns failures", {
  call_count <- 0L
  transient_err <- structure(
    list(message = "HTTP 500"),
    class = c("httr2_http_500", "error", "condition")
  )

  out <- .pairwiseLLM_retry_backoff(
    fn = function() {
      call_count <<- call_count + 1L
      if (call_count < 3L) {
        stop(transient_err)
      }
      "ok"
    },
    max_attempts = 3L,
    base_delay = 0,
    jitter = 0
  )

  expect_equal(out$result, "ok")
  expect_equal(call_count, 3L)
  expect_equal(nrow(out$retry_failures), 2L)
})

test_that("retry backoff marks exhausted transient errors", {
  call_count <- 0L
  transient_err <- structure(
    list(message = "HTTP 500"),
    class = c("httr2_http_500", "error", "condition")
  )

  err <- tryCatch(
    .pairwiseLLM_retry_backoff(
      fn = function() {
        call_count <<- call_count + 1L
        stop(transient_err)
      },
      max_attempts = 2L,
      base_delay = 0,
      jitter = 0
    ),
    error = function(e) e
  )

  expect_true(isTRUE(attr(err, "retry_exhausted")))
  expect_equal(call_count, 2L)
  failures <- attr(err, "retry_failures")
  expect_equal(nrow(failures), 2L)
})

test_that("retry backoff aborts on non-transient errors", {
  err <- tryCatch(
    .pairwiseLLM_retry_backoff(
      fn = function() stop("no retry"),
      max_attempts = 3L,
      base_delay = 0,
      jitter = 0
    ),
    error = function(e) e
  )

  expect_false(isTRUE(attr(err, "retry_exhausted")))
  failures <- attr(err, "retry_failures")
  expect_equal(nrow(failures), 1L)
})

test_that("retry helper classifies transient and non-transient errors", {
  timeout_err <- structure(list(message = "timeout"), class = c("httr2_timeout", "error", "condition"))
  http_429 <- structure(list(), class = c("httr2_http_429", "error", "condition"))
  http_404 <- structure(list(), class = c("httr2_http_404", "error", "condition"))

  expect_true(.pairwiseLLM_retry_is_transient(timeout_err))
  expect_true(.pairwiseLLM_retry_is_transient(http_429))
  expect_false(.pairwiseLLM_retry_is_transient(http_404))
})

test_that("retry helper uses httr2_http response status when present", {
  http_err <- structure(list(response = list()), class = c("httr2_http", "error", "condition"))

  with_mocked_bindings(
    `.pairwiseLLM_resp_status` = function(resp) 408L,
    {
      expect_true(.pairwiseLLM_retry_is_transient(http_err))
    }
  )
})

test_that("retry backoff validates jitter and max_attempts", {
  expect_error(
    .pairwiseLLM_retry_backoff(function() "ok", max_attempts = 0L),
    "max_attempts"
  )
  expect_error(
    .pairwiseLLM_retry_backoff(function() "ok", jitter = -1),
    "jitter"
  )
})

test_that("retry backoff applies jitter when configured", {
  call_count <- 0L
  transient_err <- structure(
    list(message = "HTTP 500"),
    class = c("httr2_http_500", "error", "condition")
  )

  withr::local_seed(123)
  out <- .pairwiseLLM_retry_backoff(
    fn = function() {
      call_count <<- call_count + 1L
      if (call_count < 2L) {
        stop(transient_err)
      }
      "ok"
    },
    max_attempts = 2L,
    base_delay = 0,
    jitter = 0.001
  )

  expect_equal(out$result, "ok")
  expect_equal(call_count, 2L)
})

test_that("retry request validates jitter and rethrows non-transient http errors", {
  http_err <- structure(list(response = list()), class = c("httr2_http", "error", "condition"))
  fake_req <- structure(list(), class = "httr2_request")

  with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) stop(http_err),
    `.pairwiseLLM_resp_status` = function(resp) 400L,
    {
      err <- tryCatch(.retry_httr2_request(fake_req, max_attempts = 1L, jitter = 0), error = function(e) e)
      expect_s3_class(err, "httr2_http")
    }
  )

  with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) structure(list(status = 500L), class = "httr2_response"),
    `.pairwiseLLM_resp_status` = function(resp) resp$status,
    {
      expect_error(
        .retry_httr2_request(fake_req, max_attempts = 2L, jitter = -0.5, base_delay = 0),
        "jitter"
      )
    }
  )
})

test_that("retry request validates max_attempts and retries transient http errors", {
  expect_error(
    .retry_httr2_request(list(), max_attempts = 0L),
    "max_attempts"
  )

  call_count <- 0L
  http_err <- structure(
    list(response = list(status = 503L)),
    class = c("httr2_http", "error", "condition")
  )
  fake_resp <- function(status) {
    structure(list(status = status), class = "httr2_response")
  }

  withr::local_seed(123)
  res <- with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        stop(http_err)
      }
      fake_resp(200L)
    },
    `.pairwiseLLM_resp_status` = function(resp) resp$status,
    {
      .retry_httr2_request(list(), max_attempts = 2L, base_delay = 0, jitter = 0.001)
    }
  )

  expect_equal(call_count, 2L)
  expect_equal(attr(res, "retry_failures")$error_code, "http_error")
})

test_that("retry request retries httr2_http errors when status is stored in `resp`", {
  call_count <- 0L
  http_err <- structure(
    list(resp = list(status = 503L)),
    class = c("httr2_http", "error", "condition")
  )
  fake_resp <- function(status) {
    structure(list(status = status), class = "httr2_response")
  }

  res <- with_mocked_bindings(
    `.pairwiseLLM_req_perform` = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        stop(http_err)
      }
      fake_resp(200L)
    },
    `.pairwiseLLM_resp_status` = function(resp) resp$status,
    {
      .retry_httr2_request(list(), max_attempts = 2L, base_delay = 0, jitter = 0)
    }
  )

  expect_equal(call_count, 2L)
  failures <- attr(res, "retry_failures")
  expect_equal(nrow(failures), 1L)
  expect_true(all(failures$error_code == "http_error"))
})
