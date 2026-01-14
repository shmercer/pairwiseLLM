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
