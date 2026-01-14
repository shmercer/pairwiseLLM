# -------------------------------------------------------------------------
# Internal helper: retry httr2 requests with exponential backoff
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.pairwiseLLM_req_perform <- function(req) {
  httr2::req_perform(req)
}

#' @keywords internal
#' @noRd
.pairwiseLLM_resp_status <- function(resp) {
  httr2::resp_status(resp)
}

.retry_httr2_request <- function(req,
                                 max_attempts = 3L,
                                 base_delay = 0.5,
                                 jitter = 0) {
  if (max_attempts < 1L) {
    rlang::abort("`max_attempts` must be at least 1.")
  }

  transient_status <- c(408L, 429L, 500L, 502L, 503L, 504L)

  attempt <- 1L
  failures <- list()

  record_failure <- function(error_code, error_detail) {
    failures[[length(failures) + 1L]] <<- tibble::tibble(
      error_code = error_code,
      error_detail = error_detail,
      attempted_at = Sys.time()
    )
  }

  add_delay <- function(delay) {
    if (!is.numeric(jitter) || length(jitter) != 1L || is.na(jitter) || jitter < 0) {
      rlang::abort("`jitter` must be a non-negative numeric scalar.")
    }
    if (jitter > 0) {
      delay <- delay + stats::runif(1L, min = 0, max = jitter)
    }
    delay
  }

  while (TRUE) {
    resp_or_err <- tryCatch(
      .pairwiseLLM_req_perform(req),
      error = function(e) e
    )

    # SUCCESS PATH
    if (!inherits(resp_or_err, "error")) {
      status <- .pairwiseLLM_resp_status(resp_or_err)

      if (!(status %in% transient_status) || attempt >= max_attempts) {
        attr(resp_or_err, "retry_failures") <- dplyr::bind_rows(failures)
        return(resp_or_err)
      }

      delay <- base_delay * (2^(attempt - 1L))
      delay <- add_delay(delay)
      record_failure("http_error", paste0("HTTP ", status))
      message(sprintf(
        "Transient HTTP %d from API (attempt %d of %d); retrying in %.1f seconds...",
        status, attempt, max_attempts, delay
      ))
      Sys.sleep(delay)
      attempt <- attempt + 1L
      next
    }

    # ERROR PATH
    err <- resp_or_err

    if (inherits(err, "httr2_http")) {
      resp_err <- err$response
      status <- tryCatch(
        .pairwiseLLM_resp_status(resp_err),
        error = function(...) NA_integer_
      )

      if (!is.na(status) &&
          status %in% transient_status &&
          attempt < max_attempts) {
        delay <- base_delay * (2^(attempt - 1L))
        delay <- add_delay(delay)
        record_failure("http_error", paste0("HTTP ", status))
        message(sprintf(
          "Transient HTTP %d from API (attempt %d of %d); retrying in %.1f seconds...",
          status, attempt, max_attempts, delay
        ))
        Sys.sleep(delay)
        attempt <- attempt + 1L
        next
      }
    }

    if (inherits(err, "httr2_timeout") || inherits(err, "curl_error_timeout")) {
      if (attempt < max_attempts) {
        delay <- base_delay * (2^(attempt - 1L))
        delay <- add_delay(delay)
        record_failure("timeout", conditionMessage(err))
        message(sprintf(
          "Timeout from API (attempt %d of %d); retrying in %.1f seconds...",
          attempt, max_attempts, delay
        ))
        Sys.sleep(delay)
        attempt <- attempt + 1L
        next
      }
    }

    # Non-transient or exhausted attempts
    if (inherits(err, "httr2_http")) {
      resp_err <- err$response
      status <- tryCatch(
        .pairwiseLLM_resp_status(resp_err),
        error = function(...) NA_integer_
      )
      if (!is.na(status) && !(status %in% transient_status)) {
        # Do not wrap non-transient HTTP errors; preserve original class
        stop(err)
      }
    }

    if (inherits(err, "httr2_timeout") || inherits(err, "curl_error_timeout")) {
      record_failure("timeout", conditionMessage(err))
    } else {
      record_failure("http_error", conditionMessage(err))
    }
    rlang::abort(
      conditionMessage(err),
      class = "pairwiseLLM_retry_error",
      retry_failures = dplyr::bind_rows(failures)
    )
  }
}
