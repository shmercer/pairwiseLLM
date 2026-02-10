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

.pairwiseLLM_retry_is_transient <- function(err,
                                            transient_status = c(408L, 429L, 500L, 502L, 503L, 504L)) {
  if (inherits(err, c("httr2_timeout", "curl_error_timeout"))) {
    return(TRUE)
  }
  http_classes <- class(err)
  http_code_class <- http_classes[grepl("^httr2_http_[0-9]{3}$", http_classes)]
  if (length(http_code_class) > 0L) {
    status <- suppressWarnings(as.integer(sub("^httr2_http_", "", http_code_class[1L])))
    return(!is.na(status) && status %in% transient_status)
  }
  if (inherits(err, "httr2_http")) {
    resp <- err$response %||% err$resp
    status <- tryCatch(
      .pairwiseLLM_resp_status(resp),
      error = function(...) NA_integer_
    )
    return(!is.na(status) && status %in% transient_status)
  }
  FALSE
}

.pairwiseLLM_retry_backoff <- function(fn,
                                       max_attempts = 3L,
                                       base_delay = 0.5,
                                       jitter = 0,
                                       transient_status = c(408L, 429L, 500L, 502L, 503L, 504L)) {
  if (max_attempts < 1L) {
    rlang::abort("`max_attempts` must be at least 1.")
  }
  if (!is.numeric(jitter) || length(jitter) != 1L || is.na(jitter) || jitter < 0) {
    rlang::abort("`jitter` must be a non-negative numeric scalar.")
  }

  attempt <- 1L
  failures <- list()

  add_delay <- function(delay) {
    if (jitter > 0) {
      delay <- delay + stats::runif(1L, min = 0, max = jitter)
    }
    delay
  }

  while (TRUE) {
    res_or_err <- tryCatch(fn(), error = function(e) e)
    if (!inherits(res_or_err, "error")) {
      return(list(result = res_or_err, retry_failures = dplyr::bind_rows(failures)))
    }

    is_transient <- .pairwiseLLM_retry_is_transient(res_or_err, transient_status = transient_status)
    if (!is_transient || attempt >= max_attempts) {
      failures <- append(
        failures,
        list(tibble::tibble(
          error_code = if (inherits(res_or_err, c("httr2_timeout", "curl_error_timeout"))) "timeout" else "http_error",
          error_detail = conditionMessage(res_or_err),
          attempted_at = Sys.time()
        ))
      )
      retry_failures <- dplyr::bind_rows(failures)
      exhausted <- is_transient && attempt >= max_attempts
      cnd <- rlang::error_cnd(
        message = conditionMessage(res_or_err),
        class = "pairwiseLLM_retry_error",
        retry_failures = retry_failures,
        retry_exhausted = exhausted,
        parent = res_or_err
      )
      attr(cnd, "retry_failures") <- retry_failures
      attr(cnd, "retry_exhausted") <- exhausted
      rlang::cnd_signal(cnd)
    }

    failures <- append(
      failures,
      list(tibble::tibble(
        error_code = if (inherits(res_or_err, c("httr2_timeout", "curl_error_timeout"))) "timeout" else "http_error",
        error_detail = conditionMessage(res_or_err),
        attempted_at = Sys.time()
      ))
    )
    delay <- base_delay * (2^(attempt - 1L))
    delay <- add_delay(delay)
    message(sprintf(
      "Transient error (attempt %d of %d); retrying in %.1f seconds...",
      attempt, max_attempts, delay
    ))
    Sys.sleep(delay)
    attempt <- attempt + 1L
  }
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
      failures <- append(
        failures,
        list(tibble::tibble(
          error_code = "http_error",
          error_detail = paste0("HTTP ", status),
          attempted_at = Sys.time()
        ))
      )
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
      resp_err <- err$response %||% err$resp
      status <- tryCatch(
        .pairwiseLLM_resp_status(resp_err),
        error = function(...) NA_integer_
      )

      if (!is.na(status) &&
          status %in% transient_status &&
          attempt < max_attempts) {
        delay <- base_delay * (2^(attempt - 1L))
        delay <- add_delay(delay)
        failures <- append(
          failures,
          list(tibble::tibble(
            error_code = "http_error",
            error_detail = paste0("HTTP ", status),
            attempted_at = Sys.time()
          ))
        )
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
        failures <- append(
          failures,
          list(tibble::tibble(
            error_code = "timeout",
            error_detail = conditionMessage(err),
            attempted_at = Sys.time()
          ))
        )
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
      resp_err <- err$response %||% err$resp
      status <- tryCatch(
        .pairwiseLLM_resp_status(resp_err),
        error = function(...) NA_integer_
      )
      if (!is.na(status) && !(status %in% transient_status)) {
        # Do not wrap non-transient HTTP errors; preserve original class
        attr(err, "retry_failures") <- dplyr::bind_rows(failures)
        stop(err)
      }
    }

    if (inherits(err, "httr2_timeout") || inherits(err, "curl_error_timeout")) {
      failures <- append(
        failures,
        list(tibble::tibble(
          error_code = "timeout",
          error_detail = conditionMessage(err),
          attempted_at = Sys.time()
        ))
      )
    } else {
      failures <- append(
        failures,
        list(tibble::tibble(
          error_code = "http_error",
          error_detail = conditionMessage(err),
          attempted_at = Sys.time()
        ))
      )
    }
    retry_failures_tbl <- dplyr::bind_rows(failures)
    cnd <- rlang::error_cnd(
      message = conditionMessage(err),
      class = "pairwiseLLM_retry_error",
      retry_failures = retry_failures_tbl
    )
    attr(cnd, "retry_failures") <- retry_failures_tbl
    rlang::cnd_signal(cnd)
  }
}
