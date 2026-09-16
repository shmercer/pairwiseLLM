# Retry only idempotent batch retrievals. Submission/live retry policies are
# deliberately separate: replaying those requests can create additional work.

#' @keywords internal
#' @noRd
.batch_now <- function() Sys.time()

#' @keywords internal
#' @noRd
.batch_retry_backoff <- function(attempt) {
  min(30, 0.5 * 2^(attempt - 1L) + stats::runif(1L, 0, 0.25))
}

#' @keywords internal
#' @noRd
.batch_retry_after <- function(resp) {
  value <- httr2::resp_header(resp, "Retry-After")
  if (is.null(value)) return(NA_real_)
  value <- trimws(value)
  if (grepl("^[0-9]+$", value)) {
    delay <- as.numeric(value)
  } else {
    retry_at <- curl::parse_date(value)
    server_date <- httr2::resp_header(resp, "Date")
    now <- if (is.null(server_date)) NA else curl::parse_date(server_date)
    if (is.na(now)) now <- .batch_now()
    delay <- as.numeric(difftime(retry_at, now, units = "secs"))
  }
  if (!is.finite(delay)) return(NA_real_)
  max(0, delay)
}

#' @keywords internal
#' @noRd
.batch_retry_status <- function(status) {
  status %in% c(408L, 429L, 500L:599L)
}

#' @keywords internal
#' @noRd
.batch_req_perform <- function(req, max_attempts = 3L) {
  if (!identical(req$method %||% "GET", "GET") || !is.null(req$body)) {
    rlang::abort("Batch retrieval retries require a GET request without a body.")
  }
  req <- httr2::req_retry(
    req,
    max_tries = max_attempts,
    retry_on_failure = TRUE,
    is_transient = function(resp) .batch_retry_status(httr2::resp_status(resp)),
    backoff = .batch_retry_backoff,
    after = .batch_retry_after
  )
  tryCatch(
    httr2::req_perform(req),
    error = function(err) {
      status <- if (inherits(err, "httr2_http")) httr2::resp_status(err$resp) else NA_integer_
      if (inherits(err, "httr2_failure") || .batch_retry_status(status)) {
        class(err) <- c("pairwiseLLM_batch_retry_exhausted", class(err))
      }
      stop(err)
    }
  )
}

#' @keywords internal
#' @noRd
.batch_retrieval_try <- function(expr, provider, batch_id, action, verbose) {
  tryCatch(
    expr,
    pairwiseLLM_batch_retry_exhausted = function(err) {
      if (isTRUE(verbose)) {
        message(sprintf(
          "[llm_resume_multi_batches] %s %s batch %s after HTTP retries: %s; will retry in next round.",
          action, provider, batch_id, conditionMessage(err)
        ))
      }
      NULL
    }
  )
}
