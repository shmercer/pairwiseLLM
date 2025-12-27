# -------------------------------------------------------------------------
# Internal helper: retry httr2 requests with exponential backoff
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.retry_httr2_request <- function(req,
                                 max_attempts = 3L,
                                 base_delay = 0.5) {
  stopifnot(max_attempts >= 1L)

  transient_status <- c(408L, 429L, 500L, 502L, 503L, 504L)

  attempt <- 1L
  while (TRUE) {
    resp_or_err <- tryCatch(
      httr2::req_perform(req),
      error = function(e) e
    )

    # SUCCESS PATH
    if (!inherits(resp_or_err, "error")) {
      status <- httr2::resp_status(resp_or_err)

      if (!(status %in% transient_status) || attempt >= max_attempts) {
        return(resp_or_err)
      }

      delay <- base_delay * (2^(attempt - 1L))
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
        httr2::resp_status(resp_err),
        error = function(...) NA_integer_
      )

      if (!is.na(status) &&
        status %in% transient_status &&
        attempt < max_attempts) {
        delay <- base_delay * (2^(attempt - 1L))
        message(sprintf(
          "Transient HTTP %d from API (attempt %d of %d); retrying in %.1f seconds...",
          status, attempt, max_attempts, delay
        ))
        Sys.sleep(delay)
        attempt <- attempt + 1L
        next
      }
    }

    # Non-transient or exhausted attempts
    stop(err)
  }
}
