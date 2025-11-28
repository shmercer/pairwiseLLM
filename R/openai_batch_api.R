#' Internal: Null-coalescing helper
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Internal: Get OpenAI API key
#'
#' @keywords internal
.openai_api_key <- function(api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (is.null(api_key) || !nzchar(api_key)) {
    stop(
      "No OpenAI API key found. Please set OPENAI_API_KEY in your environment ",
      "or pass api_key explicitly.",
      call. = FALSE
    )
  }
  api_key
}

#' Internal: Base URL for OpenAI API
#'
#' @keywords internal
.openai_base_url <- function() {
  "https://api.openai.com/v1"
}

#' Internal: Create a httr2 request with auth
#'
#' @keywords internal
.openai_request <- function(path, api_key = Sys.getenv("OPENAI_API_KEY")) {
  api_key <- .openai_api_key(api_key)

  httr2::request(paste0(.openai_base_url(), path)) |>
    httr2::req_auth_bearer_token(api_key)
}

#' Upload a JSONL batch file to OpenAI
#'
#' Uploads a `.jsonl` file to the OpenAI Files API with purpose `"batch"`,
#' which can then be used to create a Batch job.
#'
#' @param path Path to the local `.jsonl` file to upload.
#' @param purpose File purpose. For the Batch API this should be `"batch"`.
#' @param api_key Optional OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.
#'
#' @return A list representing the File object returned by the API, including
#'   `id`, `filename`, `bytes`, `purpose`, etc.
#'
#' @examples
#' # Example without real API calls, using a mock request/response:
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   tf <- tempfile(fileext = ".jsonl")
#'   writeLines("{}", tf)
#'
#'   fake_file <- list(
#'     id       = "file_123",
#'     filename = basename(tf),
#'     purpose  = "batch"
#'   )
#'
#'   testthat::with_mocked_bindings(
#'     .openai_request       = function(path, api_key) structure(list(), class = "fake_req"),
#'     httr2::req_body_multipart = function(req, ...) req,
#'     httr2::req_perform    = function(req) list(),
#'     httr2::resp_body_json = function(resp, simplifyVector = TRUE) fake_file,
#'     {
#'       res <- openai_upload_batch_file(tf)
#'       res$id  # "file_123"
#'     }
#'   )
#' }
#'
#' \dontrun{
#' # Real usage (requires OPENAI_API_KEY in your env):
#' file_obj <- openai_upload_batch_file("batch_input.jsonl")
#' file_obj$id
#' }
#'
#' @export
openai_upload_batch_file <- function(
    path,
    purpose = "batch",
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  req <- .openai_request("/files", api_key) |>
    httr2::req_body_multipart(
      file    = httr2::req_file(path),
      purpose = purpose
    )

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Create an OpenAI batch from an uploaded file
#'
#' Creates and executes a batch based on a previously uploaded input file.
#'
#' @param input_file_id The ID of the uploaded file (with purpose `"batch"`).
#' @param endpoint The endpoint for the batch, e.g. `"/v1/chat/completions"` or
#'   `"/v1/responses"`.
#' @param completion_window Time frame in which the batch should be processed.
#'   Currently only `"24h"` is supported by the API.
#' @param metadata Optional named list of metadata key–value pairs.
#' @param api_key Optional OpenAI API key.
#'
#' @return A list representing the Batch object.
#'
#' @examples
#' # Mocked example – no real HTTP calls:
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   fake_batch <- list(
#'     id            = "batch_123",
#'     input_file_id = "file_123",
#'     status        = "queued"
#'   )
#'
#'   testthat::with_mocked_bindings(
#'     .openai_request       = function(path, api_key) structure(list(), class = "fake_req"),
#'     httr2::req_body_json  = function(req, body) req,
#'     httr2::req_perform    = function(req) list(),
#'     httr2::resp_body_json = function(resp, simplifyVector = TRUE) fake_batch,
#'     {
#'       res <- openai_create_batch("file_123", endpoint = "/v1/chat/completions")
#'       res$status  # "queued"
#'     }
#'   )
#' }
#'
#' \dontrun{
#' file_obj  <- openai_upload_batch_file("batch_input.jsonl")
#' batch_obj <- openai_create_batch(
#'   input_file_id = file_obj$id,
#'   endpoint      = "/v1/chat/completions"
#' )
#' batch_obj$id
#' }
#'
#' @export
openai_create_batch <- function(
    input_file_id,
    endpoint,
    completion_window = "24h",
    metadata = NULL,
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  body <- list(
    input_file_id     = input_file_id,
    endpoint          = endpoint,
    completion_window = completion_window
  )

  if (!is.null(metadata)) {
    body$metadata <- metadata
  }

  req <- .openai_request("/batches", api_key) |>
    httr2::req_body_json(body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Retrieve an OpenAI batch
#'
#' @param batch_id The batch ID (e.g. `"batch_abc123"`).
#' @param api_key Optional OpenAI API key.
#'
#' @return A list representing the Batch object.
#'
#' @examples
#' # Mocked example:
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   fake_batch <- list(id = "batch_123", status = "completed")
#'
#'   testthat::with_mocked_bindings(
#'     .openai_request       = function(path, api_key) structure(list(), class = "fake_req"),
#'     httr2::req_perform    = function(req) list(),
#'     httr2::resp_body_json = function(resp, simplifyVector = TRUE) fake_batch,
#'     {
#'       res <- openai_get_batch("batch_123")
#'       res$status  # "completed"
#'     }
#'   )
#' }
#'
#' \dontrun{
#' batch <- openai_get_batch("batch_abc123")
#' batch$status
#' }
#'
#' @export
openai_get_batch <- function(
    batch_id,
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  path <- paste0("/batches/", batch_id)

  req  <- .openai_request(path, api_key)
  resp <- httr2::req_perform(req)

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Download the output file for a completed batch
#'
#' Given a batch ID, retrieves the batch metadata, extracts the `output_file_id`,
#' and downloads the corresponding file content to `path`.
#'
#' @param batch_id The batch ID (e.g. `"batch_abc123"`).
#' @param path Local file path to write the downloaded `.jsonl` output.
#' @param api_key Optional OpenAI API key.
#'
#' @return Invisibly, the path to the downloaded file.
#'
#' @examples
#' # Mocked example:
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   fake_batch <- list(
#'     id             = "batch_123",
#'     status         = "completed",
#'     output_file_id = "file_out_123"
#'   )
#'
#'   testthat::with_mocked_bindings(
#'     openai_get_batch  = function(batch_id, api_key) fake_batch,
#'     .openai_request   = function(path, api_key) structure(list(), class = "fake_req"),
#'     httr2::req_perform = function(req) list(),
#'     httr2::resp_body_raw = function(resp) charToRaw('{"dummy": true}\n'),
#'     {
#'       tf <- tempfile(fileext = ".jsonl")
#'       openai_download_batch_output("batch_123", tf)
#'       readLines(tf)
#'     }
#'   )
#' }
#'
#' \dontrun{
#' openai_download_batch_output("batch_abc123", "batch_output.jsonl")
#' res <- parse_openai_batch_output("batch_output.jsonl")
#' }
#'
#' @export
openai_download_batch_output <- function(
    batch_id,
    path,
    api_key = Sys.getenv("OPENAI_API_KEY")
) {
  batch <- openai_get_batch(batch_id, api_key = api_key)

  output_file_id <- batch$output_file_id %||% NULL
  if (is.null(output_file_id) || !nzchar(output_file_id)) {
    stop(
      "Batch ", batch_id, " has no output_file_id. ",
      "Status is: ", batch$status %||% "unknown",
      call. = FALSE
    )
  }

  file_path <- paste0("/files/", output_file_id, "/content")

  req  <- .openai_request(file_path, api_key)
  resp <- httr2::req_perform(req)

  raw <- httr2::resp_body_raw(resp)
  writeBin(raw, path)

  invisible(path)
}

#' Poll an OpenAI batch until it completes or fails
#'
#' Repeatedly calls [openai_get_batch()] until the batch reaches a terminal
#' status (one of `"completed"`, `"failed"`, `"cancelled"`, `"expired"`),
#' a timeout is reached, or `max_attempts` is exceeded.
#'
#' This is a synchronous helper – it will block until one of the conditions
#' above is met.
#'
#' @param batch_id The batch ID.
#' @param interval_seconds Number of seconds to wait between polling attempts.
#' @param timeout_seconds Maximum total time to wait in seconds before giving up.
#' @param max_attempts Maximum number of polling attempts. This is mainly useful
#'   for testing; default is `Inf`.
#' @param api_key Optional OpenAI API key.
#' @param verbose Logical; if `TRUE`, prints status messages to the console.
#'
#' @return The final Batch object (a list) as returned by [openai_get_batch()].
#'
#' @examples
#' # Mocked example: simulate a batch going from "in_progress" to "completed"
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   fake_batches <- list(
#'     list(id = "batch_123", status = "in_progress"),
#'     list(id = "batch_123", status = "completed", output_file_id = "file_out_123")
#'   )
#'   i <- 0L
#'
#'   testthat::with_mocked_bindings(
#'     openai_get_batch = function(batch_id, api_key) {
#'       i <<- i + 1L
#'       fake_batches[[i]]
#'     },
#'     {
#'       res <- openai_poll_batch_until_complete(
#'         batch_id         = "batch_123",
#'         interval_seconds = 0,
#'         timeout_seconds  = 10,
#'         max_attempts     = 5,
#'         verbose          = FALSE
#'       )
#'       res$status  # "completed"
#'     }
#'   )
#' }
#'
#' \dontrun{
#' # Real usage:
#' batch <- openai_create_batch("file_123", endpoint = "/v1/chat/completions")
#' final <- openai_poll_batch_until_complete(
#'   batch_id         = batch$id,
#'   interval_seconds = 10,
#'   timeout_seconds  = 3600
#' )
#' final$status
#' }
#'
#' @export
openai_poll_batch_until_complete <- function(
    batch_id,
    interval_seconds = 5,
    timeout_seconds = 600,
    max_attempts = Inf,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    verbose = TRUE
) {
  start_time <- Sys.time()
  attempts   <- 0L

  repeat {
    attempts <- attempts + 1L

    batch  <- openai_get_batch(batch_id, api_key = api_key)
    status <- batch$status %||% "unknown"

    if (verbose) {
      message("Batch ", batch_id, " status: ", status,
              " (attempt ", attempts, ")")
    }

    if (status %in% c("completed", "failed", "cancelled", "expired")) {
      return(batch)
    }

    # Check attempts limit (useful in tests)
    if (attempts >= max_attempts) {
      stop(
        "Reached max_attempts (", max_attempts, ") while polling batch ",
        batch_id, ". Last status: ", status,
        call. = FALSE
      )
    }

    # Check time-based timeout
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed > timeout_seconds) {
      stop(
        "Timeout (", timeout_seconds, " seconds) waiting for batch ",
        batch_id, " to complete. Last status: ", status,
        call. = FALSE
      )
    }

    Sys.sleep(interval_seconds)
  }
}
