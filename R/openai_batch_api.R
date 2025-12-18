#' @importFrom httr2 request req_auth_bearer_token req_body_multipart
#' @importFrom httr2 req_body_json req_perform resp_body_json resp_body_raw
#' @importFrom curl form_file
NULL

# -------------------------------------------------------------------------
# Internal OpenAI helpers
# -------------------------------------------------------------------------

#' Internal: Get OpenAI API key
#'
#' @keywords internal
#' @noRd
.openai_api_key <- function(api_key = NULL) {
  .get_api_key(
    api_key = api_key,
    env_var = "OPENAI_API_KEY",
    service = "OpenAI"
  )
}

#' @keywords internal
#' @noRd
.openai_base_url <- function() {
  # Adjust if you support Azure or other endpoints elsewhere
  "https://api.openai.com/v1"
}

#' Internal: Create a httr2 request with auth
#'
#' @keywords internal
#' @noRd
.openai_request <- function(path, api_key = NULL) {
  api_key <- .openai_api_key(api_key)

  httr2::request(paste0(.openai_base_url(), path)) |>
    httr2::req_auth_bearer_token(api_key)
}


#' Internal: Attach JSON body to an OpenAI request
#'
#' @keywords internal
#' @noRd
.openai_req_body_json <- function(req, body, ...) {
  httr2::req_body_json(req, body)
}

#' Internal: Perform an OpenAI request with retry on transient errors
#'
#' @keywords internal
#' @noRd
.openai_req_perform <- function(req) {
  .retry_httr2_request(req)
}

#' Internal: Parse JSON body from an OpenAI response
#'
#' @keywords internal
#' @noRd
.openai_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

#' Internal: Extract HTTP status from an OpenAI response
#'
#' @keywords internal
#' @noRd
.openai_resp_status <- function(resp) {
  httr2::resp_status(resp)
}


#' Upload a JSONL batch file to OpenAI
#'
#' Uploads a `.jsonl` file to the OpenAI Files API with purpose `"batch"`,
#' which can then be used to create a Batch job.
#'
#' @param path Path to the local `.jsonl` file to upload.
#' @param purpose File purpose. For the Batch API this should be `"batch"`.
#' @param api_key Optional OpenAI API key. Defaults to
#'   `Sys.getenv("OPENAI_API_KEY")`.
#'
#' @return A list representing the File object returned by the API, including
#'   `id`, `filename`, `bytes`, `purpose`, etc.
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY set in your environment and network access
#'
#' file_obj <- openai_upload_batch_file("batch_input.jsonl")
#' file_obj$id
#' }
#'
#' @export
openai_upload_batch_file <- function(
  path,
  purpose = "batch",
  api_key = NULL
) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  api_key <- .openai_api_key(api_key)

  req <- .openai_request("/files", api_key) |>
    req_body_multipart(
      file    = form_file(path),
      purpose = purpose
    )

  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = TRUE)
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
#' \dontrun{
#' # Requires OPENAI_API_KEY set in your environment and network access.
#'
#' file_obj <- openai_upload_batch_file("batch_input.jsonl")
#'
#' batch_obj <- openai_create_batch(
#'   input_file_id = file_obj$id,
#'   endpoint      = "/v1/chat/completions"
#' )
#'
#' batch_obj$status
#' }
#'
#' @export
openai_create_batch <- function(
  input_file_id,
  endpoint,
  completion_window = "24h",
  metadata = NULL,
  api_key = NULL
) {
  body <- list(
    input_file_id     = input_file_id,
    endpoint          = endpoint,
    completion_window = completion_window
  )

  if (!is.null(metadata)) {
    body$metadata <- metadata
  }

  api_key <- .openai_api_key(api_key)

  req <- .openai_request("/batches", api_key) |>
    req_body_json(body)

  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = TRUE)
}

#' Retrieve an OpenAI batch
#'
#' @param batch_id The batch ID (e.g. `"batch_abc123"`).
#' @param api_key Optional OpenAI API key.
#'
#' @return A list representing the Batch object.
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and an existing batch ID.
#'
#' batch <- openai_get_batch("batch_abc123")
#' batch$status
#' }
#'
#' @export
openai_get_batch <- function(
  batch_id,
  api_key = NULL
) {
  path <- paste0("/batches/", batch_id)

  api_key <- .openai_api_key(api_key)

  req <- .openai_request(path, api_key)
  resp <- req_perform(req)

  resp_body_json(resp, simplifyVector = TRUE)
}

#' Download the output file for a completed batch
#'
#' Given a batch ID, retrieves the batch metadata, extracts the
#' `output_file_id`, and downloads the corresponding file content to `path`.
#'
#' @param batch_id The batch ID (e.g. `"batch_abc123"`).
#' @param path Local file path to write the downloaded `.jsonl` output.
#' @param api_key Optional OpenAI API key.
#'
#' @return Invisibly, the path to the downloaded file.
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and a completed batch with an output_file_id.
#'
#' openai_download_batch_output("batch_abc123", "batch_output.jsonl")
#'
#' # You can then parse the file
#' res <- parse_openai_batch_output("batch_output.jsonl")
#' head(res)
#' }
#'
#' @export
openai_download_batch_output <- function(
  batch_id,
  path,
  api_key = NULL
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

  api_key <- .openai_api_key(api_key)

  req <- .openai_request(file_path, api_key)
  resp <- req_perform(req)

  raw <- resp_body_raw(resp)
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
#' @param timeout_seconds Maximum total time to wait in seconds before
#'   giving up.
#' @param max_attempts Maximum number of polling attempts. This is mainly useful
#'   for testing; default is `Inf`.
#' @param api_key Optional OpenAI API key.
#' @param verbose Logical; if `TRUE`, prints status messages to the console.
#'
#' @return The final Batch object (a list) as returned by [openai_get_batch()].
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and a created batch that may still be running.
#'
#' batch <- openai_create_batch("file_123", endpoint = "/v1/chat/completions")
#'
#' final <- openai_poll_batch_until_complete(
#'   batch_id         = batch$id,
#'   interval_seconds = 10,
#'   timeout_seconds  = 3600
#' )
#'
#' final$status
#' }
#'
#' @export
openai_poll_batch_until_complete <- function(
    batch_id,
    interval_seconds = 5,
    timeout_seconds = 600,
    max_attempts = Inf,
    api_key = NULL,
    verbose = TRUE
) {
  start_time <- Sys.time()
  attempts <- 0L

  repeat {
    attempts <- attempts + 1L

    batch <- openai_get_batch(batch_id, api_key = api_key)
    status <- batch$status %||% "unknown"

    if (verbose) {
      message(
        "Batch ", batch_id, " status: ", status,
        " (attempt ", attempts, ")"
      )
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


#' Run a full OpenAI batch pipeline for pairwise comparisons
#'
#' This helper wires together the existing pieces:
#' - [build_openai_batch_requests()]
#' - [write_openai_batch_file()]
#' - [openai_upload_batch_file()]
#' - [openai_create_batch()]
#' - optionally [openai_poll_batch_until_complete()]
#' - optionally [openai_download_batch_output()]
#' - optionally [parse_openai_batch_output()]
#'
#' It is a convenience wrapper around these smaller functions and is intended
#' for end-to-end batch runs on a set of pairwise comparisons. For more control
#' (or testing), you can call the components directly.
#'
#' When \code{endpoint} is not specified, it is chosen automatically:
#' \itemize{
#'   \item if \code{include_thoughts = TRUE}, the \code{"responses"} endpoint
#'         is used and, for \code{"gpt-5.1"}, a default reasoning effort of
#'         \code{"low"} is applied (unless overridden via \code{reasoning}).
#'   \item otherwise, \code{"chat.completions"} is used.
#' }
#'
#' @param pairs Tibble of pairs with at least `ID1`, `text1`, `ID2`, `text2`.
#'   Typically produced by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model OpenAI model name (e.g. `"gpt-4.1"`, `"gpt-5.1"`).
#' @param trait_name Trait name to pass to [build_openai_batch_requests()].
#' @param trait_description Trait description to pass to
#'   [build_openai_batch_requests()].
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param include_thoughts Logical; if `TRUE` and using
#'   `endpoint = "responses"`, requests reasoning-style summaries to populate
#'   the `thoughts` column in the parsed output. When `endpoint` is not
#'   supplied, `include_thoughts = TRUE` causes the `responses` endpoint to
#'   be selected automatically.
#' @param include_raw Logical; if `TRUE`, attaches the raw model response as a
#'   list-column `raw_response` in the parsed results.
#' @param endpoint One of `"chat.completions"` or `"responses"`. If `NULL` (or
#'   omitted), it is chosen automatically as described above.
#' @param batch_input_path Path to write the batch input `.jsonl` file. Defaults
#'   to a temporary file.
#' @param batch_output_path Path to write the batch output `.jsonl` file if
#'   `poll = TRUE`. Defaults to a temporary file.
#' @param poll Logical; if `TRUE`, the function will poll the batch until it
#'   reaches a terminal status using [openai_poll_batch_until_complete()] and
#'   then download and parse the output. If `FALSE`, it stops after creating
#'   the batch and returns without polling or parsing.
#' @param interval_seconds Polling interval in seconds
#'   (used when `poll = TRUE`).
#' @param timeout_seconds Maximum total time in seconds for polling before
#'   giving up (used when `poll = TRUE`).
#' @param max_attempts Maximum number of polling attempts (primarily useful for
#'   testing).
#' @param metadata Optional named list of metadata key–value pairs to pass to
#'   [openai_create_batch()].
#' @param api_key Optional OpenAI API key. Defaults to
#'   `Sys.getenv("OPENAI_API_KEY")`.
#' @param ... Additional arguments passed through to
#'   [build_openai_batch_requests()], e.g. `temperature`, `top_p`, `logprobs`,
#'   `reasoning`.
#'
#' @return A list with elements:
#' * `batch_input_path`  – path to the input `.jsonl` file.
#' * `batch_output_path` – path to the output `.jsonl` file (or `NULL` if
#'   `poll = FALSE`).
#' * `file`              – File object returned by [openai_upload_batch_file()].
#' * `batch`             – Batch object; if `poll = TRUE`, this is the final
#'   batch after polling, otherwise the initial batch returned by
#'   [openai_create_batch()].
#' * `results`           – Parsed tibble from [parse_openai_batch_output()] if
#'   `poll = TRUE`, otherwise `NULL`.
#'
#' @examples
#' # The OpenAI batch pipeline requires:
#' # - Internet access
#' # - A valid OpenAI API key in OPENAI_API_KEY (or supplied via `api_key`)
#' # - Billable API usage
#' #
#' \dontrun{
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 2, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Run a small batch using chat.completions
#' out <- run_openai_batch_pipeline(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   poll              = TRUE,
#'   interval_seconds  = 5,
#'   timeout_seconds   = 600
#' )
#'
#' print(out$batch$status)
#' print(utils::head(out$results))
#' }
#'
#' @export
run_openai_batch_pipeline <- function(
    pairs,
    model,
    trait_name,
    trait_description,
    prompt_template = set_prompt_template(),
    include_thoughts = FALSE,
    include_raw = FALSE,
    endpoint = NULL,
    batch_input_path = tempfile("openai_batch_input_", fileext = ".jsonl"),
    batch_output_path = tempfile("openai_batch_output_", fileext = ".jsonl"),
    poll = TRUE,
    interval_seconds = 5,
    timeout_seconds = 600,
    max_attempts = Inf,
    metadata = NULL,
    api_key = NULL,
    ...
) {
  # If endpoint not supplied, choose automatically based on include_thoughts
  if (is.null(endpoint)) {
    endpoint <- if (isTRUE(include_thoughts)) "responses" else "chat.completions"
  }
  endpoint <- match.arg(endpoint, c("chat.completions", "responses"))

  # Endpoint for the Batch API expects the full path
  batch_api_endpoint <- switch(endpoint,
                               "chat.completions" = "/v1/chat/completions",
                               "responses"        = "/v1/responses"
  )

  # 1) Build batch requests tibble
  batch_tbl <- build_openai_batch_requests(
    pairs             = pairs,
    model             = model,
    trait_name        = trait_name,
    trait_description = trait_description,
    prompt_template   = prompt_template,
    include_thoughts  = include_thoughts,
    endpoint          = endpoint,
    ...
  )

  # 2) Write JSONL input file
  write_openai_batch_file(batch_tbl, batch_input_path)

  file_obj <- openai_upload_batch_file(
    path    = batch_input_path,
    api_key = api_key
  )

  # 4) Create batch
  batch_obj <- openai_create_batch(
    input_file_id     = file_obj$id,
    endpoint          = batch_api_endpoint,
    completion_window = "24h",
    metadata          = metadata,
    api_key           = api_key
  )

  final_batch <- batch_obj
  results <- NULL
  out_path <- NULL

  # 5) Optional polling + download + parse
  if (isTRUE(poll)) {
    final_batch <- openai_poll_batch_until_complete(
      batch_id         = batch_obj$id,
      interval_seconds = interval_seconds,
      timeout_seconds  = timeout_seconds,
      max_attempts     = max_attempts,
      api_key          = api_key,
      verbose          = TRUE
    )

    openai_download_batch_output(
      batch_id = final_batch$id,
      path     = batch_output_path,
      api_key  = api_key
    )

    out_path <- batch_output_path
    results <- parse_openai_batch_output(batch_output_path)
  }

  list(
    batch_input_path  = batch_input_path,
    batch_output_path = out_path,
    file              = file_obj,
    batch             = final_batch,
    results           = results
  )
}

