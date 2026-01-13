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

  url <- paste0(.openai_base_url(), path)

  # Under normal conditions, httr2 returns a well-formed `httr2_request`.
  # However, in some test/mocking scenarios the returned object can lose its
  # class or even come back as a plain list. We defensively normalize the
  # request shape so downstream helpers and tests remain deterministic.
  req <- request(url) |>
    req_auth_bearer_token(api_key)

  if (!is.list(req)) {
    req <- list()
  }

  if (is.null(req$url)) {
    req$url <- url
  }
  if (is.null(req$method)) {
    req$method <- "GET"
  }
  if (is.null(req$headers) || !is.list(req$headers)) {
    req$headers <- list()
  }

  hdr_names <- names(req$headers)
  if (is.null(hdr_names)) {
    hdr_names <- character(0)
  }
  has_auth <- any(tolower(hdr_names) == "authorization")
  if (!has_auth) {
    req$headers$Authorization <- paste("Bearer", api_key)
  }

  if (is.null(req$options) || !is.list(req$options)) {
    req$options <- list()
  }
  # httr2 expects `req$body` to be NULL (unset) or a well-formed list.
  # In mocked/normalized scenarios we can accidentally end up with
  # `req$body$type` being NULL/NA, which breaks `req_body_*()` helpers
  # because comparisons propagate NA into `if (...)`.
  if (!is.null(req$body) && !is.list(req$body)) {
    req$body <- NULL
  }
  if (!is.null(req$body) && is.list(req$body)) {
    body_type <- req$body$type
    if (is.null(body_type) || length(body_type) != 1L || is.na(body_type[[1]])) {
      req$body <- NULL
    }
  }

  class(req) <- unique(c("httr2_request", class(req)))
  req
}


#' Internal: Attach JSON body to an OpenAI request
#'
#' @keywords internal
#' @noRd
.openai_req_body_json <- function(req, body, ...) {
  if (is.list(req) && !is.null(req$body) && is.list(req$body)) {
    body_type <- req$body$type
    if (is.null(body_type) || length(body_type) != 1L || is.na(body_type[[1]])) {
      req$body <- NULL
    }
  }

  out <- httr2::req_body_json(req, body)

  # IMPORTANT: guarantee this is a POST, even if req_body_json doesn't flip it
  out <- httr2::req_method(out, "POST")

  if (!is.list(out)) out <- list(url = NA_character_)
  class(out) <- unique(c("httr2_request", class(out)))
  out
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

#' Build OpenAI batch JSONL lines for paired comparisons
#'
#' This helper constructs one JSON object per pair of writing samples,
#' suitable for use with the OpenAI batch API. It supports both
#' \code{/v1/chat/completions} and \code{/v1/responses} endpoints.
#'
#' @param pairs A data frame or tibble with columns \code{ID1}, \code{text1},
#'   \code{ID2}, and \code{text2}.
#' @param model Character scalar giving the OpenAI model name.
#'   Supports standard names (e.g. \code{"gpt-4.1"}) and date-stamped versions
#'   (e.g. \code{"gpt-5.2-2025-12-11"}).
#' @param trait_name Short label for the trait (e.g., "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Character template containing the placeholders
#'   \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}}, \code{{SAMPLE_1}},
#'   and \code{{SAMPLE_2}}. Defaults to \code{set_prompt_template()}.
#' @param endpoint Which OpenAI endpoint to target. One of
#'   \code{"chat.completions"} (default) or \code{"responses"}.
#' @param temperature Optional temperature parameter. Defaults to `0` for
#'   standard models (deterministic). Must be `NULL` for reasoning models
#'   (enabled).
#' @param top_p Optional top_p parameter.
#' @param logprobs Optional logprobs parameter.
#' @param reasoning Optional reasoning effort for \code{gpt-5.1/5.2} when using
#'   the \code{/v1/responses} endpoint. Typically \code{"none"}, \code{"low"},
#'   \code{"medium"}, or \code{"high"}.
#' @param include_thoughts Logical; if TRUE and using \code{responses} endpoint
#'   with reasoning, requests a summary. Defaults \code{reasoning} to \code{"low"}
#'   for gpt-5.1/5.2 if not specified.
#' @param request_id_prefix String prefix for \code{custom_id}; the full
#'   ID takes the form \code{"<prefix>_<ID1>_vs_<ID2>"}.
#'
#' @return A tibble with one row per pair and columns:
#'   \itemize{
#'     \item \code{custom_id}: ID string used by the batch API.
#'     \item \code{method}: HTTP method (\code{"POST"}).
#'     \item \code{url}: Endpoint path (\code{"/v1/chat/completions"} or
#'           \code{"/v1/responses"}).
#'     \item \code{body}: List column containing the request body.
#'   }
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and network access.
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 3, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # 1. Basic chat.completions batch with no thoughts
#' batch_tbl_chat <- build_openai_batch_requests(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   temperature       = 0
#' )
#'
#' # 2. GPT-5.2-2025-12-11 Responses Batch with Reasoning
#' batch_resp <- build_openai_batch_requests(
#'   pairs = pairs,
#'   model = "gpt-5.2-2025-12-11",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   endpoint = "responses",
#'   include_thoughts = TRUE, # implies reasoning="low" if not set
#'   reasoning = "medium"
#' )
#' batch_tbl_chat
#' batch_tbl_resp
#' }
#'
#' @import tibble
#' @export
build_openai_batch_requests <- function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template = set_prompt_template(),
                                        endpoint = c(
                                          "chat.completions",
                                          "responses"
                                        ),
                                        temperature = NULL,
                                        top_p = NULL,
                                        logprobs = NULL,
                                        reasoning = NULL,
                                        include_thoughts = FALSE,
                                        request_id_prefix = "EXP") {
  endpoint <- match.arg(endpoint)
  pairs <- tibble::as_tibble(pairs)

  if (nrow(pairs) == 0L) {
    return(tibble::tibble(
      custom_id = character(0), method = character(0), url = character(0), body = list()
    ))
  }

  # Validate model vs temperature / top_p / logprobs / reasoning
  is_reasoning_model <- grepl("^gpt-5\\.[12]", model)

  # Default reasoning if thoughts requested on responses endpoint
  if (endpoint == "responses" && isTRUE(include_thoughts) && is.null(reasoning)) {
    if (is_reasoning_model) {
      reasoning <- "low"
    } else {
      warning("include_thoughts requested for non-reasoning model; ignores thoughts.", call. = FALSE)
    }
  }

  reasoning_active <- is_reasoning_model && (!is.null(reasoning) && reasoning != "none")

  # Apply default temperature = 0 if strictly not reasoning
  if (is.null(temperature) && !reasoning_active) {
    temperature <- 0
  }

  # Validation: only strict check for ACTIVE reasoning
  if (is_reasoning_model && reasoning_active) {
    if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
      stop("For gpt-5.1/5.2 with reasoning, temperature/top_p/logprobs must be NULL.", call. = FALSE)
    }
  }

  out_list <- vector("list", nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    id1 <- as.character(pairs$ID1[i])
    id2 <- as.character(pairs$ID2[i])
    prompt <- build_prompt(
      template = prompt_template,
      trait_name = trait_name,
      trait_desc = trait_description,
      text1 = as.character(pairs$text1[i]),
      text2 = as.character(pairs$text2[i])
    )
    custom_id <- sprintf("%s_%s_vs_%s", request_id_prefix, id1, id2)

    if (endpoint == "chat.completions") {
      body <- list(model = model, messages = list(list(role = "user", content = prompt)))
      if (!is.null(temperature)) body$temperature <- temperature
      if (!is.null(top_p)) body$top_p <- top_p
      if (!is.null(logprobs)) body$logprobs <- logprobs
      obj <- list(custom_id = custom_id, method = "POST", url = "/v1/chat/completions", body = body)
    } else {
      body <- list(model = model, input = prompt)
      if (!is.null(reasoning)) {
        block <- list(effort = reasoning)
        if (!identical(reasoning, "none") && isTRUE(include_thoughts)) block$summary <- "auto"
        body$reasoning <- block
      }
      if (!is.null(temperature)) body$temperature <- temperature
      if (!is.null(top_p)) body$top_p <- top_p
      if (!is.null(logprobs)) body$logprobs <- logprobs
      obj <- list(custom_id = custom_id, method = "POST", url = "/v1/responses", body = body)
    }
    out_list[[i]] <- obj
  }

  tibble::tibble(
    custom_id = vapply(out_list, `[[`, character(1), "custom_id"),
    method = vapply(out_list, `[[`, character(1), "method"),
    url = vapply(out_list, `[[`, character(1), "url"),
    body = lapply(out_list, `[[`, "body")
  )
}

#' Write an OpenAI batch table to a JSONL file
#'
#' This helper takes the output of \code{\link{build_openai_batch_requests}}
#' (or a compatible table) and writes one JSON object per line, in the
#' format expected by the OpenAI batch API.
#'
#' The input can either:
#' \itemize{
#'   \item Already contain a character column \code{jsonl} (one JSON string
#'         per row), in which case that column is used directly, or
#'   \item Contain the columns \code{custom_id}, \code{method},
#'         \code{url}, and \code{body}, in which case the JSON strings are
#'         constructed automatically.
#' }
#'
#' @param batch_tbl A data frame or tibble, typically the result of
#'   \code{\link{build_openai_batch_requests}}.
#' @param path File path where the JSONL file should be written.
#'
#' @return Invisibly returns \code{path}.
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and network access.
#' data("example_writing_samples")
#' pairs_all <- make_pairs(example_writing_samples)
#' pairs_small <- sample_pairs(pairs_all, n_pairs = 5, seed = 1)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' batch_tbl <- build_openai_batch_requests(
#'   pairs             = pairs_small,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' write_openai_batch_file(batch_tbl, "batch_forward.jsonl")
#' }
#'
#' @importFrom jsonlite toJSON
#' @export
write_openai_batch_file <- function(batch_tbl, path) {
  batch_tbl <- tibble::as_tibble(batch_tbl)

  # If a jsonl column already exists, use it directly (backward compatible)
  if ("jsonl" %in% names(batch_tbl)) {
    json_lines <- batch_tbl$jsonl
    if (!is.character(json_lines)) {
      stop("`jsonl` column must be a character vector.", call. = FALSE)
    }
  } else {
    # Otherwise, construct JSONL from custom_id / method / url / body
    required_cols <- c("custom_id", "method", "url", "body")
    missing_cols <- setdiff(required_cols, names(batch_tbl))
    if (length(missing_cols) > 0L) {
      stop(
        "`batch_tbl` must have either a `jsonl` column or columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }

    n <- nrow(batch_tbl)
    if (n == 0L) {
      json_lines <- character(0)
    } else {
      json_lines <- vapply(
        seq_len(n),
        function(i) {
          jsonlite::toJSON(
            list(
              custom_id = batch_tbl$custom_id[i],
              method    = batch_tbl$method[i],
              url       = batch_tbl$url[i],
              body      = batch_tbl$body[[i]]
            ),
            auto_unbox = TRUE
          )
        },
        FUN.VALUE = character(1)
      )
    }
  }

  # Write one JSON object per line
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json_lines, con = con, sep = "\n")

  invisible(path)
}

#' Parse an OpenAI Batch output JSONL file
#'
#' This function reads an OpenAI Batch API output file (JSONL) and extracts
#' pairwise comparison results for use with Bradley–Terry models. It supports
#' both the Chat Completions endpoint (where \code{object = "chat.completion"})
#' and the Responses endpoint (where \code{object = "response"}), including
#' GPT-5.1 with reasoning.
#'
#' For each line, the function:
#' \itemize{
#'   \item extracts \code{custom_id} and parses \code{ID1} and \code{ID2}
#'         from the pattern \code{"<prefix>ID1_vs_ID2"},
#'   \item pulls the raw LLM content containing the
#'         \code{<BETTER_SAMPLE>...</BETTER_SAMPLE>} tag,
#'   \item determines whether \code{SAMPLE_1} or \code{SAMPLE_2} was
#'         selected and maps that to \code{better_id},
#'   \item collects model name and token usage statistics (including
#'         reasoning tokens for GPT-5.1 Responses),
#'   \item when using the Responses endpoint with reasoning, separates
#'         reasoning summaries into the \code{thoughts} column and visible
#'         assistant output into \code{content}.
#' }
#'
#' The returned data frame is suitable as input for
#' \code{\link{build_bt_data}}.
#'
#' @param path Path to a JSONL output file downloaded from the OpenAI Batch
#'   API.
#' @param tag_prefix Character string marking the start of the better-sample
#'   tag. Defaults to \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Character string marking the end of the better-sample
#'   tag. Defaults to \code{"</BETTER_SAMPLE>"}.
#'
#' @return A tibble with one row per successfully parsed comparison and
#'   columns:
#'   \describe{
#'     \item{custom_id}{The \code{custom_id} from the batch request.}
#'     \item{ID1, ID2}{Sample IDs inferred from \code{custom_id}.}
#'     \item{model}{The model name reported by the API.}
#'     \item{object_type}{The OpenAI response object type
#'           (e.g., \code{"chat.completion"} or \code{"response"}).}
#'     \item{status_code}{HTTP-style status code from the batch output.}
#'     \item{error_message}{Error message, if present; otherwise \code{NA}.}
#'     \item{thoughts}{Reasoning / thinking summary text when available
#'           (for Responses with reasoning); otherwise \code{NA}.}
#'     \item{content}{The raw assistant visible content string (the LLM's
#'           output), used to locate the \code{<BETTER_SAMPLE>} tag. For
#'           Responses with reasoning this does not include reasoning
#'           summaries, which are kept in \code{thoughts}.}
#'     \item{better_sample}{Either \code{"SAMPLE_1"}, \code{"SAMPLE_2"},
#'           or \code{NA} if the tag was not found.}
#'     \item{better_id}{\code{ID1} if \code{SAMPLE_1} was chosen, \code{ID2}
#'           if \code{SAMPLE_2} was chosen, or \code{NA}.}
#'     \item{prompt_tokens}{Prompt/input token count (if reported).}
#'     \item{completion_tokens}{Completion/output token count (if reported).}
#'     \item{total_tokens}{Total tokens (if reported).}
#'     \item{prompt_cached_tokens}{Cached prompt tokens (if reported via
#'           \code{input_tokens_details$cached_tokens}); otherwise \code{NA}.}
#'     \item{reasoning_tokens}{Reasoning tokens (if reported via
#'           \code{output_tokens_details$reasoning_tokens}); otherwise
#'           \code{NA}.}
#'   }
#'
#' @examples
#' # Create a temporary JSONL file containing a simulated OpenAI batch result
#' tf <- tempfile(fileext = ".jsonl")
#'
#' # A single line of JSON representing a successful Chat Completion
#' # custom_id implies "LIVE_" prefix, ID1="A", ID2="B"
#' json_line <- paste0(
#'   '{"custom_id": "LIVE_A_vs_B", ',
#'   '"response": {"status_code": 200, "body": {',
#'   '"object": "chat.completion", ',
#'   '"model": "gpt-4", ',
#'   '"choices": [{"message": {"content": "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"}}], ',
#'   '"usage": {"prompt_tokens": 50, "completion_tokens": 10, "total_tokens": 60}}}}'
#' )
#'
#' writeLines(json_line, tf)
#'
#' # Parse the output
#' res <- parse_openai_batch_output(tf)
#'
#' # Inspect the result
#' print(res$better_id)
#' print(res$prompt_tokens)
#'
#' # Clean up
#' unlink(tf)
#'
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @export
parse_openai_batch_output <- function(path,
                                      tag_prefix = "<BETTER_SAMPLE>",
                                      tag_suffix = "</BETTER_SAMPLE>") {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0L) {
    stop("File contains no lines: ", path, call. = FALSE)
  }

  # Helper to parse ID1/ID2 from custom_id of form "<prefix>ID1_vs_ID2"
  parse_ids <- function(custom_id) {
    if (is.null(custom_id) || is.na(custom_id)) {
      return(list(ID1 = NA_character_, ID2 = NA_character_))
    }
    parts <- strsplit(custom_id, "_vs_", fixed = TRUE)[[1]]
    if (length(parts) != 2L) {
      return(list(ID1 = NA_character_, ID2 = NA_character_))
    }
    left <- parts[1]
    right <- parts[2]

    # ID2 is everything after "_vs_"
    id2 <- right

    # ID1 is the substring of 'left' after the last underscore.
    # e.g., "EXP_S01" -> "S01"
    m <- regexpr("_[^_]*$", left)
    if (m[1] > 0) {
      id1 <- substring(left, m[1] + 1L)
    } else {
      id1 <- left
    }

    list(ID1 = id1, ID2 = id2)
  }

  out <- vector("list", length(lines))

  for (i in seq_along(lines)) {
    line_raw <- lines[[i]]
    if (!nzchar(line_raw)) {
      out[[i]] <- NULL
      next
    }

    # Use simplifyVector = FALSE so nested structures stay as lists
    obj <- tryCatch(
      jsonlite::fromJSON(line_raw, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(obj)) {
      out[[i]] <- NULL
      next
    }

    custom_id <- obj$custom_id %||% NA_character_

    # Parse IDs immediately so they are available even if body is missing
    ids <- parse_ids(custom_id)
    ID1 <- ids$ID1
    ID2 <- ids$ID2

    response <- obj$response
    status_code <- response$status_code %||% NA_integer_

    # Error message (if any)
    err <- obj$error
    error_message <- if (!is.null(err) && !is.null(err$message)) {
      err$message
    } else {
      NA_character_
    }

    body <- response$body
    if (is.null(body)) {
      out[[i]] <- tibble::tibble(
        custom_id = custom_id,
        ID1 = ID1,
        ID2 = ID2,
        model = NA_character_,
        object_type = NA_character_,
        status_code = status_code,
        error_message = error_message,
        thoughts = NA_character_,
        content = NA_character_,
        better_sample = NA_character_,
        better_id = NA_character_,
        prompt_tokens = NA_real_,
        completion_tokens = NA_real_,
        total_tokens = NA_real_,
        prompt_cached_tokens = NA_real_,
        reasoning_tokens = NA_real_
      )
      next
    }

    object_type <- body$object %||% NA_character_
    model <- body$model %||% NA_character_

    thoughts <- NA_character_
    content <- NA_character_

    if (identical(object_type, "chat.completion")) {
      # Chat Completions: choices[[1]]$message$content
      choices <- body$choices %||% list()
      if (length(choices) >= 1L) {
        message_obj <- choices[[1]]$message
        if (!is.null(message_obj) && !is.null(message_obj$content)) {
          content <- as.character(message_obj$content)
        }
      }
    } else if (identical(object_type, "response")) {
      # Responses endpoint: collect reasoning summaries into `thoughts` and
      # message text into `content`.
      reasoning_chunks <- character(0)
      message_chunks <- character(0)

      output <- body$output %||% list()
      if (length(output) > 0L) {
        for (out_el in output) {
          # Reasoning summaries: output item with type = "reasoning"
          if (!is.null(out_el$type) && identical(out_el$type, "reasoning")) {
            rs <- out_el$summary
            if (!is.null(rs) && length(rs) > 0L) {
              if (is.list(rs) && !is.data.frame(rs)) {
                for (s in rs) {
                  if (!is.null(s$text)) {
                    reasoning_chunks <- c(
                      reasoning_chunks,
                      as.character(s$text %||% "")
                    )
                  }
                }
              } else if (is.data.frame(rs) && "text" %in% names(rs)) {
                reasoning_chunks <- c(
                  reasoning_chunks,
                  as.character(rs$text)
                )
              }
            }
          }

          # Visible output text (assistant message, etc.)
          blocks <- out_el$content %||% list()
          if (length(blocks) > 0L) {
            for (b in blocks) {
              # For Responses, content blocks often use "text" or "output_text"
              txt <- b$text %||% b$output_text %||% NULL
              if (!is.null(txt)) {
                message_chunks <- c(
                  message_chunks,
                  as.character(txt %||% "")
                )
              }
            }
          }
        }
      }

      # Backwards compatibility: some fixtures store reasoning summary at
      # body$reasoning$summary$text. If we have no reasoning_chunks yet, try
      # that shape. If summary is a character scalar (e.g. "auto"/"detailed"),
      # treat it as configuration and ignore it for thoughts.
      if (!length(reasoning_chunks) &&
        !is.null(body$reasoning) &&
        !is.null(body$reasoning$summary)) {
        rs <- body$reasoning$summary

        if (is.list(rs) && !is.null(rs$text)) {
          reasoning_chunks <- c(
            reasoning_chunks,
            as.character(rs$text %||% "")
          )
        }
        # Character summaries (e.g. "auto", "detailed") are ignored.
      }

      if (length(reasoning_chunks)) {
        thoughts <- paste(reasoning_chunks, collapse = " ")
      }

      if (length(message_chunks)) {
        content <- paste(message_chunks, collapse = "")
      } else {
        content <- NA_character_
      }
    }

    # Extract better_sample via simple tag search
    better_sample <- .extract_better_sample(
      content,
      tag_prefix = tag_prefix,
      tag_suffix = tag_suffix
    )

    better_id <- NA_character_
    if (!is.na(better_sample)) {
      if (better_sample == "SAMPLE_1") {
        better_id <- ID1
      } else if (better_sample == "SAMPLE_2") {
        better_id <- ID2
      }
    }

    # Usage: harmonize token counts across object types
    usage <- body$usage %||% list()

    # Chat completions: prompt_tokens, completion_tokens, total_tokens
    # Responses (gpt-5.x): input_tokens, output_tokens, total_tokens
    prompt_tokens <- usage$prompt_tokens %||% usage$input_tokens %||% NA_real_
    completion_tokens <- usage$completion_tokens %||% usage$output_tokens %||%
      NA_real_
    total_tokens <- usage$total_tokens %||% NA_real_

    # Detailed token info when available
    input_details <- usage$input_tokens_details %||% list()
    output_details <- usage$output_tokens_details %||% list()

    prompt_cached_tokens <- input_details$cached_tokens %||% NA_real_
    reasoning_tokens <- output_details$reasoning_tokens %||% NA_real_

    out[[i]] <- tibble::tibble(
      custom_id            = custom_id,
      ID1                  = ID1,
      ID2                  = ID2,
      model                = model,
      object_type          = object_type,
      status_code          = status_code,
      error_message        = error_message,
      thoughts             = thoughts,
      content              = content,
      better_sample        = better_sample,
      better_id            = better_id,
      prompt_tokens        = as.numeric(prompt_tokens),
      completion_tokens    = as.numeric(completion_tokens),
      total_tokens         = as.numeric(total_tokens),
      prompt_cached_tokens = as.numeric(prompt_cached_tokens),
      reasoning_tokens     = as.numeric(reasoning_tokens)
    )
  }

  # Drop NULL entries and bind rows
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(out)
}
