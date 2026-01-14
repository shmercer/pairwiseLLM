#' Internal helpers for Anthropic batch support
#'
#' These imports are used by the Anthropic Message Batches helpers defined
#' in this file.
#'
#' @name anthropic_batch_internal
#' @keywords internal
#' @noRd
#' @import tibble
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr2 resp_body_string
NULL

# Internal: parse IDs from custom_id of the form "<PREFIX>_<ID1>_vs_<ID2>"
.parse_ids_from_custom_id <- function(custom_id) {
  if (is.null(custom_id) || is.na(custom_id) || !nzchar(custom_id)) {
    return(list(ID1 = NA_character_, ID2 = NA_character_))
  }

  # Expect something like "ANTH_S01_vs_S02"
  # Split off prefix and rest
  parts <- strsplit(custom_id, "_", fixed = TRUE)[[1L]]

  if (length(parts) < 3L) {
    return(list(ID1 = NA_character_, ID2 = NA_character_))
  }

  rest <- paste(parts[-1L], collapse = "_")
  # Now rest is "S01_vs_S02" or similar
  bits <- strsplit(rest, "_vs_", fixed = TRUE)[[1L]]

  if (length(bits) != 2L) {
    return(list(ID1 = NA_character_, ID2 = NA_character_))
  }

  list(
    ID1 = bits[1L],
    ID2 = bits[2L]
  )
}

# Internal: parse a single Anthropic "message" object (from Messages API)
# into the standard pairwiseLLM schema (without custom_id / status_code).
.parse_anthropic_pair_message <- function(
  body,
  ID1,
  ID2,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>"
) {
  object_type <- body$type %||% NA_character_
  model_name <- body$model %||% NA_character_

  # Collect thinking + text content from assistant message blocks
  thoughts <- NA_character_
  content <- NA_character_

  if (!is.null(body$content) && length(body$content) > 0L) {
    thought_pieces <- character(0)
    content_pieces <- character(0)

    for (blk in body$content) {
      blk_type <- blk$type %||% NULL

      if (!is.null(blk_type) && identical(blk_type, "thinking")) {
        # Extended-thinking block (Claude 4.x/4.5)
        txt <- blk$thinking %||% blk$text %||% ""
        if (!is.null(txt)) {
          thought_pieces <- c(thought_pieces, as.character(txt))
        }
      } else {
        # Default text block
        txt <- blk$text %||% ""
        if (!is.null(txt)) {
          content_pieces <- c(content_pieces, as.character(txt))
        }
      }
    }

    if (length(thought_pieces) > 0L) {
      thoughts <- paste(thought_pieces, collapse = "\n\n")
    }
    if (length(content_pieces) > 0L) {
      content <- paste(content_pieces, collapse = "")
    }
  }

  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content,
      fixed =
        TRUE
    )) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content,
      fixed = TRUE
    )) {
      better_sample <- "SAMPLE_2"
    }
  }

  better_id <- NA_character_
  if (!is.na(better_sample)) {
    better_id <- if (better_sample == "SAMPLE_1") ID1 else ID2
  }

  usage <- body$usage %||% list()
  prompt_tokens <- usage$input_tokens %||% NA_real_
  completion_tokens <- usage$output_tokens %||% NA_real_
  total_tokens <- usage$total_tokens %||% NA_real_

  tibble::tibble(
    ID1               = ID1,
    ID2               = ID2,
    model             = model_name,
    object_type       = object_type,
    thoughts          = thoughts,
    content           = content,
    better_sample     = better_sample,
    better_id         = better_id,
    prompt_tokens     = as.numeric(prompt_tokens),
    completion_tokens = as.numeric(completion_tokens),
    total_tokens      = as.numeric(total_tokens)
  )
}

#' Build Anthropic Message Batch requests from a tibble of pairs
#'
#' This helper converts a tibble of writing pairs into a list of Anthropic
#' \emph{Message Batch} requests. Each request has a unique \code{custom_id}
#' of the form \code{"ANTH_<ID1>_vs_<ID2>"} and a \code{params} object
#' compatible with the \code{/v1/messages} API.
#'
#' The function mirrors the behaviour of
#' \code{\link{build_openai_batch_requests}} but targets Anthropic's
#' \code{/v1/messages/batches} endpoint. It applies the
#' same recommended defaults and reasoning constraints as
#' \code{\link{anthropic_compare_pair_live}}:
#'
#' \itemize{
#'   \item \code{reasoning = "none"}:
#'     \itemize{
#'       \item Default \code{temperature = 0} (deterministic behaviour),
#'         unless you explicitly supply a different \code{temperature} via
#'         \code{...}.
#'       \item Default \code{max_tokens = 768}, unless overridden via
#'         \code{max_tokens} in \code{...}.
#'     }
#'   \item \code{reasoning = "enabled"} (extended thinking):
#'     \itemize{
#'       \item \code{temperature} \strong{must} be 1. If you supply a different
#'         value in \code{...}, this function throws an error.
#'       \item Defaults to \code{max_tokens = 2048} and
#'         \code{thinking_budget_tokens = 1024}, with the constraint
#'         \code{1024 <= thinking_budget_tokens < max_tokens}. Violations of
#'         this constraint produce an error.
#'     }
#' }
#'
#' As a result, when you build batches without extended thinking
#' (\code{reasoning = "none"}), the effective default temperature is 0. When
#' you opt into extended thinking (\code{reasoning = "enabled"}), Anthropic's
#' requirement of \code{temperature = 1} is enforced for all batch requests.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}. Typically created by
#'   \code{\link{make_pairs}}, \code{\link{sample_pairs}}, and
#'   \code{\link{randomize_pair_order}}.
#' @param model Anthropic Claude model name, for example
#'   \code{"claude-sonnet-4-5"}, \code{"claude-haiku-4-5"}, or
#'   \code{"claude-opus-4-5"}.
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text description of the trait or rubric.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}. The template should embed your full
#'   instructions, rubric text, and \verb{<BETTER_SAMPLE>} tagging convention.
#' @param reasoning Character scalar indicating whether to allow extended
#'   thinking; one of \code{"none"} or \code{"enabled"}. See details above.
#' @param custom_id_prefix Prefix for the \code{custom_id} field. Defaults to
#'   \code{"ANTH"} so that IDs take the form \code{"ANTH_<ID1>_vs_<ID2>"}.
#' @param ... Additional Anthropic parameters such as \code{max_tokens},
#'   \code{temperature}, \code{top_p}, or \code{thinking_budget_tokens},
#'   which will be passed through to the Messages API.
#'
#' @return A tibble with one row per pair and two main columns:
#' \describe{
#'   \item{custom_id}{Character ID of the form
#'     \code{"<PREFIX>_<ID1>_vs_<ID2>"}.}
#'   \item{params}{List-column containing the Anthropic Messages API
#'     \code{params} object for each request, ready to be used in the
#'     \code{requests} array of \code{/v1/messages/batches}.}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
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
#' # Standard batch requests without extended thinking
#' reqs_none <- build_anthropic_batch_requests(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "none"
#' )
#'
#' reqs_none
#'
#' # Batch requests with extended thinking
#' reqs_reason <- build_anthropic_batch_requests(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "enabled"
#' )
#'
#' reqs_reason
#' }
#'
#' @export
build_anthropic_batch_requests <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  reasoning = c("none", "enabled"),
  custom_id_prefix = "ANTH",
  ...
) {
  reasoning <- match.arg(reasoning)

  pairs <- tibble::as_tibble(pairs)
  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols <- setdiff(required_cols, names(pairs))

  if (length(missing_cols) > 0L) {
    stop(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(model) || length(model) != 1L) {
    stop("`model` must be a single character.", call. = FALSE)
  }

  dots <- list(...)

  get_body_for_pair <- function(ID1, text1, ID2, text2) {
    # --------------------------------------------------------------------
    # Temperature, max_tokens, and thinking budget (per reasoning mode)
    # --------------------------------------------------------------------
    if (reasoning == "none") {
      temperature <- dots$temperature %||% 0
      max_tokens <- dots$max_tokens %||% 768L
      max_tokens <- as.integer(max_tokens)

      thinking <- NULL
      thinking_budget_tokens <- NULL
    } else {
      # reasoning == "enabled"
      if (is.null(dots$temperature)) {
        temperature <- 1
      } else if (!identical(dots$temperature, 1)) {
        stop(
          "`temperature` must be 1 when `reasoning = \"enabled\"` ",
          "for Anthropic extended thinking.",
          call. = FALSE
        )
      } else {
        temperature <- dots$temperature
      }

      max_tokens <- dots$max_tokens %||% 2048L
      max_tokens <- as.integer(max_tokens)

      thinking_budget_tokens <- dots$thinking_budget_tokens %||% 1024L
      thinking_budget_tokens <- as.integer(thinking_budget_tokens)

      if (thinking_budget_tokens < 1024L) {
        stop(
          "`thinking_budget_tokens` must be at least 1024 when ",
          "`reasoning = \"enabled\"`.",
          call. = FALSE
        )
      }
      if (thinking_budget_tokens >= max_tokens) {
        stop(
          "`thinking_budget_tokens` must be smaller than `max_tokens` ",
          "when `reasoning = \"enabled\"`.",
          call. = FALSE
        )
      }

      thinking <- list(
        type          = "enabled",
        budget_tokens = thinking_budget_tokens
      )
    }

    # Top-p and other extras can be passed through directly.
    top_p <- dots$top_p

    prompt <- build_prompt(
      template   = prompt_template,
      trait_name = trait_name,
      trait_desc = trait_description,
      text1      = text1,
      text2      = text2
    )

    params <- list(
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      messages = list(
        list(
          role = "user",
          content = list(
            list(
              type = "text",
              text = prompt
            )
          )
        )
      )
    )


    if (!is.null(top_p)) {
      params$top_p <- top_p
    }
    if (!is.null(thinking)) {
      params$thinking <- thinking
    }

    params
  }

  out <- vector("list", nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    ID1 <- as.character(pairs$ID1[i])
    ID2 <- as.character(pairs$ID2[i])
    text1 <- as.character(pairs$text1[i])
    text2 <- as.character(pairs$text2[i])

    custom_id <- if ("pair_uid" %in% names(pairs)) {
      as.character(pairs$pair_uid[i])
    } else {
      sprintf("%s_%s_vs_%s", custom_id_prefix, ID1, ID2)
    }
    params <- get_body_for_pair(ID1, text1, ID2, text2)

    out[[i]] <- list(custom_id = custom_id, params = params)
  }

  tibble::tibble(
    custom_id = vapply(out, `[[`, character(1L), "custom_id"),
    params    = lapply(out, `[[`, "params")
  )
}

#' Create an Anthropic Message Batch
#'
#' This is a thin wrapper around Anthropic's
#' \code{/v1/messages/batches} endpoint. It accepts a list of request
#' objects (each with \code{custom_id} and \code{params}) and returns the
#' resulting Message Batch object.
#'
#' Typically you will not call this directly; instead, use
#' \code{\link{run_anthropic_batch_pipeline}} which builds requests from a
#' tibble of pairs, creates the batch, polls for completion, and downloads
#' the results.
#'
#' @param requests List of request objects, each of the form
#'   \code{list(custom_id = <chr>, params = <list>)}. You can obtain this
#'   list from the output of \code{\link{build_anthropic_batch_requests}} via
#'   \code{split} / \code{Map}, or use \code{run_anthropic_batch_pipeline}.
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#'
#' @return A list representing the Message Batch object returned by Anthropic.
#'   Important fields include \code{id}, \code{processing_status},
#'   \code{request_counts}, and (after completion) \code{results_url}.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' library(pairwiseLLM)
#'
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
#' req_tbl <- build_anthropic_batch_requests(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' requests <- lapply(seq_len(nrow(req_tbl)), function(i) {
#'   list(
#'     custom_id = req_tbl$custom_id[i],
#'     params    = req_tbl$params[[i]]
#'   )
#' })
#'
#' batch <- anthropic_create_batch(requests = requests)
#' batch$id
#' batch$processing_status
#' }
#'
#' @export
anthropic_create_batch <- function(
  requests,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
) {
  if (!is.list(requests) || length(requests) == 0L) {
    stop("`requests` must be a non-empty list of request objects.",
      call. = FALSE
    )
  }

  req <- .anthropic_request(
    path              = "/v1/messages/batches",
    api_key           = api_key,
    anthropic_version = anthropic_version
  )

  req <- .anthropic_req_body_json(req, body = list(requests = requests))
  resp <- .anthropic_req_perform(req)

  .anthropic_resp_body_json(resp, simplifyVector = TRUE)
}

#' Retrieve an Anthropic Message Batch by ID
#'
#' This retrieves the latest state of a Message Batch using its \code{id}.
#' It corresponds to a \code{GET} request on
#' \code{/v1/messages/batches/<MESSAGE_BATCH_ID>}.
#'
#' @param batch_id Character scalar giving the batch ID (for example
#'   \code{"msgbatch_01HkcTjaV5uDC8jWR4ZsDV8d"}).
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#'
#' @return A list representing the Message Batch object, including fields
#'   such as \code{id}, \code{processing_status}, \code{request_counts},
#'   and (after completion) \code{results_url}.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' # After creating a batch:
#' batch <- anthropic_create_batch(requests = my_requests)
#' batch_id <- batch$id
#'
#' latest <- anthropic_get_batch(batch_id)
#' latest$processing_status
#' }
#'
#' @export
anthropic_get_batch <- function(
  batch_id,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
) {
  if (!is.character(batch_id) || length(batch_id) != 1L || !nzchar(batch_id)) {
    stop("`batch_id` must be a non-empty character scalar.", call. = FALSE)
  }

  path <- paste0("/v1/messages/batches/", batch_id)

  req <- .anthropic_request(
    path              = path,
    api_key           = api_key,
    anthropic_version = anthropic_version
  )

  resp <- .anthropic_req_perform(req)
  .anthropic_resp_body_json(resp, simplifyVector = TRUE)
}

#' Poll an Anthropic Message Batch until completion
#'
#' This helper repeatedly calls \code{\link{anthropic_get_batch}} until
#' the batch's \code{processing_status} becomes \code{"ended"} or a time
#' limit is reached. It is analogous to
#' \code{openai_poll_batch_until_complete()} but for Anthropic's
#' Message Batches API.
#'
#' @param batch_id Character scalar giving the batch ID.
#' @param interval_seconds Polling interval in seconds. Defaults to 60.
#' @param timeout_seconds Maximum total waiting time in seconds. Defaults to
#'   24 hours (\code{86400} seconds).
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#' @param verbose Logical; if \code{TRUE}, prints progress messages.
#'
#' @return The final Message Batch object as returned by
#'   \code{\link{anthropic_get_batch}} once \code{processing_status == "ended"}
#'   or the last object retrieved before timing out.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' batch <- anthropic_create_batch(requests = my_requests)
#' final <- anthropic_poll_batch_until_complete(batch$id, interval_seconds = 30)
#' final$processing_status
#' }
#'
#' @export
anthropic_poll_batch_until_complete <- function(
  batch_id,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  verbose = TRUE
) {
  start_time <- Sys.time()
  last_batch <- NULL

  repeat {
    batch <- anthropic_get_batch(
      batch_id          = batch_id,
      api_key           = api_key,
      anthropic_version = anthropic_version
    )
    last_batch <- batch

    status <- batch$processing_status %||% NA_character_

    if (verbose) {
      msg <- sprintf(
        "Batch %s status: %s | processing=%s, succeeded=%s, errored=%s,
        canceled=%s, expired=%s",
        batch$id %||% "<unknown>",
        status,
        batch$request_counts$processing %||% NA_integer_,
        batch$request_counts$succeeded %||% NA_integer_,
        batch$request_counts$errored %||% NA_integer_,
        batch$request_counts$canceled %||% NA_integer_,
        batch$request_counts$expired %||% NA_integer_
      )
      message(msg)
    }

    if (identical(status, "ended")) {
      break
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.infinite(timeout_seconds) && elapsed > timeout_seconds) {
      if (verbose) {
        warning(
          "Timeout reached while waiting for Anthropic batch to complete. ",
          "Returning the last retrieved batch object."
        )
      }
      break
    }

    Sys.sleep(interval_seconds)
  }

  last_batch
}

#' Download Anthropic Message Batch results (.jsonl)
#'
#' Once a Message Batch has finished processing (status \code{"ended"}),
#' Anthropic exposes a \code{results_url} field pointing to a \code{.jsonl}
#' file containing one JSON object per request result.
#'
#' This helper downloads that file and writes it to disk. It is the
#' Anthropic counterpart to \code{openai_download_batch_output()}.
#'
#' @param batch_id Character scalar giving the batch ID.
#' @param output_path File path where the \code{.jsonl} results should be
#'   written.
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#'
#' @return Invisibly, the \code{output_path}.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' final <- anthropic_poll_batch_until_complete(batch$id)
#' jsonl_path <- tempfile(fileext = ".jsonl")
#' anthropic_download_batch_results(final$id, jsonl_path)
#' }
#'
#' @export
anthropic_download_batch_results <- function(
  batch_id,
  output_path,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
) {
  batch <- anthropic_get_batch(
    batch_id          = batch_id,
    api_key           = api_key,
    anthropic_version = anthropic_version
  )

  results_url <- batch$results_url %||% ""
  if (!nzchar(results_url)) {
    stop(
      "Batch has no `results_url` yet. ",
      "Ensure that `processing_status` is 'ended' before downloading results.",
      call. = FALSE
    )
  }

  # Build a fresh request using the full results URL
  req <- httr2::request(results_url)
  req <- httr2::req_headers(
    req,
    "x-api-key"         = .anthropic_api_key(api_key),
    "anthropic-version" = anthropic_version
  )

  resp <- .anthropic_req_perform(req)
  txt <- httr2::resp_body_string(resp)

  # The results are already JSONL; write as-is.
  con <- file(output_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(txt), con)

  invisible(output_path)
}

#' Parse Anthropic Message Batch output into a tibble
#'
#' This function parses a \code{.jsonl} file produced by
#' \code{\link{anthropic_download_batch_results}}. Each line in the file
#' is a JSON object with at least:
#'
#' \preformatted{
#' {
#'   "custom_id": "ANTH_S01_vs_S02",
#'   "result": {
#'     "type": "succeeded" | "errored" | "canceled" | "expired",
#'     "message": { ... }  # when type == "succeeded"
#'     "error":   { ... }  # when type == "errored" (optional)
#'   }
#' }
#' }
#'
#' Results may be returned in any order. This function uses the
#' \code{custom_id} field to recover \code{ID1} and \code{ID2} and then
#' applies the same parsing logic as \code{\link{anthropic_compare_pair_live}},
#' including extraction of extended thinking blocks (when enabled) into
#' a separate \code{thoughts} column.
#'
#' @param jsonl_path Path to a \code{.jsonl} file produced by
#'   \code{\link{anthropic_download_batch_results}}.
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   \code{"</BETTER_SAMPLE>"}.
#'
#' @return A tibble with one row per result. The columns mirror
#'   \code{\link{anthropic_compare_pair_live}} with batch-specific additions:
#'
#' \describe{
#'   \item{custom_id}{Batch custom ID (for example \code{"ANTH_S01_vs_S02"}).}
#'   \item{ID1, ID2}{Sample IDs recovered from \code{custom_id}.}
#'   \item{model}{Model name reported by Anthropic.}
#'   \item{object_type}{Anthropic object type (for example \code{"message"}).}
#'   \item{status_code}{HTTP-style status code (200 for succeeded results,
#'         \code{NA} otherwise).}
#'   \item{result_type}{One of \code{"succeeded"}, \code{"errored"},
#'         \code{"canceled"}, \code{"expired"}.}
#'   \item{error_message}{Error message for non-succeeded results,
#'         otherwise NA.}
#'   \item{thoughts}{Extended thinking text returned by Claude when reasoning
#'         is enabled (for example when \code{reasoning = "enabled"}),
#'         otherwise NA.}
#'   \item{content}{Concatenated assistant text for succeeded results.}
#'   \item{better_sample}{"SAMPLE_1", "SAMPLE_2", or NA.}
#'   \item{better_id}{ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen,
#'         otherwise NA.}
#'   \item{prompt_tokens}{Prompt / input token count (if reported).}
#'   \item{completion_tokens}{Completion / output token count (if reported).}
#'   \item{total_tokens}{Total token count (reported or computed upstream).}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires a completed Anthropic batch file
#' tbl <- parse_anthropic_batch_output("anthropic-results.jsonl")
#' }
#'
#' @export
parse_anthropic_batch_output <- function(
  jsonl_path,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>"
) {
  if (!file.exists(jsonl_path)) {
    stop("`jsonl_path` does not exist: ", jsonl_path, call. = FALSE)
  }

  lines <- readLines(jsonl_path, warn = FALSE)
  if (length(lines) == 0L) {
    return(
      tibble::tibble(
        custom_id         = character(0),
        ID1               = character(0),
        ID2               = character(0),
        model             = character(0),
        object_type       = character(0),
        status_code       = integer(0),
        result_type       = character(0),
        error_message     = character(0),
        thoughts          = character(0),
        content           = character(0),
        better_sample     = character(0),
        better_id         = character(0),
        prompt_tokens     = numeric(0),
        completion_tokens = numeric(0),
        total_tokens      = numeric(0)
      )
    )
  }

  out <- vector("list", length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (!nzchar(trimws(line))) {
      next
    }

    obj <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) {
        warning(
          "Failed to parse JSON on line ", i, " of ", jsonl_path, ": ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (is.null(obj)) {
      out[[i]] <- tibble::tibble(
        custom_id         = NA_character_,
        ID1               = NA_character_,
        ID2               = NA_character_,
        model             = NA_character_,
        object_type       = NA_character_,
        status_code       = NA_integer_,
        result_type       = NA_character_,
        error_message     = "Failed to parse JSON line.",
        thoughts          = NA_character_,
        content           = NA_character_,
        better_sample     = NA_character_,
        better_id         = NA_character_,
        prompt_tokens     = NA_real_,
        completion_tokens = NA_real_,
        total_tokens      = NA_real_
      )
      next
    }

    custom_id <- obj$custom_id %||% NA_character_
    ids <- .parse_ids_from_custom_id(custom_id)
    ID1 <- ids$ID1
    ID2 <- ids$ID2

    result <- obj$result %||% list()
    r_type <- result$type %||% NA_character_

    if (!identical(r_type, "succeeded")) {
      err_msg <- NA_character_
      if (!is.null(result$error) && !is.null(result$error$message)) {
        err_msg <- as.character(result$error$message)
      }

      out[[i]] <- tibble::tibble(
        custom_id         = custom_id,
        ID1               = ID1,
        ID2               = ID2,
        model             = NA_character_,
        object_type       = NA_character_,
        status_code       = NA_integer_,
        result_type       = as.character(r_type),
        error_message     = err_msg,
        thoughts          = NA_character_,
        content           = NA_character_,
        better_sample     = NA_character_,
        better_id         = NA_character_,
        prompt_tokens     = NA_real_,
        completion_tokens = NA_real_,
        total_tokens      = NA_real_
      )
      next
    }

    message_body <- result$message %||% list()
    parsed <- .parse_anthropic_pair_message(
      body       = message_body,
      ID1        = ID1,
      ID2        = ID2,
      tag_prefix = tag_prefix,
      tag_suffix = tag_suffix
    )

    out[[i]] <- tibble::tibble(
      custom_id         = custom_id,
      ID1               = parsed$ID1,
      ID2               = parsed$ID2,
      model             = parsed$model,
      object_type       = parsed$object_type,
      status_code       = 200L,
      result_type       = as.character(r_type),
      error_message     = NA_character_,
      thoughts          = parsed$thoughts,
      content           = parsed$content,
      better_sample     = parsed$better_sample,
      better_id         = parsed$better_id,
      prompt_tokens     = parsed$prompt_tokens,
      completion_tokens = parsed$completion_tokens,
      total_tokens      = parsed$total_tokens
    )
  }

  dplyr::bind_rows(out)
}

#' Run an Anthropic batch pipeline for pairwise comparisons
#'
#' This high-level helper mirrors \code{\link{run_openai_batch_pipeline}} but
#' targets Anthropic's \emph{Message Batches API}. It:
#'
#' \enumerate{
#'   \item Builds Anthropic batch requests from a tibble of pairs using
#'     \code{\link{build_anthropic_batch_requests}}.
#'   \item Writes a JSON file containing the \code{requests} object for
#'     reproducibility.
#'   \item Creates a Message Batch via \code{\link{anthropic_create_batch}}.
#'   \item Optionally polls until the batch reaches \code{processing_status =
#'     "ended"} using \code{\link{anthropic_poll_batch_until_complete}}.
#'   \item If polling is enabled, downloads the \code{.jsonl} result file with
#'     \code{\link{anthropic_download_batch_results}} and parses it via
#'     \code{\link{parse_anthropic_batch_output}}.
#' }
#'
#' It is the Anthropic analogue of \code{\link{run_openai_batch_pipeline}} and
#' returns a list with the same overall structure so that downstream code can
#' treat the two backends uniformly.
#'
#' When \code{include_thoughts = TRUE} and \code{reasoning} is left at its
#' default of \code{"none"}, this function automatically upgrades
#' \code{reasoning} to \code{"enabled"} so that Claude's extended thinking
#' blocks are returned and parsed into the \code{thoughts} column by
#' \code{\link{parse_anthropic_batch_output}}.
#'
#' @details
#' **Temperature and reasoning defaults**
#'
#' Temperature and thinking-mode behaviour are controlled by
#' \code{\link{build_anthropic_batch_requests}}:
#' \itemize{
#'   \item When \code{reasoning = "none"} (no extended thinking):
#'     \itemize{
#'       \item The default \code{temperature} is \code{0} (deterministic),
#'         unless you explicitly supply a \code{temperature} argument via
#'         \code{...}.
#'       \item The default \code{max_tokens} is \code{768}, unless you
#'         override it via \code{max_tokens} in \code{...}.
#'     }
#'   \item When \code{reasoning = "enabled"} (extended thinking enabled):
#'     \itemize{
#'       \item \code{temperature} \strong{must} be \code{1}. If you supply a
#'         different value in \code{...},
#'         \code{build_anthropic_batch_requests()} will throw an error.
#'       \item By default, \code{max_tokens = 2048} and
#'         \code{thinking_budget_tokens = 1024}, subject to the constraint
#'         \code{1024 <= thinking_budget_tokens < max_tokens}. Violations of
#'         this constraint also produce an error.
#'     }
#' }
#'
#' Therefore, when you run batches without extended thinking (the usual case),
#' the effective default is a temperature of \code{0}. When you explicitly use
#' extended thinking (either by setting \code{reasoning = "enabled"} or by
#' using \code{include_thoughts = TRUE}), Anthropic's requirement of
#' \code{temperature = 1} is enforced.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}.
#' @param model Anthropic model name (for example \code{"claude-sonnet-4-5"}).
#' @param trait_name Trait name to pass to
#'   \code{\link{build_anthropic_batch_requests}}.
#' @param trait_description Trait description to pass to
#'   \code{\link{build_anthropic_batch_requests}}.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param reasoning Character scalar; one of \code{"none"} or \code{"enabled"}.
#'   See details above for how \code{include_thoughts} influences this value and
#'   how temperature defaults are derived.
#' @param include_thoughts Logical; if \code{TRUE}, requests extended thinking
#'   from Claude (by setting \code{reasoning = "enabled"} when necessary) and
#'   parses any thinking blocks into a \code{thoughts} column in the batch
#'   results.
#' @param batch_input_path Path to write the JSON file containing the
#'   \code{requests} object. Defaults to a temporary file with suffix
#'   \code{".json"}.
#' @param batch_output_path Path to write the downloaded \code{.jsonl} results
#'   if \code{poll = TRUE}. Defaults to a temporary file with suffix
#'   \code{".jsonl"}.
#' @param poll Logical; if \code{TRUE}, the function will poll the batch until
#'   it reaches \code{processing_status = "ended"} using
#'   \code{\link{anthropic_poll_batch_until_complete}} and then download and
#'   parse the output. If \code{FALSE}, it stops after creating the batch and
#'   returns without polling or parsing.
#' @param interval_seconds Polling interval in seconds (used when
#'   \code{poll = TRUE}).
#' @param timeout_seconds Maximum total time in seconds for polling before
#'   giving up (used when \code{poll = TRUE}).
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#' @param verbose Logical; if \code{TRUE}, prints progress messages while
#'   polling.
#' @param ... Additional Anthropic parameters forwarded to
#'   \code{\link{build_anthropic_batch_requests}} (for example
#'   \code{max_tokens}, \code{temperature}, \code{top_p},
#'   \code{thinking_budget_tokens}).
#'
#' @return A list with elements (aligned with
#'   \code{\link{run_openai_batch_pipeline}}):
#' \describe{
#'   \item{batch_input_path}{Path to the JSON file containing the batch
#'     \code{requests} object.}
#'   \item{batch_output_path}{Path to the downloaded \code{.jsonl} results file
#'     if \code{poll = TRUE}, otherwise \code{NULL}.}
#'   \item{file}{Always \code{NULL} for Anthropic batches (OpenAI uses a File
#'     object here). Included for structural compatibility.}
#'   \item{batch}{Message Batch object; if \code{poll = TRUE}, this is the final
#'     batch after polling, otherwise the initial batch returned by
#'     \code{\link{anthropic_create_batch}}.}
#'   \item{results}{Parsed tibble from
#'     \code{\link{parse_anthropic_batch_output}} if \code{poll = TRUE},
#'     otherwise \code{NULL}.}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 5, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Standard batch without extended thinking
#' pipeline_none <- run_anthropic_batch_pipeline(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "none",
#'   include_thoughts  = FALSE,
#'   interval_seconds  = 60,
#'   timeout_seconds   = 3600,
#'   verbose           = TRUE
#' )
#'
#' pipeline_none$batch$processing_status
#' head(pipeline_none$results)
#'
#' # Batch with extended thinking and thoughts column
#' pipeline_thoughts <- run_anthropic_batch_pipeline(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   include_thoughts  = TRUE,
#'   interval_seconds  = 60,
#'   timeout_seconds   = 3600,
#'   verbose           = TRUE
#' )
#'
#' pipeline_thoughts$batch$processing_status
#' head(pipeline_thoughts$results)
#' }
#'
#' @export
run_anthropic_batch_pipeline <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  reasoning = c("none", "enabled"),
  include_thoughts = FALSE,
  batch_input_path = NULL,
  batch_output_path = NULL,
  poll = TRUE,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  verbose = TRUE,
  ...
) {
  reasoning <- match.arg(reasoning)

  # If caller requested thoughts but reasoning was left as "none",
  # upgrade to "enabled" so the thinking block is actually used.
  if (isTRUE(include_thoughts) && identical(reasoning, "none")) {
    reasoning <- "enabled"
  }

  # 1) Build batch requests tibble
  req_tbl <- build_anthropic_batch_requests(
    pairs             = pairs,
    model             = model,
    trait_name        = trait_name,
    trait_description = trait_description,
    prompt_template   = prompt_template,
    reasoning         = reasoning,
    ...
  )

  # Convert tibble to list-of-requests structure expected by
  # /v1/messages/batches
  requests <- lapply(seq_len(nrow(req_tbl)), function(i) {
    list(
      custom_id = req_tbl$custom_id[i],
      params    = req_tbl$params[[i]]
    )
  })

  # 2) Write JSON input (for reproducibility only)
  if (is.null(batch_input_path)) {
    batch_input_path <- tempfile(
      pattern = "anthropic-batch-input-",
      fileext = ".json"
    )
  }

  input_obj <- list(requests = requests)
  json_txt <- jsonlite::toJSON(input_obj, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json_txt, batch_input_path, useBytes = TRUE)

  # 3) Create batch
  batch_obj <- anthropic_create_batch(
    requests          = requests,
    api_key           = api_key,
    anthropic_version = anthropic_version
  )

  final_batch <- batch_obj
  results <- NULL
  out_path <- NULL

  # 4) Optional polling + download + parse
  if (isTRUE(poll)) {
    final_batch <- anthropic_poll_batch_until_complete(
      batch_id          = batch_obj$id,
      interval_seconds  = interval_seconds,
      timeout_seconds   = timeout_seconds,
      api_key           = api_key,
      anthropic_version = anthropic_version,
      verbose           = verbose
    )

    if (is.null(batch_output_path)) {
      batch_output_path <- tempfile(
        pattern = "anthropic-batch-output-",
        fileext = ".jsonl"
      )
    }

    anthropic_download_batch_results(
      batch_id          = final_batch$id,
      output_path       = batch_output_path,
      api_key           = api_key,
      anthropic_version = anthropic_version
    )

    out_path <- batch_output_path
    results <- parse_anthropic_batch_output(batch_output_path)
  }

  list(
    batch_input_path  = batch_input_path,
    batch_output_path = out_path,
    file              = NULL,
    batch             = final_batch,
    results           = results
  )
}
