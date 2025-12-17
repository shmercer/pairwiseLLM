#' Internal: parse a Gemini GenerateContentResponse into the standard tibble row
#'
#' For batch responses, Gemini 3 Pro currently typically returns:
#'   * `candidates[[1]]$content$parts[[1]]$text`             = final answer
#'   * `candidates[[1]]$content$parts[[1]]$thoughtSignature` = opaque signature
#'   * `usageMetadata$thoughtsTokenCount`                    =
#'      hidden reasoning tokens
#'
#' When `include_thoughts = TRUE` and >= 2 parts are present, we mirror the live
#' behavior: first part = `thoughts`, remaining parts = `content`.
#' When only one part is present, we treat it as `content` and leave `thoughts`
#' as NA (batch isn't returning visible thoughts text).
#'
#' @keywords internal
.parse_gemini_pair_response <- function(
  custom_id,
  ID1,
  ID2,
  response,
  include_thoughts = FALSE
) {
  resp <- response

  # Sometimes we see list(response = [ {candidates=...} ])
  if (!is.null(resp$response) && is.null(resp$candidates) &&
    is.list(resp$response)) {
    resp <- resp$response
  }
  # And that response can be a length-1 array
  if (is.list(resp) && is.null(resp$candidates) && length(resp) == 1L &&
    is.list(resp[[1]]) && !is.null(resp[[1]]$candidates)) {
    resp <- resp[[1]]
  }

  # Error-shaped response
  if (!is.null(resp$error) && is.null(resp$candidates)) {
    return(tibble::tibble(
      custom_id            = custom_id,
      ID1                  = ID1,
      ID2                  = ID2,
      model                = NA_character_,
      object_type          = "generateContent",
      status_code          = 200L,
      result_type          = "errored",
      error_message        = resp$error$message %||% NA_character_,
      thoughts             = NA_character_,
      thought_signature    = NA_character_,
      thoughts_token_count = NA_real_,
      content              = NA_character_,
      better_sample        = NA_character_,
      better_id            = NA_character_,
      prompt_tokens        = NA_real_,
      completion_tokens    = NA_real_,
      total_tokens         = NA_real_
    ))
  }

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------
  get_first_candidate <- function(x) {
    cands <- x$candidates
    if (is.null(cands)) {
      return(NULL)
    }

    if (is.data.frame(cands)) {
      if (nrow(cands) == 0L) {
        return(NULL)
      }
      cand <- lapply(cands[1, , drop = FALSE], function(v) {
        if (is.list(v) && length(v) == 1L) v[[1]] else v
      })
      return(cand)
    }

    if (is.list(cands) && length(cands) > 0L) {
      return(cands[[1]])
    }

    NULL
  }

  get_parts_list <- function(content_obj) {
    if (is.null(content_obj)) {
      return(list())
    }

    if (is.data.frame(content_obj)) {
      if ("parts" %in% names(content_obj)) {
        parts_obj <- content_obj$parts[[1]]
      } else {
        parts_obj <- content_obj
      }
    } else if (is.list(content_obj) && !is.null(content_obj$parts)) {
      parts_obj <- content_obj$parts
    } else {
      parts_obj <- content_obj
    }

    if (is.null(parts_obj)) {
      list()
    } else if (is.data.frame(parts_obj)) {
      lapply(seq_len(nrow(parts_obj)), function(i) {
        as.list(parts_obj[i, , drop = FALSE])
      })
    } else if (is.list(parts_obj)) {
      parts_obj
    } else {
      list(parts_obj)
    }
  }

  get_part_text <- function(p) {
    if (is.null(p)) {
      return("")
    }
    if (is.list(p) && !is.data.frame(p) && !is.null(p$text)) {
      return(as.character(p$text %||% ""))
    }
    if (is.data.frame(p) && "text" %in% names(p)) {
      return(as.character(p$text[[1]] %||% ""))
    }
    if (is.character(p)) {
      return(paste(p, collapse = ""))
    }
    ""
  }

  get_part_thought_signature <- function(p) {
    if (is.null(p)) {
      return(NA_character_)
    }
    if (is.list(p) && !is.data.frame(p) && !is.null(p$thoughtSignature)) {
      return(as.character(p$thoughtSignature %||% NA_character_))
    }
    if (is.data.frame(p) && "thoughtSignature" %in% names(p)) {
      return(as.character(p$thoughtSignature[[1]] %||% NA_character_))
    }
    NA_character_
  }

  find_named <- function(x, name) {
    if (!is.list(x)) {
      return(NULL)
    }
    if (!is.null(x[[name]])) {
      return(x[[name]])
    }
    for (el in x) {
      if (is.list(el)) {
        found <- find_named(el, name)
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }

  as_num_scalar <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }
    v <- suppressWarnings(as.numeric(unlist(x,
      recursive = TRUE,
      use.names = FALSE
    )))
    if (!length(v)) {
      return(NA_real_)
    }
    v[1]
  }

  as_chr_scalar <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    v <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
    if (!length(v)) {
      return(NA_character_)
    }
    v[1]
  }

  # -------------------------------------------------------------------
  # Extract thoughts + content
  # -------------------------------------------------------------------
  cand <- get_first_candidate(resp)
  parts <- list()
  if (!is.null(cand) && !is.null(cand$content)) {
    parts <- get_parts_list(cand$content)
  }

  thoughts <- NA_character_
  thought_signature <- NA_character_
  content <- NA_character_

  if (length(parts) > 0L) {
    # First part may carry a thoughtSignature in batch mode
    thought_signature <- get_part_thought_signature(parts[[1]])

    if (isTRUE(include_thoughts) && length(parts) >= 2L) {
      # Live-style: first part = thoughts, rest = content
      t_text <- get_part_text(parts[[1]])
      c_texts <- vapply(parts[-1], get_part_text, FUN.VALUE = character(1L))
      c_texts <- c_texts[nzchar(c_texts)]

      thoughts <- if (nzchar(t_text)) t_text else NA_character_
      content <- if (length(c_texts)) {
        paste(c_texts, collapse = "")
      } else {
        NA_character_
      }
    } else {
      # Default / batch-style: collapse everything into content
      c_texts <- vapply(parts, get_part_text, FUN.VALUE = character(1L))
      c_texts <- c_texts[nzchar(c_texts)]
      content <- if (length(c_texts)) {
        paste(c_texts, collapse = "")
      } else {
        NA_character_
      }
    }
  }

  # -------------------------------------------------------------------
  # Parse <BETTER_SAMPLE>
  # -------------------------------------------------------------------
  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl("<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>", content,
      fixed = TRUE
    )) {
      better_sample <- "SAMPLE_1"
    } else if (grepl("<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>", content,
      fixed = TRUE
    )) {
      better_sample <- "SAMPLE_2"
    }
  }

  better_id <- NA_character_
  if (!is.na(better_sample)) {
    better_id <- if (identical(better_sample, "SAMPLE_1")) ID1 else ID2
  }

  # -------------------------------------------------------------------
  # Usage + model
  # -------------------------------------------------------------------
  usage_obj <- find_named(resp, "usageMetadata")

  prompt_tokens_raw <- if (!is.null(usage_obj)) {
    find_named(usage_obj, "promptTokenCount")
  } else {
    NULL
  }
  completion_tokens_raw <- if (!is.null(usage_obj)) {
    find_named(usage_obj, "candidatesTokenCount")
  } else {
    NULL
  }
  total_tokens_raw <- if (!is.null(usage_obj)) {
    find_named(usage_obj, "totalTokenCount")
  } else {
    NULL
  }
  thoughts_token_count_raw <- if (!is.null(usage_obj)) {
    find_named(usage_obj, "thoughtsTokenCount")
  } else {
    NULL
  }

  prompt_tokens <- as_num_scalar(prompt_tokens_raw)
  completion_tokens <- as_num_scalar(completion_tokens_raw)
  total_tokens <- as_num_scalar(total_tokens_raw)
  thoughts_token_count <- as_num_scalar(thoughts_token_count_raw)

  # Prefer modelVersion if present; fall back to model
  model_raw <- find_named(resp, "modelVersion")
  if (is.null(model_raw)) {
    model_raw <- find_named(resp, "model")
  }
  model <- as_chr_scalar(model_raw)

  tibble::tibble(
    custom_id            = custom_id,
    ID1                  = ID1,
    ID2                  = ID2,
    model                = model,
    object_type          = "generateContent",
    status_code          = 200L,
    result_type          = "succeeded",
    error_message        = NA_character_,
    thoughts             = thoughts,
    thought_signature    = thought_signature,
    thoughts_token_count = thoughts_token_count,
    content              = content,
    better_sample        = better_sample,
    better_id            = better_id,
    prompt_tokens        = prompt_tokens,
    completion_tokens    = completion_tokens,
    total_tokens         = total_tokens
  )
}

#' Build Gemini batch requests from a tibble of pairs
#'
#' This helper converts a tibble of writing pairs into a set of Gemini
#' GenerateContent requests suitable for use with the Batch API
#' (\code{models/*:batchGenerateContent}).
#'
#' Each pair receives a unique \code{custom_id} of the form
#' \code{"GEM_<ID1>_vs_<ID2>"} and a corresponding request object containing
#' the prompt and generation configuration.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}. Typically created by
#'   \code{\link{make_pairs}}, \code{\link{sample_pairs}}, and
#'   \code{\link{randomize_pair_order}}.
#' @param model Gemini model name, for example \code{"gemini-3-pro-preview"}.
#'   This parameter is not embedded in each request object (the model is
#'   provided via the path), but is included here for symmetry with other
#'   backends and potential validation.
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text description of the trait or rubric.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}. The template should embed your full
#'   instructions, rubric text, and \verb{<BETTER_SAMPLE>} tagging convention.
#' @param thinking_level One of \code{"low"}, \code{"medium"}, or \code{"high"}.
#'   This is mapped to Gemini's \code{thinkingConfig.thinkingLevel}, where
#'   \code{"low"} maps to "Low" and both \code{"medium"} and \code{"high"} map
#'   to "High". "Medium" currently behaves like "High".
#' @param custom_id_prefix Prefix for the \code{custom_id} field. Defaults to
#'   \code{"GEM"} so that IDs take the form \code{"GEM_<ID1>_vs_<ID2>"}.
#' @param temperature Optional numeric temperature. If \code{NULL}, it is
#'   omitted and Gemini uses its own default.
#' @param top_p Optional nucleus sampling parameter. If \code{NULL}, omitted.
#' @param top_k Optional top-k sampling parameter. If \code{NULL}, omitted.
#' @param max_output_tokens Optional integer. If \code{NULL}, omitted.
#' @param include_thoughts Logical; if \code{TRUE}, sets
#'   \code{thinkingConfig.includeThoughts = TRUE} so that Gemini returns
#'   visible chain-of-thought. For most pairwise scoring use cases this should
#'   remain \code{FALSE}.
#' @param ... Reserved for future extensions. Any \code{thinking_budget}
#'   entries are ignored (Gemini 3 Pro does not support thinking budgets).
#'
#' @return A tibble with one row per pair and two main columns:
#' \describe{
#'   \item{custom_id}{Character ID of the form
#'      \code{"<PREFIX>_<ID1>_vs_<ID2>"}.}
#'   \item{request}{List-column containing the Gemini GenerateContent request
#'     object for each pair.}
#' }
#' @examples
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
#' reqs <- build_gemini_batch_requests(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   thinking_level    = "low",
#'   include_thoughts  = TRUE
#' )
#'
#' reqs
#'
#' @export
build_gemini_batch_requests <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  thinking_level = c("low", "medium", "high"),
  custom_id_prefix = "GEM",
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  include_thoughts = FALSE,
  ...
) {
  thinking_level <- match.arg(thinking_level)

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

  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty character scalar.", call. = FALSE)
  }

  dots <- list(...)
  if (!is.null(dots$thinking_budget)) {
    warning(
      "`thinking_budget` is ignored for Gemini 3. ",
      "Use `thinking_level` instead and do not supply both.",
      call. = FALSE
    )
  }

  # Map R-level thinking_level to Gemini values
  tl_map <- c(low = "Low", medium = "High", high = "High")
  if (identical(thinking_level, "medium")) {
    warning(
      "`thinking_level = \"medium\"` is not yet officially
      documented for the REST API; ",
      "mapping to \"High\" internally.",
      call. = FALSE
    )
  }

  get_request_for_pair <- function(ID1, text1, ID2, text2) {
    prompt <- build_prompt(
      template   = prompt_template,
      trait_name = trait_name,
      trait_desc = trait_description,
      text1      = text1,
      text2      = text2
    )

    generation_config <- list()

    if (!is.null(temperature)) {
      generation_config$temperature <- temperature
    }
    if (!is.null(top_p)) {
      generation_config$topP <- top_p
    }
    if (!is.null(top_k)) {
      generation_config$topK <- top_k
    }
    if (!is.null(max_output_tokens)) {
      generation_config$maxOutputTokens <- max_output_tokens
    }

    thinking_config <- list(
      includeThoughts = isTRUE(include_thoughts),
      thinkingLevel   = tl_map[[thinking_level]]
    )

    generation_config$thinkingConfig <- thinking_config

    list(
      contents = list(
        list(
          role = "user",
          parts = list(
            list(text = prompt)
          )
        )
      ),
      generationConfig = generation_config
    )
  }

  out <- vector("list", nrow(pairs))
  custom_ids <- character(nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    ID1 <- as.character(pairs$ID1[i])
    text1 <- as.character(pairs$text1[i])
    ID2 <- as.character(pairs$ID2[i])
    text2 <- as.character(pairs$text2[i])

    custom_ids[i] <- sprintf("%s_%s_vs_%s", custom_id_prefix, ID1, ID2)
    out[[i]] <- get_request_for_pair(ID1, text1, ID2, text2)
  }

  tibble::tibble(
    custom_id = custom_ids,
    ID1       = pairs$ID1,
    ID2       = pairs$ID2,
    request   = out
  )
}

#' Create a Gemini Batch job from request objects
#'
#' This is a thin wrapper around the REST endpoint
#' \code{/v1beta/models/<MODEL>:batchGenerateContent}. It accepts a list of
#' GenerateContent request objects and returns the created Batch job.
#'
#' Typically you will not call this directly; instead, use
#' \code{\link{run_gemini_batch_pipeline}} which builds requests from a tibble
#' of pairs, creates the batch, polls for completion, and parses the results.
#'
#' @param requests List of GenerateContent request objects, each of the form
#'   \code{list(contents = ..., generationConfig = ...)}. You can obtain this
#'   list from the output of \code{\link{build_gemini_batch_requests}} via
#'   \code{batch$request}.
#' @param model Gemini model name, for example \code{"gemini-3-pro-preview"}.
#' @param api_key Optional Gemini API key. Defaults to
#'   \code{Sys.getenv("GEMINI_API_KEY")}.
#' @param api_version API version string for the path; defaults to
#'   \code{"v1beta"}.
#' @param display_name Optional display name for the batch.
#'
#' @return A list representing the Batch job object returned by Gemini.
#'   Important fields include \code{name}, \code{metadata$state},
#'    and (after completion) \code{response$inlinedResponses} or
#'   \code{response$responsesFile}.
#'
#' @examples
#' # --- Offline preparation: build GenerateContent requests ---
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 2, seed = 123)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' batch_tbl <- build_gemini_batch_requests(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   thinking_level    = "low"
#' )
#'
#' # Extract the list of request objects
#' requests <- batch_tbl$request
#'
#' # Inspect a single GenerateContent request (purely local)
#' requests[[1]]
#'
#' # --- Online step: create the Gemini Batch job ---
#' # Requires network access and a valid Gemini API key.
#' \dontrun{
#' batch <- gemini_create_batch(
#'   requests = requests,
#'   model    = "gemini-3-pro-preview"
#' )
#'
#' batch$name
#' batch$metadata$state
#' }
#'
#' @export
gemini_create_batch <- function(
  requests,
  model,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  display_name = NULL
) {
  if (!is.list(requests) || length(requests) == 0L) {
    stop("`requests` must be a non-empty list of request objects.",
      call. = FALSE
    )
  }

  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty character scalar.", call. = FALSE)
  }

  req_items <- lapply(requests, function(r) {
    list(request = r)
  })

  if (is.null(display_name) || !nzchar(display_name)) {
    display_name <- sprintf(
      "pairwiseLLM-gemini-batch-%s",
      format(Sys.time(), "%Y%m%d-%H%M%S")
    )
  }

  body <- list(
    batch = list(
      display_name = display_name,
      input_config = list(
        requests = list(
          requests = req_items
        )
      )
    )
  )

  path <- sprintf("/%s/models/%s:batchGenerateContent", api_version, model)

  req <- .gemini_request(path = path, api_key = api_key)
  req <- .gemini_req_body_json(req, body = body)
  resp <- .gemini_req_perform(req)

  .gemini_resp_body_json(resp, simplifyVector = TRUE)
}

#' Retrieve a Gemini Batch job by name
#'
#' This retrieves the latest state of a Batch job using its \code{name} as
#' returned by \code{\link{gemini_create_batch}}.
#'
#' It corresponds to a GET request on \code{/v1beta/<BATCH_NAME>}, where
#' \code{BATCH_NAME} is a string such as \code{"batches/123456"}.
#'
#' @param batch_name Character scalar giving the batch name.
#' @param api_key Optional Gemini API key. Defaults to
#'   \code{Sys.getenv("GEMINI_API_KEY")}.
#' @param api_version API version string for the path; defaults to
#'   \code{"v1beta"}.
#'
#' @return A list representing the Batch job object.
#'
#' @examples
#' # Offline: basic batch name validation / object you would pass
#' batch_name <- "batches/123456"
#'
#' # Online: retrieve the batch state from Gemini (requires API key + network)
#' \dontrun{
#' batch <- gemini_get_batch(batch_name = batch_name)
#' batch$name
#' batch$metadata$state
#' }
#'
#' @export
gemini_get_batch <- function(
  batch_name,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta"
) {
  if (!is.character(batch_name) || length(batch_name) != 1L ||
    !nzchar(batch_name)) {
    stop("`batch_name` must be a non-empty character scalar.", call. = FALSE)
  }

  path <- sprintf("/%s/%s", api_version, batch_name)

  req <- .gemini_request(path = path, api_key = api_key)
  resp <- .gemini_req_perform(req)

  .gemini_resp_body_json(resp, simplifyVector = TRUE)
}

#' Poll a Gemini Batch job until completion
#'
#' This helper repeatedly calls \code{\link{gemini_get_batch}} until the
#' batch's \code{metadata$state} enters a terminal state or a time limit is
#' reached. For the REST API, states have the form "BATCH_STATE_*".
#'
#' @param batch_name Character scalar giving the batch name.
#' @param interval_seconds Polling interval in seconds. Defaults to 60.
#' @param timeout_seconds Maximum total waiting time in seconds. Defaults to
#'   24 hours (86400 seconds).
#' @param api_key Optional Gemini API key. Defaults to
#'   \code{Sys.getenv("GEMINI_API_KEY")}.
#' @param api_version API version string for the path; defaults to
#'   \code{"v1beta"}.
#' @param verbose Logical; if \code{TRUE}, prints progress messages.
#'
#' @return The final Batch job object as returned by
#'   \code{\link{gemini_get_batch}}.
#'
#' @examples
#' # Offline: polling parameters and batch name are plain R objects
#' batch_name <- "batches/123456"
#'
#' # Online: poll until the batch reaches a terminal state (requires network)
#' \dontrun{
#' final_batch <- gemini_poll_batch_until_complete(
#'   batch_name       = batch_name,
#'   interval_seconds = 10,
#'   timeout_seconds  = 600,
#'   verbose          = TRUE
#' )
#' final_batch$metadata$state
#' }
#'
#' @export
gemini_poll_batch_until_complete <- function(
  batch_name,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  verbose = TRUE
) {
  if (!is.character(batch_name) || length(batch_name) != 1L ||
    !nzchar(batch_name)) {
    stop("`batch_name` must be a non-empty character scalar.", call. = FALSE)
  }

  start_time <- Sys.time()
  last_batch <- NULL

  terminal_states <- c(
    "BATCH_STATE_SUCCEEDED",
    "BATCH_STATE_FAILED",
    "BATCH_STATE_CANCELLED",
    "BATCH_STATE_EXPIRED"
  )

  repeat {
    batch <- gemini_get_batch(
      batch_name = batch_name,
      api_key = api_key,
      api_version = api_version
    )
    last_batch <- batch

    state <- if (!is.null(batch$metadata$state)) {
      batch$metadata$state
    } else {
      NA_character_
    }

    if (verbose) {
      message(sprintf(
        "Batch %s state: %s",
        if (!is.null(batch$name)) batch$name else "<unknown>",
        state
      ))
    }

    if (!is.na(state) && state %in% terminal_states) {
      break
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.infinite(timeout_seconds) && elapsed > timeout_seconds) {
      if (verbose) {
        warning(
          "Timeout reached while waiting for Gemini batch to complete. ",
          "Returning the last retrieved batch object."
        )
      }
      break
    }

    Sys.sleep(interval_seconds)
  }

  last_batch
}

#' Download Gemini Batch results to a JSONL file
#'
#' For inline batch requests, Gemini returns results under
#' \code{response$inlinedResponses$inlinedResponses}. In the v1beta REST API
#' this often comes back as a data frame with one row per request and a
#' \code{"response"} column, where each \code{"response"} is itself a data frame
#'  of \code{GenerateContentResponse} objects.
#'
#' This helper writes those results to a local \code{.jsonl} file where each
#' line is a JSON object of the form:
#'
#' \preformatted{
#' {"custom_id": "<GEM_ID1_vs_ID2>",
#'  "result": {
#'    "type": "succeeded",
#'    "response": { ... GenerateContentResponse ... }
#'  }}
#' }
#'
#' or, when an error occurred:
#'
#' \preformatted{
#' {"custom_id": "<GEM_ID1_vs_ID2>",
#'  "result": {
#'    "type": "errored",
#'    "error": { ... }
#'  }}
#' }
#'
#' @param batch Either a parsed batch object (as returned by
#'   \code{gemini_get_batch()}) or a character batch name such as
#'   \code{"batches/123..."}.
#' @param requests_tbl Tibble/data frame with a \code{custom_id} column in the
#'   same order as the submitted requests.
#' @param output_path Path to the JSONL file to create.
#' @param api_key Optional Gemini API key (used only when
#'   \code{batch} is a name).
#' @param api_version API version (default \code{"v1beta"}).
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' # This example requires a Gemini API key and network access.
#' # It assumes you have already created and run a Gemini batch job.
#' \dontrun{
#' # Name of an existing Gemini batch
#' batch_name <- "batches/123456"
#'
#' # Requests table used to create the batch (must include custom_id)
#' requests_tbl <- tibble::tibble(
#'   custom_id = c("GEM_S01_vs_S02", "GEM_S03_vs_S04")
#' )
#'
#' # Download inline batch results to a local JSONL file
#' out_file <- tempfile(fileext = ".jsonl")
#'
#' gemini_download_batch_results(
#'   batch        = batch_name,
#'   requests_tbl = requests_tbl,
#'   output_path  = out_file
#' )
#'
#' # Inspect the downloaded JSONL
#' readLines(out_file, warn = FALSE)
#' }
#'
#' @export
gemini_download_batch_results <- function(
  batch,
  requests_tbl,
  output_path,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta"
) {
  # Allow passing either the batch object or its name
  if (is.character(batch) && length(batch) == 1L) {
    batch <- gemini_get_batch(
      batch_name = batch,
      api_key = api_key,
      api_version = api_version
    )
  }

  if (!inherits(requests_tbl, "data.frame") ||
    !"custom_id" %in% names(requests_tbl)) {
    stop("`requests_tbl` must be a tibble/data frame with a `custom_id`
         column.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Locate the inline responses in the batch object
  # ---------------------------------------------------------------------------
  inlined_root <- batch$response$inlinedResponses
  if (!is.null(inlined_root$inlinedResponses)) {
    inlined <- inlined_root$inlinedResponses
  } else if (!is.null(inlined_root)) {
    inlined <- inlined_root
  } else {
    stop(
      "Batch does not contain response$inlinedResponses$inlinedResponses. ",
      "File-based batch results are not yet supported by
      gemini_download_batch_results().",
      call. = FALSE
    )
  }

  # At this point, for the inline generateContent batch, `inlined` is a
  # data frame with one row per request and a `response` column that is
  # itself a data frame with rows = requests and columns = candidates,
  # usageMetadata, modelVersion, responseId.
  #
  # We normalise this into a per-request list of GenerateContentResponse
  # objects so that parse_gemini_batch_output() and
  # .parse_gemini_pair_response() can treat them just like live responses.

  if (is.data.frame(inlined) && "response" %in% names(inlined)) {
    resp_df <- inlined$response
  } else if (is.data.frame(inlined)) {
    # Fallback: maybe the data frame itself is already the response table
    resp_df <- inlined
  } else {
    stop(
      "Unsupported structure for inlined responses (class: ",
      paste(class(inlined), collapse = ", "),
      ").",
      call. = FALSE
    )
  }

  if (!is.data.frame(resp_df)) {
    stop(
      "Expected a data.frame for inline responses, but got class: ",
      paste(class(resp_df), collapse = ", "),
      call. = FALSE
    )
  }

  n_res <- nrow(resp_df)
  n_req <- nrow(requests_tbl)
  n <- min(n_req, n_res)

  if (n_req != n_res) {
    warning(
      "Number of inlined responses (", n_res,
      ") does not match number of requests (", n_req, "). ",
      "Matching by position up to min(n)."
    )
  }

  con <- file(output_path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  for (i in seq_len(n)) {
    custom_id <- requests_tbl$custom_id[i]

    # Extract the i-th response row
    response_row <- resp_df[i, , drop = FALSE]

    # Round-trip through JSON to get a clean nested list in the same style
    # as the live GenerateContentResponse, so the existing parser can be reused.
    clean_response <- jsonlite::fromJSON(
      jsonlite::toJSON(response_row, auto_unbox = TRUE, null = "null"),
      simplifyVector = FALSE
    )

    line_obj <- list(
      custom_id = custom_id,
      result = list(
        type     = "succeeded",
        response = clean_response
      )
    )

    json_line <- jsonlite::toJSON(line_obj, auto_unbox = TRUE, null = "null")
    writeLines(json_line, con = con, useBytes = TRUE)
  }

  invisible(output_path)
}

#' Parse Gemini batch JSONL output into a tibble of pairwise results
#'
#' This reads a JSONL file created by [gemini_download_batch_results()] and
#' converts each line into a row that mirrors the structure used for live
#' Gemini calls, including a `thoughts` column when the batch was run with
#' `include_thoughts = TRUE`.
#'
#' @param results_path Path to the JSONL file produced by
#'   [gemini_download_batch_results()].
#' @param requests_tbl Tibble/data frame with at least columns `custom_id`,
#'   `ID1`, `ID2`, and (optionally) `request`. If a `request` list-column is
#'   present, it is used to detect whether `thinkingConfig.includeThoughts`
#'   was enabled for that pair.
#'
#' @return A tibble with one row per request and columns:
#'   * `custom_id`, `ID1`, `ID2`
#'   * `model`, `object_type`, `status_code`, `result_type`, `error_message`
#'   * `thoughts`, `thought_signature`, `thoughts_token_count`
#'   * `content`, `better_sample`, `better_id`
#'   * `prompt_tokens`, `completion_tokens`, `total_tokens`
#'
#' @examples
#' #' # This example assumes you have already:
#' # 1. Built Gemini batch requests with `build_gemini_batch_requests()`
#' # 2. Submitted and completed a batch job via the Gemini API
#' # 3. Downloaded the results using `gemini_download_batch_results()`
#' \dontrun{
#' # Path to a JSONL file created by `gemini_download_batch_results()`
#' results_path <- "gemini_batch_results.jsonl"
#'
#' # Requests table used to build the batch (must contain custom_id, ID1, ID2)
#' # as returned by `build_gemini_batch_requests()`
#' requests_tbl <- readRDS("gemini_batch_requests.rds")
#'
#' # Parse batch output into a tidy tibble of pairwise results
#' results <- parse_gemini_batch_output(
#'   results_path = results_path,
#'   requests_tbl = requests_tbl
#' )
#'
#' results
#' }
#'
#' @export
parse_gemini_batch_output <- function(results_path, requests_tbl) {
  if (!file.exists(results_path)) {
    stop("`results_path` does not exist: ", results_path, call. = FALSE)
  }

  if (!inherits(requests_tbl, "data.frame") ||
    !"custom_id" %in% names(requests_tbl) ||
    !"ID1" %in% names(requests_tbl) ||
    !"ID2" %in% names(requests_tbl)) {
    stop(
      "`requests_tbl` must be a data frame with columns `custom_id`, `ID1`,
      and `ID2`.",
      call. = FALSE
    )
  }

  lines <- readLines(results_path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0L) {
    return(
      tibble::tibble(
        custom_id            = character(0),
        ID1                  = character(0),
        ID2                  = character(0),
        model                = character(0),
        object_type          = character(0),
        status_code          = integer(0),
        result_type          = character(0),
        error_message        = character(0),
        thoughts             = character(0),
        thought_signature    = character(0),
        thoughts_token_count = numeric(0),
        content              = character(0),
        better_sample        = character(0),
        better_id            = character(0),
        prompt_tokens        = numeric(0),
        completion_tokens    = numeric(0),
        total_tokens         = numeric(0)
      )
    )
  }

  # Parse each line individually; if JSON parse fails, create an "errored" stub
  objs <- lapply(
    lines,
    function(z) {
      tryCatch(
        jsonlite::fromJSON(z, simplifyVector = FALSE),
        error = function(e) {
          list(
            custom_id = NULL,
            result = list(
              type = "errored",
              error = list(
                message = paste(
                  "Failed to parse JSON line:",
                  conditionMessage(e)
                )
              )
            )
          )
        }
      )
    }
  )

  # Build lookup from custom_id -> row index in requests_tbl
  req_ids <- as.character(requests_tbl$custom_id)
  line_ids <- vapply(
    objs,
    function(o) as.character(o$custom_id %||% NA_character_),
    FUN.VALUE = character(1L)
  )
  matches <- match(line_ids, req_ids)

  has_request_col <- "request" %in% names(requests_tbl)

  rows <- vector("list", length(objs))

  for (i in seq_along(objs)) {
    obj <- objs[[i]]
    custom_id <- obj$custom_id %||% NA_character_
    result <- obj$result %||% list()
    result_ty <- result$type %||% NA_character_

    idx <- matches[i]
    ID1 <- if (!is.na(idx)) {
      as.character(requests_tbl$ID1[idx])
    } else {
      NA_character_
    }
    ID2 <- if (!is.na(idx)) {
      as.character(requests_tbl$ID2[idx])
    } else {
      NA_character_
    }

    # Detect include_thoughts from original request, if available
    include_thoughts <- FALSE
    if (!is.na(idx) && has_request_col) {
      req_obj <- requests_tbl$request[[idx]]
      if (!is.null(req_obj$generationConfig) &&
        !is.null(req_obj$generationConfig$thinkingConfig) &&
        !is.null(req_obj$generationConfig$thinkingConfig$includeThoughts)) {
        include_thoughts <-
          isTRUE(req_obj$generationConfig$thinkingConfig$includeThoughts)
      }
    }

    if (identical(result_ty, "succeeded")) {
      body <- result$response %||% list()

      row <- .parse_gemini_pair_response(
        custom_id        = custom_id,
        ID1              = ID1,
        ID2              = ID2,
        response         = body,
        include_thoughts = include_thoughts
      )
      row$result_type <- result_ty
      rows[[i]] <- row
    } else if (identical(result_ty, "errored")) {
      err <- result$error %||% list()
      error_msg <- err$message %||% NA_character_

      rows[[i]] <- tibble::tibble(
        custom_id            = custom_id,
        ID1                  = ID1,
        ID2                  = ID2,
        model                = NA_character_,
        object_type          = "generateContent",
        status_code          = NA_integer_,
        result_type          = result_ty,
        error_message        = error_msg,
        thoughts             = NA_character_,
        thought_signature    = NA_character_,
        thoughts_token_count = NA_real_,
        content              = NA_character_,
        better_sample        = NA_character_,
        better_id            = NA_character_,
        prompt_tokens        = NA_real_,
        completion_tokens    = NA_real_,
        total_tokens         = NA_real_
      )
    } else {
      rows[[i]] <- tibble::tibble(
        custom_id            = custom_id,
        ID1                  = ID1,
        ID2                  = ID2,
        model                = NA_character_,
        object_type          = "generateContent",
        status_code          = NA_integer_,
        result_type          = result_ty,
        error_message        = NA_character_,
        thoughts             = NA_character_,
        thought_signature    = NA_character_,
        thoughts_token_count = NA_real_,
        content              = NA_character_,
        better_sample        = NA_character_,
        better_id            = NA_character_,
        prompt_tokens        = NA_real_,
        completion_tokens    = NA_real_,
        total_tokens         = NA_real_
      )
    }
  }

  dplyr::bind_rows(rows)
}

#' Run a Gemini batch pipeline for pairwise comparisons
#'
#' This helper ties together the core batch operations:
#' \enumerate{
#'   \item Build batch requests from a tibble of pairs.
#'   \item Create a Batch job via \code{\link{gemini_create_batch}}.
#'   \item Optionally poll for completion and download results.
#'   \item Parse the JSONL results into a tibble via
#'         \code{\link{parse_gemini_batch_output}}.
#' }
#'
#' The returned list mirrors the structure of
#' \code{\link{run_openai_batch_pipeline}} and
#' \code{\link{run_anthropic_batch_pipeline}}.
#'
#' @param pairs Tibble/data frame of pairs.
#' @param model Gemini model name, for example \code{"gemini-3-pro-preview"}.
#' @param trait_name Trait name.
#' @param trait_description Trait description.
#' @param prompt_template Prompt template string.
#' @param thinking_level One of \code{"low"}, \code{"medium"}, or \code{"high"}.
#' @param batch_input_path Path where the batch input JSON should be written.
#' @param batch_output_path Path where the batch output JSONL should be written
#'   (only used if \code{poll = TRUE}).
#' @param poll Logical; if \code{TRUE}, poll the batch until completion and
#'   parse results. If \code{FALSE}, only create the batch and write the input
#'   file.
#' @param interval_seconds Polling interval when \code{poll = TRUE}.
#' @param timeout_seconds Maximum total waiting time when \code{poll = TRUE}.
#' @param api_key Optional Gemini API key.
#' @param api_version API version string.
#' @param verbose Logical; if \code{TRUE}, prints progress messages.
#' @param include_thoughts Logical; if `TRUE`, sets
#'   `thinkingConfig.includeThoughts = TRUE` in each request, mirroring
#'   [gemini_compare_pair_live()]. Parsed results will include a `thoughts`
#'   column when visible thoughts are returned by the API (currently batch
#'   typically only exposes `thoughtSignature` + `thoughtsTokenCount`).
#' @param ... Additional arguments forwarded to
#'   \code{\link{build_gemini_batch_requests}} (for example
#'   \code{temperature}, \code{top_p}, \code{top_k},
#'   \code{max_output_tokens}).
#'
#' @return A list with elements:
#' \describe{
#'   \item{batch_input_path}{Path to the written batch input JSON.}
#'   \item{batch_output_path}{Path to the batch output JSONL (or \code{NULL}
#'         when \code{poll = FALSE}).}
#'   \item{file}{Reserved for parity with OpenAI/Anthropic; always \code{NULL}
#'         for Gemini inline batches.}
#'   \item{batch}{The created Batch job object.}
#'   \item{results}{Parsed tibble of results (or \code{NULL} when
#'         \code{poll = FALSE}).}
#' }
#'
#' @examples
#' # This example requires:
#' # - A valid Gemini API key (set in GEMINI_API_KEY)
#' # - Internet access
#' # - Billable Gemini API usage
#' \dontrun{
#' # Example pairwise data
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 5, seed = 123)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Run the full Gemini batch pipeline
#' res <- run_gemini_batch_pipeline(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   thinking_level    = "low",
#'   poll              = TRUE,
#'   include_thoughts  = FALSE
#' )
#'
#' # Parsed pairwise comparison results
#' res$results
#'
#' # Inspect batch metadata
#' res$batch
#'
#' # Paths to saved input/output files
#' res$batch_input_path
#' res$batch_output_path
#' }
#'
#' @export
run_gemini_batch_pipeline <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  thinking_level = c("low", "medium", "high"),
  batch_input_path = tempfile(
    pattern = "gemini-batch-input-",
    fileext = ".json"
  ),
  batch_output_path = tempfile(
    pattern = "gemini-batch-output-",
    fileext = ".jsonl"
  ),
  poll = TRUE,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  verbose = TRUE,
  include_thoughts = FALSE,
  ...
) {
  thinking_level <- match.arg(thinking_level)

  req_tbl <- build_gemini_batch_requests(
    pairs             = pairs,
    model             = model,
    trait_name        = trait_name,
    trait_description = trait_description,
    prompt_template   = prompt_template,
    thinking_level    = thinking_level,
    include_thoughts  = include_thoughts,
    ...
  )

  # Save input JSON for reproducibility
  input_payload <- list(
    requests = lapply(seq_len(nrow(req_tbl)), function(i) {
      list(
        custom_id = req_tbl$custom_id[i],
        ID1       = req_tbl$ID1[i], # <-- ADD THIS
        ID2       = req_tbl$ID2[i], # <-- ADD THIS
        request   = req_tbl$request[[i]]
      )
    })
  )

  jsonlite::write_json(
    input_payload,
    path        = batch_input_path,
    auto_unbox  = TRUE,
    pretty      = TRUE,
    null        = "null"
  )

  if (verbose) {
    message(sprintf(
      "Creating Gemini batch for %d pair(s) (model = %s,
      thinking_level = %s)...",
      nrow(req_tbl), model, thinking_level
    ))
  }

  batch <- gemini_create_batch(
    requests     = req_tbl$request,
    model        = model,
    api_key      = api_key,
    api_version  = api_version
  )

  if (!isTRUE(poll)) {
    return(list(
      batch_input_path  = batch_input_path,
      batch_output_path = NULL,
      file              = NULL,
      batch             = batch,
      results           = NULL
    ))
  }

  if (verbose) {
    message("Polling Gemini batch until completion...")
  }

  batch_name <- if (!is.null(batch$name)) {
    batch$name
  } else {
    stop(
      "Gemini batch response did not contain a `name` field.",
      call. = FALSE
    )
  }

  final_batch <- gemini_poll_batch_until_complete(
    batch_name = batch_name,
    interval_seconds = interval_seconds,
    timeout_seconds = timeout_seconds,
    api_key = api_key,
    api_version = api_version,
    verbose = verbose
  )

  gemini_download_batch_results(
    batch        = final_batch,
    requests_tbl = req_tbl,
    output_path  = batch_output_path,
    api_key      = api_key,
    api_version  = api_version
  )

  results <- parse_gemini_batch_output(
    results_path = batch_output_path,
    requests_tbl = req_tbl
  )

  list(
    batch_input_path  = batch_input_path,
    batch_output_path = batch_output_path,
    file              = NULL,
    batch             = final_batch,
    results           = results
  )
}
