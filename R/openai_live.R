#' @importFrom httr2 resp_status
NULL

#' Live OpenAI comparison for a single pair of samples
#'
#' This function sends a single pairwise comparison prompt to the OpenAI API
#' and parses the result into a small tibble. It is the live / on-demand
#' analogue of \code{\link{build_openai_batch_requests}} plus
#' \code{\link{parse_openai_batch_output}}.
#'
#' It supports both the Chat Completions endpoint ("/v1/chat/completions") and
#' the Responses endpoint ("/v1/responses", for example gpt-5.1 with reasoning),
#' using the same prompt template and model / parameter rules as the batch
#' pipeline.
#'
#' For the Responses endpoint, the function collects:
#' \itemize{
#'   \item Reasoning / "thoughts" text (if available) into the \code{thoughts}
#'         column. Reasoning summaries are typically provided on the
#'         \code{type = "reasoning"} output item under \code{summary}.
#'   \item Visible assistant output into the \code{content} column, taken from
#'         the \code{type = "message"} output item's \code{content[[*]]$text}.
#' }
#' Reasoning text is not prefixed into \code{content}; instead it is kept
#' separate in \code{thoughts} for consistency with Anthropic and Gemini.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model OpenAI model name (for example "gpt-4.1", "gpt-5.1").
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param endpoint Which OpenAI endpoint to use. One of
#'   \code{"chat.completions"} or \code{"responses"}.
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   \code{"</BETTER_SAMPLE>"}.
#' @param api_key Optional OpenAI API key. Defaults to
#'   \code{Sys.getenv("OPENAI_API_KEY")}.
#' @param include_raw Logical; if TRUE, adds a list-column \code{raw_response}
#'   containing the parsed JSON body returned by OpenAI (or NULL on parse
#'   failure). This is useful for debugging parsing problems.
#' @param ... Additional OpenAI parameters, for example
#'   \code{temperature}, \code{top_p}, \code{logprobs}, \code{reasoning},
#'   and (optionally) \code{include_thoughts}. The same validation rules for
#'   gpt-5 models are applied as in \code{\link{build_openai_batch_requests}}.
#'   When using the Responses endpoint with reasoning models, you can request
#'   reasoning summaries in the \code{thoughts} column by setting
#'   \code{endpoint = "responses"}, a non-"none" reasoning effort, and
#'   \code{include_thoughts = TRUE}.
#'
#' @return A tibble with one row and columns:
#' \describe{
#'   \item{custom_id}{ID string of the form \code{"LIVE_<ID1>_vs_<ID2>"}.}
#'   \item{ID1, ID2}{The sample IDs you supplied.}
#'   \item{model}{Model name reported by the API.}
#'   \item{object_type}{OpenAI object type (for example "chat.completion" or
#'     "response").}
#'   \item{status_code}{HTTP-style status code (200 if successful).}
#'   \item{error_message}{Error message if something goes wrong; otherwise NA.}
#'   \item{thoughts}{Reasoning / thinking summary text when available,
#'     otherwise NA.}
#'   \item{content}{Concatenated text from the assistant's visible output. For
#'     the Responses endpoint this is taken from the \code{type = "message"}
#'     output items and does not include reasoning summaries.}
#'   \item{better_sample}{"SAMPLE_1", "SAMPLE_2", or NA.}
#'   \item{better_id}{ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen,
#'     otherwise NA.}
#'   \item{prompt_tokens}{Prompt / input token count (if reported).}
#'   \item{completion_tokens}{Completion / output token count (if reported).}
#'   \item{total_tokens}{Total token count (if reported).}
#'   \item{raw_response}{(Optional) list-column containing the parsed JSON
#'     body.}
#' }
#'
#' @export
openai_compare_pair_live <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  include_raw = FALSE,
  ...
) {
  endpoint <- match.arg(endpoint)

  if (!is.character(ID1) || length(ID1) != 1L) {
    stop("`ID1` must be a single character.", call. = FALSE)
  }
  if (!is.character(ID2) || length(ID2) != 1L) {
    stop("`ID2` must be a single character.", call. = FALSE)
  }
  if (!is.character(text1) || length(text1) != 1L) {
    stop("`text1` must be a single character.", call. = FALSE)
  }
  if (!is.character(text2) || length(text2) != 1L) {
    stop("`text2` must be a single character.", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1L) {
    stop("`model` must be a single character.", call. = FALSE)
  }

  dots <- list(...)
  temperature <- dots$temperature %||% NULL
  top_p <- dots$top_p %||% NULL
  logprobs <- dots$logprobs %||% NULL
  reasoning_effort <- dots$reasoning %||% NULL
  include_thoughts <- dots$include_thoughts %||% FALSE

  is_gpt5 <- grepl("^gpt-5", model)
  is_gpt51 <- grepl("^gpt-5\\.1", model)

  if (is_gpt51) {
    if (!is.null(reasoning_effort) && reasoning_effort != "none") {
      if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
        stop(
          "For gpt-5.1 with reasoning effort not equal to 'none', ",
          "temperature, top_p, and logprobs must be NULL.",
          call. = FALSE
        )
      }
    }
  } else if (is_gpt5) {
    if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
      stop(
        "For gpt-5* models other than gpt-5.1, temperature, top_p, ",
        "and logprobs must be NULL.",
        call. = FALSE
      )
    }
  }

  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  if (endpoint == "chat.completions") {
    body <- list(
      model = model,
      messages = list(
        list(
          role    = "user",
          content = prompt
        )
      )
    )
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p)) body$top_p <- top_p
    if (!is.null(logprobs)) body$logprobs <- logprobs

    path <- "/chat/completions"
  } else {
    body <- list(
      model = model,
      input = prompt
    )

    if (!is.null(reasoning_effort)) {
      # Always set effort; optionally request a reasoning summary when
      # include_thoughts is TRUE.
      reasoning_list <- list(effort = reasoning_effort)
      if (isTRUE(include_thoughts)) {
        reasoning_list$summary <- "auto"
      }
      body$reasoning <- reasoning_list
    }

    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p)) body$top_p <- top_p
    if (!is.null(logprobs)) body$logprobs <- logprobs

    path <- "/responses"
  }

  # Build request, add JSON body, and perform with retry-aware helper
  req <- .openai_request(path, api_key)
  req <- .openai_req_body_json(req, body = body)

  resp <- .openai_req_perform(req)

  status_code <- .openai_resp_status(resp)
  error_message <- NA_character_

  body_parsed <- tryCatch(
    .openai_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  # Default values if parsing fails
  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = NA_character_,
      object_type       = NA_character_,
      status_code       = status_code,
      error_message     = "Failed to parse response body as JSON.",
      thoughts          = NA_character_,
      content           = NA_character_,
      better_sample     = NA_character_,
      better_id         = NA_character_,
      prompt_tokens     = NA_real_,
      completion_tokens = NA_real_,
      total_tokens      = NA_real_
    )

    if (include_raw) {
      res$raw_response <- list(NULL)
    }

    return(res)
  }

  body <- body_parsed
  object_type <- body$object %||% NA_character_
  model_name <- body$model %||% NA_character_

  # Error handling: OpenAI uses an "error" object for API errors
  if (!is.null(body$error)) {
    msg <- body$error$message %||% "Unknown error from OpenAI."
    error_message <- as.character(msg)
  } else if (status_code >= 400L) {
    error_message <- paste0("HTTP ", status_code, " from OpenAI.")
  }

  thoughts <- NA_character_
  content <- NA_character_

  if (identical(object_type, "chat.completion")) {
    choices <- body$choices %||% list()
    if (length(choices) >= 1L) {
      message_obj <- choices[[1]]$message
      if (!is.null(message_obj) && !is.null(message_obj$content)) {
        content <- as.character(message_obj$content)
      }
    }
  } else if (identical(object_type, "response")) {
    # /v1/responses: collect reasoning summaries (if any) into `thoughts`
    # and visible assistant text into `content`.
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
            if (!is.null(b$text)) {
              message_chunks <- c(
                message_chunks,
                as.character(b$text %||% "")
              )
            }
          }
        }
      }
    }

    # Backwards compatibility: older fixtures store reasoning summary at
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
      # Character summaries like "auto"/"detailed" are ignored here.
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

  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix),
      content,
      fixed = TRUE
    )) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix),
      content,
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
  prompt_tokens <- usage$prompt_tokens %||% usage$input_tokens %||% NA_real_
  completion_tokens <- usage$completion_tokens %||% usage$output_tokens %||%
    NA_real_
  total_tokens <- usage$total_tokens %||% NA_real_

  res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = model_name,
    object_type       = object_type,
    status_code       = status_code,
    error_message     = error_message,
    thoughts          = thoughts,
    content           = content,
    better_sample     = better_sample,
    better_id         = better_id,
    prompt_tokens     = as.numeric(prompt_tokens),
    completion_tokens = as.numeric(completion_tokens),
    total_tokens      = as.numeric(total_tokens)
  )

  if (include_raw) {
    res$raw_response <- list(body)
  }

  res
}

#' Live OpenAI comparisons for a tibble of pairs
#'
#' This is a thin row-wise wrapper around
#' \code{\link{openai_compare_pair_live}}. It takes a tibble of pairs
#'  (ID1 / text1 / ID2 / text2), submits each pair to
#' the OpenAI API, and binds the results into a single tibble.
#'
#' The output has the same columns as \code{\link{openai_compare_pair_live}},
#' with one row per pair, making it easy to pass into
#' \code{\link{build_bt_data}} and \code{\link{fit_bt_model}}.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}. Typically created by
#'   \code{\link{make_pairs}}, \code{\link{sample_pairs}}, and
#'   \code{\link{randomize_pair_order}}.
#' @param model OpenAI model name (for example "gpt-4.1", "gpt-5.1").
#' @param trait_name Trait name to pass to \code{openai_compare_pair_live}.
#' @param trait_description Trait description to pass to
#'   \code{openai_compare_pair_live}.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param endpoint Which OpenAI endpoint to target. One of
#'   \code{"chat.completions"} or \code{"responses"}.
#' @param api_key Optional OpenAI API key.
#' @param verbose Logical; if TRUE, prints status, timing, and result summaries.
#' @param status_every Integer; print status / timing for every
#'   \code{status_every}-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if TRUE, shows a textual progress bar.
#' @param include_raw Logical; if TRUE, each row of the returned tibble will
#'   include a \code{raw_response} list-column with the parsed JSON body from
#'   OpenAI.
#' @param ... Additional OpenAI parameters (temperature, top_p, logprobs,
#'   reasoning, and so on) passed on to \code{openai_compare_pair_live}.
#'
#' @return A tibble with one row per pair and the same columns as
#'   \code{\link{openai_compare_pair_live}}, including a \code{thoughts}
#'   column for reasoning summaries (when available).
#'
#' @examples
#' \dontrun{
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
#' # Live comparisons for multiple pairs
#' res_live <- submit_openai_pairs_live(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   temperature       = 0,
#'   verbose           = TRUE,
#'   status_every      = 2,
#'   progress          = TRUE,
#'   include_raw       = FALSE
#' )
#'
#' res_live$better_id
#'
#' # Using gpt-5.1 with reasoning = "low" on the responses endpoint
#' res_live_gpt5 <- submit_openai_pairs_live(
#'   pairs             = pairs,
#'   model             = "gpt-5.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "responses",
#'   reasoning         = "low",
#'   temperature       = NULL,
#'   top_p             = NULL,
#'   logprobs          = NULL,
#'   verbose           = TRUE,
#'   status_every      = 3,
#'   progress          = TRUE,
#'   include_raw       = TRUE
#' )
#'
#' str(res_live_gpt5$raw_response[[1]], max.level = 2)
#' }
#'
#' @export
submit_openai_pairs_live <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  api_key = Sys.getenv("OPENAI_API_KEY"),
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  ...
) {
  endpoint <- match.arg(endpoint)

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

  n <- nrow(pairs)
  if (n == 0L) {
    res <- tibble::tibble(
      custom_id         = character(0),
      ID1               = character(0),
      ID2               = character(0),
      model             = character(0),
      object_type       = character(0),
      status_code       = integer(0),
      error_message     = character(0),
      thoughts          = character(0),
      content           = character(0),
      better_sample     = character(0),
      better_id         = character(0),
      prompt_tokens     = numeric(0),
      completion_tokens = numeric(0),
      total_tokens      = numeric(0)
    )
    if (include_raw) {
      res$raw_response <- list()
    }
    return(res)
  }

  if (!is.numeric(status_every) || length(status_every) != 1L ||
    status_every < 1) {
    stop("`status_every` must be a single positive integer.", call. = FALSE)
  }
  status_every <- as.integer(status_every)

  fmt_secs <- function(x) sprintf("%.1fs", x)

  if (verbose) {
    message(sprintf(
      "Submitting %d live pair(s) for comparison (model=%s, endpoint=%s)...",
      n, model, endpoint
    ))
  }

  pb <- NULL
  if (progress && n > 0L) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  }

  start_time <- Sys.time()
  out <- vector("list", n)

  for (i in seq_len(n)) {
    show_status <- verbose && (i %% status_every == 1L)

    if (show_status) {
      message(sprintf(
        "[Live pair %d of %d] Comparing %s vs %s ...",
        i, n, pairs$ID1[i], pairs$ID2[i]
      ))
    }

    res <- openai_compare_pair_live(
      ID1               = as.character(pairs$ID1[i]),
      text1             = as.character(pairs$text1[i]),
      ID2               = as.character(pairs$ID2[i]),
      text2             = as.character(pairs$text2[i]),
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      endpoint          = endpoint,
      api_key           = api_key,
      include_raw       = include_raw,
      ...
    )

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }

    if (!is.na(res$error_message)) {
      message(sprintf(
        "    WARNING: API Error (status %s): %s",
        res$status_code, res$error_message
      ))
    } else if (show_status) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg <- elapsed / i
      remain <- n - i
      est_rem <- avg * remain

      message(sprintf(
        "    Result: %s preferred (%s) | tokens: prompt=%s, completion=%s,
        total=%s",
        res$better_id,
        res$better_sample,
        res$prompt_tokens,
        res$completion_tokens,
        res$total_tokens
      ))
      message(sprintf(
        "    Timing: elapsed=%s | avg/pair=%s | est remaining=%s",
        fmt_secs(elapsed),
        fmt_secs(avg),
        fmt_secs(est_rem)
      ))
    }

    out[[i]] <- res
  }

  if (!is.null(pb)) {
    close(pb)
  }

  if (verbose) {
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time,
      units = "secs"
    ))
    avg <- total_elapsed / n
    message(sprintf(
      "Completed %d live pair(s) in %s (avg %.2fs per pair).",
      n, fmt_secs(total_elapsed), avg
    ))
  }

  dplyr::bind_rows(out)
}
