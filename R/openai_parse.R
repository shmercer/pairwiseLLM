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

  # Safe helper: x %||% y
  `%||%` <- function(x, y) if (is.null(x)) y else x

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
    better_sample <- NA_character_
    if (!is.na(content)) {
      if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content,
        fixed = TRUE
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
