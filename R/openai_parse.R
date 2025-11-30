##' Parse an OpenAI Batch output JSONL file
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
#'         \code{<BETTER_SAMPLE>...</BETTER_SAMPLE>} tag into \code{content},
#'   \item for Responses objects with reasoning summaries, collects the
#'         reasoning summary text into a separate \code{thoughts} column,
#'   \item determines whether \code{SAMPLE_1} or \code{SAMPLE_2} was
#'         selected and maps that to \code{better_id},
#'   \item collects model name and basic token usage statistics.
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
#'     \item{thoughts}{Reasoning summary text for reasoning-enabled responses
#'           when present in the batch output; otherwise \code{NA}.}
#'     \item{content}{The assistant message content string (the LLM's final
#'           answer), used to locate the \code{<BETTER_SAMPLE>} tag.}
#'     \item{better_sample}{Either \code{"SAMPLE_1"}, \code{"SAMPLE_2"},
#'           or \code{NA} if the tag was not found.}
#'     \item{better_id}{\code{ID1} if \code{SAMPLE_1} was chosen, \code{ID2}
#'           if \code{SAMPLE_2} was chosen, or \code{NA}.}
#'     \item{prompt_tokens}{Prompt/input token count (if reported).}
#'     \item{completion_tokens}{Completion/output token count (if reported).}
#'     \item{total_tokens}{Total tokens (if reported).}
#'   }
#'
#' @examples
#' \dontrun{
#' # Suppose 'openai_batch_gpt4.1_output.jsonl' is an output file you
#' # downloaded from the OpenAI Batch UI after running a gpt-4.1 batch.
#' results <- parse_openai_batch_output("openai_batch_gpt4.1_output.jsonl")
#'
#' # Convert to BT data and fit a model
#' bt_data <- build_bt_data(results)
#' fit     <- fit_bt_model(bt_data, engine = "auto")
#' summary <- summarize_bt_fit(fit)
#' summary
#' }
#'
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @export
parse_openai_batch_output <- function(path,
                                      tag_prefix = "<BETTER_SAMPLE>",
                                      tag_suffix = "</BETTER_SAMPLE>") {
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
    left  <- parts[1]
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

    custom_id   <- obj$custom_id %||% NA_character_
    response    <- obj$response
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
        custom_id         = custom_id,
        ID1               = NA_character_,
        ID2               = NA_character_,
        model             = NA_character_,
        object_type       = NA_character_,
        status_code       = status_code,
        error_message     = error_message,
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

    object_type <- body$object %||% NA_character_
    model       <- body$model %||% NA_character_

    thoughts <- NA_character_
    content  <- NA_character_

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
      # Responses endpoint: separate reasoning summary ("thoughts") from
      # assistant message content ("content").

      # 1) Top-level reasoning summary
      if (!is.null(body$reasoning) && !is.null(body$reasoning$summary)) {
        rs <- body$reasoning$summary
        if (!is.null(rs$text)) {
          thoughts <- as.character(rs$text)
        } else if (is.list(rs)) {
          txts <- vapply(
            rs,
            function(s) if (!is.null(s$text)) as.character(s$text) else "",
            FUN.VALUE = character(1L)
          )
          txts <- txts[nzchar(txts)]
          if (length(txts) > 0L) {
            thoughts <- paste(txts, collapse = "\n\n")
          }
        }
      }

      # 2) Reasoning summary from output items of type "reasoning"
      if (is.na(thoughts) || !nzchar(thoughts)) {
        output_items <- body$output %||% list()
        if (length(output_items) > 0L) {
          for (out_el in output_items) {
            if (!identical(out_el$type, "reasoning")) next
            rs <- out_el$summary
            if (is.null(rs)) next

            if (!is.null(rs$text)) {
              thoughts <- as.character(rs$text)
              break
            } else if (is.list(rs)) {
              txts <- vapply(
                rs,
                function(s) if (!is.null(s$text)) as.character(s$text) else "",
                FUN.VALUE = character(1L)
              )
              txts <- txts[nzchar(txts)]
              if (length(txts) > 0L) {
                thoughts <- paste(txts, collapse = "\n\n")
                break
              }
            }
          }
        }
      }

      # Assistant message content: output items of type "message"
      output_items <- body$output %||% list()
      collected <- character(0)

      if (length(output_items) > 0L) {
        for (out_el in output_items) {
          if (!identical(out_el$type, "message")) next
          blocks <- out_el$content %||% list()
          if (length(blocks) > 0L) {
            for (b in blocks) {
              if (!is.null(b$text)) {
                collected <- c(collected, as.character(b$text))
              }
            }
          }
        }
      }

      if (length(collected) > 0L) {
        content <- paste(collected, collapse = "")
      } else {
        content <- NA_character_
      }
    }

    # Extract better_sample via simple tag search
    better_sample <- NA_character_
    if (!is.na(content)) {
      if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content, fixed = TRUE)) {
        better_sample <- "SAMPLE_1"
      } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content, fixed = TRUE)) {
        better_sample <- "SAMPLE_2"
      }
    }

    ids <- parse_ids(custom_id)
    better_id <- NA_character_
    if (!is.na(better_sample)) {
      if (better_sample == "SAMPLE_1") {
        better_id <- ids$ID1
      } else if (better_sample == "SAMPLE_2") {
        better_id <- ids$ID2
      }
    }

    # Usage: harmonize token counts across object types
    usage <- body$usage %||% list()

    # Chat completions: prompt_tokens, completion_tokens, total_tokens
    # Responses (gpt-5.x): input_tokens, output_tokens, total_tokens
    prompt_tokens     <- usage$prompt_tokens     %||% usage$input_tokens  %||% NA_real_
    completion_tokens <- usage$completion_tokens %||% usage$output_tokens %||% NA_real_
    total_tokens      <- usage$total_tokens      %||% NA_real_

    out[[i]] <- tibble::tibble(
      custom_id         = custom_id,
      ID1               = ids$ID1,
      ID2               = ids$ID2,
      model             = model,
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
  }

  # Drop NULL entries and bind rows
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(out)
}
