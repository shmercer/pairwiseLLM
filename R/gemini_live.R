# Internal: low-level helpers for the Google Gemini API
# These mirror the OpenAI / Anthropic helpers but follow the Gemini 3 REST docs.

#' @keywords internal
.gemini_base_url <- function() {
  "https://generativelanguage.googleapis.com"
}

#' @keywords internal
.gemini_request <- function(path, api_key = NULL) {
  api_key <- .gemini_api_key(api_key)

  req <- httr2::request(.gemini_base_url())
  req <- httr2::req_url_path_append(req, sub("^/", "", path))

  # Per Gemini docs, use x-goog-api-key header (not ?key=).
  req <- httr2::req_headers(
    req,
    "x-goog-api-key" = api_key,
    "Content-Type"   = "application/json"
  )

  req
}

#' @keywords internal
.gemini_req_body_json <- function(req, body) {
  httr2::req_body_json(req, data = body)
}

#' @keywords internal
#' @noRd
.gemini_req_perform <- function(req) {
  .retry_httr2_request(req)
}

#' @keywords internal
.gemini_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

#' @keywords internal
.gemini_resp_status <- function(resp) {
  httr2::resp_status(resp)
}

#' Live Google Gemini comparison for a single pair of samples
#'
#' This function sends a single pairwise comparison prompt to the Google Gemini
#' Generative Language API (Gemini 3 Pro) and parses the result into a one-row
#' tibble that mirrors the structure used for OpenAI / Anthropic live calls.
#'
#' It expects the prompt template to instruct the model to choose exactly one of
#' SAMPLE_1 or SAMPLE_2 and wrap the decision in <BETTER_SAMPLE> tags, for
#' example:
#'
#'   <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
#'
#'   or
#'
#'   <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
#'
#' If `include_thoughts = TRUE`, the function additionally requests Gemini's
#' explicit chain-of-thought style reasoning (\"thoughts\") via the
#' `thinkingConfig` block and stores it in a separate `thoughts` column, while
#' still using the final answer content to detect the `<BETTER_SAMPLE>` tag.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character containing the first sample text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character containing the second sample text.
#' @param model Gemini model identifier (for example `"gemini-3-pro-preview"`).
#'   The value is interpolated into the path
#'   `"/{api_version}/models/<model>:generateContent"`.
#' @param trait_name Short label for the trait (e.g. `"Overall Quality"`).
#' @param trait_description Full-text trait / rubric description.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()]. The template should embed `<BETTER_SAMPLE>` tags.
#' @param api_key Optional Gemini API key (defaults to
#'   `Sys.getenv("GEMINI_API_KEY")`).
#' @param thinking_level One of `"low"`, `"medium"`, or `"high"`. This controls
#'   the maximum depth of internal reasoning for Gemini 3 Pro. For pairwise
#'   scoring, `"low"` is used by default to reduce latency and cost. Currently,
#'   the Gemini REST API only supports `"Low"` and `"High"` values; `"medium"`
#'   is mapped internally to `"High"` with a warning.
#' @param temperature Optional numeric temperature. If `NULL` (default), the
#'   parameter is omitted and Gemini uses its own default (currently 1.0).
#' @param top_p Optional nucleus sampling parameter. If `NULL`, omitted.
#' @param top_k Optional top-k sampling parameter. If `NULL`, omitted.
#' @param max_output_tokens Optional maximum output token count. If `NULL`,
#'   omitted.
#' @param api_version API version to use, default `"v1beta"`. For plain text
#'   pairwise comparisons v1beta is recommended.
#' @param include_raw Logical; if `TRUE`, the returned tibble includes a
#'   `raw_response` list-column with the parsed JSON body.
#' @param include_thoughts Logical; if `TRUE`, requests explicit reasoning
#'   output from Gemini via `generationConfig$thinkingConfig` and stores the
#'   first text part as `thoughts`, with subsequent parts collapsed into
#'   `content`. If `FALSE` (default), all text parts are collapsed into
#'   `content` and `thoughts` is `NA`.
#' @param ... Reserved for future extensions. Any `thinking_budget` entry in
#'   `...` is ignored (and a warning is emitted) because Gemini 3 does not allow
#'   `thinking_budget` and `thinking_level` to be used together.
#'
#' @return A tibble with one row and columns:
#'   * `custom_id` - `"LIVE_<ID1>_vs_<ID2>"`.
#'   * `ID1`, `ID2` - provided sample IDs.
#'   * `model` - model name returned by the API (or the requested model).
#'   * `object_type` - `"generateContent"` on success, otherwise `NA`.
#'   * `status_code` - HTTP status code (200 on success).
#'   * `error_message` - error message for failures, otherwise `NA`.
#'   * `thoughts` - explicit chain-of-thought style reasoning text if
#'     `include_thoughts = TRUE` and the model returns it; otherwise `NA`.
#'   * `content` - concatenated text of the assistant's final answer (used to
#'     locate the `<BETTER_SAMPLE>` tag).
#'   * `better_sample` - `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`.
#'   * `better_id` - `ID1` if `SAMPLE_1` is chosen,
#'     `ID2` if `SAMPLE_2`, or `NA`.
#'   * `prompt_tokens`, `completion_tokens`, `total_tokens` - usage counts if
#'     reported by the API, otherwise `NA_real_`.
#'
#' @examples
#' # Requires:
#' # - GEMINI_API_KEY set in your environment
#' # - Internet access
#' # - Billable Gemini API usage
#' \dontrun{
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' res <- gemini_compare_pair_live(
#'   ID1               = "S01",
#'   text1             = "Text 1",
#'   ID2               = "S02",
#'   text2             = "Text 2",
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   thinking_level    = "low",
#'   include_thoughts  = FALSE,
#'   include_raw       = FALSE
#' )
#'
#' res
#' res$better_id
#' }
#'
#' @export
gemini_compare_pair_live <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  thinking_level = c("low", "medium", "high"),
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  api_version = "v1beta",
  include_raw = FALSE,
  include_thoughts = FALSE,
  ...
) {
  # Basic validation / normalisation
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty character scalar.", call. = FALSE)
  }

  thinking_level <- match.arg(thinking_level)

  dots <- list(...)
  if (!is.null(dots$thinking_budget)) {
    warning(
      "`thinking_budget` is ignored for Gemini 3. ",
      "Use `thinking_level` instead and do not supply both.",
      call. = FALSE
    )
  }

  ID1 <- as.character(ID1)
  ID2 <- as.character(ID2)
  text1 <- as.character(text1)
  text2 <- as.character(text2)

  # Interpolate using existing prompt builder
  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  # ---------------------------------------------------------------------------
  # Build generationConfig (temperature, sampling, max tokens, thinkingConfig)
  # ---------------------------------------------------------------------------

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

  # Map R-level thinking_level ("low", "medium", "high") to Gemini JSON values.
  # Gemini 3 currently supports "Low" and "High". "Medium" is not yet supported,
  # so we map it to "High" with a warning.
  if (!is.null(thinking_level)) {
    tl_map <- c(low = "Low", medium = "High", high = "High")

    if (identical(thinking_level, "medium")) {
      warning(
        "`thinking_level = \"medium\"` is not yet supported by the Gemini
        REST API; ",
        "mapping to \"High\" internally.",
        call. = FALSE
      )
    }

    thinking_config <- list(
      includeThoughts = isTRUE(include_thoughts),
      thinkingLevel   = tl_map[[thinking_level]]
    )

    generation_config$thinkingConfig <- thinking_config
  }

  # Core request body
  body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = prompt)
        )
      )
    )
  )

  # Attach generationConfig only if non-empty
  if (length(generation_config) > 0L) {
    body$generationConfig <- generation_config
  }

  path <- sprintf("/%s/models/%s:generateContent", api_version, model)

  req <- .gemini_request(path = path, api_key = api_key)
  req <- .gemini_req_body_json(req, body = body)

  resp <- NULL
  body_parsed <- NULL
  status_code <- NA_integer_
  error_message <- NA_character_

  # Perform request; capture any HTTP/httr2 error so we can return a row
  tryCatch(
    {
      resp <- .gemini_req_perform(req)
      status_code <- .gemini_resp_status(resp)
      body_parsed <- .gemini_resp_body_json(resp, simplifyVector = FALSE)
    },
    error = function(err) {
      # Default error message
      error_message <<- conditionMessage(err)

      # If this is an httr2 HTTP error, try to extract status + body
      if (inherits(err, "httr2_http") && !is.null(err$resp)) {
        # Status code from the error's response
        status_code <<- httr2::resp_status(err$resp)

        # Try to pull the raw body text - often contains a JSON error
        body_raw <- tryCatch(
          httr2::resp_body_string(err$resp),
          error = function(e) NA_character_
        )

        if (!is.na(body_raw) && nzchar(body_raw)) {
          # Append the raw body to the error_message so it is in the tibble
          error_message <<- paste0(error_message, " | body: ", body_raw)
        }
      }
    }
  )

  custom_id <- sprintf("LIVE_%s_vs_%s", ID1, ID2)

  # If we didn't get a parsed body, return an "error" row
  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id         = custom_id,
      ID1               = ID1,
      ID2               = ID2,
      model             = model,
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
    if (include_raw) {
      res$raw_response <- list(NULL)
    }
    return(res)
  }

  # ---------------------------------------------------------------------------
  # Extract thoughts and content from candidates[[1]]$content$parts
  # ---------------------------------------------------------------------------

  object_type <- "generateContent"
  model_name <- body_parsed$model %||% model

  thoughts <- NA_character_
  content <- NA_character_

  candidates <- body_parsed$candidates %||% list()
  if (length(candidates) > 0L) {
    first <- candidates[[1]]
    cont <- first$content %||% list()
    if (length(cont) > 0L) {
      parts <- cont$parts %||% cont
      if (is.list(parts) && length(parts) > 0L) {
        if (isTRUE(include_thoughts) && length(parts) >= 2L) {
          # Convention: first part = thoughts, rest = final answer
          if (!is.null(parts[[1]]$text)) {
            thoughts <- as.character(parts[[1]]$text %||% "")
          }
          collected <- vapply(
            parts[-1],
            function(p) if (!is.null(p$text)) as.character(p$text) else "",
            FUN.VALUE = character(1L)
          )
          if (length(collected) > 0L) {
            content <- paste(collected, collapse = "")
          }
        } else {
          # Legacy / default behavior: collapse all text parts into `content`
          collected <- vapply(
            parts,
            function(p) if (!is.null(p$text)) as.character(p$text) else "",
            FUN.VALUE = character(1L)
          )
          if (length(collected) > 0L) {
            content <- paste(collected, collapse = "")
          }
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Parse <BETTER_SAMPLE> tag from content
  # ---------------------------------------------------------------------------

  better_sample <- NA_character_
  tag_prefix <- "<BETTER_SAMPLE>"
  tag_suffix <- "</BETTER_SAMPLE>"

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
    better_id <- if (better_sample == "SAMPLE_1") ID1 else ID2
  }

  usage <- body_parsed$usageMetadata %||% list()
  prompt_tokens <- usage$promptTokenCount %||% NA_real_
  completion_tokens <- usage$candidatesTokenCount %||% NA_real_
  total_tokens <- usage$totalTokenCount %||% NA_real_

  res <- tibble::tibble(
    custom_id         = custom_id,
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
    res$raw_response <- list(body_parsed)
  }

  res
}
