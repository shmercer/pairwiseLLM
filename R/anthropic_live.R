#' @importFrom httr2 resp_status
NULL

# -------------------------------------------------------------------------
# Internal helpers for Anthropic
# -------------------------------------------------------------------------

#' Internal: Get Anthropic API key
#'
#' @keywords internal
#' @noRd
.anthropic_api_key <- function(api_key = NULL) {
  .get_api_key(
    api_key = api_key,
    env_var = "ANTHROPIC_API_KEY",
    service = "Anthropic"
  )
}


# Base Anthropic request builder ------------------------------------------
.anthropic_request <- function(
  path,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
) {
  api_key <- .anthropic_api_key(api_key)

  httr2::request("https://api.anthropic.com") |>
    httr2::req_url_path_append(sub("^/", "", path)) |>
    httr2::req_headers(
      `x-api-key`         = api_key,
      `anthropic-version` = anthropic_version
    )
}

# Internal wrappers around httr2 so tests can mock them easily ---------

.anthropic_req_body_json <- function(req, body) {
  httr2::req_body_json(req, body)
}

.anthropic_req_perform <- function(req) {
  httr2::req_perform(req)
}

.anthropic_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

.anthropic_resp_status <- function(resp) {
  httr2::resp_status(resp)
}

# -------------------------------------------------------------------------
# Public functions
# -------------------------------------------------------------------------

#' Live Anthropic (Claude) comparison for a single pair of samples
#'
#' This function sends a single pairwise comparison prompt to the Anthropic
#' Messages API (Claude models) and parses the result into a small tibble.
#'
#' It mirrors the behaviour and output schema of
#' \code{\link{openai_compare_pair_live}}, but targets Anthropic's
#' \code{/v1/messages} endpoint. The prompt template, `<BETTER_SAMPLE>` tag
#' convention, and downstream parsing / BT modelling can remain unchanged.
#'
#' The function is designed to work with Claude models such as Sonnet, Haiku,
#' and Opus in the "4.5" family. You can pass any valid Anthropic model string,
#' for example:
#' \itemize{
#'   \item \code{"claude-sonnet-4-5"}
#'   \item \code{"claude-haiku-4-5"}
#'   \item \code{"claude-opus-4-5"}
#' }
#' The API typically responds with a dated model string such as
#' \code{"claude-sonnet-4-5-20250929"} in the \code{model} field.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Anthropic Claude model name (for example
#'   \code{"claude-sonnet-4-5"}, \code{"claude-haiku-4-5"},
#'   or \code{"claude-opus-4-5"}).
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}. The template should embed the full
#'   instructions, rubric text, and `<BETTER_SAMPLE>` tagging convention.
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   \code{"</BETTER_SAMPLE>"}.
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#' @param reasoning Character scalar indicating whether to allow more extensive
#'   internal "thinking" before the visible answer. Two values are recognised:
#'   \itemize{
#'     \item \code{"none"} – standard prompting (recommended default).
#'     \item \code{"enabled"} – uses Anthropic's extended thinking mode by
#'       sending a \code{thinking} block with a token budget; this also changes
#'       the default \code{max_tokens} and constrains \code{temperature}.
#'   }
#' @param include_raw Logical; if \code{TRUE}, adds a list-column
#'   \code{raw_response} containing the parsed JSON body returned by Anthropic
#'   (or \code{NULL} on parse failure). This is useful for debugging parsing
#'   problems.
#' @param include_thoughts Logical or \code{NULL}. When \code{TRUE} and
#'   \code{reasoning = "none"}, this function upgrades to extended thinking
#'   mode by setting \code{reasoning = "enabled"} before constructing the
#'   request, which in turn implies \code{temperature = 1} and adds a
#'   \code{thinking} block. When \code{FALSE} and \code{reasoning = "enabled"},
#'   a warning is issued but extended thinking is still used. When
#'   \code{NULL} (the default), \code{reasoning} is used as-is.
#' @param ... Additional Anthropic parameters such as \code{max_tokens},
#'   \code{temperature}, \code{top_p} or a custom \code{thinking_budget_tokens},
#'   which will be passed through to the Messages API.
#'
#'   When \code{reasoning = "none"} the defaults are:
#'   \itemize{
#'     \item \code{temperature = 0} (deterministic behaviour) unless you
#'       supply \code{temperature} explicitly.
#'     \item \code{max_tokens = 768} unless you supply \code{max_tokens}.
#'   }
#'
#'   When \code{reasoning = "enabled"} (extended thinking), the Anthropic API
#'   imposes additional constraints:
#'   \itemize{
#'     \item \code{temperature} \strong{must} be 1. If you supply a different
#'       value, this function will throw an error.
#'     \item \code{thinking_budget_tokens} must satisfy
#'       \code{thinking_budget_tokens >= 1024} and
#'       \code{thinking_budget_tokens < max_tokens}. If you supply a value that
#'       violates these constraints, this function will throw an error.
#'     \item By default, \code{max_tokens = 2048} and
#'       \code{thinking_budget_tokens = 1024}.
#'   }
#'
#' @return A tibble with one row and columns:
#' \describe{
#'   \item{custom_id}{ID string of the form \code{"LIVE_<ID1>_vs_<ID2>"}.}
#'   \item{ID1, ID2}{The sample IDs you supplied.}
#'   \item{model}{Model name reported by the API.}
#'   \item{object_type}{Anthropic object type (for example \code{"message"}).}
#'   \item{status_code}{HTTP-style status code (200 if successful).}
#'   \item{error_message}{Error message if something goes wrong; otherwise NA.}
#'   \item{thoughts}{Summarised thinking / reasoning text when
#'     \code{reasoning = "enabled"} and the API returns thinking blocks;
#'     otherwise \code{NA}.}
#'   \item{content}{Concatenated text from the assistant output (excluding
#'     thinking blocks).}
#'   \item{better_sample}{"SAMPLE_1", "SAMPLE_2", or NA.}
#'   \item{better_id}{ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen,
#'     otherwise NA.}
#'   \item{prompt_tokens}{Prompt / input token count (if reported).}
#'   \item{completion_tokens}{Completion / output token count (if reported).}
#'   \item{total_tokens}{Total token count (reported by the API or computed as
#'     input + output tokens when not provided).}
#'   \item{raw_response}{(Optional) list-column containing the parsed JSON body.}
#' }
#'
#' @details
#' **Recommended defaults for pairwise writing comparisons**
#'
#' For stable, reproducible comparisons we recommend:
#' \itemize{
#'   \item \code{reasoning = "none"} with \code{temperature = 0} and
#'     \code{max_tokens = 768} for standard pairwise scoring.
#'   \item \code{reasoning = "enabled"} when you explicitly want extended
#'     thinking; in this mode Anthropic requires \code{temperature = 1}.
#'     The default in this function is \code{max_tokens = 2048} and
#'     \code{thinking_budget_tokens = 1024}, which satisfies the documented
#'     constraints \code{thinking_budget_tokens >= 1024} and
#'     \code{thinking_budget_tokens < max_tokens}.
#' }
#'
#' When \code{reasoning = "enabled"}, this function also sends a
#' \code{thinking} block to the Anthropic API:
#'
#' \preformatted{
#' "thinking": {
#'   "type": "enabled",
#'   "budget_tokens": <thinking_budget_tokens>
#' }
#' }
#'
#' Setting \code{include_thoughts = TRUE} when \code{reasoning = "none"}
#' is a convenient way to opt into Anthropic's extended thinking mode without
#' changing the \code{reasoning} argument explicitly. In that case,
#' \code{reasoning} is upgraded to \code{"enabled"}, the default
#' \code{temperature} becomes 1, and a \code{thinking} block is included in the
#' request. When \code{reasoning = "none"} and \code{include_thoughts} is
#' \code{FALSE} or \code{NULL}, the default temperature remains 0 unless
#' you explicitly override it.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' samples <- example_writing_samples[1:2, ]
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Short, deterministic comparison (no explicit thinking block)
#' res_claude <- anthropic_compare_pair_live(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "none",
#'   include_raw       = FALSE
#' )
#'
#' res_claude$better_id
#'
#' # Allow more internal thinking and a longer explanation
#' res_claude_reason <- anthropic_compare_pair_live(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "enabled",
#'   include_raw       = TRUE,
#'   include_thoughts  = TRUE
#' )
#'
#' res_claude_reason$total_tokens
#' substr(res_claude_reason$content, 1, 200)
#' }
#'
#' @export
anthropic_compare_pair_live <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  reasoning = c("none", "enabled"),
  include_raw = FALSE,
  include_thoughts = NULL,
  ...
) {
  reasoning <- match.arg(reasoning)

  if (!is.character(ID1) || length(ID1) != 1L) stop("`ID1` must be a single character.", call. = FALSE)
  if (!is.character(ID2) || length(ID2) != 1L) stop("`ID2` must be a single character.", call. = FALSE)
  if (!is.character(text1) || length(text1) != 1L) stop("`text1` must be a single character.", call. = FALSE)
  if (!is.character(text2) || length(text2) != 1L) stop("`text2` must be a single character.", call. = FALSE)
  if (!is.character(model) || length(model) != 1L) stop("`model` must be a single character.", call. = FALSE)

  # ------------------------------------------------------------------
  # include_thoughts -> reasoning mapping
  # ------------------------------------------------------------------
  if (!is.null(include_thoughts)) {
    if (isTRUE(include_thoughts) && identical(reasoning, "none")) {
      reasoning <- "enabled"
    } else if (!isTRUE(include_thoughts) && identical(reasoning, "enabled")) {
      warning(
        "include_thoughts = FALSE but reasoning = 'enabled'; ",
        "keeping reasoning = 'enabled'.",
        call. = FALSE
      )
    }
  }

  dots <- list(...)

  # ------------------------------------------------------------------
  # Temperature defaults & validation
  # ------------------------------------------------------------------
  if (reasoning == "none") {
    # Default to deterministic behaviour when not using extended thinking
    temperature <- dots$temperature %||% 0
  } else {
    if (is.null(dots$temperature)) {
      temperature <- 1
    } else if (!identical(dots$temperature, 1)) {
      stop(
        "For Anthropic extended thinking (reasoning = 'enabled'), ",
        "`temperature` must be 1 according to the Anthropic API.\n",
        "Either omit `temperature` or set `temperature = 1`.",
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------------
  # max_tokens & thinking budget
  # ------------------------------------------------------------------
  if (is.null(dots$max_tokens)) {
    max_tokens <- if (reasoning == "none") 768L else 2048L
  } else {
    max_tokens <- as.integer(dots$max_tokens)
  }

  top_p <- dots$top_p %||% NULL

  thinking_budget <- NULL
  if (reasoning == "enabled") {
    tb_user <- dots$thinking_budget_tokens
    default_budget <- 1024L

    if (is.null(tb_user)) {
      thinking_budget <- default_budget
    } else {
      thinking_budget <- as.integer(tb_user)
    }

    if (thinking_budget < 1024L) {
      stop(
        "`thinking_budget_tokens` must be at least 1024 when reasoning = 'enabled'. ",
        "Got: ", thinking_budget,
        call. = FALSE
      )
    }
    if (thinking_budget >= max_tokens) {
      stop(
        "`thinking_budget_tokens` (", thinking_budget, ") must be smaller than `max_tokens` (",
        max_tokens, ") for Anthropic extended thinking.\n",
        "Try something like: max_tokens = 2048, thinking_budget_tokens = 1024.",
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------------
  # Build prompt and request body
  # ------------------------------------------------------------------
  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  body <- list(
    model = model,
    max_tokens = max_tokens,
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

  if (!is.null(temperature)) body$temperature <- temperature
  if (!is.null(top_p)) body$top_p <- top_p

  if (reasoning == "enabled" && !is.null(thinking_budget)) {
    body$thinking <- list(
      type          = "enabled",
      budget_tokens = thinking_budget
    )
  }

  # ------------------------------------------------------------------
  # Perform request and parse response
  # ------------------------------------------------------------------
  req <- .anthropic_request(
    path              = "/v1/messages",
    api_key           = api_key,
    anthropic_version = anthropic_version
  ) |>
    .anthropic_req_body_json(body)

  resp <- .anthropic_req_perform(req)

  status_code <- .anthropic_resp_status(resp)
  error_message <- NA_character_

  body_parsed <- tryCatch(
    .anthropic_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  # Handle parse failure
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

  # If HTTP status is non-success, try to extract an error message
  if (status_code >= 300L) {
    error_message <- body$error$message %||% sprintf("HTTP error %s from Anthropic.", status_code)
  }

  object_type <- body$type %||% NA_character_
  model_name <- body$model %||% NA_character_

  # ------------------------------------------------------------------
  # Collect thinking + text content from assistant content blocks
  # ------------------------------------------------------------------
  thoughts <- NA_character_
  content <- NA_character_

  if (!is.null(body$content) && length(body$content) > 0L) {
    thought_chunks <- character(0)
    text_chunks <- character(0)

    for (blk in body$content) {
      blk_type <- blk$type %||% NULL

      if (identical(blk_type, "thinking") && !is.null(blk$thinking)) {
        # Summarized thinking text
        thought_chunks <- c(thought_chunks, as.character(blk$thinking %||% ""))
      } else if (identical(blk_type, "redacted_thinking") && !is.null(blk$data)) {
        # Encrypted/redacted thinking; keep as-is but label so it's obvious
        redacted_txt <- as.character(blk$data %||% "")
        if (nzchar(redacted_txt)) {
          thought_chunks <- c(
            thought_chunks,
            paste0("[REDACTED_THINKING]", redacted_txt)
          )
        }
      } else if (identical(blk_type, "text") && !is.null(blk$text)) {
        text_chunks <- c(text_chunks, as.character(blk$text %||% ""))
      }
    }

    if (length(thought_chunks) > 0L) {
      thoughts <- paste(thought_chunks, collapse = "")
    }
    if (length(text_chunks) > 0L) {
      content <- paste(text_chunks, collapse = "")
    }
  }

  # ------------------------------------------------------------------
  # Better-sample extraction
  # ------------------------------------------------------------------
  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content, fixed = TRUE)) {
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

  if (!is.null(usage$total_tokens)) {
    total_tokens <- usage$total_tokens
  } else if (!is.null(prompt_tokens) && !is.null(completion_tokens)) {
    total_tokens <- as.numeric(prompt_tokens) + as.numeric(completion_tokens)
  } else {
    total_tokens <- NA_real_
  }

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

#' Live Anthropic (Claude) comparisons for a tibble of pairs
#'
#' This is a thin row-wise wrapper around \code{\link{anthropic_compare_pair_live}}.
#' It takes a tibble of pairs (ID1 / text1 / ID2 / text2), submits each pair to
#' the Anthropic Messages API, and binds the results into a single tibble.
#'
#' The output has the same columns as \code{\link{anthropic_compare_pair_live}},
#' with one row per pair, making it easy to pass into \code{\link{build_bt_data}}
#' and \code{\link{fit_bt_model}}.
#'
#' @details
#' **Temperature and reasoning behaviour**
#'
#' Temperature and extended-thinking behaviour are controlled by
#' \code{\link{anthropic_compare_pair_live}}:
#' \itemize{
#'   \item When \code{reasoning = "none"} (no extended thinking), the default
#'     \code{temperature} is \code{0} (deterministic) unless you explicitly
#'     supply a different \code{temperature} via \code{...}.
#'   \item When \code{reasoning = "enabled"} (extended thinking), Anthropic
#'     requires \code{temperature = 1}. If you supply a different value, an
#'     error is raised by \code{\link{anthropic_compare_pair_live}}.
#' }
#'
#' If you set \code{include_thoughts = TRUE} while \code{reasoning = "none"},
#' the underlying calls upgrade to \code{reasoning = "enabled"}, which in turn
#' implies \code{temperature = 1} and adds a \code{thinking} block to the API
#' request. When \code{include_thoughts = FALSE} (the default), and you leave
#' \code{reasoning = "none"}, the effective default temperature is \code{0}.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}. Typically created by
#'   \code{\link{make_pairs}}, \code{\link{sample_pairs}}, and
#'   \code{\link{randomize_pair_order}}.
#' @param model Anthropic model name (for example \code{"claude-sonnet-4-5"},
#'   \code{"claude-haiku-4-5"}, or \code{"claude-opus-4-5"}).
#' @param trait_name Trait name to pass to \code{anthropic_compare_pair_live}.
#' @param trait_description Trait description to pass to
#'   \code{anthropic_compare_pair_live}.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param api_key Optional Anthropic API key. Defaults to
#'   \code{Sys.getenv("ANTHROPIC_API_KEY")}.
#' @param anthropic_version Anthropic API version string passed as the
#'   \code{anthropic-version} HTTP header. Defaults to \code{"2023-06-01"}.
#' @param reasoning Character scalar passed to
#'   \code{\link{anthropic_compare_pair_live}} (one of \code{"none"} or
#'   \code{"enabled"}).
#' @param verbose Logical; if \code{TRUE}, prints status, timing, and result
#'   summaries.
#' @param status_every Integer; print status / timing for every
#'   \code{status_every}-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if \code{TRUE}, shows a textual progress bar.
#' @param include_raw Logical; if \code{TRUE}, each row of the returned tibble
#'   will include a \code{raw_response} list-column with the parsed JSON body
#'   from Anthropic.
#' @param include_thoughts Logical or \code{NULL}; forwarded to
#'   \code{\link{anthropic_compare_pair_live}}. When \code{TRUE} and
#'   \code{reasoning = "none"}, the underlying calls upgrade to extended
#'   thinking mode (\code{reasoning = "enabled"}), which implies
#'   \code{temperature = 1} and adds a \code{thinking} block. When
#'   \code{FALSE} or \code{NULL}, \code{reasoning} is used as-is.
#' @param ... Additional Anthropic parameters (for example \code{temperature},
#'   \code{top_p}, \code{max_tokens}) passed on to
#'   \code{\link{anthropic_compare_pair_live}}.
#'
#' @return A tibble with one row per pair and the same columns as
#'   \code{\link{anthropic_compare_pair_live}}.
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
#' # Deterministic comparisons (no extended thinking, temperature defaults to 0)
#' res_claude <- submit_anthropic_pairs_live(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "none",
#'   verbose           = TRUE,
#'   status_every      = 2,
#'   progress          = TRUE,
#'   include_raw       = FALSE
#' )
#'
#' res_claude$better_id
#'
#' # Comparisons with extended thinking (temperature fixed at 1)
#' res_claude_reason <- submit_anthropic_pairs_live(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   reasoning         = "enabled",
#'   include_thoughts  = TRUE,
#'   verbose           = TRUE,
#'   status_every      = 2,
#'   progress          = TRUE,
#'   include_raw       = TRUE
#' )
#'
#' res_claude_reason$better_id
#' }
#'
#' @export
submit_anthropic_pairs_live <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  reasoning = c("none", "enabled"),
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  include_thoughts = NULL,
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

  if (!is.numeric(status_every) || length(status_every) != 1L || status_every < 1) {
    stop("`status_every` must be a single positive integer.", call. = FALSE)
  }
  status_every <- as.integer(status_every)

  fmt_secs <- function(x) sprintf("%.1fs", x)

  if (verbose) {
    message(sprintf(
      "Submitting %d Anthropic live pair(s) for comparison (model=%s)...",
      n, model
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
        "[Anthropic live pair %d of %d] Comparing %s vs %s ...",
        i, n, pairs$ID1[i], pairs$ID2[i]
      ))
    }

    res <- anthropic_compare_pair_live(
      ID1               = as.character(pairs$ID1[i]),
      text1             = as.character(pairs$text1[i]),
      ID2               = as.character(pairs$ID2[i]),
      text2             = as.character(pairs$text2[i]),
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      api_key           = api_key,
      anthropic_version = anthropic_version,
      reasoning         = reasoning,
      include_raw       = include_raw,
      include_thoughts  = include_thoughts,
      ...
    )

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }

    if (!is.na(res$error_message)) {
      message(sprintf(
        "    WARNING: Anthropic API Error (status %s): %s",
        res$status_code, res$error_message
      ))
    } else if (show_status) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg <- elapsed / i
      remain <- n - i
      est_rem <- avg * remain

      message(sprintf(
        "    Result: %s preferred (%s) | tokens: prompt=%s, completion=%s, total=%s",
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
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    avg <- total_elapsed / n
    message(sprintf(
      "Completed %d Anthropic live pair(s) in %s (avg %.2fs per pair).",
      n, fmt_secs(total_elapsed), avg
    ))
  }

  dplyr::bind_rows(out)
}
