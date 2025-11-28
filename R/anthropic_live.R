#' @importFrom httr2 resp_status
NULL

# -------------------------------------------------------------------------
# Internal helpers for Anthropic
# -------------------------------------------------------------------------

# Resolve Anthropic API key -----------------------------------------------
.anthropic_api_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop(
      "No Anthropic API key found. Please set the ANTHROPIC_API_KEY ",
      "environment variable or supply `api_key` explicitly.",
      call. = FALSE
    )
  }
  key
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
#'     \item \code{"enabled"} – sends a \code{thinking} block to the Anthropic
#'       API (with a token budget) and increases the default \code{max_tokens}
#'       used for the visible answer.
#'   }
#' @param include_raw Logical; if \code{TRUE}, adds a list-column
#'   \code{raw_response} containing the parsed JSON body returned by Anthropic
#'   (or \code{NULL} on parse failure). This is useful for debugging parsing
#'   problems.
#' @param ... Additional Anthropic parameters such as \code{max_tokens},
#'   \code{temperature}, \code{top_p} or a custom \code{thinking_budget_tokens},
#'   which will be passed through to the Messages API. If you supply
#'   \code{thinking_budget_tokens} it will override the default thinking budget
#'   used when \code{reasoning = "enabled"}.
#'
#' @return A tibble with one row and columns:
#' \describe{
#'   \item{custom_id}{ID string of the form \code{"LIVE_<ID1>_vs_<ID2>"}.}
#'   \item{ID1, ID2}{The sample IDs you supplied.}
#'   \item{model}{Model name reported by the API.}
#'   \item{object_type}{Anthropic object type (for example \code{"message"}).}
#'   \item{status_code}{HTTP-style status code (200 if successful).}
#'   \item{error_message}{Error message if something goes wrong; otherwise NA.}
#'   \item{content}{Concatenated text from the assistant output.}
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
#'   \item \code{temperature = 0} (deterministic comparisons).
#'   \item \code{max_tokens = 768} when \code{reasoning = "none"}.
#'   \item \code{max_tokens = 1536} when \code{reasoning = "enabled"}.
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
#' where \code{thinking_budget_tokens} defaults to 2000, but can be overridden
#' via \code{...}.
#'
#' @examples
#' \dontrun{
#' # Requires ANTHROPIC_API_KEY and network access.
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' samples <- example_writing_samples[1:2, ]
#'
#' td   <- trait_description("overall_quality")
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
#'   include_raw       = TRUE
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
    prompt_template   = set_prompt_template(),
    tag_prefix        = "<BETTER_SAMPLE>",
    tag_suffix        = "</BETTER_SAMPLE>",
    api_key           = Sys.getenv("ANTHROPIC_API_KEY"),
    anthropic_version = "2023-06-01",
    reasoning         = c("none", "enabled"),
    include_raw       = FALSE,
    ...
) {
  reasoning <- match.arg(reasoning)

  if (!is.character(ID1)  || length(ID1)  != 1L) stop("`ID1` must be a single character.",  call. = FALSE)
  if (!is.character(ID2)  || length(ID2)  != 1L) stop("`ID2` must be a single character.",  call. = FALSE)
  if (!is.character(text1) || length(text1) != 1L) stop("`text1` must be a single character.", call. = FALSE)
  if (!is.character(text2) || length(text2) != 1L) stop("`text2` must be a single character.", call. = FALSE)
  if (!is.character(model) || length(model) != 1L) stop("`model` must be a single character.", call. = FALSE)

  dots <- list(...)

  reasoning <- match.arg(reasoning)

  # temperature: still deterministic unless user overrides
  temperature <- dots$temperature %||% 0

  if (is.null(dots$max_tokens)) {
    max_tokens <- if (reasoning == "none") 768L else 2048L
  } else {
    max_tokens <- dots$max_tokens
  }

  top_p <- dots$top_p %||% NULL

  thinking_budget <- NULL
  if (reasoning == "enabled") {
    # default thinking budget: at least 1024, at most max_tokens - 256
    default_budget <- 1024L

    thinking_budget <- dots$thinking_budget_tokens %||% default_budget

    # Enforce documented constraints gracefully
    if (thinking_budget < 1024L) {
      thinking_budget <- 1024L
    }
    if (thinking_budget >= max_tokens) {
      # Keep a 25% margin for visible output
      thinking_budget <- as.integer(floor(max_tokens * 0.5))
      if (thinking_budget < 1024L) {
        stop("`max_tokens` too small to support extended thinking: ",
             "it must be large enough that thinking_budget_tokens >= 1024 ",
             "and thinking_budget_tokens < max_tokens.",
             call. = FALSE)
      }
    }
  }

  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  body <- list(
    model      = model,
    max_tokens = max_tokens,
    messages   = list(
      list(
        role    = "user",
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
  if (!is.null(top_p))       body$top_p       <- top_p

  if (reasoning == "enabled" && !is.null(thinking_budget)) {
    body$thinking <- list(
      type          = "enabled",
      budget_tokens = thinking_budget
    )
  }

  if (!is.null(temperature)) body$temperature <- temperature
  if (!is.null(top_p))       body$top_p       <- top_p

  req <- .anthropic_request(
    path              = "/v1/messages",
    api_key           = api_key,
    anthropic_version = anthropic_version
  ) |>
    .anthropic_req_body_json(body)

  resp <- .anthropic_req_perform(req)

  status_code   <- .anthropic_resp_status(resp)
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

  object_type <- body$type  %||% NA_character_
  model_name  <- body$model %||% NA_character_

  # Collect text content from assistant message blocks
  content <- NA_character_
  if (!is.null(body$content) && length(body$content) > 0L) {
    collected <- character(0)
    for (blk in body$content) {
      if (!is.null(blk$text)) {
        collected <- c(collected, as.character(blk$text %||% ""))
      }
    }
    if (length(collected) > 0L) {
      content <- paste(collected, collapse = "")
    }
  }

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

  prompt_tokens     <- usage$input_tokens  %||% NA_real_
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
#'   \code{anthropic_compare_pair_live} (one of \code{"none"} or
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
#' @param ... Additional Anthropic parameters (for example \code{temperature},
#'   \code{top_p}, \code{max_tokens}) passed on to
#'   \code{anthropic_compare_pair_live}.
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
#' td   <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' res_claude <- submit_anthropic_pairs_live(
#'   pairs             = pairs,
#'   model             = "claude-sonnet-4-5",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   temperature       = 0,
#'   max_tokens        = 768,
#'   reasoning         = "none",
#'   verbose           = TRUE,
#'   status_every      = 2,
#'   progress          = TRUE,
#'   include_raw       = FALSE
#' )
#'
#' res_claude$better_id
#' }
#'
#' @export
submit_anthropic_pairs_live <- function(
    pairs,
    model,
    trait_name,
    trait_description,
    prompt_template   = set_prompt_template(),
    api_key           = Sys.getenv("ANTHROPIC_API_KEY"),
    anthropic_version = "2023-06-01",
    reasoning         = c("none", "enabled"),
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = FALSE,
    ...
) {
  reasoning <- match.arg(reasoning)

  pairs <- tibble::as_tibble(pairs)
  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols  <- setdiff(required_cols, names(pairs))

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
      avg     <- elapsed / i
      remain  <- n - i
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
