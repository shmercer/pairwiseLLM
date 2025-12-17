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
#'   column.
#'   \item Visible assistant output into the \code{content} column.
#' }
#'
#' **Temperature Defaults:**
#' If `temperature` is not provided in `...`:
#' * It defaults to `0` (deterministic) for standard models or when reasoning is
#'   disabled.
#' * It remains `NULL` when reasoning is enabled, as the API does not support
#'   temperature in that mode.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model OpenAI model name (e.g. "gpt-4.1", "gpt-5.2-2025-12-11").
#' @param trait_name Short label for the trait (e.g. "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string.
#' @param endpoint Which OpenAI endpoint to use: \code{"chat.completions"} or
#'   \code{"responses"}.
#' @param tag_prefix Prefix for the better-sample tag.
#' @param tag_suffix Suffix for the better-sample tag.
#' @param api_key Optional OpenAI API key.
#' @param include_raw Logical; if TRUE, adds a \code{raw_response} column.
#' @param ... Additional OpenAI parameters, for example
#'   \code{temperature}, \code{top_p}, \code{logprobs}, \code{reasoning},
#'   and (optionally) \code{include_thoughts}. The same validation rules for
#'   gpt-5 models are applied as in \code{\link{build_openai_batch_requests}}.
#'   When using the Responses endpoint with reasoning models, you can request
#'   reasoning summaries in the \code{thoughts} column by setting
#'   \code{endpoint = "responses"}, a non-"none" reasoning effort, and
#'   \code{include_thoughts = TRUE}.
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
#' @examples
#' \dontrun{
#' # Requires API key set and internet access
#'
#' # 1. Standard comparison using GPT-4.1
#' res <- openai_compare_pair_live(
#'   ID1 = "A", text1 = "Text A...",
#'   ID2 = "B", text2 = "Text B...",
#'   model = "gpt-4.1",
#'   trait_name = "clarity",
#'   trait_description = "Which text is clearer?",
#'   temperature = 0
#' )
#'
#' # 2. Reasoning comparison using GPT-5.2
#' res_reasoning <- openai_compare_pair_live(
#'   ID1 = "A", text1 = "Text A...",
#'   ID2 = "B", text2 = "Text B...",
#'   model = "gpt-5.2-2025-12-11",
#'   trait_name = "clarity",
#'   trait_description = "Which text is clearer?",
#'   endpoint = "responses",
#'   include_thoughts = TRUE,
#'   reasoning = "high"
#' )
#' print(res_reasoning$thoughts)
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

  if (!is.character(ID1) || length(ID1) != 1L) stop("ID1 invalid")
  if (!is.character(ID2) || length(ID2) != 1L) stop("ID2 invalid")
  if (!is.character(text1) || length(text1) != 1L) stop("text1 invalid")
  if (!is.character(text2) || length(text2) != 1L) stop("text2 invalid")
  if (!is.character(model) || length(model) != 1L) stop("model invalid")

  dots <- list(...)
  top_p <- dots$top_p %||% NULL
  logprobs <- dots$logprobs %||% NULL
  reasoning_effort <- dots$reasoning %||% NULL
  include_thoughts <- dots$include_thoughts %||% FALSE

  # Determine temperature default
  is_reasoning_model <- grepl("^gpt-5\\.[12]", model)
  reasoning_active <- is_reasoning_model &&
    (!is.null(reasoning_effort) && reasoning_effort != "none")

  temperature <- if ("temperature" %in% names(dots)) {
    dots$temperature
  } else if (reasoning_active) {
    NULL # Must be NULL for reasoning
  } else {
    0 # Default to 0 for everything else (standard or disabled reasoning)
  }

  # Validation: Only block temp/top_p/logprobs if reasoning is ACTIVE
  # We do NOT block generic gpt-5 models here, allowing temp=0.
  if (is_reasoning_model && reasoning_active) {
    if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
      stop(
        "For gpt-5.1/5.2 with reasoning effort not equal to 'none', ",
        "temperature, top_p, and logprobs must be NULL.",
        call. = FALSE
      )
    }
  }

  prompt <- build_prompt(
    template = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1 = text1,
    text2 = text2
  )

  if (endpoint == "chat.completions") {
    body <- list(model = model, messages = list(list(role = "user", content = prompt)))
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p)) body$top_p <- top_p
    if (!is.null(logprobs)) body$logprobs <- logprobs
    path <- "/chat/completions"
  } else {
    body <- list(model = model, input = prompt)
    if (!is.null(reasoning_effort)) {
      reasoning_list <- list(effort = reasoning_effort)
      if (isTRUE(include_thoughts)) reasoning_list$summary <- "auto"
      body$reasoning <- reasoning_list
    }
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p)) body$top_p <- top_p
    if (!is.null(logprobs)) body$logprobs <- logprobs
    path <- "/responses"
  }

  req <- .openai_request(path, api_key)
  req <- .openai_req_body_json(req, body = body)
  resp <- .openai_req_perform(req)
  status_code <- .openai_resp_status(resp)

  body_parsed <- tryCatch(
    .openai_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1 = ID1, ID2 = ID2, model = NA_character_, object_type = NA_character_,
      status_code = status_code, error_message = "Failed to parse JSON.",
      thoughts = NA_character_, content = NA_character_,
      better_sample = NA_character_, better_id = NA_character_,
      prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_
    )
    if (include_raw) res$raw_response <- list(NULL)
    return(res)
  }

  body <- body_parsed
  object_type <- body$object %||% NA_character_
  model_name <- body$model %||% NA_character_
  error_message <- NA_character_

  if (!is.null(body$error)) {
    error_message <- as.character(body$error$message %||% "Unknown API error.")
  } else if (status_code >= 400L) {
    error_message <- paste0("HTTP ", status_code)
  }

  thoughts <- NA_character_
  content <- NA_character_

  if (identical(object_type, "chat.completion")) {
    choices <- body$choices %||% list()
    if (length(choices) >= 1L && !is.null(choices[[1]]$message$content)) {
      content <- as.character(choices[[1]]$message$content)
    }
  } else if (identical(object_type, "response")) {
    reasoning_chunks <- character(0)
    message_chunks <- character(0)
    output <- body$output %||% list()

    if (length(output) > 0L) {
      for (out_el in output) {
        if (identical(out_el$type, "reasoning")) {
          rs <- out_el$summary

          # Priority 1: Data frame (check first because is.list(df) is TRUE)
          if (is.data.frame(rs) && "text" %in% names(rs)) {
            reasoning_chunks <- c(reasoning_chunks, as.character(rs$text))
          }
          # Priority 2: Generic list
          else if (is.list(rs)) {
            for (s in rs) if (!is.null(s$text)) reasoning_chunks <- c(reasoning_chunks, s$text)
          }
        }
        if (length(out_el$content) > 0L) {
          for (b in out_el$content) if (!is.null(b$text)) message_chunks <- c(message_chunks, b$text)
        }
      }
    }

    # Backwards compatibility check
    # Ensure intermediate objects are lists before digging deeper
    if (!length(reasoning_chunks) &&
      !is.null(body$reasoning) && is.list(body$reasoning) &&
      !is.null(body$reasoning$summary) && is.list(body$reasoning$summary) &&
      !is.null(body$reasoning$summary$text)) {
      reasoning_chunks <- c(reasoning_chunks, body$reasoning$summary$text)
    }

    if (length(reasoning_chunks)) thoughts <- paste(reasoning_chunks, collapse = " ")
    if (length(message_chunks)) content <- paste(message_chunks, collapse = "")
  }

  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_2"
    }
  }

  better_id <- if (!is.na(better_sample)) (if (better_sample == "SAMPLE_1") ID1 else ID2) else NA_character_

  usage <- body$usage %||% list()

  res <- tibble::tibble(
    custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1 = ID1, ID2 = ID2, model = model_name, object_type = object_type,
    status_code = status_code, error_message = error_message,
    thoughts = thoughts, content = content,
    better_sample = better_sample, better_id = better_id,
    prompt_tokens = as.numeric(usage$prompt_tokens %||% usage$input_tokens %||% NA),
    completion_tokens = as.numeric(usage$completion_tokens %||% usage$output_tokens %||% NA),
    total_tokens = as.numeric(usage$total_tokens %||% NA)
  )

  if (include_raw) res$raw_response <- list(body)
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
#' # Requires API key set and internet access
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
#' # Using gpt-5.1 with reasoning on the responses endpoint
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
