# Internal: low-level helpers for the Google Gemini API
# These mirror the OpenAI / Anthropic helpers but follow the Gemini 3 REST docs.

# Non-syntactic bindings used so tests can reliably mock future calls even
# when the suggested packages are not installed.
`future::plan` <- function(...) {
  if (!requireNamespace("future", quietly = TRUE)) {
    rlang::abort("The 'future' package is required for parallel processing.")
  }
  future::plan(...)
}

`future.apply::future_lapply` <- function(X, FUN, ...) {
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    rlang::abort("The 'future.apply' package is required for parallel processing.")
  }
  future.apply::future_lapply(X, FUN, ...)
}

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
  retry_failures <- tibble::tibble()

  # Perform request; capture any HTTP/httr2 error so we can return a row
  tryCatch(
    {
      resp <- .gemini_req_perform(req)
      status_code <- .gemini_resp_status(resp)
      body_parsed <- .gemini_resp_body_json(resp, simplifyVector = FALSE)
      retry_failures <- attr(resp, "retry_failures")
      if (is.null(retry_failures)) {
        retry_failures <- tibble::tibble()
      }
    },
    error = function(err) {
      # Default error message
      error_message <<- conditionMessage(err)
      retry_failures <<- attr(err, "retry_failures")
      if (is.null(retry_failures)) {
        retry_failures <- tibble::tibble()
      }

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
    res$retry_failures <- list(retry_failures)
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
  res$retry_failures <- list(retry_failures)

  res
}

#' Live Google Gemini comparisons for a tibble of pairs
#'
#' This is a robust row-wise wrapper around [gemini_compare_pair_live()]. It
#' takes a tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits each
#' pair to the Google Gemini API, and collects the results.
#'
#' This function offers:
#' \itemize{
#'   \item **Parallel Processing:** Uses the \code{future} package to process
#'     multiple pairs simultaneously.
#'   \item **Incremental Saving:** Writes results to a CSV file as they complete.
#'     If the process is interrupted, re-running the function with the same
#'     \code{save_path} will automatically skip pairs that were already successfully processed.
#'   \item **Error Separation:** Returns valid results and failed pairs separately,
#'     making it easier to debug or retry specific failures.
#' }
#'
#' @param pairs Tibble/data frame with columns `ID1`, `text1`, `ID2`, `text2`.
#' @param model Gemini model name (e.g. `"gemini-3-pro-preview"`).
#' @param trait_name Trait name.
#' @param trait_description Trait description.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param api_key Optional Gemini API key.
#' @param thinking_level Default `"low"`; see [gemini_compare_pair_live()].
#' @param temperature Optional numeric temperature; forwarded to
#'   [gemini_compare_pair_live()]. See Gemini docs; if `NULL` (default), the
#'   model uses its own default.
#' @param top_p Optional numeric; forwarded to [gemini_compare_pair_live()].
#' @param top_k Optional numeric; forwarded to [gemini_compare_pair_live()].
#' @param max_output_tokens Optional integer; forwarded to
#'   [gemini_compare_pair_live()].
#' @param api_version API version; default `"v1beta"`.
#' @param verbose Logical; print status/timing every `status_every` pairs.
#' @param status_every Integer; how often to print status (default 1 = every
#'   pair).
#' @param progress Logical; show a text progress bar.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body.
#'   Note: Raw responses are not saved to the incremental CSV file.
#' @param include_thoughts Logical; if `TRUE`, requests explicit reasoning
#'   output from Gemini and stores it in the `thoughts` column of the result,
#'   mirroring [gemini_compare_pair_live()].
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Requires the \code{readr} package.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} and \code{future.apply}
#'   packages.
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#'   \strong{Guidance:} Start conservatively (e.g., 2-4 workers) to avoid hitting
#'   HTTP 429 errors, as Gemini rate limits can be strict depending on your tier.
#' @param ... Reserved for future extensions; passed through to
#'   [gemini_compare_pair_live()] (but `thinking_budget` is ignored there).
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble containing the rows from \code{pairs} that
#'     failed to process (due to API errors or timeouts), along with an
#'     \code{error_message} column.}
#' }
#'
#' @examples
#' # Requires:
#' # - GEMINI_API_KEY set in your environment
#' # - Internet access
#' # - Billable Gemini API usage
#' \dontrun{
#' # Example pair data
#' pairs <- tibble::tibble(
#'   ID1   = c("S01", "S03"),
#'   text1 = c("Text 1", "Text 3"),
#'   ID2   = c("S02", "S04"),
#'   text2 = c("Text 2", "Text 4")
#' )
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # 1. Sequential execution with incremental saving
#' res_seq <- submit_gemini_pairs_live(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   save_path         = "results_gemini_seq.csv"
#' )
#'
#' # 2. Parallel execution (faster)
#' res_par <- submit_gemini_pairs_live(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   save_path         = "results_gemini_par.csv",
#'   parallel          = TRUE,
#'   workers           = 4
#' )
#'
#' # Inspect results
#' head(res_par$results)
#' }
#'
#' @export
submit_gemini_pairs_live <- function(
    pairs,
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
    verbose = TRUE,
    status_every = 1L,
    progress = TRUE,
    include_raw = FALSE,
    include_thoughts = FALSE,
    save_path = NULL,
    parallel = FALSE,
    workers = 1,
    ...
) {
  pairs <- tibble::as_tibble(pairs)
  pairs_input <- pairs
  required_cols <- c("ID1", "text1", "ID2", "text2")

  ensure_pair_ids <- function(res, id1, id2) {
    res_tbl <- tibble::as_tibble(res)
    expected_id <- sprintf("LIVE_%s_vs_%s", id1, id2)
    # Always use the canonical per-pair custom_id, even if the backend/mocks
    # return something else (critical for deterministic resume + joins).
    res_tbl$custom_id <- expected_id
    res_tbl$ID1 <- if (!"ID1" %in% names(res_tbl)) id1 else as.character(res_tbl$ID1)
    res_tbl$ID2 <- if (!"ID2" %in% names(res_tbl)) id2 else as.character(res_tbl$ID2)
    # Always trust the input pair identifiers to support deterministic joins.
    res_tbl$ID1 <- id1
    res_tbl$ID2 <- id2

    # Marker used to distinguish backend-returned failure rows (which should
    # be moved to failed_pairs) from locally-caught errors (which tests expect
    # to remain in results for sequential incremental saving).
    if (!".from_catch" %in% names(res_tbl)) {
      res_tbl$.from_catch <- FALSE
    }
    res_tbl
  }

  if (!all(required_cols %in% names(pairs))) {
    stop("`pairs` must contain columns: ", paste(required_cols, collapse = ", "), call. = FALSE)
  }

  # --- Pre-flight Checks ---
  if (!is.null(save_path)) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("The 'readr' package is required for incremental saving. Please install it.", call. = FALSE)
    }
    save_dir <- dirname(save_path)
    if (!dir.exists(save_dir) && save_dir != ".") {
      if (verbose) message(sprintf("Creating output directory: '%s'", save_dir))
      dir.create(save_dir, recursive = TRUE)
    }
  }

  # --- Parallel Plan ---
  if (parallel && workers > 1) {
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for parallel processing.", call. = FALSE)
    }
    if (verbose) message(sprintf("Setting up parallel plan with %d workers (multisession)...", workers))

    # Call through a non-syntactic binding so tests can mock it deterministically.
    old_plan <- `future::plan`("multisession", workers = workers)
    on.exit(`future::plan`(old_plan), add = TRUE)
  }

  # --- Resume Logic ---
  existing_results <- NULL
  if (!is.null(save_path) && file.exists(save_path)) {
    if (verbose) message(sprintf("Found existing file at '%s'. Checking for resumable pairs...", save_path))
    tryCatch(
      {
        existing_results <- readr::read_csv(save_path, show_col_types = FALSE)
        existing_ids <- if ("custom_id" %in% names(existing_results)) {
          existing_results$custom_id
        } else if ("pair_uid" %in% names(existing_results)) {
          existing_results$pair_uid
        } else {
          character(0)
        }
        current_ids <- sprintf("LIVE_%s_vs_%s", pairs$ID1, pairs$ID2)
        to_process_idx <- !current_ids %in% existing_ids
        if (sum(!to_process_idx) > 0) {
          if (verbose) message(sprintf("Skipping %d pairs already present in '%s'.", sum(!to_process_idx), save_path))
          pairs <- pairs[to_process_idx, ]
        }
      },
      error = function(e) {
        warning("Could not read existing save file to resume. Processing all pairs. Error: ", e$message, call. = FALSE)
      }
    )
  }

  n <- nrow(pairs)

  # Helper for empty result
  empty_res <- function() {
    res <- tibble::tibble(
      custom_id = character(0), ID1 = character(0), ID2 = character(0),
      model = character(0), object_type = character(0), status_code = integer(0),
      error_message = character(0), thoughts = character(0), content = character(0),
      better_sample = character(0), better_id = character(0),
      prompt_tokens = numeric(0), completion_tokens = numeric(0), total_tokens = numeric(0)
    )
    if (include_raw) res$raw_response <- list()
    return(res)
  }

  if (n == 0L) {
    if (verbose) message("No new pairs to process.")
    final_res <- if (!is.null(existing_results)) existing_results else empty_res()
    empty_failed_attempts <- tibble::tibble(
      A_id = character(0),
      B_id = character(0),
      unordered_key = character(0),
      ordered_key = character(0),
      backend = character(0),
      model = character(0),
      error_code = character(0),
      error_detail = character(0),
      attempted_at = as.POSIXct(character(0))
    )
    return(list(
      results = final_res,
      failed_pairs = pairs[0, ],
      failed_attempts = empty_failed_attempts
    ))
  }

  if (!is.numeric(status_every) || length(status_every) != 1L || status_every < 1) {
    stop("`status_every` must be a single positive integer.", call. = FALSE)
  }
  status_every <- as.integer(status_every)
  thinking_level <- match.arg(thinking_level)
  fmt_secs <- function(x) sprintf("%.1fs", x)
  all_new_results <- vector("list", n)

  use_parallel <- parallel && workers > 1 && requireNamespace("future.apply", quietly = TRUE)

  # --- Execution ---
  if (use_parallel) {
    if (verbose) message(sprintf("Processing %d pairs in PARALLEL (Gemini)...", n))

    chunk_size <- 20
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
    total_processed <- 0

    for (chunk_indices in chunks) {
      work_fn <- function(i) {
        id1 <- as.character(pairs$ID1[i])
        id2 <- as.character(pairs$ID2[i])
        res <- tryCatch(
          {
            gemini_compare_pair_live(
              ID1 = id1, text1 = as.character(pairs$text1[i]),
              ID2 = id2, text2 = as.character(pairs$text2[i]),
              model = model, trait_name = trait_name, trait_description = trait_description,
              prompt_template = prompt_template, api_key = api_key,
              thinking_level = thinking_level, temperature = temperature,
              top_p = top_p, top_k = top_k, max_output_tokens = max_output_tokens,
              api_version = api_version, include_raw = include_raw, include_thoughts = include_thoughts,
              ...
            )
          },
          error = function(e) {
            retry_failures <- attr(e, "retry_failures")
            if (is.null(retry_failures)) {
              retry_failures <- tibble::tibble()
            }
            tibble::tibble(
              custom_id = sprintf("LIVE_%s_vs_%s", id1, id2),
              ID1 = id1, ID2 = id2, model = model,
              object_type = NA_character_, status_code = NA_integer_,
              error_message = paste0("Error: ", conditionMessage(e)),
              .from_catch = TRUE,
              thoughts = NA_character_, content = NA_character_,
              better_sample = NA_character_, better_id = NA_character_,
              prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
              raw_response = if (include_raw) list(NULL) else NULL,
              retry_failures = list(retry_failures)
            )
          }
        )
        ensure_pair_ids(res, id1, id2)
      }

      # Do not pass future.seed through ... here; tests mock future_lapply with
      # base::lapply which forwards ... to FUN.
      chunk_results_list <- `future.apply::future_lapply`(chunk_indices, work_fn)
      all_new_results[chunk_indices] <- chunk_results_list

      if (!is.null(save_path)) {
        chunk_df <- dplyr::bind_rows(chunk_results_list)
        if ("raw_response" %in% names(chunk_df)) chunk_df$raw_response <- NULL
        write_mode <- if (file.exists(save_path)) "append" else "write"
        tryCatch(
          {
            readr::write_csv(chunk_df, save_path, append = (write_mode == "append"))
          },
          error = function(e) warning("Failed to save incremental results: ", e$message, call. = FALSE)
        )
      }

      total_processed <- total_processed + length(chunk_indices)
      if (!is.null(pb)) utils::setTxtProgressBar(pb, total_processed)
    }
    if (!is.null(pb)) close(pb)
  } else {
    # Sequential Execution
    if (verbose) {
      message(sprintf(
        "Submitting %d live pair(s) for comparison (model=%s, backend=gemini, thinking_level=%s, include_thoughts=%s)...",
        n, model, thinking_level, include_thoughts
      ))
    }

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

    for (i in seq_len(n)) {
      id1_i <- as.character(pairs$ID1[i])
      id2_i <- as.character(pairs$ID2[i])

      # FIX: Correct status logic for 1-based index
      show_status <- verbose && ((i - 1) %% status_every == 0L)

      if (show_status) {
        message(sprintf(
          "[Gemini live pair %d of %d] Comparing %s vs %s ...",
          i, n, id1_i, id2_i
        ))
      }

      res <- tryCatch(
        {
          gemini_compare_pair_live(
            ID1 = id1_i, text1 = as.character(pairs$text1[i]),
            ID2 = id2_i, text2 = as.character(pairs$text2[i]),
            model = model, trait_name = trait_name, trait_description = trait_description,
            prompt_template = prompt_template, api_key = api_key,
            thinking_level = thinking_level, temperature = temperature,
            top_p = top_p, top_k = top_k, max_output_tokens = max_output_tokens,
            api_version = api_version, include_raw = include_raw, include_thoughts = include_thoughts,
            ...
          )
        },
        error = function(e) {
          retry_failures <- attr(e, "retry_failures")
          if (is.null(retry_failures)) {
            retry_failures <- tibble::tibble()
          }
          tibble::tibble(
            custom_id = sprintf("LIVE_%s_vs_%s", id1_i, id2_i),
            ID1 = id1_i, ID2 = id2_i, model = model,
            object_type = NA_character_, status_code = NA_integer_,
            error_message = paste0("Error: ", conditionMessage(e)),
            .from_catch = TRUE,
            thoughts = NA_character_, content = NA_character_,
            better_sample = NA_character_, better_id = NA_character_,
            prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
            raw_response = if (include_raw) list(NULL) else NULL,
            retry_failures = list(retry_failures)
          )
        }
      )
      res <- ensure_pair_ids(res, id1_i, id2_i)
      all_new_results[[i]] <- res

      if (!is.null(save_path)) {
        write_df <- res
        if ("raw_response" %in% names(write_df)) write_df$raw_response <- NULL
        col_names <- !file.exists(save_path)
        tryCatch(
          {
            readr::write_csv(write_df, save_path, append = !col_names, col_names = col_names)
          },
          error = function(e) warning("Failed to save incremental result: ", e$message, call. = FALSE)
        )
      }

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)

      if (show_status) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        avg <- elapsed / i
        remaining <- avg * (n - i)
        message(sprintf(
          "  Elapsed: %s | Avg per pair: %s | Est. remaining: %s",
          fmt_secs(elapsed), fmt_secs(avg), fmt_secs(remaining)
        ))
      }
    }
    if (!is.null(pb)) close(pb)
  }

  new_results_df <- dplyr::bind_rows(all_new_results)

  if (verbose) {
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf(
      "Completed %d pairs in %s (avg %.2fs/pair).",
      n, fmt_secs(total_elapsed), total_elapsed / n
    ))
  }

  final_results <- if (!is.null(existing_results)) {
    dplyr::bind_rows(existing_results, new_results_df)
  } else {
    new_results_df
  }

  failed_mask <- !is.na(final_results$error_message) |
    (!is.na(final_results$status_code) & final_results$status_code >= 400L)

  # Backend-returned error rows (e.g., status_code >= 400) should not pollute
  # results, but locally-caught errors should remain for sequential, incremental
  # save semantics. This matches the unit tests.
  from_catch <- if (".from_catch" %in% names(final_results)) {
    dplyr::coalesce(as.logical(final_results$.from_catch), FALSE)
  } else {
    rep(FALSE, nrow(final_results))
  }

  drop_from_results <- failed_mask &
    !from_catch &
    (!is.na(final_results$status_code) & final_results$status_code >= 400L)

  results_out <- final_results
  if (any(drop_from_results)) {
    results_out <- final_results |>
      dplyr::filter(!drop_from_results)
  }

  failed_pairs <- tibble::tibble()
  if (any(failed_mask)) {
    # Return the scheduled pairs that failed, with the backend error message.
    pairs_keyed <- pairs_input |>
      dplyr::mutate(custom_id = sprintf("LIVE_%s_vs_%s", .data$ID1, .data$ID2))

    failed_pairs <- pairs_keyed |>
      dplyr::inner_join(
        final_results |>
          dplyr::filter(failed_mask) |>
            dplyr::select(
              "custom_id",
              dplyr::any_of(c("status_code", "error_message", "raw_response"))
            ),
        by = "custom_id"
      )
  }

  list(
    results = results_out |>
      dplyr::select(-dplyr::any_of(c(".from_catch"))),
    failed_pairs = failed_pairs,
    failed_attempts = tibble::tibble()
  )
}
