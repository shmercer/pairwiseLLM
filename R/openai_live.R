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
#'   \item{custom_id}{Stable ID for the pair (\code{pair_uid} if supplied via
#'     \code{...}; otherwise \code{"LIVE_<ID1>_vs_<ID2>"}).}
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
  api_key = NULL,
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
  pair_uid <- dots$pair_uid %||% NULL

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

  # ✅ Resolve key only at the last responsible moment (right before HTTP)
  api_key <- .openai_api_key(api_key)

  req <- .openai_request(path, api_key)
  req <- .openai_req_body_json(req, body = body)
  resp <- .openai_req_perform(req)
  status_code <- .openai_resp_status(resp)
  retry_failures <- attr(resp, "retry_failures")
  if (is.null(retry_failures)) {
    retry_failures <- tibble::tibble()
  }

  body_parsed <- tryCatch(
    .openai_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  custom_id <- .pairwiseLLM_make_custom_id(ID1, ID2, pair_uid)

  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id = custom_id,
      ID1 = ID1, ID2 = ID2, model = NA_character_, object_type = NA_character_,
      status_code = status_code, error_message = "Failed to parse JSON.",
      thoughts = NA_character_, content = NA_character_,
      better_sample = NA_character_, better_id = NA_character_,
      prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_
    )
    if (include_raw) res$raw_response <- list(NULL)
    res$retry_failures <- list(retry_failures)
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
    custom_id = custom_id,
    ID1 = ID1, ID2 = ID2, model = model_name, object_type = object_type,
    status_code = status_code, error_message = error_message,
    thoughts = thoughts, content = content,
    better_sample = better_sample, better_id = better_id,
    prompt_tokens = as.numeric(usage$prompt_tokens %||% usage$input_tokens %||% NA),
    completion_tokens = as.numeric(usage$completion_tokens %||% usage$output_tokens %||% NA),
    total_tokens = as.numeric(usage$total_tokens %||% NA)
  )

  if (include_raw) res$raw_response <- list(body)
  res$retry_failures <- list(retry_failures)
  res
}

#' Live OpenAI comparisons for a tibble of pairs
#'
#' This is a robust row-wise wrapper around
#' \code{\link{openai_compare_pair_live}}. It takes a tibble of pairs
#' (ID1 / text1 / ID2 / text2), submits each pair to the OpenAI API, and
#' collects the results.
#'
#' This function improves upon simple looping by offering:
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
#'   \code{status_every}-th pair. Defaults to 1 (every pair).
#' @param progress Logical; if TRUE, shows a textual progress bar.
#' @param include_raw Logical; if TRUE, each row of the returned tibble will
#'   include a \code{raw_response} list-column with the parsed JSON body from
#'   OpenAI. Note: Raw responses are not saved to the incremental CSV file.
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Requires the \code{readr} package.
#' @param parallel Logical; if TRUE, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} and \code{future.apply}
#'   packages.
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#'   \strong{Guidance:} A value between 4 and 8 is usually safe. Setting this
#'   too high (e.g., >20) may trigger OpenAI rate limit errors (HTTP 429)
#'   depending on your usage tier.
#' @param ... Additional OpenAI parameters (temperature, top_p, logprobs,
#'   reasoning, and so on) passed on to \code{openai_compare_pair_live}.
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair and
#'     columns such as \code{better_id}, \code{better_sample}, \code{thoughts},
#'     and \code{content}. See \code{\link{openai_compare_pair_live}} for
#'     details.}
#'   \item{failed_pairs}{A tibble containing the rows from \code{pairs} that
#'     failed to process (due to API errors or timeouts), along with an
#'     \code{error_message} column. These can be easily re-submitted.}
#'   \item{failed_attempts}{A tibble of attempt-level failures (retries,
#'     timeouts, parse errors, invalid winners), separate from observed outcomes.}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires API key set and internet access
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 10, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # 1. Sequential execution with incremental saving
#' # If interrupted, running this again will resume progress.
#' res_seq <- submit_openai_pairs_live(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   save_path         = "results_seq.csv"
#' )
#'
#' # 2. Parallel execution (faster)
#' # Note: On Windows, this opens background R sessions.
#' res_par <- submit_openai_pairs_live(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   save_path         = "results_par.csv",
#'   parallel          = TRUE,
#'   workers           = 4
#' )
#'
#' # Inspect results
#' head(res_par$results)
#'
#' # Check for failures
#' if (nrow(res_par$failed_pairs) > 0) {
#'   message("Some pairs failed:")
#'   print(res_par$failed_pairs)
#' }
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
  api_key = NULL,
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
) {
  endpoint <- match.arg(endpoint)
  pairs <- tibble::as_tibble(pairs)
  pairs_input <- pairs
  required_cols <- c("ID1", "text1", "ID2", "text2")

  if (!all(required_cols %in% names(pairs))) {
    stop("`pairs` must contain columns: ", paste(required_cols, collapse = ", "), call. = FALSE)
  }

  # --- Pre-flight Checks: Dependencies & Directories ---
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

  # --- Handle Parallel Plan Internally ---
  # We use 'multisession' as it is robust across Windows, Mac, and Linux.
  if (parallel && workers > 1) {
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for parallel processing.", call. = FALSE)
    }

    if (verbose) message(sprintf("Setting up parallel plan with %d workers (multisession)...", workers))

    # Set the plan and capture the OLD plan to restore it on exit
    old_plan <- future::plan("multisession", workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  # --- Resume Logic ---
  existing_results <- NULL
  if (!is.null(save_path) && file.exists(save_path)) {
    if (verbose) message(sprintf("Found existing file at '%s'. Checking for resumable pairs...", save_path))
    tryCatch(
      {
        existing_results <- readr::read_csv(save_path, show_col_types = FALSE)

        # We assume custom_id is constructed as LIVE_<ID1>_vs_<ID2>
        existing_ids <- if ("custom_id" %in% names(existing_results)) {
          existing_results$custom_id
        } else if ("pair_uid" %in% names(existing_results)) {
          existing_results$pair_uid
        } else {
          character(0)
        }
        current_ids <- .pairwiseLLM_make_custom_id(pairs$ID1, pairs$ID2, if ("pair_uid" %in% names(pairs)) pairs$pair_uid else NULL)

        # Identify new pairs
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

  # Empty Result Helper
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
    # If resuming and all are done, failed_pairs is empty relative to the *new* input
    return(list(
      results = final_res,
      failed_pairs = pairs[0, ],
      failed_attempts = empty_failed_attempts
    ))
  }

  if (!is.numeric(status_every) || length(status_every) != 1L || status_every < 1) {
    stop("`status_every` must be a single positive integer.", call. = FALSE)
  }

  fmt_secs <- function(x) sprintf("%.1fs", x)
  all_new_results <- vector("list", n)

  # Determine if we really can use parallel
  use_parallel <- parallel && workers > 1 && requireNamespace("future.apply", quietly = TRUE)

  # --- Execution ---
  if (use_parallel) {
    if (verbose) message(sprintf("Processing %d pairs in PARALLEL...", n))

    # Chunking to allow incremental saving during parallel execution
    chunk_size <- 20
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
    total_processed <- 0

    for (chunk_indices in chunks) {
      work_fn <- function(i) {
        id1 <- as.character(pairs$ID1[i])
        id2 <- as.character(pairs$ID2[i])
        pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
        tryCatch(
          {
            openai_compare_pair_live(
              ID1 = id1, text1 = as.character(pairs$text1[i]),
              ID2 = id2, text2 = as.character(pairs$text2[i]),
              model = model, trait_name = trait_name, trait_description = trait_description,
              prompt_template = prompt_template, endpoint = endpoint,
              api_key = api_key, include_raw = include_raw, pair_uid = pair_uid, ...
            )
          },
          error = function(e) {
            retry_failures <- attr(e, "retry_failures")
            if (is.null(retry_failures)) {
              retry_failures <- tibble::tibble()
            }
            tibble::tibble(
              custom_id = .pairwiseLLM_make_custom_id(id1, id2, pair_uid),
              ID1 = id1, ID2 = id2, model = model, object_type = NA_character_,
              status_code = NA_integer_,
              error_message = paste0("Error: ", conditionMessage(e)),
              thoughts = NA_character_, content = NA_character_,
              better_sample = NA_character_, better_id = NA_character_,
              prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
              raw_response = if (include_raw) list(NULL) else NULL,
              retry_failures = list(retry_failures)
            )
          }
        )
      }

      # Execute chunk
      chunk_results_list <- future.apply::future_lapply(chunk_indices, work_fn, future.seed = TRUE)
      all_new_results[chunk_indices] <- chunk_results_list

      # Incremental Save
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
    if (verbose) message(sprintf("Processing %d pairs SEQUENTIALLY...", n))
    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

    for (i in seq_len(n)) {
      pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
      res <- tryCatch(
        {
          openai_compare_pair_live(
            ID1 = as.character(pairs$ID1[i]), text1 = as.character(pairs$text1[i]),
            ID2 = as.character(pairs$ID2[i]), text2 = as.character(pairs$text2[i]),
            model = model, trait_name = trait_name, trait_description = trait_description,
            prompt_template = prompt_template, endpoint = endpoint,
            api_key = api_key, include_raw = include_raw, pair_uid = pair_uid, ...
          )
        },
        error = function(e) {
          retry_failures <- attr(e, "retry_failures")
          if (is.null(retry_failures)) {
            retry_failures <- tibble::tibble()
          }
          tibble::tibble(
            custom_id = .pairwiseLLM_make_custom_id(pairs$ID1[i], pairs$ID2[i], pair_uid),
            ID1 = as.character(pairs$ID1[i]), ID2 = as.character(pairs$ID2[i]), model = model,
            object_type = NA_character_, status_code = NA_integer_,
            error_message = paste0("Error: ", conditionMessage(e)),
            thoughts = NA_character_, content = NA_character_,
            better_sample = NA_character_, better_id = NA_character_,
            prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
            raw_response = if (include_raw) list(NULL) else NULL,
            retry_failures = list(retry_failures)
          )
        }
      )
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

  # Merge existing (resume) results with new results
  final_results <- if (!is.null(existing_results)) {
    dplyr::bind_rows(existing_results, new_results_df)
  } else {
    new_results_df
  }

  # Filter failures
  failed_mask <- !is.na(final_results$error_message) |
    (final_results$status_code >= 400 & !is.na(final_results$status_code))

  normalized <- .normalize_llm_results(
    raw = final_results,
    pairs = pairs_input,
    backend = "openai",
    model = model,
    include_raw = include_raw
  )

  list(
    results = normalized$results,
    failed_pairs = normalized$failed_pairs,
    failed_attempts = normalized$failed_attempts
  )
}
