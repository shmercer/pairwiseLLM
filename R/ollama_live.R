#' Live Ollama comparison for a single pair of samples
#'
#' `ollama_compare_pair_live()` sends a single pairwise comparison prompt to a
#' local Ollama server and parses the result into the standard pairwiseLLM
#' tibble format.
#'
#' The function targets the `/api/generate` endpoint on a running Ollama
#' instance and expects a single non-streaming response. Model names should
#' match those available in your Ollama installation (for example
#' `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).
#'
#' Temperature and context length are controlled as follows:
#'
#' \itemize{
#'   \item By default, \code{temperature = 0} for all models.
#'   \item For Qwen models (model names beginning with \code{"qwen"}) and
#'     \code{think = TRUE}, \code{temperature} is set to \code{0.6}.
#'   \item The context window is set via \code{options$num_ctx}, which
#'     defaults to \code{8192L} but may be overridden via the \code{num_ctx}
#'     argument.
#' }
#'
#' If the Ollama response includes a \code{thinking} field (as described in the
#' Ollama API), that string is stored in the \code{thoughts} column of the
#' returned tibble; otherwise \code{thoughts} is \code{NA}. This allows
#' pairwiseLLM to consume Ollama's native thinking output in a way that is
#' consistent with other backends that expose explicit reasoning traces.
#'
#' The Ollama backend is intended to be compatible with the existing OpenAI,
#' Anthropic, and Gemini backends, so the returned tibble can be used
#' directly with downstream helpers such as [build_bt_data()] and
#' [fit_bt_model()].
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Ollama model name (for example \code{"mistral-small3.2:24b"},
#'   \code{"qwen3:32b"}, \code{"gemma3:27b"}).
#' @param trait_name Short label for the trait (for example
#'   \code{"Overall Quality"}).
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param host Base URL of the Ollama server. Defaults to the option
#'   \code{getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434")}.
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   \code{"</BETTER_SAMPLE>"}.
#' @param think Logical; if \code{TRUE} and the model is a Qwen model (name
#'   starts with \code{"qwen"}), the temperature is set to \code{0.6}.
#'   Otherwise the temperature is \code{0}. The \code{think} argument does
#'   not itself modify the HTTP request body; it is used only for choosing
#'   the temperature, but the function will parse a \code{thinking} field
#'   from the response whenever one is present.
#' @param num_ctx Integer; context window to use via \code{options$num_ctx}.
#'   The default is \code{8192L}.
#' @param include_raw Logical; if \code{TRUE}, adds a list-column
#'   \code{raw_response} containing the parsed JSON body returned by Ollama
#'   (or \code{NULL} on parse failure). This is useful for debugging.
#' @param ... Reserved for future extensions. When `pair_uid` is supplied via
#'   `...`, it is used verbatim as `custom_id`.
#'
#' @return A tibble with one row and columns:
#'
#' \itemize{
#'   \item \code{custom_id} – stable ID for the pair (\code{pair_uid} if
#'     supplied via \code{...}; otherwise \code{"LIVE_<ID1>_vs_<ID2>"}).
#'   \item \code{ID1}, \code{ID2} – the sample IDs supplied to the function.
#'   \item \code{model} – model name reported by the API (or the requested
#'     model).
#'   \item \code{object_type} – backend object type (for example
#'     \code{"ollama.generate"}).
#'   \item \code{status_code} – HTTP-style status code (\code{200} if
#'     successful).
#'   \item \code{error_message} – error message if something goes wrong;
#'     otherwise \code{NA}.
#'   \item \code{thoughts} – reasoning / thinking text when a
#'     \code{thinking} field is returned by Ollama; otherwise \code{NA}.
#'   \item \code{content} – visible response text from the model (from the
#'     \code{response} field).
#'   \item \code{better_sample} – \code{"SAMPLE_1"}, \code{"SAMPLE_2"}, or
#'     \code{NA}, based on tags found in \code{content}.
#'   \item \code{better_id} – \code{ID1} if \code{"SAMPLE_1"} is chosen,
#'     \code{ID2} if \code{"SAMPLE_2"} is chosen, otherwise \code{NA}.
#'   \item \code{prompt_tokens} – prompt / input token count (if reported).
#'   \item \code{completion_tokens} – completion / output token count (if
#'     reported).
#'   \item \code{total_tokens} – total token count (if reported).
#'   \item \code{raw_response} – optional list-column containing the parsed
#'     JSON body (present only when \code{include_raw = TRUE}).
#' }
#'
#' @details
#' In typical workflows, users will call [llm_compare_pair()] with
#' \code{backend = "ollama"} rather than using
#' \code{ollama_compare_pair_live()} directly. The direct helper is exported
#' so that advanced users can work with Ollama in a more explicit and
#' backend-specific way.
#'
#' The function assumes that:
#'
#' \itemize{
#'   \item An Ollama server is running and reachable at \code{host}.
#'   \item The requested \code{model} has already been pulled, for example
#'     via \code{ollama pull mistral-small3.2:24b} on the command line.
#' }
#'
#' When the Ollama response includes a \code{thinking} field (as documented
#' in the Ollama API), that string is copied into the \code{thoughts} column
#' of the returned tibble; otherwise \code{thoughts} is \code{NA}. This
#' parsed thinking output can be logged, inspected, or analyzed alongside
#' the visible comparison decisions.
#'
#' @examples
#' \dontrun{
#' # Requires a running Ollama server and locally available models.
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' ID1 <- example_writing_samples$ID[1]
#' ID2 <- example_writing_samples$ID[2]
#' text1 <- example_writing_samples$text[1]
#' text2 <- example_writing_samples$text[2]
#'
#' # Make sure an Ollama server is running
#'
#' # mistral example
#' res_mistral <- ollama_compare_pair_live(
#'   ID1               = ID1,
#'   text1             = text1,
#'   ID2               = ID2,
#'   text2             = text2,
#'   model             = "mistral-small3.2:24b",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' res_mistral$better_id
#'
#' # qwen example with reasoning
#' res_qwen_think <- ollama_compare_pair_live(
#'   ID1               = ID1,
#'   text1             = text1,
#'   ID2               = ID2,
#'   text2             = text2,
#'   model             = "qwen3:32b",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   think             = TRUE,
#'   include_raw       = TRUE
#' )
#'
#' res_qwen_think$better_id
#' res_qwen_think$thoughts
#' }
#'
#' @seealso
#' * [submit_ollama_pairs_live()] for single-backend, row-wise comparisons.
#' * [llm_compare_pair()] for backend-agnostic single-pair comparisons.
#' * [submit_llm_pairs()] for backend-agnostic comparisons over tibbles of
#'   pairs.
#'
#' @export
ollama_compare_pair_live <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  host = getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434"),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  think = FALSE,
  num_ctx = 8192L,
  include_raw = FALSE,
  ...
) {
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
  if (!is.character(host) || length(host) != 1L || !nzchar(host)) {
    stop("`host` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.numeric(num_ctx) || length(num_ctx) != 1L || num_ctx <= 0) {
    stop("`num_ctx` must be a single positive number.", call. = FALSE)
  }

  pair_uid <- list(...)$pair_uid %||% NULL

  # Temperature rule:
  # - default: 0
  # - Qwen + think = TRUE: 0.6
  is_qwen <- grepl("^qwen", model, ignore.case = TRUE)
  temperature <- if (isTRUE(think) && is_qwen) 0.6 else 0

  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    # Explicitly control Ollama thinking behavior
    think = isTRUE(think),
    options = list(
      num_ctx     = as.integer(num_ctx),
      temperature = temperature
    )
  )

  # Build request: httr2 pieces qualified; httr2 verbs we want
  # to mock unqualified
  req <- httr2::request(host) |>
    httr2::req_url_path_append("api", "generate") |>
    req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- .retry_httr2_request(req)
  retry_failures <- attr(resp, "retry_failures")
  if (is.null(retry_failures)) {
    retry_failures <- tibble::tibble()
  }

  status_code <- resp_status(resp)
  error_message <- NA_character_

  body_parsed <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  custom_id <- .pairwiseLLM_make_custom_id(ID1, ID2, pair_uid)

  # If parsing fails, return a stub row with an error message
  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id         = custom_id,
      ID1               = ID1,
      ID2               = ID2,
      model             = NA_character_,
      object_type       = "ollama.generate",
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
    res$retry_failures <- list(retry_failures)

    return(res)
  }

  body <- body_parsed

  model_name <- body$model %||% model
  object_type <- "ollama.generate"

  # Only expose thinking when think = TRUE
  raw_thinking <- body$thinking %||% NULL
  thoughts <- if (isTRUE(think) && !is.null(raw_thinking)) {
    as.character(raw_thinking)
  } else {
    NA_character_
  }

  content <- body$response %||% NA_character_

  # Extract better_sample and better_id from tag in content
  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(
      paste0(tag_prefix, "SAMPLE_1", tag_suffix),
      content,
      fixed = TRUE
    )) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(
      paste0(tag_prefix, "SAMPLE_2", tag_suffix),
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

  # Token counts: Ollama exposes prompt_eval_count + eval_count
  prompt_tokens <- body$prompt_eval_count %||% NA_real_
  completion_tokens <- body$eval_count %||% NA_real_
  total_tokens <- NA_real_
  if (!is.na(prompt_tokens) && !is.na(completion_tokens)) {
    total_tokens <- as.numeric(prompt_tokens) + as.numeric(completion_tokens)
  }

  # Error handling: non-200 or explicit error message
  if (status_code != 200L) {
    raw_err <- body$error %||% body$message %||% NULL
    if (!is.null(raw_err)) {
      error_message <- as.character(raw_err)
    } else {
      error_message <- sprintf(
        "Ollama request failed with status %s.",
        status_code
      )
    }
  }

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
    res$raw_response <- list(body)
  }
  res$retry_failures <- list(retry_failures)

  res
}

#' Live Ollama comparisons for a tibble of pairs
#'
#' `submit_ollama_pairs_live()` is a robust row-wise wrapper around
#' [ollama_compare_pair_live()]. It takes a tibble of pairs (`ID1` / `text1` /
#' `ID2` / `text2`), submits each pair to a local (or remote) Ollama server,
#' and collects the results.
#'
#' This function offers:
#' \itemize{
#'   \item **Incremental Saving:** Writes results to a CSV file as they complete.
#'     If the process is interrupted, re-running the function with the same
#'     \code{save_path} will automatically skip pairs that were already successfully processed.
#'   \item **Parallel Processing:** Uses the \code{future} package to process
#'     multiple pairs simultaneously. \strong{Note:} Since Ollama typically runs
#'     locally on the GPU, parallel processing may degrade performance or cause
#'     out-of-memory errors unless the hardware can handle concurrent requests.
#'     Defaults are set to sequential processing.
#' }
#'
#' Temperature and context length are controlled as follows:
#'
#' * By default, `temperature = 0` for all models.
#' * For Qwen models (model names beginning with `"qwen"`) and `think = TRUE`,
#'   `temperature` is set to `0.6`.
#' * The context window is set via `options$num_ctx`, which defaults to
#'   `8192` but may be overridden via the `num_ctx` argument.
#'
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`. Typically created by [make_pairs()], [sample_pairs()],
#'   and [randomize_pair_order()].
#' @param model Ollama model name (for example `"mistral-small3.2:24b"`,
#'   `"qwen3:32b"`, `"gemma3:27b"`).
#' @param trait_name Trait name to pass to [ollama_compare_pair_live()].
#' @param trait_description Trait description to pass to
#'   [ollama_compare_pair_live()].
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param host Base URL of the Ollama server. Defaults to the option
#'   `getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434")`.
#' @param verbose Logical; if `TRUE`, prints status, timing, and result
#'   summaries.
#' @param status_every Integer; print status and timing for every
#'   `status_every`-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if `TRUE`, shows a textual progress bar.
#' @param think Logical; see [ollama_compare_pair_live()] for behavior. When
#'   `TRUE` and the model name starts with `"qwen"`, the temperature is set
#'   to `0.6`; otherwise the temperature remains `0`.
#' @param num_ctx Integer; context window to use via `options$num_ctx`. The
#'   default is `8192L`.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body from
#'   Ollama. Note: Raw responses are not saved to the incremental CSV file.
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Requires the \code{readr} package.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} and \code{future.apply}
#'   packages. Defaults to `FALSE`.
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#' @param ... Reserved for future extensions and forwarded to
#'   [ollama_compare_pair_live()].
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble containing the rows from \code{pairs} that
#'     failed to process (due to API errors or timeouts), along with an
#'     \code{error_message} column.}
#'   \item{failed_attempts}{A tibble of attempt-level failures (retries,
#'     timeouts, parse errors, invalid winners), separate from observed outcomes.}
#' }
#'
#' @details
#' In most user-facing workflows, it is more convenient to call
#' [submit_llm_pairs()] with `backend = "ollama"` rather than using
#' `submit_ollama_pairs_live()` directly.
#'
#' As with [ollama_compare_pair_live()], this function assumes that:
#'
#' * An Ollama server is running and reachable at `host`.
#' * The requested models have been pulled in advance (for example
#'   `ollama pull mistral-small3.2:24b`).
#'
#' @examples
#' \dontrun{
#' # Requires a running Ollama server and locally available models.
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
#' # Live comparisons with incremental saving
#' res_mistral <- submit_ollama_pairs_live(
#'   pairs             = pairs,
#'   model             = "mistral-small3.2:24b",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   save_path         = "ollama_results.csv",
#'   verbose           = TRUE
#' )
#'
#' # Access results
#' res_mistral$results
#' }
#'
#' @seealso
#' * [ollama_compare_pair_live()] for single-pair Ollama comparisons.
#' * [submit_llm_pairs()] for backend-agnostic comparisons over tibbles of
#'   pairs.
#'
#' @export
submit_ollama_pairs_live <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  host = getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434"),
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  think = FALSE,
  num_ctx = 8192L,
  include_raw = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
) {
  pairs <- tibble::as_tibble(pairs)
  pairs_input <- pairs
  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols <- setdiff(required_cols, names(pairs))

  if (length(missing_cols) > 0L) {
    stop(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Pre-flight Checks (Save Path) ---
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
        if ("custom_id" %in% names(existing_results)) {
          existing_ids <- if ("custom_id" %in% names(existing_results)) {
            existing_results$custom_id
          } else if ("pair_uid" %in% names(existing_results)) {
            existing_results$pair_uid
          } else {
            character(0)
          }
          current_ids <- .pairwiseLLM_make_custom_id(
            pairs$ID1,
            pairs$ID2,
            if ("pair_uid" %in% names(pairs)) pairs$pair_uid else NULL
          )
          to_process_idx <- !current_ids %in% existing_ids
          if (sum(!to_process_idx) > 0) {
            if (verbose) message(sprintf("Skipping %d pairs already present in '%s'.", sum(!to_process_idx), save_path))
            pairs <- pairs[to_process_idx, ]
          }
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

  if (!is.numeric(status_every) || length(status_every) != 1L ||
    status_every < 1) {
    stop("`status_every` must be a single positive integer.", call. = FALSE)
  }
  status_every <- as.integer(status_every)

  fmt_secs <- function(x) sprintf("%.1fs", x)

  use_parallel <- parallel && workers > 1 && requireNamespace("future.apply", quietly = TRUE)
  all_new_results <- vector("list", n)

  # --- Execution ---
  if (use_parallel) {
    if (verbose) message(sprintf("Processing %d pairs in PARALLEL (Ollama, %d workers)...", n, workers))

    # We process in chunks to allow for some progress updates and incremental writes
    chunk_size <- 10 # Smaller chunk size for local LLMs usually safer
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
    total_processed <- 0

    for (chunk_indices in chunks) {
      work_fn <- function(i) {
        id1_i <- as.character(pairs$ID1[i])
        id2_i <- as.character(pairs$ID2[i])
        pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
        tryCatch(
          {
            ollama_compare_pair_live(
              ID1               = id1_i,
              text1             = as.character(pairs$text1[i]),
              ID2               = id2_i,
              text2             = as.character(pairs$text2[i]),
              model             = model,
              trait_name        = trait_name,
              trait_description = trait_description,
              prompt_template   = prompt_template,
              host              = host,
              think             = think,
              num_ctx           = num_ctx,
              include_raw       = include_raw,
              pair_uid          = pair_uid,
              ...
            )
          },
          error = function(e) {
            retry_failures <- attr(e, "retry_failures")
            if (is.null(retry_failures)) {
              retry_failures <- tibble::tibble()
            }
            # Return error row
            tibble::tibble(
              custom_id = .pairwiseLLM_make_custom_id(
                id1_i,
                id2_i,
                if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
              ),
              ID1 = id1_i,
              ID2 = id2_i,
              model = model,
              object_type = NA_character_,
              status_code = NA_integer_,
              error_message = paste0("Error: ", conditionMessage(e)),
              thoughts = NA_character_,
              content = NA_character_,
              better_sample = NA_character_,
              better_id = NA_character_,
              prompt_tokens = NA_real_,
              completion_tokens = NA_real_,
              total_tokens = NA_real_,
              raw_response = if (include_raw) list(NULL) else NULL,
              retry_failures = list(retry_failures)
            )
          }
        )
      }

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
    # --- Sequential Execution ---
    if (verbose) {
      message(sprintf(
        "Submitting %d live pair(s) for comparison (backend=ollama, model=%s)...",
        n, model
      ))
    }

    start_time <- Sys.time()
    pb <- if (progress && n > 0L) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

    for (i in seq_len(n)) {
      pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
      show_status <- verbose && ((i - 1) %% status_every == 0L)

      id1_i <- as.character(pairs$ID1[i])
      id2_i <- as.character(pairs$ID2[i])

      if (show_status) {
        message(sprintf(
          "[Live pair %d of %d] Comparing %s vs %s ...",
          i, n, id1_i, id2_i
        ))
      }

      res <- tryCatch(
        ollama_compare_pair_live(
          ID1               = id1_i,
          text1             = as.character(pairs$text1[i]),
          ID2               = id2_i,
          text2             = as.character(pairs$text2[i]),
          model             = model,
          trait_name        = trait_name,
          trait_description = trait_description,
          prompt_template   = prompt_template,
          host              = host,
          think             = think,
          num_ctx           = num_ctx,
          include_raw       = include_raw,
          pair_uid          = pair_uid,
          ...
        ),
        error = function(e) {
          retry_failures <- attr(e, "retry_failures")
          if (is.null(retry_failures)) {
            retry_failures <- tibble::tibble()
          }
          if (verbose) {
            message(sprintf(
              "    ERROR: Ollama comparison failed for pair %s vs %s: %s",
              id1_i, id2_i, conditionMessage(e)
            ))
          }

          out_row <- tibble::tibble(
            custom_id = .pairwiseLLM_make_custom_id(
              id1_i,
              id2_i,
              if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
            ),
            ID1 = id1_i,
            ID2 = id2_i,
            model = model,
            object_type = NA_character_,
            status_code = NA_integer_,
            error_message = paste0(
              "Error during Ollama comparison: ",
              conditionMessage(e)
            ),
            thoughts = NA_character_,
            content = NA_character_,
            better_sample = NA_character_,
            better_id = NA_character_,
            prompt_tokens = NA_real_,
            completion_tokens = NA_real_,
            total_tokens = NA_real_
          )

          if (include_raw) {
            out_row$raw_response <- list(NULL)
          }
          out_row$retry_failures <- list(retry_failures)

          out_row
        }
      )
      all_new_results[[i]] <- res

      # Incremental Save
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

      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, i)
      }

      if (!is.na(res$error_message) && verbose) {
        # Error already printed in tryCatch if verbose
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
    }
    if (!is.null(pb)) close(pb)
  }

  if (verbose) {
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    avg <- total_elapsed / n
    message(sprintf(
      "Completed %d live pair(s) in %s (avg %.2fs per pair).",
      n, fmt_secs(total_elapsed), avg
    ))
  }

  new_results_df <- dplyr::bind_rows(all_new_results)

  final_results <- if (!is.null(existing_results)) {
    dplyr::bind_rows(existing_results, new_results_df)
  } else {
    new_results_df
  }

  # Identify failures
  failed_mask <- !is.na(final_results$error_message)

  normalized <- .normalize_llm_results(
    raw = final_results,
    pairs = pairs_input,
    backend = "ollama",
    model = model,
    include_raw = include_raw
  )

  list(
    results = normalized$results,
    failed_pairs = normalized$failed_pairs,
    failed_attempts = normalized$failed_attempts
  )
}

#' Ensure only one Ollama model is loaded in memory
#'
#' `ensure_only_ollama_model_loaded()` is a small convenience helper for
#' managing memory when working with large local models via Ollama. It
#' inspects the current set of active models using the `ollama ps` command
#' and attempts to unload any models that are not the one you specify.
#'
#' This can be useful when running multiple large models (for example
#' `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`) on a single
#' machine, where keeping all of them loaded simultaneously may exhaust
#' GPU or system memory.
#'
#' The function is intentionally conservative:
#' \itemize{
#'   \item If the `ollama` command is not available on the system \emph{or}
#'         `ollama ps` returns an error or empty output, no action is taken
#'         and a message is printed when \code{verbose = TRUE}.
#'   \item If no active models are reported, no action is taken.
#'   \item Only models with names different from \code{model} are passed to
#'         \code{ollama stop <name>}.
#' }
#'
#' This helper is not called automatically by the package; it is intended
#' to be used programmatically in development scripts and ad hoc workflows
#' before running comparisons with [ollama_compare_pair_live()] or
#' [submit_ollama_pairs_live()].
#'
#' @param model Character scalar giving the Ollama model name that should
#'   remain loaded (for example `"mistral-small3.2:24b"`, `"qwen3:32b"`,
#'   `"gemma3:27b"`). All other models currently reported by
#'   \code{ollama ps} will be candidates for unloading.
#' @param verbose Logical; if `TRUE` (the default), the function prints
#'   informational messages about the models detected and any unload
#'   operations performed. If `FALSE`, the function runs quietly.
#'
#' @return Invisibly returns a character vector containing the names of
#'   models that were requested to be unloaded (i.e., those passed to
#'   \code{ollama stop}). If no models were unloaded, an empty character
#'   vector is returned.
#'
#' @details
#' This function relies on the `ollama` command-line interface being
#' available on the system \code{PATH}. If the command cannot be executed
#' or returns a non-zero status code, the function will issue a message
#' (when \code{verbose = TRUE}) and return without making any changes.
#'
#' The exact output format of \code{ollama ps} is treated as an
#' implementation detail: this helper assumes that the first non-empty line
#' is a header and that subsequent non-empty lines begin with the model
#' name as the first whitespace-separated field. If the format changes in a
#' future version of Ollama, parsing may fail and the function will simply
#' fall back to doing nothing.
#'
#' Because `ollama stop` affects the global Ollama server state for the
#' current machine, you should only use this helper in environments where
#' you are comfortable unloading models that might be in use by other
#' processes.
#'
#' @examples
#' \dontrun{
#' # Keep only mistral-small3.2:24b loaded in Ollama, unloading any
#' # other active models
#' ensure_only_ollama_model_loaded("mistral-small3.2:24b")
#' }
#'
#' @seealso
#' * [ollama_compare_pair_live()] for single-pair Ollama comparisons.
#' * [submit_ollama_pairs_live()] for row-wise Ollama comparisons across
#'   many pairs.
#'
#' @export
ensure_only_ollama_model_loaded <- function(model, verbose = TRUE) {
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty character scalar.", call. = FALSE)
  }

  ps <- tryCatch(
    .ollama_system2("ollama", args = "ps", stdout = TRUE, stderr = TRUE),
    error = function(e) {
      if (verbose) {
        message("Unable to run `ollama ps`: ", conditionMessage(e))
      }
      structure(character(0), status = 1L)
    }
  )

  status <- attr(ps, "status", exact = TRUE)

  if (length(ps) == 0L || (!is.null(status) && !identical(status, 0L))) {
    if (verbose) {
      message(
        "`ollama ps` failed or returned no output; ",
        "no models will be unloaded."
      )
    }
    return(invisible(character(0)))
  }

  lines <- trimws(ps)
  lines <- lines[nzchar(lines)]

  if (!length(lines)) {
    if (verbose) {
      message(
        "No non-empty output lines from `ollama ps`; ",
        "no models will be unloaded."
      )
    }
    return(invisible(character(0)))
  }

  # Assume the first non-empty line is a header, remaining lines are data
  if (length(lines) >= 2L) {
    data_lines <- lines[-1L]
  } else {
    data_lines <- character(0)
  }

  if (!length(data_lines)) {
    if (verbose) {
      message("No active Ollama models reported by `ollama ps`.")
    }
    return(invisible(character(0)))
  }

  # Extract the first whitespace-separated field as the model name
  model_names <- sub("\\s+.*$", "", data_lines)
  model_names <- unique(model_names[nzchar(model_names)])

  if (!length(model_names)) {
    if (verbose) {
      message("Could not parse any model names from `ollama ps` output.")
    }
    return(invisible(character(0)))
  }

  if (verbose) {
    message("Active Ollama models: ", paste(model_names, collapse = ", "))
  }

  to_unload <- setdiff(model_names, model)

  if (!length(to_unload)) {
    if (verbose) {
      message(
        "No models to unload; only `", model,
        "` is active or no other active models were found."
      )
    }
    return(invisible(character(0)))
  }

  for (m in to_unload) {
    if (verbose) {
      message("Unloading Ollama model: ", m)
    }
    try(
      .ollama_system2("ollama", args = c("stop", m), stdout = FALSE, stderr = FALSE),
      silent = TRUE
    )
  }

  invisible(to_unload)
}

# Internal helper so tests can mock system calls without touching base::system2
.ollama_system2 <- function(command, args, stdout = TRUE, stderr = TRUE, ...) {
  system2(command, args = args, stdout = stdout, stderr = stderr, ...)
}
