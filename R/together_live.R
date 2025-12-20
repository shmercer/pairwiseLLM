#' Live Together.ai comparison for a single pair of samples
#'
#' `together_compare_pair_live()` sends a single pairwise comparison prompt to
#' the Together.ai Chat Completions API (`/v1/chat/completions`) and parses the
#' result into a small tibble. It is the Together.ai analogue of
#' [openai_compare_pair_live()] and uses the same prompt template and tag
#' conventions (for example `<BETTER_SAMPLE>...</BETTER_SAMPLE>`).
#'
#' For models such as `"deepseek-ai/DeepSeek-R1"` that emit internal reasoning
#' wrapped in `<think>...</think>` tags, this helper will:
#' \itemize{
#'   \item Extract the `<think>...</think>` block into the `thoughts` column.
#'   \item Remove the `<think>...</think>` block from the visible `content`
#'         column, so `content` contains only the user-facing answer.
#' }
#'
#' Other Together.ai models (for example `"moonshotai/Kimi-K2-Instruct-0905"`,
#' `"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
#' `"deepseek-ai/DeepSeek-V3"`) are supported via the same API but may not use
#' `<think>` tags; in those cases, `thoughts` will be `NA` and the full model
#' output will appear in `content`.
#'
#' Temperature handling:
#' \itemize{
#'   \item If `temperature` is **not** supplied in `...`, the function applies
#'         backend defaults:
#'         \itemize{
#'           \item `"deepseek-ai/DeepSeek-R1"` → `temperature = 0.6`.
#'           \item All other models → `temperature = 0`.
#'         }
#'   \item If `temperature` is included in `...`, that value is used and the
#'         defaults are not applied.
#' }
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Together.ai model name (for example
#'   `"deepseek-ai/DeepSeek-R1"`, `"moonshotai/Kimi-K2-Instruct-0905"`,
#'   `"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
#'   `"deepseek-ai/DeepSeek-V3"`).
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   `"<BETTER_SAMPLE>"`.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   `"</BETTER_SAMPLE>"`.
#' @param api_key Optional Together.ai API key. If `NULL` or empty, the helper
#'   falls back to the `TOGETHER_API_KEY` environment variable via
#'   `.together_api_key()`.
#' @param include_raw Logical; if `TRUE`, adds a list-column `raw_response`
#'   containing the parsed JSON body returned by Together.ai (or `NULL` on parse
#'   failure). This is useful for debugging parsing problems.
#' @param ... Additional Together.ai parameters, typically including
#'   `temperature`, `top_p`, and provider-specific options. These are passed
#'   through to the JSON request body as top-level fields. If `temperature` is
#'   omitted, the function uses backend defaults (0.6 for
#'   `"deepseek-ai/DeepSeek-R1"`, 0 for all other models).
#'
#' @return A tibble with one row and columns:
#' \describe{
#'   \item{custom_id}{ID string of the form `"LIVE_<ID1>_vs_<ID2>"`.}
#'   \item{ID1, ID2}{The sample IDs you supplied.}
#'   \item{model}{Model name reported by the API.}
#'   \item{object_type}{API object type, typically `"chat.completion"`.}
#'   \item{status_code}{HTTP-style status code (200 if successful).}
#'   \item{error_message}{Error message if something goes wrong; otherwise `NA`.}
#'   \item{thoughts}{Internal reasoning text, for example `<think>...</think>`
#'     blocks from models like `"deepseek-ai/DeepSeek-R1"`.}
#'   \item{content}{Concatenated visible assistant output (without `<think>`
#'     blocks).}
#'   \item{better_sample}{"SAMPLE_1", "SAMPLE_2", or `NA`, based on the
#'     `<BETTER_SAMPLE>` tag.}
#'   \item{better_id}{`ID1` if `"SAMPLE_1"` is chosen, `ID2` if `"SAMPLE_2"` is
#'     chosen, otherwise `NA`.}
#'   \item{prompt_tokens}{Prompt / input token count (if reported).}
#'   \item{completion_tokens}{Completion / output token count (if reported).}
#'   \item{total_tokens}{Total token count (if reported).}
#'   \item{raw_response}{(Optional) list-column containing the parsed JSON body.}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires TOGETHER_API_KEY set in your environment and network access.
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' samples <- example_writing_samples[1:2, ]
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Example: DeepSeek-R1 with default temperature = 0.6 if not supplied
#' res_deepseek <- together_compare_pair_live(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "deepseek-ai/DeepSeek-R1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' res_deepseek$better_id
#' res_deepseek$thoughts
#'
#' # Example: Kimi-K2 with default temperature = 0 unless overridden
#' res_kimi <- together_compare_pair_live(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "moonshotai/Kimi-K2-Instruct-0905",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' res_kimi$better_id
#' }
#'
#' @export
together_compare_pair_live <- function(
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
  api_key = NULL,
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

  key <- .together_api_key(api_key)

  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  dots <- list(...)

  # Model-specific temperature defaults:
  # - DeepSeek-R1: 0.6
  # - All other models: 0
  if (is.null(dots$temperature)) {
    dots$temperature <- if (identical(model, "deepseek-ai/DeepSeek-R1")) {
      0.6
    } else {
      0
    }
  }

  body <- c(
    list(
      model = model,
      messages = list(
        list(
          role    = "user",
          content = prompt
        )
      )
    ),
    dots
  )

  req <- .together_request("/v1/chat/completions", key)
  req <- .together_req_body_json(req, body = body)

  # Perform request, but catch HTTP / network errors and return an
  # error-row tibble instead of throwing.
  resp <- tryCatch(
    .together_req_perform(req),
    error = function(e) {
      status_local <- NA_integer_

      # If this is an httr2 HTTP error (e.g., HTTP 503), try to
      # extract the status code from the underlying response.
      if (inherits(e, "httr2_http")) {
        resp_err <- e$response
        if (!is.null(resp_err)) {
          status_local <- httr2::resp_status(resp_err)
        }
      }

      msg <- conditionMessage(e)

      res <- tibble::tibble(
        custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        object_type       = NA_character_,
        status_code       = status_local,
        error_message     = paste0("Together.ai request error: ", msg),
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

      res
    }
  )

  # If the error handler above returned a tibble, we’re done.
  if (inherits(resp, "tbl_df")) {
    return(resp)
  }

  status_code <- .together_resp_status(resp)
  error_message <- NA_character_

  body_parsed <- tryCatch(
    .together_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  # Default "hard fail" if parsing fails completely
  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = NA_character_,
      object_type       = NA_character_,
      status_code       = status_code,
      error_message     = "Failed to parse Together.ai response body as JSON.",
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

  # Error handling: Together uses an "error" object similar to OpenAI
  if (!is.null(body$error)) {
    msg <- body$error$message %||% "Unknown error from Together.ai."
    error_message <- as.character(msg)
  } else if (status_code >= 400L) {
    error_message <- paste0("HTTP ", status_code, " from Together.ai.")
  }

  model_name <- as.character(body$model %||% model %||% NA_character_)
  object_type <- as.character(body$object %||% "chat.completion")

  choices <- body$choices %||% list()
  thoughts <- NA_character_
  content <- NA_character_

  if (length(choices) > 0L && is.null(body$error)) {
    # Assume first choice is the one we care about
    choice1 <- choices[[1]]

    raw_content <- choice1$message$content %||% ""
    raw_content <- as.character(raw_content)

    # Extract <think>...</think> (if present) into thoughts, and strip from
    # content. Use DOTALL so we match across newlines.
    if (grepl("<think>", raw_content, fixed = TRUE) &&
      grepl("</think>", raw_content, fixed = TRUE)) {
      think_pattern <- "(?s)<think>(.*?)</think>"

      # Capture inner text
      m <- regexpr(think_pattern, raw_content, perl = TRUE)
      if (m[1L] > 0L) {
        think_block <- regmatches(raw_content, m)[[1L]]
        think_inner <- sub(
          "(?s)^<think>(.*?)</think>$",
          "\\1",
          think_block,
          perl = TRUE
        )
        thoughts <- trimws(think_inner)

        # Remove the entire <think>...</think> block from visible content
        content_clean <- gsub(
          think_pattern,
          "",
          raw_content,
          perl = TRUE
        )
        content <- trimws(content_clean)
      } else {
        # Fallback: couldn't match with DOTALL; treat entire message as content
        content <- raw_content
      }
    } else {
      # No explicit <think> tags; treat entire message as visible content
      content <- raw_content
    }
  }

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

#' Live Together.ai comparisons for a tibble of pairs
#'
#' `submit_together_pairs_live()` is a robust row-wise wrapper around
#' [together_compare_pair_live()]. It takes a tibble of pairs (`ID1`, `text1`,
#' `ID2`, `text2`), submits each pair to the Together.ai Chat Completions API,
#' and collects the results.
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
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`. Typically created by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model Together.ai model name, for example `"deepseek-ai/DeepSeek-R1"`,
#'   `"moonshotai/Kimi-K2-Instruct-0905"`,
#'   `"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
#'   `"deepseek-ai/DeepSeek-V3"`.
#' @param trait_name Trait name to pass to [together_compare_pair_live()].
#' @param trait_description Trait description to pass to
#'   [together_compare_pair_live()].
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param api_key Optional Together.ai API key. If `NULL` or empty, falls back
#'   to `TOGETHER_API_KEY` via `.together_api_key()`.
#' @param verbose Logical; if `TRUE`, prints status, timing, and result
#'   summaries.
#' @param status_every Integer; print status / timing for every
#'   `status_every`-th pair. Defaults to 1 (every pair).
#' @param progress Logical; if `TRUE`, shows a textual progress bar.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body from
#'   Together.ai. Note: Raw responses are not saved to the incremental CSV file.
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Requires the \code{readr} package.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} and \code{future.apply}
#'   packages.
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#'   \strong{Guidance:} Together.ai rate limits vary by usage tier. Start with
#'   4 to 8 workers to avoid hitting HTTP 429 errors.
#' @param ... Additional Together.ai parameters, such as `temperature`, `top_p`,
#'   or other provider-specific options. These are forwarded to
#'   [together_compare_pair_live()].
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair and
#'     columns such as `better_id`, `better_sample`, `thoughts`, and `content`.}
#'   \item{failed_pairs}{A tibble containing the rows from `pairs` that failed
#'     to process (due to API errors or timeouts), along with an
#'     `error_message` column. These can be easily re-submitted.}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires TOGETHER_API_KEY and network access.
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
#' res_seq <- submit_together_pairs_live(
#'   pairs             = pairs,
#'   model             = "deepseek-ai/DeepSeek-R1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   save_path         = "results_seq.csv"
#' )
#'
#' # 2. Parallel execution (faster)
#' # Note: On Windows, this opens background R sessions.
#' res_par <- submit_together_pairs_live(
#'   pairs             = pairs,
#'   model             = "deepseek-ai/DeepSeek-R1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
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
submit_together_pairs_live <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
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
  pairs <- tibble::as_tibble(pairs)
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

        existing_ids <- existing_results$custom_id
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
    if (include_raw) res$raw_response <- list()
    return(res)
  }

  if (n == 0L) {
    if (verbose) message("No new pairs to process.")
    final_res <- if (!is.null(existing_results)) existing_results else empty_res()
    return(list(results = final_res, failed_pairs = pairs[0, ]))
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
    if (verbose) message(sprintf("Processing %d pairs in PARALLEL (Together.ai)...", n))

    chunk_size <- 20
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
    total_processed <- 0

    for (chunk_indices in chunks) {
      work_fn <- function(i) {
        id1 <- as.character(pairs$ID1[i])
        id2 <- as.character(pairs$ID2[i])

        tryCatch(
          {
            together_compare_pair_live(
              ID1               = id1,
              text1             = as.character(pairs$text1[i]),
              ID2               = id2,
              text2             = as.character(pairs$text2[i]),
              model             = model,
              trait_name        = trait_name,
              trait_description = trait_description,
              prompt_template   = prompt_template,
              api_key           = api_key,
              include_raw       = include_raw,
              ...
            )
          },
          error = function(e) {
            tibble::tibble(
              custom_id = sprintf("LIVE_%s_vs_%s", id1, id2),
              ID1 = id1, ID2 = id2, model = model,
              object_type = NA_character_, status_code = NA_integer_,
              error_message = paste0("Error: ", conditionMessage(e)),
              thoughts = NA_character_, content = NA_character_,
              better_sample = NA_character_, better_id = NA_character_,
              prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
              raw_response = if (include_raw) list(NULL) else NULL
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
    # Sequential Execution
    if (verbose) message(sprintf("Processing %d pairs SEQUENTIALLY (Together.ai)...", n))
    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

    for (i in seq_len(n)) {
      res <- tryCatch(
        {
          together_compare_pair_live(
            ID1               = as.character(pairs$ID1[i]),
            text1             = as.character(pairs$text1[i]),
            ID2               = as.character(pairs$ID2[i]),
            text2             = as.character(pairs$text2[i]),
            model             = model,
            trait_name        = trait_name,
            trait_description = trait_description,
            prompt_template   = prompt_template,
            api_key           = api_key,
            include_raw       = include_raw,
            ...
          )
        },
        error = function(e) {
          tibble::tibble(
            custom_id = sprintf("LIVE_%s_vs_%s", pairs$ID1[i], pairs$ID2[i]),
            ID1 = as.character(pairs$ID1[i]), ID2 = as.character(pairs$ID2[i]),
            model = model, object_type = NA_character_, status_code = NA_integer_,
            error_message = paste0("Error: ", conditionMessage(e)),
            thoughts = NA_character_, content = NA_character_,
            better_sample = NA_character_, better_id = NA_character_,
            prompt_tokens = NA_real_, completion_tokens = NA_real_, total_tokens = NA_real_,
            raw_response = if (include_raw) list(NULL) else NULL
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

  final_results <- if (!is.null(existing_results)) {
    dplyr::bind_rows(existing_results, new_results_df)
  } else {
    new_results_df
  }

  failed_mask <- !is.na(final_results$error_message) |
    (final_results$status_code >= 400 & !is.na(final_results$status_code))

  list(
    results = final_results,
    failed_pairs = final_results[failed_mask, ]
  )
}

# -------------------------------------------------------------------------
# Internal Together.ai helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.together_base_url <- function() {
  "https://api.together.xyz"
}

#' @keywords internal
#' @noRd
.together_request <- function(path, api_key) {
  # api_key here is expected to already be resolved (non-empty)
  httr2::request(paste0(.together_base_url(), path)) |>
    httr2::req_auth_bearer_token(api_key)
}

# -------------------------------------------------------------------------
# Internal Together.ai wrappers around httr2
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.together_req_body_json <- function(req, body) {
  # httr2::req_body_json() expects `data` for the JSON body argument
  httr2::req_body_json(req, data = body)
}

#' @keywords internal
#' @noRd
.together_req_perform <- function(req) {
  .retry_httr2_request(req)
}

#' @keywords internal
#' @noRd
.together_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

#' @keywords internal
#' @noRd
.together_resp_status <- function(resp) {
  httr2::resp_status(resp)
}
