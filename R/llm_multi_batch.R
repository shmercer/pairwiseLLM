#' Multi‑batch submission and polling wrappers
#'
#' These functions provide higher‑level wrappers around the existing
#' provider‑specific batch APIs in **pairwiseLLM**.  They allow a large tibble of
#' pairwise comparisons to be automatically split into multiple batch jobs,
#' submitted concurrently (without polling), recorded in a registry for safe
#' resumption, and later polled until completion and merged into a single
#' results data frame.  They do not modify any of the underlying API functions
#' such as [run_openai_batch_pipeline()] or [run_anthropic_batch_pipeline()],
#' but orchestrate these calls to support resilient multi‑batch workflows.
#'
#' @section `llm_submit_pairs_multi_batch()`:
#' Splits a tibble of comparison pairs into chunks and submits one batch per
#' chunk using the appropriate provider pipeline.  Each batch is created with
#' `poll = FALSE`, so the function returns immediately after the batch jobs
#' have been created.  Metadata for each batch—including the `batch_id`,
#' provider type, and input/output file paths—is collected and (optionally)
#' written to a CSV registry for later resumption.
#'
#' @param pairs A tibble of pairs with columns `ID1`, `text1`, `ID2`, `text2`.
#'   Typically produced by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model Model identifier for the chosen backend.  Passed through to
#'   the corresponding `run_*_batch_pipeline()` function.
#' @param trait_name,trait_description,prompt_template Parameters forwarded
#'   to [run_openai_batch_pipeline()], [run_anthropic_batch_pipeline()], or
#'   [run_gemini_batch_pipeline()].  See those functions for details.
#' @param backend One of `"openai"`, `"anthropic"`, or `"gemini"`.  Determines
#'   which provider pipeline is used for each batch.
#' @param batch_size Integer giving the maximum number of pairs per batch.
#'   Exactly one of `batch_size` or `n_segments` must be supplied; if
#'   `batch_size` is supplied, the number of segments is computed as
#'   `ceiling(nrow(pairs) / batch_size)`.  The final segment may contain fewer
#'   pairs than `batch_size`.
#' @param n_segments Integer giving the number of segments to create.  Exactly
#'   one of `batch_size` or `n_segments` must be supplied; if `n_segments` is
#'   supplied, each segment contains approximately `nrow(pairs) / n_segments`
#'   pairs.  The last segment may be smaller.
#' @param output_dir Directory in which to write all batch files, including the
#'   `.jsonl` input/output files, the optional registry CSV, and (if requested)
#'   parsed results CSVs.  A temporary directory is created by default.
#' @param write_registry Logical; if `TRUE`, a CSV registry of batch jobs
#'   is written to `file.path(output_dir, "jobs_registry.csv")`.  The registry
#'   can be reloaded with [readr::read_csv()] and passed to
#'   [llm_resume_multi_batches()] for polling and resumption.  If `FALSE`,
#'   the registry is returned in memory only.
#' @param keep_jsonl Logical; if `FALSE`, the `.jsonl` input and output files
#'   for each batch will be deleted after the job results have been parsed in
#'   [llm_resume_multi_batches()].  Since the provider APIs require file paths,
#'   the files are always created during submission; this option controls
#'   whether to retain them on disk after completion.
#' @param verbose Logical; if `TRUE`, prints progress messages during batch
#'   submission.  Messages include the segment index, the number of pairs in
#'   each segment, the chosen provider, and confirmation that the batch
#'   has been created along with the input file path.  Defaults to `FALSE`.
#' @param ... Additional arguments passed through to the provider‑specific
#'   `run_*_batch_pipeline()` function.  These may include arguments such as
#'   `include_thoughts`, `reasoning`, `include_raw`, `temperature`, etc.

#' @param openai_max_retries Integer giving the maximum number of times
#'   to retry the initial OpenAI batch submission when a transient
#'   HTTP 5xx error occurs.  When creating a segment on the OpenAI
#'   backend, [run_openai_batch_pipeline()] internally uploads the
#'   JSONL file and creates the batch.  On rare occasions this call
#'   can return a 500 error; specifying a positive value here
#'   (e.g. 3) will automatically retry the submission up to that
#'   many times.  Between retries, the function sleeps for a brief
#'   period proportional to the current attempt.  Defaults to 3.
#'
#' @param validate Logical; if `TRUE`, attach a compact `validation_report` to the output (computed by [validate_backend_results()]). For `llm_submit_pairs_multi_batch()` this value is recorded in the job metadata so that downstream workflows can apply consistent validation defaults.
#' @param validate_strict Logical; only used when `validate = TRUE`. If `TRUE`, enforce validity by calling [validate_pairwise_results()] (errors on invalid winners). If `FALSE` (default), validation is report-only.
#' @param normalize_winner Logical; only used when `validate = TRUE`. If `TRUE`, normalize common winner tokens before validating (see [validate_backend_results()]).
#' @return A list with two elements: `jobs`, a list of per‑batch metadata
#'   (similar to the example in the advanced vignette), and `registry`,
#'   a tibble summarising all jobs.  The `registry` contains columns
#'   `segment_index`, `provider`, `model`, `batch_id`, `batch_input_path`,
#'   `batch_output_path`, `csv_path`, `done`, and `results` (initialized to
#'   `NULL`).  If `write_registry` is `TRUE`, the tibble is also written
#'   to disk as `jobs_registry.csv`.
#'
#' @examples
#' # Example: split a small set of pairs into five segments, submit
#' # them to the Gemini backend, and then poll and combine the results.
#' # Requires a funded API key and internet access.
#' \dontrun{
#' # Construct ten random pairs from the example writing samples
#' set.seed(123)
#' pairs <- sample_pairs(example_writing_samples, n_pairs = 10)
#'
#' # Directory to store batch files and results
#' outdir <- tempfile("multi_batch_example_")
#'
#' # Submit the pairs in five batches.  We write the registry to disk
#' # and print progress messages as each batch is created.
#' job_info <- llm_submit_pairs_multi_batch(
#'   pairs             = pairs,
#'   model             = "gemini-3-pro-preview",
#'   trait_name        = "writing_quality",
#'   trait_description = "Which text shows better writing quality?",
#'   n_segments        = 5,
#'   output_dir        = outdir,
#'   write_registry    = TRUE,
#'   verbose           = TRUE
#' )
#'
#' # Resume polling until all batches complete.  The per-batch and
#' # combined results are written to CSV files, the registry is
#' # refreshed on disk, and progress messages are printed.
#' results <- llm_resume_multi_batches(
#'   jobs               = job_info$jobs,
#'   output_dir         = outdir,
#'   interval_seconds   = 60,
#'   per_job_delay      = 2,
#'   write_results_csv  = TRUE,
#'   keep_jsonl         = FALSE,
#'   write_registry     = TRUE,
#'   verbose            = TRUE,
#'   write_combined_csv = TRUE
#' )
#'
#' # Access the combined results tibble
#' head(results$combined)
#' }
#'
#' @export
llm_submit_pairs_multi_batch <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini"),
  batch_size = NULL,
  n_segments = NULL,
  output_dir = tempfile("llm_multi_batch_"),
  write_registry = FALSE,
  keep_jsonl = TRUE,
  verbose = FALSE,
  ...,
  openai_max_retries = 3,
  validate = FALSE,
  validate_strict = FALSE,
  normalize_winner = FALSE
) {
  backend <- match.arg(backend)

  # Validate input and splitting options
  if (!is.null(batch_size) && !is.null(n_segments)) {
    stop("Specify only one of 'batch_size' or 'n_segments'.", call. = FALSE)
  }
  if (is.null(batch_size) && is.null(n_segments)) {
    stop("Either 'batch_size' or 'n_segments' must be supplied.", call. = FALSE)
  }

  n_pairs <- nrow(pairs)
  # Determine segment sizes
  if (!is.null(batch_size)) {
    n_segments <- ceiling(n_pairs / batch_size)
    sizes <- rep(batch_size, n_segments)
    # Adjust last segment to exact remainder
    remainder <- n_pairs - batch_size * (n_segments - 1L)
    sizes[n_segments] <- remainder
  } else {
    # n_segments supplied
    sizes <- rep(floor(n_pairs / n_segments), n_segments)
    remainder <- n_pairs - sum(sizes)
    # distribute remainder among the first segments
    if (remainder > 0L) {
      sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1L
    }
  }

  # Create output directory if it does not exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Build segment index vector
  indices <- rep(seq_along(sizes), times = sizes)
  # List of jobs; tibble of registry rows
  jobs <- vector("list", length(sizes))
  job_rows <- vector("list", length(sizes))

  # Loop over each segment and submit a batch
  for (seg in seq_along(sizes)) {
    seg_indices <- which(indices == seg)
    pairs_seg <- pairs[seg_indices, , drop = FALSE]

    # Construct file paths
    input_path <- file.path(output_dir, sprintf("batch_%02d_input.jsonl", seg))
    output_path <- file.path(output_dir, sprintf("batch_%02d_output.jsonl", seg))
    # CSV path for parsed results (used only if writing CSVs later)
    csv_path <- file.path(output_dir, sprintf("batch_%02d_results.csv", seg))

    # Optional verbose message before submission
    if (isTRUE(verbose)) {
      message(sprintf(
        "[llm_submit_pairs_multi_batch] Submitting segment %d of %d (%d pairs) to %s...",
        seg, length(sizes), nrow(pairs_seg), backend
      ))
    }

    # Submit the batch without polling
    if (backend == "openai") {
      # Retry the OpenAI batch submission up to openai_max_retries times
      oa_success <- FALSE
      oa_attempt <- 0L
      pipeline <- NULL
      while (!oa_success && oa_attempt < openai_max_retries) {
        oa_attempt <- oa_attempt + 1L
        pipeline <- tryCatch(
          {
            run_openai_batch_pipeline(
              pairs             = pairs_seg,
              model             = model,
              trait_name        = trait_name,
              trait_description = trait_description,
              prompt_template   = prompt_template,
              batch_input_path  = input_path,
              batch_output_path = output_path,
              poll              = FALSE,
              ...
            )
          },
          error = function(e) {
            # Retry on HTTP 5xx errors; propagate others
            if (inherits(e, "httr2_http_500") || inherits(e, "httr2_http_502") ||
              inherits(e, "httr2_http_503") || inherits(e, "httr2_http_504") ||
              inherits(e, "httr2_http_522") || inherits(e, "httr2_http_524")) {
              if (isTRUE(verbose)) {
                message(sprintf(
                  "[llm_submit_pairs_multi_batch] Error creating OpenAI batch (segment %d, attempt %d/%d): %s",
                  seg, oa_attempt, openai_max_retries, conditionMessage(e)
                ))
              }
              Sys.sleep(oa_attempt) # simple linear backoff
              return(NULL)
            } else {
              stop(e)
            }
          }
        )
        if (!is.null(pipeline)) {
          oa_success <- TRUE
        }
      }
      if (!oa_success) {
        stop(
          sprintf(
            "Failed to create OpenAI batch for segment %d after %d attempts.",
            seg, openai_max_retries
          ),
          call. = FALSE
        )
      }
      batch_id <- pipeline$batch$id
      # When verbose, confirm file uploaded successfully (OpenAI uploads the input file)
      if (isTRUE(verbose)) {
        message(sprintf(
          "[llm_submit_pairs_multi_batch] Segment %d: OpenAI batch created (id = %s). Input path: %s",
          seg, batch_id, input_path
        ))
      }
    } else if (backend == "anthropic") {
      pipeline <- run_anthropic_batch_pipeline(
        pairs             = pairs_seg,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        batch_input_path  = input_path,
        batch_output_path = output_path,
        poll              = FALSE,
        ...
      )
      batch_id <- pipeline$batch$id
      if (isTRUE(verbose)) {
        message(sprintf(
          "[llm_submit_pairs_multi_batch] Segment %d: Anthropic batch created (id = %s). Input path: %s",
          seg, batch_id, input_path
        ))
      }
    } else if (backend == "gemini") {
      pipeline <- run_gemini_batch_pipeline(
        pairs             = pairs_seg,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        batch_input_path  = input_path,
        batch_output_path = output_path,
        poll              = FALSE,
        ...
      )
      # Gemini returns a 'name' field rather than 'id'
      batch_id <- pipeline$batch$name %||% pipeline$batch$id
      if (isTRUE(verbose)) {
        message(sprintf(
          "[llm_submit_pairs_multi_batch] Segment %d: Gemini batch created (name = %s). Input path: %s",
          seg, batch_id, input_path
        ))
      }
    } else {
      stop("Unsupported backend: ", backend, call. = FALSE)
    }

    jobs[[seg]] <- list(
      segment_index     = seg,
      provider          = backend,
      model             = model,
      batch_id          = batch_id,
      batch_input_path  = input_path,
      batch_output_path = output_path,
      csv_path          = csv_path,
      done              = FALSE,
      results           = NULL
    )

    job_rows[[seg]] <- tibble::tibble(
      segment_index     = seg,
      provider          = backend,
      model             = model,
      batch_id          = batch_id,
      batch_input_path  = input_path,
      batch_output_path = output_path,
      csv_path          = csv_path,
      done              = FALSE
    )
  }

  # Combine registry rows into a tibble
  registry_tbl <- dplyr::bind_rows(job_rows)

  # Optionally write registry to disk
  if (isTRUE(write_registry)) {
    registry_file <- file.path(output_dir, "jobs_registry.csv")
    readr::write_csv(registry_tbl, registry_file)
  }

  invisible(list(jobs = jobs, registry = registry_tbl))
}

#' Resume polling and download results for multiple batch jobs
#'
#' This function takes the output of [llm_submit_pairs_multi_batch()] (or a
#' previously written registry CSV) and polls each batch until completion,
#' downloading and parsing results as they finish.  It implements a
#' conservative polling loop with a configurable interval between rounds and
#' a small delay between individual jobs to reduce the risk of API rate‑limit
#' errors.  The httr2 retry wrapper is still invoked for each API call, so
#' transient HTTP errors will be retried with exponential back‑off.
#'
#' @param jobs A list of job objects returned by
#'   [llm_submit_pairs_multi_batch()].  If `NULL`, a registry CSV is loaded
#'   from `output_dir` and converted into an internal jobs structure.
#' @param output_dir Directory containing the batch files and (optionally) the
#'   registry CSV.  If `jobs` is `NULL`, this directory must be supplied so
#'   that the registry can be loaded.  When `jobs` is provided and
#'   `output_dir` is `NULL`, the directory is inferred from the first job’s
#'   `batch_output_path`.  When writing results CSVs or updating the registry,
#'   this directory is used.
#' @param interval_seconds Number of seconds to wait between rounds of polling
#'   unfinished batches.  The default (`60`) mirrors the example in the
#'   advanced vignette.
#' @param per_job_delay Number of seconds to wait between polling individual
#'   jobs within a single round.  A small delay (e.g. 2) can help prevent 429
#'   (Too Many Requests) responses.
#' @param write_results_csv Logical; if `TRUE`, each batch’s parsed results are
#'   written to a CSV file (`csv_path`) in `output_dir` as soon as they are
#'   available.  If `FALSE` (the default), results are kept in memory.
#' @param keep_jsonl Logical; if `FALSE`, the `.jsonl` input and output files
#'   will be deleted after the job results have been parsed.  Defaults to `TRUE`.
#' @param tag_prefix,tag_suffix Character strings passed to
#'   [parse_anthropic_batch_output()] and [parse_gemini_batch_output()].  These
#'   tags mark the start and end of the “better” sample in each provider’s
#'   output.  The defaults match those used in the vignette.
#' @param write_combined_csv Logical; if `TRUE`, the combined results tibble
#'   returned by the function will also be written to a CSV file.  The path
#'   to write this file is determined by `combined_csv_path`.  Defaults to
#'   `FALSE`.
#' @param combined_csv_path Optional file path for the combined results CSV.
#'   If `write_combined_csv = TRUE` and `combined_csv_path` is `NULL`, the
#'   combined results will be written to
#'   `file.path(output_dir, "combined_results.csv")`.  When a non‑NULL
#'   value is supplied, it is treated as an absolute path if it begins with
#'   “/”, “~/”, or a Windows drive letter (e.g. “C:”), or if it contains a
#'   directory component (i.e. `dirname(combined_csv_path) != "."`).  In that
#'   case it will be used exactly as given.  Otherwise the file name is
#'   assumed to be relative to `output_dir`.  This argument is ignored when
#'   `write_combined_csv = FALSE`.

#' @param openai_max_retries Integer giving the maximum number of times to
#'   retry certain OpenAI API calls when a transient HTTP 5xx error occurs.
#'   In particular, when downloading batch output with
#'   [openai_download_batch_output()], the function will attempt to fetch
#'   the output file up to `openai_max_retries` times if an
#'   `httr2_http_500` error is raised.  Between retries the function sleeps
#'   for `per_job_delay` seconds.  Set to a small positive value (e.g. 3)
#'   to automatically recover from occasional server errors.  Defaults to 3.

#' @param write_registry Logical; if `TRUE`, a CSV registry of batch jobs
#'   will be written (or updated) at the end of polling.  When reading
#'   jobs from a saved registry via `output_dir`, this argument can be used
#'   to control whether the registry is refreshed on disk as job statuses
#'   change.  Defaults to `FALSE`.  See [llm_submit_pairs_multi_batch()] for
#'   additional details on the registry format.
#'
#' @param verbose Logical; if `TRUE`, prints progress messages during
#'   polling and result processing.  Messages include the batch ID, provider,
#'   and current state on each polling round, as well as summary messages
#'   when combined results are written to disk.  Defaults to `FALSE`.
#'
#' @param validate Logical; if `TRUE`, attach a compact `validation_report` to the output (computed by [validate_backend_results()]). For `llm_submit_pairs_multi_batch()` this value is recorded in the job metadata so that downstream workflows can apply consistent validation defaults.
#' @param validate_strict Logical; only used when `validate = TRUE`. If `TRUE`, enforce validity by calling [validate_pairwise_results()] (errors on invalid winners). If `FALSE` (default), validation is report-only.
#' @param normalize_winner Logical; only used when `validate = TRUE`. If `TRUE`, normalize common winner tokens before validating (see [validate_backend_results()]).
#' @param max_rounds Optional integer; maximum number of polling rounds before stopping with an error. If `NULL` (default), a conservative cap is applied only when `interval_seconds == 0` and `per_job_delay == 0`.
#' @return A list with two elements: `jobs`, the updated jobs list with each
#'   element containing parsed results and a `done` flag, and `combined`,
#'   a tibble obtained by binding all completed results (`NULL` if no batches
#'   completed).  If `write_results_csv` is `TRUE`, the combined tibble is still
#'   returned in memory.
#'   If `write_combined_csv` is `TRUE`, the combined tibble is also written
#'   to a CSV file on disk (see `combined_csv_path` for details) but is still
#'   returned in memory.
#'
#' @examples
#' # Continuing the example from llm_submit_pairs_multi_batch():
#' # After submitting multiple batches, resume polling and combine the results.
#' \dontrun{
#' # Suppose `outdir` is the directory where batch files were written and
#' # `jobs` is the list of job metadata returned by llm_submit_pairs_multi_batch().
#'
#' results <- llm_resume_multi_batches(
#'   jobs               = jobs,
#'   output_dir         = outdir,
#'   interval_seconds   = 60,
#'   per_job_delay      = 2,
#'   write_results_csv  = TRUE,
#'   keep_jsonl         = FALSE,
#'   write_registry     = TRUE,
#'   verbose            = TRUE,
#'   write_combined_csv = TRUE
#' )
#'
#' # The combined results are available in the `combined` element
#' print(results$combined)
#' }
#'
#' @export
llm_resume_multi_batches <- function(
  jobs = NULL,
  output_dir = NULL,
  interval_seconds = 60,
  per_job_delay = 2,
  write_results_csv = FALSE,
  keep_jsonl = TRUE,
  write_registry = FALSE,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  verbose = FALSE,
  write_combined_csv = FALSE,
  combined_csv_path = NULL,
  openai_max_retries = 3,
  validate = FALSE,
  validate_strict = FALSE,
  normalize_winner = FALSE,
  max_rounds = NULL
) {
  validation_report <- NULL


  # Validate inputs; either jobs must be supplied or output_dir must be provided
  if (is.null(jobs)) {
    if (is.null(output_dir)) {
      stop(
        "Either 'jobs' must be supplied or 'output_dir' must be provided to load a registry from disk.",
        call. = FALSE
      )
    }
    registry_file <- file.path(output_dir, "jobs_registry.csv")
    if (!file.exists(registry_file)) {
      stop(
        "No registry file found at ", registry_file, ". Please supply 'jobs' or ensure that 'output_dir' contains a 'jobs_registry.csv' created by llm_submit_pairs_multi_batch().",
        call. = FALSE
      )
    }
    tbl <- readr::read_csv(registry_file, show_col_types = FALSE)
    jobs <- purrr::pmap(
      tbl,
      function(segment_index, provider, model, batch_id,
               batch_input_path, batch_output_path, csv_path, done, ...) {
        # Convert segment_index to integer and done to logical (if present)
        seg_int <- as.integer(segment_index)
        done_log <- if (is.null(done)) FALSE else as.logical(done)
        list(
          segment_index     = seg_int,
          provider          = provider,
          model             = model,
          batch_id          = batch_id,
          batch_input_path  = batch_input_path,
          batch_output_path = batch_output_path,
          csv_path          = csv_path,
          done              = done_log,
          results           = NULL
        )
      }
    )
  }
  # Infer output_dir from first job if not supplied
  if (is.null(output_dir) && length(jobs) > 0L) {
    output_dir <- dirname(jobs[[1]]$batch_output_path)
  }

  unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

  # Prevent accidental infinite loops during testing or misconfigured polling.
  # If max_rounds is NULL, apply a conservative cap only when polling would spin
  # (interval_seconds == 0 and per_job_delay == 0). Otherwise, no cap is applied.
  if (is.null(max_rounds)) {
    max_rounds_internal <- if (identical(interval_seconds, 0) && identical(per_job_delay, 0)) 1000L else Inf
  } else {
    max_rounds_internal <- max_rounds
  }
  round_i <- 0L

  while (length(unfinished) > 0L) {
    round_i <- round_i + 1L
    if (is.finite(max_rounds_internal) && round_i > max_rounds_internal) {
      stop(
        "Polling exceeded max_rounds (", max_rounds_internal, ") without completing all jobs. ",
        "This usually indicates a job that never reaches a terminal status in the mocked API or provider response.",
        call. = FALSE
      )
    }
    for (j in unfinished) {
      job <- jobs[[j]]
      if (job$done) next

      provider <- job$provider
      batch_id <- job$batch_id

      # Optional progress message before polling a job
      if (isTRUE(verbose)) {
        message(sprintf(
          "[llm_resume_multi_batches] Polling segment %d (provider=%s, id=%s)...",
          job$segment_index, provider, batch_id
        ))
      }

      # Poll based on provider type
      if (provider == "openai") {
        # Wrap the OpenAI batch retrieval in tryCatch to handle transient errors
        batch <- tryCatch(
          openai_get_batch(batch_id),
          error = function(e) {
            if (isTRUE(verbose)) {
              message(sprintf(
                "[llm_resume_multi_batches] Error retrieving OpenAI batch %s: %s",
                batch_id, conditionMessage(e)
              ))
            }
            return(NULL)
          }
        )
        if (!is.null(batch)) {
          status <- batch$status %||% "unknown"
          if (isTRUE(verbose)) {
            message(sprintf(
              "[llm_resume_multi_batches] OpenAI batch %s status: %s",
              batch_id, status
            ))
          }
          # Terminal states as per API: completed, failed, cancelled, expired
          if (status %in% c("completed", "failed", "cancelled", "expired")) {
            if (identical(status, "completed")) {
              # Download and parse with retry logic on 5xx errors
              success <- FALSE
              attempt <- 0L
              while (!success && attempt < openai_max_retries) {
                attempt <- attempt + 1L
                try_res <- tryCatch(
                  {
                    openai_download_batch_output(
                      batch_id = batch_id,
                      path     = job$batch_output_path
                    )
                    TRUE
                  },
                  error = function(e) {
                    # Only retry on HTTP 5xx errors; propagate other errors
                    cls <- class(e)
                    if (inherits(e, "httr2_http_500") || inherits(e, "httr2_http_502") ||
                      inherits(e, "httr2_http_503") || inherits(e, "httr2_http_504") ||
                      inherits(e, "httr2_http_522") || inherits(e, "httr2_http_524") ||
                      inherits(e, "httr2_http_error") && grepl("^5", as.character(e$response$status_code))) {
                      if (isTRUE(verbose)) {
                        message(sprintf(
                          "[llm_resume_multi_batches] Error downloading OpenAI batch %s (attempt %d/%d): %s",
                          batch_id, attempt, openai_max_retries, conditionMessage(e)
                        ))
                      }
                      Sys.sleep(per_job_delay)
                      return(FALSE)
                    } else {
                      # Unexpected error: propagate
                      stop(e)
                    }
                  }
                )
                if (isTRUE(try_res)) {
                  success <- TRUE
                }
              }
              if (success) {
                res <- parse_openai_batch_output(job$batch_output_path)
                jobs[[j]]$results <- res
                if (isTRUE(write_results_csv)) {
                  readr::write_csv(res, job$csv_path)
                }
                if (!isTRUE(keep_jsonl)) {
                  unlink(job$batch_input_path)
                  unlink(job$batch_output_path)
                }
              } else {
                # If retries exhausted, leave job unfinished for next round
                if (isTRUE(verbose)) {
                  message(sprintf(
                    "[llm_resume_multi_batches] Failed to download OpenAI batch %s after %d attempts; will retry in next round.",
                    batch_id, openai_max_retries
                  ))
                }
                next
              }
            }
            # Mark job as done regardless of completion status
            jobs[[j]]$done <- TRUE
          }
        }
      } else if (provider == "anthropic") {
        batch <- anthropic_get_batch(batch_id)
        status <- batch$processing_status %||% "unknown"
        if (isTRUE(verbose)) {
          message(sprintf(
            "[llm_resume_multi_batches] Anthropic batch %s processing_status: %s",
            batch_id, status
          ))
        }
        if (status %in% c("ended", "errored", "canceled", "expired")) {
          if (identical(status, "ended")) {
            out_path <- anthropic_download_batch_results(
              batch_id    = batch_id,
              output_path = job$batch_output_path
            )
            res <- parse_anthropic_batch_output(
              jsonl_path  = out_path,
              tag_prefix  = tag_prefix,
              tag_suffix  = tag_suffix
            )
            jobs[[j]]$results <- res
            if (isTRUE(write_results_csv)) {
              readr::write_csv(res, job$csv_path)
            }
            if (!isTRUE(keep_jsonl)) {
              unlink(job$batch_input_path)
              unlink(job$batch_output_path)
            }
          }
          jobs[[j]]$done <- TRUE
        }
      } else if (provider == "gemini") {
        # Wrap the Gemini API call in tryCatch so that transient errors (e.g. 4xx/5xx)
        # do not abort the polling loop.  If an error occurs, we simply skip
        # processing this job in the current round and will try again later.
        batch <- tryCatch(
          gemini_get_batch(batch_id),
          error = function(e) {
            # Always report the error when verbose is TRUE or by default
            if (isTRUE(verbose)) {
              message(sprintf(
                "[llm_resume_multi_batches] Error retrieving Gemini batch %s: %s",
                batch_id, conditionMessage(e)
              ))
            }
            return(NULL)
          }
        )
        if (!is.null(batch)) {
          # The Gemini REST API reports the batch state under metadata$state in
          # the form "BATCH_STATE_SUCCEEDED", etc.  Some versions may also
          # include a top-level `state` field.  Prefer metadata$state, then
          # fall back to state.  Normalise by stripping any "BATCH_STATE_"
          # prefix so we can compare against simple values like "SUCCEEDED".
          state_raw <- batch$metadata$state %||% batch$state %||% "STATE_UNSPECIFIED"
          state <- sub("^BATCH_STATE_", "", state_raw)
          if (isTRUE(verbose)) {
            message(sprintf(
              "[llm_resume_multi_batches] Gemini batch %s state: %s (raw: %s)",
              batch_id, state, state_raw
            ))
          }
          if (state %in% c("SUCCEEDED", "FAILED", "CANCELLED", "EXPIRED")) {
            if (identical(state, "SUCCEEDED")) {
              # Reconstruct the requests table from the input JSON so that
              # parse_gemini_batch_output() can match custom_id, ID1, ID2.
              req_data <- jsonlite::read_json(job$batch_input_path, simplifyVector = FALSE)
              req_items <- req_data$requests
              if (is.null(req_items) || length(req_items) == 0L) {
                stop(
                  "Failed to reconstruct Gemini requests for segment ",
                  job$segment_index,
                  ". Ensure that the input JSON contains a 'requests' list.",
                  call. = FALSE
                )
              }
              # Reconstruct a full requests table.  Include the original
              # request list-column when available so that
              # parse_gemini_batch_output() can detect whether
              # includeThoughts was enabled for each pair.  When reading
              # back from the batch input JSON we stored a list of
              # objects with custom_id, ID1, ID2 and request; preserve
              # these fields here.  See parse_gemini_batch_output() for
              # details.
              req_tbl <- tibble::tibble(
                custom_id = vapply(req_items, `[[`, character(1), "custom_id"),
                ID1       = vapply(req_items, `[[`, character(1), "ID1"),
                ID2       = vapply(req_items, `[[`, character(1), "ID2"),
                request   = lapply(req_items, `[[`, "request")
              )
              # Download the batch results to the designated output path
              gemini_download_batch_results(
                batch        = batch_id,
                requests_tbl = req_tbl,
                output_path  = job$batch_output_path
              )
              # Parse the downloaded JSONL into a tidy tibble
              res <- parse_gemini_batch_output(
                results_path = job$batch_output_path,
                requests_tbl = req_tbl
              )
              jobs[[j]]$results <- res
              if (isTRUE(write_results_csv)) {
                readr::write_csv(res, job$csv_path)
              }
              if (!isTRUE(keep_jsonl)) {
                unlink(job$batch_input_path)
                unlink(job$batch_output_path)
              }
            }
            jobs[[j]]$done <- TRUE
          }
        }
      } else {
        stop("Unsupported provider type in jobs list: ", provider, call. = FALSE)
      }
      Sys.sleep(per_job_delay)
    }

    unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))
    # Report progress after each round
    if (isTRUE(verbose)) {
      completed <- sum(vapply(jobs, `[[`, logical(1), "done"))
      total <- length(jobs)
      message(sprintf(
        "[llm_resume_multi_batches] Completed %d of %d jobs; %d remaining.",
        completed, total, length(unfinished)
      ))
    }
    if (length(unfinished) > 0L) {
      Sys.sleep(interval_seconds)
    }
  }

  # Update registry CSV if requested or if it exists on disk
  registry_file <- file.path(output_dir, "jobs_registry.csv")
  if (isTRUE(write_registry) || file.exists(registry_file)) {
    registry_tbl <- tibble::tibble(
      segment_index     = vapply(jobs, `[[`, integer(1), "segment_index"),
      provider          = vapply(jobs, `[[`, character(1), "provider"),
      model             = vapply(jobs, `[[`, character(1), "model"),
      batch_id          = vapply(jobs, `[[`, character(1), "batch_id"),
      batch_input_path  = vapply(jobs, `[[`, character(1), "batch_input_path"),
      batch_output_path = vapply(jobs, `[[`, character(1), "batch_output_path"),
      csv_path          = vapply(jobs, `[[`, character(1), "csv_path"),
      done              = vapply(jobs, `[[`, logical(1), "done")
    )
    readr::write_csv(registry_tbl, registry_file)
  }

  # Combine results into a single tibble (if any)
  completed_results <- purrr::compact(lapply(jobs, `[[`, "results"))
  combined <- if (length(completed_results) > 0L) {
    dplyr::bind_rows(completed_results)
  } else {
    NULL
  }


  # Validate combined results if requested
  if (isTRUE(validate)) {
    if (is.null(combined)) {
      validation_report <- list(
        backend = "llm_resume_multi_batches",
        n_rows = 0L,
        n_missing_id = 0L,
        n_missing_winner = 0L,
        n_invalid_winner = 0L
      )
    } else {
      vr <- validate_backend_results(
        combined,
        backend = "llm_resume_multi_batches",
        normalize_winner = isTRUE(normalize_winner),
        strict = isTRUE(validate_strict),
        return_report = TRUE
      )
      combined <- vr$data
      validation_report <- vr$report
    }
  }
  # Optionally write the combined results to CSV
  if (isTRUE(write_combined_csv) && !is.null(combined)) {
    # Determine the file path: if user provided combined_csv_path, use it;
    # otherwise construct a default name within output_dir
    if (is.null(combined_csv_path) || !nzchar(combined_csv_path)) {
      # Default file name within the output directory
      combined_path <- file.path(output_dir, "combined_results.csv")
    } else {
      # Determine if the provided path should be treated as absolute.  We
      # consider paths that start with a leading slash or tilde (e.g.
      # "~/") or that specify a Windows drive letter as absolute.  In
      # addition, if the supplied path contains a directory component
      # (dirname != "."), we assume the user has provided a full path and
      # therefore do not prepend `output_dir` again.  This avoids
      # duplicating `output_dir` when the user passes
      # file.path(output_dir, "file.csv").
      is_abs <- grepl("^[~/]", combined_csv_path) || grepl("^[A-Za-z]:", combined_csv_path)
      has_dir <- dirname(combined_csv_path) != "."
      if (is_abs || has_dir) {
        combined_path <- combined_csv_path
      } else {
        combined_path <- file.path(output_dir, combined_csv_path)
      }
    }
    # Ensure directory exists
    dir.create(dirname(combined_path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(combined, combined_path)
    if (isTRUE(verbose)) {
      message(sprintf(
        "[llm_resume_multi_batches] Combined results written to %s",
        combined_path
      ))
    }
  }

  invisible(list(jobs = jobs, combined = combined, validation_report = validation_report))
}
