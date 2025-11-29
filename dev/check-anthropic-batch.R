# dev/check-anthropic-batch.R
#
# Live batch harness for Anthropic Message Batches API via
# pairwiseLLM::run_anthropic_batch_pipeline().
#
# Patterned after:
#   - dev/check-anthropic.R              (live pairwise calls)
#   - dev/check-anthropic-thinking.R     (thinking vs non-thinking)
#   - dev/check-anthropic-reasoning-parsing.R (parsing harness)
#
# This is NOT exported; it’s for interactive QA / dev only.

# Small helper: x %||% y (copied pattern from other dev helpers)
`%||%` <- function(x, y) if (!is.null(x)) x else y

check_anthropic_batch_models <- function(
    models = c("claude-sonnet-4-5",
               "claude-haiku-4-5",
               "claude-opus-4-5"),
    reasoning_modes  = c("none", "enabled"),
    n_pairs          = 3L,
    poll             = TRUE,
    interval_seconds = 30,
    timeout_seconds  = 3600,
    anthropic_version = "2023-06-01",
    verbose          = TRUE
) {
  # ----------------------------------------------------------------------
  # Basic dependency + key checks (same style as other dev helpers)
  # ----------------------------------------------------------------------
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop("Set ANTHROPIC_API_KEY in your environment before running this check.",
         call. = FALSE)
  }

  # Use your built-in example data + helpers, mirroring dev/check-anthropic.R
  # (live models dev harness). :contentReference[oaicite:2]{index=2}
  if (!exists("example_writing_samples", where = asNamespace("pairwiseLLM"))) {
    stop("example_writing_samples dataset not found. ",
         "Make sure pairwiseLLM is loaded.", call. = FALSE)
  }

  data("example_writing_samples", package = "pairwiseLLM", envir = environment())
  samples <- get("example_writing_samples", envir = environment())

  if (nrow(samples) < 2) {
    stop("Need at least 2 example writing samples for the check.", call. = FALSE)
  }

  # ----------------------------------------------------------------------
  # Build a small set of pairs for testing
  # ----------------------------------------------------------------------
  # We re-use the same helpers as the main package pipeline, just limiting
  # to n_pairs for cost reasons.
  pairs_all <- pairwiseLLM::make_pairs(samples)
  if (nrow(pairs_all) == 0L) {
    stop("make_pairs() returned no pairs; cannot run batch check.", call. = FALSE)
  }

  # Limit to n_pairs, but respect upper bound
  n_pairs <- as.integer(n_pairs)
  if (!is.finite(n_pairs) || n_pairs < 1L) {
    stop("`n_pairs` must be a positive integer.", call. = FALSE)
  }

  n_pairs <- min(n_pairs, nrow(pairs_all))

  # Use sample_pairs + randomize_pair_order so this mirrors typical usage
  pairs <- pairs_all |>
    pairwiseLLM::sample_pairs(n_pairs = n_pairs, seed = 123) |>
    pairwiseLLM::randomize_pair_order(seed = 456)

  if (verbose) {
    message(sprintf(
      "Using %d pair(s) from example_writing_samples for Anthropic batch checks.",
      nrow(pairs)
    ))
  }

  td   <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  # ----------------------------------------------------------------------
  # Main loop over models x reasoning modes
  # ----------------------------------------------------------------------
  results <- list()
  i <- 1L

  for (model in models) {
    for (mode in reasoning_modes) {

      if (verbose) {
        message(sprintf(
          "\n[Batch Check %d] model = %s, reasoning = %s (pairs = %d)",
          i, model, mode, nrow(pairs)
        ))
      }

      pipeline <- tryCatch(
        pairwiseLLM::run_anthropic_batch_pipeline(
          pairs             = pairs,
          model             = model,
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          reasoning         = mode,
          batch_input_path  = tempfile(
            pattern = sprintf("anthropic-batch-input-%s-%s-", model, mode),
            fileext = ".json"
          ),
          batch_output_path = tempfile(
            pattern = sprintf("anthropic-batch-output-%s-%s-", model, mode),
            fileext = ".jsonl"
          ),
          poll              = poll,
          interval_seconds  = interval_seconds,
          timeout_seconds   = timeout_seconds,
          api_key           = key,
          anthropic_version = anthropic_version,
          verbose           = verbose
        ),
        error = function(e) e
      )

      # ------------------------------------------------------------------
      # Handle error in the pipeline itself
      # ------------------------------------------------------------------
      if (inherits(pipeline, "error")) {
        if (verbose) {
          message("  ❌ Error in run_anthropic_batch_pipeline(): ",
                  conditionMessage(pipeline))
        }

        results[[i]] <- tibble::tibble(
          model              = model,
          reasoning          = mode,
          processing_status  = NA_character_,
          n_pairs            = nrow(pairs),
          n_results          = NA_integer_,
          n_succeeded        = NA_integer_,
          n_errored          = NA_integer_,
          n_canceled         = NA_integer_,
          n_expired          = NA_integer_,
          n_missing_better   = NA_integer_,
          ok                 = FALSE,
          error_message      = conditionMessage(pipeline),
          batch_id           = NA_character_,
          results_path       = NA_character_
        )

        i <- i + 1L
        next
      }

      batch      <- pipeline$batch
      results_df <- pipeline$results

      status <- batch$processing_status %||% NA_character_
      batch_id <- batch$id %||% NA_character_

      # request_counts (if present)
      rc <- batch$request_counts %||% list()
      rc_processing <- rc$processing %||% NA_integer_
      rc_succeeded  <- rc$succeeded  %||% NA_integer_
      rc_errored    <- rc$errored    %||% NA_integer_
      rc_canceled   <- rc$canceled   %||% NA_integer_
      rc_expired    <- rc$expired    %||% NA_integer_

      if (verbose) {
        message(sprintf("  Batch ID: %s", batch_id))
        message(sprintf(
          "  processing_status: %s | request_counts: processing=%s, succeeded=%s, errored=%s, canceled=%s, expired=%s",
          status, rc_processing, rc_succeeded, rc_errored, rc_canceled, rc_expired
        ))
      }

      # ------------------------------------------------------------------
      # If poll = FALSE, we likely have no results yet
      # ------------------------------------------------------------------
      if (!poll || is.null(results_df)) {
        results[[i]] <- tibble::tibble(
          model              = model,
          reasoning          = mode,
          processing_status  = status,
          n_pairs            = nrow(pairs),
          n_results          = NA_integer_,
          n_succeeded        = NA_integer_,
          n_errored          = NA_integer_,
          n_canceled         = NA_integer_,
          n_expired          = NA_integer_,
          n_missing_better   = NA_integer_,
          ok                 = NA,
          error_message      = NA_character_,
          batch_id           = batch_id,
          results_path       = pipeline$batch_output_path %||% NA_character_
        )

        i <- i + 1L
        next
      }

      # ------------------------------------------------------------------
      # When polling is enabled: summarize parsed results
      # ------------------------------------------------------------------
      res_tbl <- tibble::as_tibble(results_df)

      n_results <- nrow(res_tbl)

      # Count by result_type
      if ("result_type" %in% names(res_tbl)) {
        counts <- table(res_tbl$result_type)

        get_count <- function(name) {
          if (name %in% names(counts)) {
            as.integer(counts[[name]])
          } else {
            0L
          }
        }

        n_succeeded <- get_count("succeeded")
        n_errored   <- get_count("errored")
        n_canceled  <- get_count("canceled")
        n_expired   <- get_count("expired")
      } else {
        n_succeeded <- n_errored <- n_canceled <- n_expired <- NA_integer_
      }

      # How many succeeded rows are missing better_id?
      n_missing_better <- NA_integer_
      if ("better_id" %in% names(res_tbl) && "result_type" %in% names(res_tbl)) {
        n_missing_better <- sum(
          res_tbl$result_type == "succeeded" &
            (is.na(res_tbl$better_id) | res_tbl$better_id == ""),
          na.rm = TRUE
        )
      }

      # Basic "ok" flag: ended, counts match, and no missing better_id in successes
      ok <- TRUE
      msg <- character()

      if (is.na(status) || !identical(status, "ended")) {
        ok  <- FALSE
        msg <- c(msg, sprintf("processing_status is %s (expected 'ended')", status))
      }

      # If request_counts present, compare to observed
      if (!is.na(rc_succeeded) && !is.na(n_succeeded) && rc_succeeded != n_succeeded) {
        ok  <- FALSE
        msg <- c(msg, sprintf(
          "request_counts$succeeded (%s) != parsed succeeded (%s)",
          rc_succeeded, n_succeeded
        ))
      }

      if (!is.na(rc_errored) && !is.na(n_errored) && rc_errored != n_errored) {
        ok  <- FALSE
        msg <- c(msg, sprintf(
          "request_counts$errored (%s) != parsed errored (%s)",
          rc_errored, n_errored
        ))
      }

      if (!is.na(n_missing_better) && n_missing_better > 0L) {
        ok  <- FALSE
        msg <- c(msg, sprintf(
          "%s succeeded row(s) with missing better_id", n_missing_better
        ))
      }

      # Check that returned model families look like the requested one
      if ("model" %in% names(res_tbl) && any(!is.na(res_tbl$model))) {
        base_family <- sub("-[0-9]+$", "", model)  # e.g., claude-sonnet-4-5
        bad <- !is.na(res_tbl$model) &
          !startsWith(res_tbl$model, base_family)
        if (any(bad)) {
          ok  <- FALSE
          msg <- c(msg, sprintf(
            "Some returned model names do not start with requested family `%s`",
            base_family
          ))
        }
      }

      if (verbose) {
        message(sprintf("  Parsed %d result row(s).", n_results))
        message(sprintf(
          "  result_type counts: succeeded=%s, errored=%s, canceled=%s, expired=%s",
          n_succeeded, n_errored, n_canceled, n_expired
        ))
        message(sprintf("  n_missing_better (among successes): %s", n_missing_better))

        # Show a quick snippet of the first few content / better_id rows
        if (n_results > 0L) {
          snippet_n <- min(3L, n_results)
          message("  First few results:")
          for (j in seq_len(snippet_n)) {
            row <- res_tbl[j, ]
            snippet <- substr(row$content %||% "", 1L, 100L)
            message(sprintf(
              "    [%d] custom_id=%s | result_type=%s | better_id=%s | snippet=%s",
              j,
              row$custom_id %||% "<NA>",
              row$result_type %||% "<NA>",
              row$better_id %||% "<NA>",
              snippet
            ))
          }
        }
      }

      results[[i]] <- tibble::tibble(
        model              = model,
        reasoning          = mode,
        processing_status  = status,
        n_pairs            = nrow(pairs),
        n_results          = n_results,
        n_succeeded        = n_succeeded,
        n_errored          = n_errored,
        n_canceled         = n_canceled,
        n_expired          = n_expired,
        n_missing_better   = n_missing_better,
        ok                 = ok,
        error_message      = if (length(msg) == 0L) NA_character_ else paste(msg, collapse = " | "),
        batch_id           = batch_id,
        results_path       = pipeline$batch_output_path %||% NA_character_
      )

      i <- i + 1L
    }
  }

  out <- dplyr::bind_rows(results)

  if (verbose) {
    message("\n===== Anthropic batch check summary =====")
    print(out)
  }

  invisible(out)
}
