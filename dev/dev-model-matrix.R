# dev/dev-model-matrix.R
#
# Check that pairwiseLLM works across a matrix of models and "thinking"
# settings for all backends, using the batch pipelines.
#
# NEW:
#  - Captures full console output to a log file.
#  - Saves a summary table (per backend/model/thinking).
#  - Saves per-run results to CSV (if results are returned).
#
# Inspect:
#  - summary:   <out_dir>/model_matrix_summary.csv
#  - log file:  <out_dir>/model_matrix_console.log
#  - results:   <out_dir>/results_<backend>_<model>_<thinking>.csv

library(pairwiseLLM)
library(dplyr)
library(tibble)

has_key <- function(env) {
  nzchar(Sys.getenv(env, ""))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------
# Output directory + logging
# ---------------------------------------------------------------------

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
out_dir   <- file.path("dev-output", "model-matrix", timestamp)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

log_path <- file.path(out_dir, "model_matrix_console.log")
log_con  <- file(log_path, open = "wt")

cat("[INFO] Logging console output to:", log_path, "\n")

# Capture stdout + messages into the log, but still show on console (split = TRUE)
sink(log_con, split = TRUE)
sink(log_con, type = "message", append = TRUE)

on.exit({
  # Stop sinks safely
  for (i in seq_len(2)) {
    try(sink(NULL), silent = TRUE)
    try(sink(NULL, type = "message"), silent = TRUE)
  }
  try(close(log_con), silent = TRUE)
}, add = TRUE)

cat("[INFO] Output directory:", out_dir, "\n")

# ---------------------------------------------------------------------
# Data: small set of pairs
# ---------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

cat("\n[INFO] Using", nrow(pairs), "pairs for model matrix checks.\n")

# ---------------------------------------------------------------------
# Model configuration
# ---------------------------------------------------------------------

openai_models <- tibble::tribble(
  ~model,        ~supports_thinking,
  "gpt-5.1",     TRUE,
  "gpt-5",       TRUE,
  "gpt-5-mini",  TRUE,
  "gpt-5-nano",  TRUE,
  "gpt-4.1",     FALSE,
  "gpt-4.1-mini",FALSE,
  "gpt-4.1-nano",FALSE,
  "gpt-4o",      FALSE
)

anthropic_models <- tibble::tribble(
  ~model,                      ~supports_thinking,
  "claude-sonnet-4-5",         TRUE,
  "claude-haiku-4-5",          TRUE,
  "claude-opus-4-5",           TRUE,
  "claude-opus-4-1",           TRUE,
  "claude-sonnet-4-0",         TRUE,
  "claude-3-7-sonnet-latest",  TRUE,
  "claude-opus-4-0",           TRUE,
  "claude-3-5-haiku-latest",   FALSE  # no thinking
)

gemini_models <- tibble::tribble(
  ~model,                 ~supports_thinking,
  "gemini-3-pro-preview", TRUE,
  "gemini-2.5-pro",       TRUE,
  "gemini-2.5-flash",     TRUE,
  "gemini-2.5-flash-lite",TRUE,
  "gemini-2.0-flash",     TRUE,
  "gemini-2.0-flash-lite",FALSE  # non-thinking
)

# ---------------------------------------------------------------------
# Global summary storage
# ---------------------------------------------------------------------

summary_rows <- list()

add_summary_row <- function(row) {
  # row should be a named list
  summary_rows <<- append(summary_rows, list(as_tibble(row)))
}

# ---------------------------------------------------------------------
# Helper: safe filename for model
# ---------------------------------------------------------------------

safe_model_name <- function(model) {
  gsub("[^A-Za-z0-9_.-]", "_", model)
}

# ---------------------------------------------------------------------
# Helper: run one (backend, model, thinking) combo via batch
# ---------------------------------------------------------------------

run_model_batch <- function(backend,
                            model,
                            thinking = FALSE,
                            pairs,
                            td,
                            tmpl) {
  thinking_flag <- if (thinking) "thinking" else "no_thinking"

  cat("\n------------------------------------------------------------\n")
  cat("[RUN]", backend, "|", model, "|", toupper(thinking_flag), "\n")

  # Backend-specific args for llm_submit_pairs_batch()
  batch_args <- list(
    pairs             = pairs,
    backend           = backend,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    include_raw       = FALSE
  )

  if (backend == "openai") {
    if (thinking) {
      # Responses endpoint + reasoning for gpt-5.* models
      batch_args$include_thoughts <- TRUE
      batch_args$endpoint         <- "responses"
      batch_args$reasoning        <- "low"
    } else {
      batch_args$include_thoughts <- FALSE
      batch_args$endpoint         <- "chat.completions"
    }
  } else if (backend == "anthropic") {
    if (thinking) {
      batch_args$include_thoughts <- TRUE
      batch_args$reasoning        <- "enabled"
    } else {
      batch_args$include_thoughts <- FALSE
      batch_args$reasoning        <- "none"
    }
  } else if (backend == "gemini") {
    batch_args$include_thoughts <- thinking
  }

  # Polling defaults – adjust if needed
  batch_args$poll             <- TRUE
  batch_args$interval_seconds <- 10
  batch_args$timeout_seconds  <- 900

  run_error <- NULL

  res <- tryCatch(
    {
      do.call(llm_submit_pairs_batch, batch_args)
    },
    error = function(e) {
      run_error <<- e
      cat("[ERROR]", conditionMessage(e), "\n")
      NULL
    }
  )

  # Build base summary fields
  run_id <- paste(backend, model, thinking_flag, sep = "::")

  batch_status <- NA_character_
  batch_input_path  <- NA_character_
  batch_output_path <- NA_character_
  results_csv_path  <- NA_character_

  n_results           <- NA_integer_
  n_non_na_better     <- NA_integer_
  n_non_na_thoughts   <- NA_integer_
  any_error_rows      <- NA
  unique_status_codes <- NA_character_

  if (!is.null(res)) {
    # batch object
    if (!is.null(res$batch)) {
      batch_status <- res$batch$status %||%
        res$batch$processing_status %||%
        NA_character_
    }

    batch_input_path  <- res$batch_input_path  %||% NA_character_
    batch_output_path <- res$batch_output_path %||% NA_character_

    # results tibble
    results <- res$results

    if (!is.null(results)) {
      n_results <- nrow(results)

      if ("better_sample" %in% names(results)) {
        n_non_na_better <- sum(!is.na(results$better_sample) &
                                 results$better_sample != "")
      }

      if ("thoughts" %in% names(results)) {
        n_non_na_thoughts <- sum(!is.na(results$thoughts) &
                                   results$thoughts != "")
      }

      if ("error_message" %in% names(results)) {
        any_error_rows <- any(!is.na(results$error_message) &
                                nzchar(results$error_message))
      }

      if ("status_code" %in% names(results)) {
        unique_status_codes <- paste(
          sort(unique(results$status_code)),
          collapse = ","
        )
      }

      # Save per-run results to CSV
      results_csv_path <- file.path(
        out_dir,
        sprintf(
          "results_%s_%s_%s.csv",
          backend,
          safe_model_name(model),
          thinking_flag
        )
      )
      write.csv(results, results_csv_path, row.names = FALSE)

      cat("[INFO] Saved results CSV to:", results_csv_path, "\n")

      # Print a compact head for quick inspection
      cat("[INFO] Results head:\n")
      print(
        results |>
          select(
            custom_id, ID1, ID2, model,
            dplyr::any_of(c(
              "object_type",
              "result_type",
              "status_code",
              "better_sample",
              "better_id",
              "prompt_tokens",
              "completion_tokens",
              "total_tokens",
              "prompt_cached_tokens",
              "reasoning_tokens"
            )),
            dplyr::any_of("error_message"),
            dplyr::any_of("thoughts")
          ) |>
          head()
      )

      cat("[INFO] unique(better_sample):",
          paste(unique(results$better_sample), collapse = ", "),
          "\n")

      if (thinking && "thoughts" %in% names(results)) {
        cat("[INFO] Non-NA thoughts rows:",
            n_non_na_thoughts, "of", n_results, "\n")
      }
    } else {
      cat("[WARN] No results tibble returned for this run.\n")
    }
  }

  # Summarize this run
  add_summary_row(list(
    backend             = backend,
    model               = model,
    thinking            = thinking,
    run_id              = run_id,
    ok                  = is.null(run_error) && !is.null(res),
    run_error_message   = if (!is.null(run_error)) conditionMessage(run_error) else NA_character_,
    batch_status        = batch_status,
    batch_input_path    = batch_input_path,
    batch_output_path   = batch_output_path,
    results_csv_path    = results_csv_path,
    n_results           = n_results,
    n_non_na_better     = n_non_na_better,
    n_non_na_thoughts   = n_non_na_thoughts,
    any_error_rows      = any_error_rows,
    unique_status_codes = unique_status_codes
  ))

  invisible(res)
}

# ---------------------------------------------------------------------
# OpenAI: gpt-4.x & gpt-5.x
# ---------------------------------------------------------------------

if (has_key("OPENAI_API_KEY")) {
  cat("\n========== OpenAI model matrix ==========\n")

  for (i in seq_len(nrow(openai_models))) {
    m      <- openai_models$model[i]
    can_th <- openai_models$supports_thinking[i]

    # Baseline: no thinking
    run_model_batch(
      backend  = "openai",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    # Thinking run only for gpt-5* models
    if (isTRUE(can_th)) {
      run_model_batch(
        backend  = "openai",
        model    = m,
        thinking = TRUE,
        pairs    = pairs,
        td       = td,
        tmpl     = tmpl
      )
    } else {
      cat("[SKIP] Thinking not supported for OpenAI model:", m, "\n")
    }
  }
} else {
  cat("\n[SKIP] OPENAI_API_KEY not set; skipping OpenAI model matrix.\n")
}

# ---------------------------------------------------------------------
# Anthropic: Claude 4/4.5/3.x
# ---------------------------------------------------------------------

if (has_key("ANTHROPIC_API_KEY")) {
  cat("\n========== Anthropic model matrix ==========\n")

  for (i in seq_len(nrow(anthropic_models))) {
    m      <- anthropic_models$model[i]
    can_th <- anthropic_models$supports_thinking[i]

    # Baseline: reasoning = "none"
    run_model_batch(
      backend  = "anthropic",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    if (isTRUE(can_th)) {
      run_model_batch(
        backend  = "anthropic",
        model    = m,
        thinking = TRUE,
        pairs    = pairs,
        td       = td,
        tmpl     = tmpl
      )
    } else {
      cat("[SKIP] Thinking not supported for Anthropic model:", m, "\n")
    }
  }
} else {
  cat("\n[SKIP] ANTHROPIC_API_KEY not set; skipping Anthropic model matrix.\n")
}

# ---------------------------------------------------------------------
# Gemini: 3 Pro, 2.5, 2.0
# ---------------------------------------------------------------------

if (has_key("GEMINI_API_KEY")) {
  cat("\n========== Gemini model matrix ==========\n")

  for (i in seq_len(nrow(gemini_models))) {
    m      <- gemini_models$model[i]
    can_th <- gemini_models$supports_thinking[i]

    # Baseline: include_thoughts = FALSE
    run_model_batch(
      backend  = "gemini",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    if (isTRUE(can_th)) {
      run_model_batch(
        backend  = "gemini",
        model    = m,
        thinking = TRUE,
        pairs    = pairs,
        td       = td,
        tmpl     = tmpl
      )
    } else {
      cat("[SKIP] Thinking not supported for Gemini model:", m, "\n")
    }
  }
} else {
  cat("\n[SKIP] GEMINI_API_KEY not set; skipping Gemini model matrix.\n")
}

# ---------------------------------------------------------------------
# Save summary table
# ---------------------------------------------------------------------

if (length(summary_rows)) {
  summary_tbl <- bind_rows(summary_rows)

  summary_csv <- file.path(out_dir, "model_matrix_summary.csv")
  summary_rds <- file.path(out_dir, "model_matrix_summary.rds")

  write.csv(summary_tbl, summary_csv, row.names = FALSE)
  saveRDS(summary_tbl, summary_rds)

  cat("\n[INFO] Summary table saved to:\n")
  cat("  -", summary_csv, "\n")
  cat("  -", summary_rds, "\n")
} else {
  cat("\n[WARN] No summary rows recorded.\n")
}

cat("\n[DONE] Model matrix batch checks complete.\n")
cat("[INFO] Full console log at:", log_path, "\n")
