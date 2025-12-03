# dev/dev-model-matrix-live.R
#
# Check that pairwiseLLM works across a matrix of models and "thinking"
# settings for all backends, using LIVE submissions (submit_llm_pairs()).
#
# For each (backend, model, thinking) combo:
#   - runs submit_llm_pairs()
#   - saves a CSV of the results
#   - appends a row to a summary table
#   - logs all console output to a log file
#
# Inspect afterwards:
#   - summary:   dev-output/model-matrix-live/<ts>/live_model_matrix_summary.csv
#   - log file:  dev-output/model-matrix-live/<ts>/live_model_matrix_console.log
#   - results:   dev-output/model-matrix-live/<ts>/live_results_<backend>_<model>_<thinking>.csv

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
out_dir   <- file.path("dev-output", "model-matrix-live", timestamp)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

log_path <- file.path(out_dir, "live_model_matrix_console.log")
log_con  <- file(log_path, open = "wt")

cat("[INFO] Logging LIVE console output to:", log_path, "\n")

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

cat("[INFO] LIVE output directory:", out_dir, "\n")

# ---------------------------------------------------------------------
# Data: small set of pairs
# ---------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 321) |>
  randomize_pair_order(seed = 654)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

cat("\n[INFO] Using", nrow(pairs), "pairs for LIVE model matrix checks.\n")

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
  summary_rows <<- append(summary_rows, list(as_tibble(row)))
}

# ---------------------------------------------------------------------
# Helper: safe filename for model
# ---------------------------------------------------------------------

safe_model_name <- function(model) {
  gsub("[^A-Za-z0-9_.-]", "_", model)
}

# ---------------------------------------------------------------------
# Helper: run one (backend, model, thinking) combo via LIVE API
# ---------------------------------------------------------------------
# Uses submit_llm_pairs() as the unified entry point.

run_model_live <- function(backend,
                           model,
                           thinking = FALSE,
                           pairs,
                           td,
                           tmpl) {
  thinking_flag <- if (thinking) "thinking" else "no_thinking"

  cat("\n------------------------------------------------------------\n")
  cat("[LIVE RUN]", backend, "|", model, "|", toupper(thinking_flag), "\n")

  # Base args for submit_llm_pairs()
  args <- list(
    pairs             = pairs,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = backend,
    # default endpoint = "chat.completions"; we only override for OpenAI
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = FALSE
  )

  if (backend == "openai") {
    if (thinking) {
      # GPT-5.* + Responses endpoint + reasoning
      args$endpoint        <- "responses"
      args$include_thoughts <- TRUE  # forwarded via ... to openai helpers
      args$reasoning       <- "low"
      # leave temperature/top_p/logprobs as defaults consistent with your live helper
    } else {
      # Non-thinking: use chat.completions
      args$endpoint        <- "chat.completions"
      args$include_thoughts <- FALSE
    }

  } else if (backend == "anthropic") {
    # submit_llm_pairs() ignores `endpoint` for anthropic and forwards ... to
    # submit_anthropic_pairs_live() → anthropic_compare_pair_live()
    if (thinking) {
      args$reasoning        <- "enabled"
      args$include_thoughts <- TRUE
      # You can also set max_tokens, thinking_budget_tokens, etc. here if desired.
    } else {
      args$reasoning        <- "none"
      args$include_thoughts <- FALSE
    }

  } else if (backend == "gemini") {
    # submit_llm_pairs() forwards ... to submit_gemini_pairs_live() →
    # gemini_compare_pair_live()
    args$include_thoughts <- thinking
    if (thinking) {
      args$thinking_level <- "low"  # your live Gemini helper already supports this
    }
  }

  run_error <- NULL

  res <- tryCatch(
    {
      do.call(submit_llm_pairs, args)
    },
    error = function(e) {
      run_error <<- e
      cat("[ERROR]", conditionMessage(e), "\n")
      NULL
    }
  )

  # Build base summary fields
  run_id <- paste(backend, model, thinking_flag, sep = "::")

  results_csv_path    <- NA_character_
  n_results           <- NA_integer_
  n_non_na_better     <- NA_integer_
  n_non_na_thoughts   <- NA_integer_
  any_error_rows      <- NA
  unique_status_codes <- NA_character_

  if (!is.null(res) && is.data.frame(res)) {
    results <- res

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

    # Save per-run live results to CSV
    results_csv_path <- file.path(
      out_dir,
      sprintf(
        "live_results_%s_%s_%s.csv",
        backend,
        safe_model_name(model),
        thinking_flag
      )
    )
    write.csv(results, results_csv_path, row.names = FALSE)

    cat("[INFO] Saved LIVE results CSV to:", results_csv_path, "\n")

    # Print compact head
    cat("[INFO] LIVE Results head:\n")
    print(
      results |>
        select(
          dplyr::any_of(c("custom_id", "ID1", "ID2")),
          dplyr::any_of("model"),
          dplyr::any_of(c(
            "object_type",
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

    if ("better_sample" %in% names(results)) {
      cat("[INFO] unique(better_sample):",
          paste(unique(results$better_sample), collapse = ", "),
          "\n")
    }

    if (thinking && "thoughts" %in% names(results)) {
      cat("[INFO] Non-NA thoughts rows:",
          n_non_na_thoughts, "of", n_results, "\n")
    }
  } else if (!is.null(res) && !is.data.frame(res)) {
    cat("[WARN] submit_llm_pairs returned a non-data.frame result.\n")
  }

  # Summarize this run
  add_summary_row(list(
    backend             = backend,
    model               = model,
    thinking            = thinking,
    run_id              = run_id,
    ok                  = is.null(run_error) && !is.null(res),
    run_error_message   = if (!is.null(run_error)) conditionMessage(run_error) else NA_character_,
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
# OpenAI LIVE: gpt-4.x & gpt-5.x
# ---------------------------------------------------------------------

if (has_key("OPENAI_API_KEY")) {
  cat("\n========== LIVE OpenAI model matrix ==========\n")

  for (i in seq_len(nrow(openai_models))) {
    m      <- openai_models$model[i]
    can_th <- openai_models$supports_thinking[i]

    # Baseline: no thinking
    run_model_live(
      backend  = "openai",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    # Thinking run only for gpt-5* models
    if (isTRUE(can_th)) {
      run_model_live(
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
  cat("\n[SKIP] OPENAI_API_KEY not set; skipping LIVE OpenAI model matrix.\n")
}

# ---------------------------------------------------------------------
# Anthropic LIVE: Claude 4/4.5/3.x
# ---------------------------------------------------------------------

if (has_key("ANTHROPIC_API_KEY")) {
  cat("\n========== LIVE Anthropic model matrix ==========\n")

  for (i in seq_len(nrow(anthropic_models))) {
    m      <- anthropic_models$model[i]
    can_th <- anthropic_models$supports_thinking[i]

    # Baseline: reasoning = "none"
    run_model_live(
      backend  = "anthropic",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    if (isTRUE(can_th)) {
      run_model_live(
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
  cat("\n[SKIP] ANTHROPIC_API_KEY not set; skipping LIVE Anthropic model matrix.\n")
}

# ---------------------------------------------------------------------
# Gemini LIVE: 3 Pro, 2.5, 2.0
# ---------------------------------------------------------------------

if (has_key("GEMINI_API_KEY")) {
  cat("\n========== LIVE Gemini model matrix ==========\n")

  for (i in seq_len(nrow(gemini_models))) {
    m      <- gemini_models$model[i]
    can_th <- gemini_models$supports_thinking[i]

    # Baseline
    run_model_live(
      backend  = "gemini",
      model    = m,
      thinking = FALSE,
      pairs    = pairs,
      td       = td,
      tmpl     = tmpl
    )

    if (isTRUE(can_th)) {
      run_model_live(
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
  cat("\n[SKIP] GEMINI_API_KEY not set; skipping LIVE Gemini model matrix.\n")
}

# ---------------------------------------------------------------------
# Save summary table
# ---------------------------------------------------------------------

if (length(summary_rows)) {
  summary_tbl <- bind_rows(summary_rows)

  summary_csv <- file.path(out_dir, "live_model_matrix_summary.csv")
  summary_rds <- file.path(out_dir, "live_model_matrix_summary.rds")

  write.csv(summary_tbl, summary_csv, row.names = FALSE)
  saveRDS(summary_tbl, summary_rds)

  cat("\n[INFO] LIVE summary table saved to:\n")
  cat("  -", summary_csv, "\n")
  cat("  -", summary_rds, "\n")
} else {
  cat("\n[WARN] No LIVE summary rows recorded.\n")
}

cat("\n[DONE] LIVE model matrix checks complete.\n")
cat("[INFO] Full LIVE console log at:", log_path, "\n")
