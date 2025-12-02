# dev/check-llm-batch.R
#
# Manual dev script to test batch processing for all backends
# using the unified llm_submit_pairs_batch() interface.
#
# ⚠️ Requires valid API keys:
#   - OPENAI_API_KEY
#   - ANTHROPIC_API_KEY
#   - GEMINI_API_KEY
#
# Run from the package root:
#   devtools::load_all()
#   source("dev/check-llm-batch.R")

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Please install devtools to run this script.", call. = FALSE)
}

devtools::load_all(".")
library(tibble)
library(dplyr)

## ----------------------------------------------------------------------
## Helper: check API keys (simple version)
## ----------------------------------------------------------------------

has_key <- function(var) {
  val <- Sys.getenv(var, unset = "")
  nzchar(val)
}

message("API key availability:")
message("  OPENAI_API_KEY    : ", if (has_key("OPENAI_API_KEY"))    "YES" else "NO")
message("  ANTHROPIC_API_KEY : ", if (has_key("ANTHROPIC_API_KEY")) "YES" else "NO")
message("  GEMINI_API_KEY    : ", if (has_key("GEMINI_API_KEY"))    "YES" else "NO")

## ----------------------------------------------------------------------
## Data + prompt setup
## ----------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

set.seed(123)
pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 1) |>
  randomize_pair_order(seed = 2)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

message("\nUsing ", nrow(pairs), " pairs for batch testing.")

## ----------------------------------------------------------------------
## Helper: pretty-print JSONL input/output for debugging
## ----------------------------------------------------------------------

print_jsonl_preview <- function(path, n = 5L, label = NULL) {
  if (is.null(path) || !nzchar(path)) {
    message("  [jsonl] Path is NULL or empty.")
    return(invisible())
  }

  if (!file.exists(path)) {
    message("  [jsonl] File does not exist: ", path)
    return(invisible())
  }

  lbl <- label %||% basename(path)
  lines <- readLines(path, warn = FALSE)
  n_show <- min(length(lines), n)

  message("\n  --- JSONL preview: ", lbl, " (showing ", n_show, " of ", length(lines), " lines) ---")
  if (n_show > 0) {
    cat(paste(head(lines, n_show), collapse = "\n"), "\n")
  } else {
    message("  [jsonl] File is empty.")
  }

  invisible()
}

## ----------------------------------------------------------------------
## Helper: run a backend with llm_submit_pairs_batch()
## ----------------------------------------------------------------------

run_backend_batch <- function(
    backend,
    model,
    include_thoughts = FALSE,
    include_raw      = FALSE,
    poll             = TRUE,
    ...
) {
  message("\n====================================================")
  message("Backend: ", backend)
  message("Model  : ", model)
  message("include_thoughts: ", include_thoughts, " | include_raw: ", include_raw)
  message("poll  : ", poll)
  message("====================================================")

  out <- tryCatch(
    {
      batch <- llm_submit_pairs_batch(
        pairs             = pairs,
        backend           = backend,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = include_thoughts,
        include_raw       = include_raw,
        poll              = poll,
        ...
      )

      # Basic structure
      message("\n[batch] Structure:")
      str(batch$batch, max.level = 1)

      # Input JSONL
      message("\n[batch_input_path]: ", batch$batch_input_path)
      print_jsonl_preview(batch$batch_input_path, n = 5L, label = paste0(backend, " input"))

      # Output JSONL (if any)
      if (!is.null(batch$batch_output_path)) {
        message("\n[batch_output_path]: ", batch$batch_output_path)
        print_jsonl_preview(batch$batch_output_path, n = 5L, label = paste0(backend, " output"))
      } else {
        message("\n[batch_output_path]: <NULL> (poll = ", poll, ")")
      }

      # Parsed results (if any)
      if (!is.null(batch$results)) {
        message("\n[results] Head:")
        res_head <- batch$results |>
          select(
            dplyr::any_of(c(
              "custom_id", "ID1", "ID2", "model", "better_sample",
              "better_id", "status_code", "error_message", "thoughts"
            ))
          ) |>
          head(10)

        print(res_head)
      } else {
        message("\n[results]: <NULL>")
      }

      invisible(batch)
    },
    error = function(e) {
      message("\n[ERROR] Backend: ", backend, " | Model: ", model)
      message("  ", conditionMessage(e))
      invisible(NULL)
    }
  )

  out
}

## ----------------------------------------------------------------------
## OpenAI batch tests
## ----------------------------------------------------------------------

if (has_key("OPENAI_API_KEY")) {
  # 1) Standard batch: no thoughts, default endpoint for OpenAI backend
  openai_batch <- run_backend_batch(
    backend          = "openai",
    model            = "gpt-5.1",
    include_thoughts = FALSE,
    include_raw      = FALSE
  )

  # 2) Optional: reasoning / thoughts via responses endpoint
  #    Adjust model/args if you want to exercise GPT-5.1 + reasoning.
  #    This assumes llm_submit_pairs_batch passes ... through to
  #    build_openai_batch_requests().
  openai_batch_thoughts <- run_backend_batch(
    backend          = "openai",
    model            = "gpt-5.1",   # or "gpt-5.1" if configured
    include_thoughts = TRUE,
    include_raw      = FALSE,
    reasoning        = "low"        # forwarded via ...
  )
} else {
  message("\n[SKIP] OpenAI batch tests: OPENAI_API_KEY not set.")
}

## ----------------------------------------------------------------------
## Anthropic batch tests
## ----------------------------------------------------------------------

if (has_key("ANTHROPIC_API_KEY")) {
  anthropic_batch <- run_backend_batch(
    backend          = "anthropic",
    model            = "claude-sonnet-4-5",
    include_thoughts = TRUE,
    include_raw      = FALSE
  )
} else {
  message("\n[SKIP] Anthropic batch tests: ANTHROPIC_API_KEY not set.")
}

## ----------------------------------------------------------------------
## Gemini batch tests
## ----------------------------------------------------------------------

if (has_key("GEMINI_API_KEY")) {
  gemini_batch <- run_backend_batch(
    backend          = "gemini",
    model            = "gemini-3-pro-preview",
    include_thoughts = TRUE,
    include_raw      = FALSE
  )
} else {
  message("\n[SKIP] Gemini batch tests: GEMINI_API_KEY not set.")
}

message("\nDone. Inspect console output above for JSONL previews and parsed results.")
