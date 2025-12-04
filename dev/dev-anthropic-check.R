# =====================================================================
# dev-anthropic-check.R
#
# Manual/dev script to sanity-check Anthropic (Claude) integrations in
# pairwiseLLM:
#   * Live vs batch endpoints
#   * reasoning = "none" vs "enabled" / include_thoughts = TRUE
#   * Temperature and thinking settings
#   * Parsing of API responses and batch .jsonl outputs
#
# Usage:
#   source("dev/dev-anthropic-check.R")
# =====================================================================

# ---------------------------------------------------------------------
# 0. Setup & helpers
# ---------------------------------------------------------------------

if (!requireNamespace("pairwiseLLM", quietly = TRUE)) {
  stop("Package 'pairwiseLLM' is not installed. Install it before running this dev script.")
}

library(pairwiseLLM)

if (!requireNamespace("tibble", quietly = TRUE)) {
  stop("Package 'tibble' is required.")
}

if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  stop("ANTHROPIC_API_KEY is not set in the environment. Set it before running this script.")
}

# Pretty section header
dev_header <- function(title) {
  line <- paste(rep("=", nchar(title) + 8), collapse = "")
  cat("\n", line, "\n*** ", title, " ***\n", line, "\n", sep = "")
}

# Safely print a tibble or object
dev_show <- function(x, title = NULL, n = 5) {
  if (!is.null(title)) dev_header(title)
  if (inherits(x, "data.frame")) {
    print(utils::head(x, n = n))
  } else {
    print(x)
  }
}

# Small helper to print error/status summaries for live calls
summarise_live_result <- function(res, label) {
  dev_header(paste0("LIVE RESULT SUMMARY: ", label))
  if (!is.null(res$error_message) && !all(is.na(res$error_message))) {
    cat("Status code: ", res$status_code, "\n", sep = "")
    cat("Error message: ", res$error_message, "\n", sep = "")
  } else {
    cat("Status code: ", res$status_code, "\n", sep = "")
    cat("Error message: <none>\n")
  }
  cat("Model: ", res$model, "\n", sep = "")
  cat("Better ID: ", res$better_id, " (", res$better_sample, ")\n", sep = "")
  cat("Tokens: prompt=", res$prompt_tokens,
      ", completion=", res$completion_tokens,
      ", total=", res$total_tokens, "\n", sep = "")
}

# ---------------------------------------------------------------------
# 1. Data & prompt setup
# ---------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

# Use first 3 samples to construct 2 pairs
set.seed(123)
pairs_full <- example_writing_samples[1:3, ]

pairs <- pairs_full |>
  make_pairs() |>
  sample_pairs(n_pairs = 2, seed = 123) |>
  randomize_pair_order(seed = 456)

dev_show(pairs, "PAIRS USED FOR ANTHROPIC DEV CHECK")

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

model_name <- "claude-sonnet-4-5"

# ---------------------------------------------------------------------
# 2. LIVE: reasoning = "none" (temperature should default to 0)
# ---------------------------------------------------------------------

dev_header("LIVE: reasoning = 'none' (temperature should be 0)")

live_none <- anthropic_compare_pair_live(
  ID1               = pairs$ID1[1],
  text1             = pairs$text1[1],
  ID2               = pairs$ID2[1],
  text2             = pairs$text2[1],
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  include_raw       = TRUE
  # temperature and max_tokens omitted -> defaults should be applied:
  # temperature = 0, max_tokens = 768
)

dev_show(live_none, "LIVE RESULT (reasoning = 'none')")
summarise_live_result(live_none, "reasoning = 'none'")
if ("raw_response" %in% names(live_none)) {
  dev_header("LIVE RAW RESPONSE (reasoning = 'none')")
  str(live_none$raw_response[[1]], max.level = 2)
}

# ---------------------------------------------------------------------
# 3. LIVE: reasoning = "enabled" (temperature must be 1, thinking block)
# ---------------------------------------------------------------------

dev_header("LIVE: reasoning = 'enabled' (temperature must be 1, thinking block)")

live_reason <- anthropic_compare_pair_live(
  ID1               = pairs$ID1[2],
  text1             = pairs$text1[2],
  ID2               = pairs$ID2[2],
  text2             = pairs$text2[2],
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "enabled",
  include_raw       = TRUE
  # temperature omitted -> function enforces temperature = 1
  # and adds a thinking block with budget_tokens >= 1024
)

dev_show(live_reason, "LIVE RESULT (reasoning = 'enabled')")
summarise_live_result(live_reason, "reasoning = 'enabled'")
if ("raw_response" %in% names(live_reason)) {
  dev_header("LIVE RAW RESPONSE (reasoning = 'enabled')")
  str(live_reason$raw_response[[1]], max.level = 2)
}

# ---------------------------------------------------------------------
# 4. BATCH: reasoning = "none", include_thoughts = FALSE
#    (temperature = 0, no thinking block)
# ---------------------------------------------------------------------

dev_header("BATCH: reasoning = 'none', include_thoughts = FALSE")

batch_input_none  <- tempfile("anthropic-batch-input-none-",  fileext = ".json")
batch_output_none <- tempfile("anthropic-batch-output-none-", fileext = ".jsonl")

batch_none <- run_anthropic_batch_pipeline(
  pairs             = pairs,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  include_thoughts  = FALSE,
  batch_input_path  = batch_input_none,
  batch_output_path = batch_output_none,
  poll              = TRUE,
  interval_seconds  = 10,
  timeout_seconds   = 600,
  verbose           = TRUE
)

dev_show(batch_none$batch, "BATCH OBJECT (reasoning = 'none')")
dev_show(batch_none$results, "PARSED BATCH RESULTS (reasoning = 'none')")

dev_header("BATCH INPUT JSON (reasoning = 'none')")
if (file.exists(batch_none$batch_input_path)) {
  cat(paste(readLines(batch_none$batch_input_path, warn = FALSE), collapse = "\n"), "\n")
} else {
  cat("Batch input path not found: ", batch_none$batch_input_path, "\n", sep = "")
}

dev_header("BATCH OUTPUT JSONL (reasoning = 'none')")
if (file.exists(batch_none$batch_output_path)) {
  cat(paste(readLines(batch_none$batch_output_path, warn = FALSE), collapse = "\n"), "\n")
} else {
  cat("Batch output path not found: ", batch_none$batch_output_path, "\n", sep = "")
}

# ---------------------------------------------------------------------
# 5. BATCH: include_thoughts = TRUE (auto-upgrade to extended thinking)
#    reasoning default starts as "none", but include_thoughts = TRUE
#    should upgrade internally to reasoning = "enabled" with:
#      * temperature = 1
#      * thinking block + valid budget_tokens
# ---------------------------------------------------------------------

dev_header("BATCH: include_thoughts = TRUE (auto-upgrade to extended thinking)")

batch_input_think  <- tempfile("anthropic-batch-input-think-",  fileext = ".json")
batch_output_think <- tempfile("anthropic-batch-output-think-", fileext = ".jsonl")

batch_think <- run_anthropic_batch_pipeline(
  pairs             = pairs,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  # reasoning left at default "none", but include_thoughts = TRUE
  # should cause run_anthropic_batch_pipeline() to upgrade reasoning
  include_thoughts  = TRUE,
  batch_input_path  = batch_input_think,
  batch_output_path = batch_output_think,
  poll              = TRUE,
  interval_seconds  = 10,
  timeout_seconds   = 600,
  verbose           = TRUE
)

dev_show(batch_think$batch, "BATCH OBJECT (include_thoughts = TRUE)")
dev_show(batch_think$results, "PARSED BATCH RESULTS (include_thoughts = TRUE)")

dev_header("BATCH INPUT JSON (include_thoughts = TRUE)")
if (file.exists(batch_think$batch_input_path)) {
  cat(paste(readLines(batch_think$batch_input_path, warn = FALSE), collapse = "\n"), "\n")
} else {
  cat("Batch input path not found: ", batch_think$batch_input_path, "\n", sep = "")
}

dev_header("BATCH OUTPUT JSONL (include_thoughts = TRUE)")
if (file.exists(batch_think$batch_output_path)) {
  cat(paste(readLines(batch_think$batch_output_path, warn = FALSE), collapse = "\n"), "\n")
} else {
  cat("Batch output path not found: ", batch_think$batch_output_path, "\n", sep = "")
}

# ---------------------------------------------------------------------
# 6. Quick recap
# ---------------------------------------------------------------------

dev_header("DEV CHECK COMPLETE")

cat(
  "\nSummary of what to inspect:\n",
  "1) LIVE, reasoning = 'none':\n",
  "   - Expect deterministic behaviour, temperature = 0 (see internal defaults / tests).\n",
  "   - Check parsed 'better_id', token counts, and raw_response structure.\n\n",
  "2) LIVE, reasoning = 'enabled':\n",
  "   - Expect extended reasoning, temperature = 1, thinking block in raw_response (if enabled by Anthropic).\n",
  "   - Check 'thoughts' vs 'content' columns, token counts.\n\n",
  "3) BATCH, reasoning = 'none':\n",
  "   - Inspect batch_input JSON -> params$temperature should be 0, no 'thinking' block.\n",
  "   - Confirm parsed .jsonl output matches expectations in batch_none$results.\n\n",
  "4) BATCH, include_thoughts = TRUE:\n",
  "   - Inspect batch_input JSON -> params$temperature should be 1, with 'thinking' block & budget_tokens.\n",
  "   - Confirm parsed .jsonl output and 'thoughts' column (if any) look correct.\n\n",
  sep = ""
)
