# dev/check-openai-batch.R
#
# Manual smoke test for OpenAI batch comparison pipeline
# - Builds batch requests with include_thoughts = TRUE
# - Runs batch pipeline (optionally polling)
# - Verifies that results include thoughts + content and better_id
#
# If your API key / account does not have access to the Responses endpoint
# or reasoning models, the batch run may fail with an HTTP error; this script
# will print a warning and skip result checks.
#
# Requires:
#   - OPENAI_API_KEY set in your environment
#   - Network access
#
# This script will incur OpenAI API cost for the chosen model and number of pairs.

library(pairwiseLLM)
library(dplyr)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY is not set. Please set it before running this script.", call. = FALSE)
}

# -------------------------------------------------------------------
# Config
# -------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")

# Build a small set of pairs for testing
pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Reasoning-enabled model
batch_model   <- "gpt-5.1"  # change if your account has a different reasoning model
reason_effort <- "low"

# Whether to actually poll and download results.
# Set poll_results = FALSE if you just want to inspect the batch object and JSONL input.
poll_results <- TRUE

cat("\n=== Building OpenAI batch requests (responses + reasoning summaries) ===\n")

batch_tbl <- build_openai_batch_requests(
  pairs             = pairs,
  model             = batch_model,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "responses",
  reasoning         = reason_effort,
  include_thoughts  = TRUE,
  request_id_prefix = "DEV"
)

print(batch_tbl %>% select(custom_id, method, url) %>% head())

# Inspect first body to confirm reasoning$summary = "auto"
cat("\nFirst request body (truncated):\n")
str(batch_tbl$body[[1]][c("model", "input", "reasoning")], max.level = 2)

# -------------------------------------------------------------------
# Run full batch pipeline
# -------------------------------------------------------------------
cat("\n=== Running OpenAI batch pipeline ===\n")

pipeline <- tryCatch(
  run_openai_batch_pipeline(
    pairs             = pairs,
    model             = batch_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "responses",
    batch_input_path  = tempfile("openai_batch_input_",  fileext = ".jsonl"),
    batch_output_path = tempfile("openai_batch_output_", fileext = ".jsonl"),
    poll              = poll_results,
    interval_seconds  = 10,
    timeout_seconds   = 600
  ),
  error = function(e) {
    cat("\n[WARNING] OpenAI batch pipeline failed:\n  ",
        conditionMessage(e), "\n", sep = "")
    if (inherits(e, "httr2_http")) {
      cat(
        "This often means:\n",
        "  - Your API key/account does not have access to the Responses endpoint,\n",
        "  - The chosen model (", batch_model, ") is not available to you,\n",
        "  - Or the batch job was rejected by the API.\n",
        "You can try changing `batch_model` or using `endpoint = \"chat.completions\"`.\n",
        sep = ""
      )
    }
    return(NULL)
  }
)

if (is.null(pipeline)) {
  cat("\nSkipping result inspection due to batch error above.\n")
} else {
  cat("\nBatch input path: ", pipeline$batch_input_path,  "\n", sep = "")
  cat("Batch output path:", pipeline$batch_output_path %||% "<not downloaded>", "\n")

  cat("\nBatch status:", pipeline$batch$status %||% "<unknown>", "\n")

  if (!is.null(pipeline$results)) {
    res <- pipeline$results

    cat("\n=== Parsed batch results (first few rows) ===\n")
    print(
      res %>%
        select(
          custom_id, model, object_type, status_code, error_message,
          thoughts, content, better_sample, better_id,
          prompt_tokens, completion_tokens, total_tokens
        ) %>%
        head()
    )

    cat("\nSanity checks on parsed results:\n")
    cat("  Any non-NA thoughts:   ", any(!is.na(res$thoughts)), "\n")
    cat("  Any <BETTER_SAMPLE> tags in content: ",
        any(grepl("<BETTER_SAMPLE>", res$content, fixed = TRUE)), "\n")
    cat("  Any non-NA better_id:  ", any(!is.na(res$better_id)), "\n")

    if (any(!is.na(res$thoughts))) {
      cat("\n--- Example thoughts (first non-NA) ---\n")
      first_idx <- which(!is.na(res$thoughts))[1]
      cat("custom_id:", res$custom_id[first_idx], "\n")
      cat(substr(res$thoughts[first_idx], 1, 500), "\n")
    }
  } else {
    cat("\nNo parsed results yet (poll_results = FALSE or batch not completed).\n")
  }
}

cat("\nDone.\n")
