## --------------------------------------------------------------------
## Example live test for the Gemini batch API using pairwiseLLM
##
## This script demonstrates how to split a set of 10 comparison pairs into
## five separate batch jobs using the newly added multi‑batch helpers.
## It submits all batches to the Gemini batch API, polls until completion,
## downloads and parses the results, and writes per‑batch CSVs as well as
## returning a combined tibble.  To run this script you must set the
## GEMINI_API_KEY environment variable before invoking R.
##
## To execute, source this file in an R session after installing the
## development version of the pairwiseLLM package that includes the
## `llm_submit_pairs_multi_batch()` and `llm_resume_multi_batches()` functions.

library(pairwiseLLM)
library(dplyr)

# Load example writing samples and construct a set of 10 pairs
data("example_writing_samples", package = "pairwiseLLM")
pairs <- example_writing_samples %>%
  make_pairs() %>%
  sample_pairs(n_pairs = 10, seed = 123) %>%
  randomize_pair_order(seed = 456)

# Define the trait to evaluate and the prompt template
td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Set up an output directory for inputs, outputs, and per‑batch results
out_dir <- "./dev-output/gemini_multi_batch_test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Starting Gemini multi‑batch test...\n")
# Submit the pairs as multiple batch jobs (5 segments of 2 pairs each)
multi_job <- llm_submit_pairs_multi_batch(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "gemini",
  n_segments        = 5L,
  output_dir        = out_dir,
  write_registry    = TRUE,
  verbose           = TRUE,
  include_thoughts = TRUE,
)

# Display registry information for debugging
cat("\nCreated", length(multi_job$jobs), "batch jobs.\n")
cat("GEMINI_API_KEY length:", nchar(Sys.getenv("GEMINI_API_KEY")), "(characters)\n")
cat("Job details:\n")
print(
  do.call(
    rbind,
    lapply(multi_job$jobs, function(j) {
      data.frame(
        segment_index     = j$segment_index,
        provider          = j$provider,
        batch_id          = j$batch_id,
        batch_input_path  = j$batch_input_path,
        batch_output_path = j$batch_output_path,
        csv_path          = j$csv_path,
        done              = j$done
      )
    })
  )
)

# List files in the output directory before polling
cat("\nFiles in output directory before polling:\n")
print(list.files(out_dir, recursive = TRUE, full.names = TRUE))

# Poll and download the results.  This call blocks until all five batches
# have reached a terminal state.  Setting write_results_csv = TRUE writes
# each batch’s parsed results to CSV in `out_dir`.
res <- llm_resume_multi_batches(
  jobs              = multi_job$jobs,
  # Use a shorter polling interval and enable verbose output for debugging
  interval_seconds  = 15,
  per_job_delay     = 2,
  write_results_csv = TRUE,
  keep_jsonl        = TRUE,
  tag_prefix        = "<BETTER_SAMPLE>",
  tag_suffix        = "</BETTER_SAMPLE>",
  verbose           = TRUE,
  write_combined_csv = TRUE,
  combined_csv_path  = file.path(out_dir, "gemini_combined_results.csv")
)

# Summarize job statuses after polling
cat("\nPolling complete. Summary of job statuses:\n")
print(
  do.call(
    rbind,
    lapply(res$jobs, function(j) {
      data.frame(
        segment_index     = j$segment_index,
        batch_id          = j$batch_id,
        done              = j$done,
        n_results         = if (is.null(j$results)) NA_integer_ else nrow(j$results)
      )
    })
  )
)

# List files in the output directory after polling
cat("\nFiles in output directory after polling:\n")
print(list.files(out_dir, recursive = TRUE, full.names = TRUE))

# Access the combined tibble of all parsed results
combined_results <- res$combined
cat("\nCombined results tibble (first few rows):\n")
print(head(combined_results))

## End of script
