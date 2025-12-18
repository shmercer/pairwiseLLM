## --------------------------------------------------------------------
## Example live test for the OpenAI batch API using pairwiseLLM
##
## This script demonstrates how to split a set of 10 comparison pairs into
## five separate batch jobs using the newly added multi‑batch helpers.  It
## submits all batches to the OpenAI batch API, polls until completion,
## downloads and parses the results (including extended "thoughts" output),
## and writes per‑batch CSVs as well as a single combined CSV.  To run
## this script you must set the `OPENAI_API_KEY` environment variable
## before invoking R.
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
  sample_pairs(n_pairs = 10, seed = 4321) %>%
  randomize_pair_order(seed = 8765)

# Define the trait to evaluate and the prompt template
td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Set up an output directory under ./dev-output for inputs, outputs, and results
out_dir <- file.path("dev-output", "openai_multi_batch_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Starting OpenAI multi‑batch test...\n")
# Submit the pairs as multiple batch jobs (5 segments of 2 pairs each) and
# request that thoughts/reasoning be included in the output via include_thoughts
multi_job <- llm_submit_pairs_multi_batch(
  pairs             = pairs,
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "openai",
  n_segments        = 5L,
  output_dir        = out_dir,
  write_registry    = TRUE,
  include_thoughts  = TRUE,
  verbose           = TRUE
)

# Display registry information for debugging
cat("\nCreated", length(multi_job$jobs), "batch jobs.\n")
cat("OPENAI_API_KEY length:", nchar(Sys.getenv("OPENAI_API_KEY")), "(characters)\n")
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
# each batch’s parsed results to CSV in `out_dir`.  Setting write_combined_csv
# writes a single CSV containing the combined results from all batches.
res <- llm_resume_multi_batches(
  jobs               = multi_job$jobs,
  interval_seconds   = 15,
  per_job_delay      = 2,
  write_results_csv  = TRUE,
  keep_jsonl         = TRUE,
  verbose            = TRUE,
  write_combined_csv = TRUE,
  combined_csv_path  = "openai_combined_results.csv"
)

# Summarize job statuses after polling
cat("\nPolling complete. Summary of job statuses:\n")
print(
  do.call(
    rbind,
    lapply(res$jobs, function(j) {
      data.frame(
        segment_index = j$segment_index,
        batch_id      = j$batch_id,
        done          = j$done,
        n_results     = if (is.null(j$results)) NA_integer_ else nrow(j$results)
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
