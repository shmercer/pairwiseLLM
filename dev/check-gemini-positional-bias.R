#!/usr/bin/env Rscript

# dev/check-gemini-positional-bias.R
# Gemini positional bias check using the package pairing + consistency helpers:
#   - make_pairs()
#   - randomize_pair_order()
#   - sample_reverse_pairs()
#   - compute_reverse_consistency()
#   - check_positional_bias()

if (!requireNamespace("pairwiseLLM", quietly = TRUE)) {
  stop("Please install/load pairwiseLLM first.")
}

library(pairwiseLLM)
library(dplyr)

message("=== Gemini positional bias check (batch: forward + reverse) ===")

# -------------------------------------------------------------------
# 1. Load example data and build forward + reverse pairs
# -------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

set.seed(20251130)  # for reproducibility

# All unordered pairs from the example samples
pairs_all <- make_pairs(example_writing_samples)

# Randomize SAMPLE_1 / SAMPLE_2 position once
pairs_forward <- randomize_pair_order(pairs_all, seed = 20251130)

# Build a full set of reversed pairs using the helper:
# reverse_pct = 1 => reverse all rows (one reversed copy per pair)
# Note: order of rows in the reversed tibble may differ, but
# compute_reverse_consistency() is order-invariant (it uses unordered keys).
pairs_reverse <- sample_reverse_pairs(
  pairs        = pairs_forward,
  reverse_pct  = 1,
  seed         = 20251130
)

n_forward <- nrow(pairs_forward)
n_reverse <- nrow(pairs_reverse)

message(sprintf(
  "Built %d forward pairs and %d reverse pairs from %d samples.",
  n_forward, n_reverse, nrow(example_writing_samples)
))

# -------------------------------------------------------------------
# 2. Set up trait / prompt / model
# -------------------------------------------------------------------

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()
model <- "gemini-3-pro-preview"

message("Using settings:")
message(sprintf("  model            = %s", model))
message(sprintf("  trait_name       = %s", td$name))
message(sprintf("  thinking_level   = %s", "low"))
message(sprintf("  include_thoughts = %s", TRUE))

# -------------------------------------------------------------------
# 3. Run Gemini batch pipeline: FORWARD pairs
# -------------------------------------------------------------------

message("Running Gemini batch pipeline for FORWARD pairs...")

batch_forward <- run_gemini_batch_pipeline(
  pairs             = pairs_forward,
  model             = model,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  include_thoughts  = TRUE,
  interval_seconds  = 30,
  timeout_seconds   = 60 * 60,   # 1 hour
  verbose           = TRUE
)

results_forward <- batch_forward$results

message("Forward batch completed.")
message(sprintf("  Forward results rows: %d", nrow(results_forward)))

# -------------------------------------------------------------------
# 4. Run Gemini batch pipeline: REVERSE pairs
# -------------------------------------------------------------------

message("Running Gemini batch pipeline for REVERSE pairs...")

batch_reverse <- run_gemini_batch_pipeline(
  pairs             = pairs_reverse,
  model             = model,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  include_thoughts  = TRUE,
  interval_seconds  = 30,
  timeout_seconds   = 60 * 60,   # 1 hour
  verbose           = TRUE
)

results_reverse <- batch_reverse$results

message("Reverse batch completed.")
message(sprintf("  Reverse results rows: %d", nrow(results_reverse)))

# -------------------------------------------------------------------
# 5. Save raw results for reproducibility
# -------------------------------------------------------------------

if (!dir.exists("dev")) dir.create("dev", showWarnings = FALSE)

forward_rds <- file.path("dev", "gemini_positional_bias_forward_results.rds")
forward_csv <- file.path("dev", "gemini_positional_bias_forward_results.csv")
reverse_rds <- file.path("dev", "gemini_positional_bias_reverse_results.rds")
reverse_csv <- file.path("dev", "gemini_positional_bias_reverse_results.csv")

saveRDS(results_forward, forward_rds)
utils::write.csv(results_forward, forward_csv, row.names = FALSE)

saveRDS(results_reverse, reverse_rds)
utils::write.csv(results_reverse, reverse_csv, row.names = FALSE)

message("Saved Gemini FORWARD results to:")
message(sprintf("  RDS : %s", forward_rds))
message(sprintf("  CSV : %s", forward_csv))

message("Saved Gemini REVERSE results to:")
message(sprintf("  RDS : %s", reverse_rds))
message(sprintf("  CSV : %s", reverse_csv))

# -------------------------------------------------------------------
# 6. Compute reverse consistency (forward vs reverse)
# -------------------------------------------------------------------

message("Computing reverse consistency (forward vs reverse)...")

# compute_reverse_consistency() expects ID1, ID2, better_id in each tibble.
rc <- compute_reverse_consistency(
  main_results    = results_forward,
  reverse_results = results_reverse
)

message("Reverse consistency summary:")
print(rc$summary)

# -------------------------------------------------------------------
# 7. Check positional bias using the consistency object
# -------------------------------------------------------------------

message("Checking positional bias with bootstrap CI...")

pos_bias <- check_positional_bias(
  consistency = rc,
  n_boot      = 1000,
  conf_level  = 0.95,
  seed        = 20251130
)

message("Positional bias summary:")
print(pos_bias$summary)

pos_bias_rds <- file.path("dev", "gemini_positional_bias_summary.rds")
saveRDS(pos_bias, pos_bias_rds)
message(sprintf("Saved positional bias object to: %s", pos_bias_rds))

message("=== Gemini positional bias check complete ===")
