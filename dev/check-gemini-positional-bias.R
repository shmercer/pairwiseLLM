#!/usr/bin/env Rscript

# dev/check-gemini-positional-bias.R
# Full Gemini batch run on example_writing_samples + positional bias check

if (!requireNamespace("pairwiseLLM", quietly = TRUE)) {
  stop("Please install/load pairwiseLLM first.")
}

library(pairwiseLLM)

# Use your usual dev workflow: run this from RStudio after devtools::load_all()
message("=== Gemini positional bias check (batch) ===")

# -------------------------------------------------------------------
# 1. Load example data and build randomized pairs
# -------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

set.seed(20251130)  # for reproducibility

pairs <- make_pairs(example_writing_samples)
pairs <- randomize_pair_order(pairs)

message(sprintf("Built %d pairs from %d samples.",
                nrow(pairs), nrow(example_writing_samples)))

# Trait / prompt template (same pattern used for other backends)
td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# -------------------------------------------------------------------
# 2. Run full Gemini batch pipeline
# -------------------------------------------------------------------

model <- "gemini-3-pro-preview"

message("Running Gemini batch pipeline...")
message(sprintf("  model           = %s", model))
message(sprintf("  trait_name      = %s", td$name))
message(sprintf("  thinking_level  = %s", "low"))
message(sprintf("  include_thoughts = %s", TRUE))

batch_result <- run_gemini_batch_pipeline(
  pairs             = pairs,
  model             = model,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  # Let Gemini return thought metadata + signatures; visible thoughts may or
  # may not be present, but the batch parser handles both cases.
  include_thoughts  = TRUE,
  # For a full example file this might take a bit; tune as needed
  interval_seconds  = 30,
  timeout_seconds   = 60 * 60,   # 1 hour
  verbose           = TRUE
)

gemini_results <- batch_result$results

message("Gemini batch completed.")
message(sprintf("  Results rows: %d", nrow(gemini_results)))

# Save a copy for later inspection / reproducibility
if (!dir.exists("dev")) dir.create("dev", showWarnings = FALSE)

results_rds  <- file.path("dev", "gemini_positional_bias_results.rds")
results_csv  <- file.path("dev", "gemini_positional_bias_results.csv")

saveRDS(gemini_results, results_rds)
utils::write.csv(gemini_results, results_csv, row.names = FALSE)

message("Saved Gemini batch results to:")
message(sprintf("  RDS : %s", results_rds))
message(sprintf("  CSV : %s", results_csv))

# -------------------------------------------------------------------
# 3. Check positional bias using the package helper
# -------------------------------------------------------------------

message("Computing positional bias metrics from Gemini results...")

# Assumes check_positional_bias() uses the standard columns (ID1, ID2, better_id)
# as produced by the batch/live pipelines.
pos_bias <- check_positional_bias(gemini_results)

print(pos_bias)

# Optionally save the summary too
summary_rds <- file.path("dev", "gemini_positional_bias_summary.rds")
saveRDS(pos_bias, summary_rds)
message(sprintf("Saved positional bias summary to: %s", summary_rds))

message("=== Gemini positional bias check complete ===")
