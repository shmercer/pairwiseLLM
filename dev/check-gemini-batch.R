# dev/check-gemini-batch.R

library(pairwiseLLM)
library(dplyr)

status <- check_llm_api_keys()
print(status)

key <- Sys.getenv("GEMINI_API_KEY")
if (!nzchar(key)) {
  stop("GEMINI_API_KEY is not set; cannot run batch check.")
}

data("example_writing_samples", package = "pairwiseLLM")

# Use a small sample of pairs for live testing
pairs_all <- make_pairs(example_writing_samples)

n_pairs <- 3L
n_pairs <- min(n_pairs, nrow(pairs_all))

pairs <- pairs_all |>
  sample_pairs(n_pairs = n_pairs, seed = 123) |>
  randomize_pair_order(seed = 456)

print(pairs)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

model_name <- "gemini-3-pro-preview"

batch_input_path <- tempfile("gemini-batch-input-",  fileext = ".json")
batch_output_path <- tempfile("gemini-batch-output-", fileext = ".jsonl")

cat("\n=== Gemini batch test ===\n")

pipeline <- run_gemini_batch_pipeline(
  pairs             = pairs,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  batch_input_path  = batch_input_path,
  batch_output_path = batch_output_path,
  poll              = TRUE,
  interval_seconds  = 30,
  timeout_seconds   = 600,
  api_key           = key,
  api_version       = "v1beta",
  verbose           = TRUE
)

str(pipeline$batch)
cat("\nResults tibble:\n")
print(pipeline$results)
