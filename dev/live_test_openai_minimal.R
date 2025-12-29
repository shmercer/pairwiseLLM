# Live OpenAI smoke test for pairwiseLLM (developer utility)
#
# Run from an interactive R session:
#   source(system.file("dev/live_test_openai_minimal.R", package = "pairwiseLLM"))
#
# Prereqs:
#   - Set OPENAI_API_KEY (recommended: in a project .Renviron)
#   - Optional: set PAIRWISELLM_OPENAI_TEST_MODEL (default: "gpt-5-nano")

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(tibble)
  library(dplyr)
})

cat("\n=== pairwiseLLM: live OpenAI smoke test ===\n")

model <- Sys.getenv("PAIRWISELLM_OPENAI_TEST_MODEL", unset = "gpt-5-nano")
endpoint <- Sys.getenv("PAIRWISELLM_OPENAI_TEST_ENDPOINT", unset = "responses")

cat("Model: ", model, "\n", sep = "")
cat("Endpoint: ", endpoint, "\n", sep = "")

# Load built-in example data
data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples %>%
  transmute(ID = as.character(ID), text = as.character(text)) %>%
  slice(1:6)

pairs <- tibble(
  ID1 = samples$ID[1:3],
  text1 = samples$text[1:3],
  ID2 = samples$ID[4:6],
  text2 = samples$text[4:6]
)

cat("\nSubmitting ", nrow(pairs), " pairs...\n", sep = "")

res <- submit_openai_pairs_live(
  pairs = pairs,
  model = model,
  trait_name = "Overall writing quality",
  trait_description = "Which writing sample is better overall?",
  endpoint = endpoint,
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = TRUE,
  validate = TRUE,
  validate_strict = FALSE,
  temperature = 0
)

cat("\n=== Results (first rows) ===\n")
print(res$results %>% select(custom_id, ID1, ID2, better_id, status_code, error_message))

if (nrow(res$failed_pairs) > 0) {
  cat("\nFAILED PAIRS:\n")
  print(res$failed_pairs)
}

cat("\nDone.\n")
