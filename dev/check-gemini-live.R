# dev/check-gemini-live.R

library(pairwiseLLM)
library(dplyr)

# 1. Check keys -------------------------------------------------------------

status <- check_llm_api_keys()
print(status)

# 2. Get a single pair ------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

pairs_1 <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 1, seed = 123) |>
  randomize_pair_order(seed = 456)

print(pairs_1)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

model_name <- "gemini-3-pro-preview"  # or your chosen Gemini model

cat("\n=== Gemini single-pair test ===\n")

# 3. Call your Gemini live helper -------------------------------------------
# Assumes you have defined gemini_compare_pair_live() already

res <- gemini_compare_pair_live(
  ID1               = pairs_1$ID1[1],
  text1             = pairs_1$text1[1],
  ID2               = pairs_1$ID2[1],
  text2             = pairs_1$text2[1],
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",   # default for Gemini 3 Pro in this package
  include_thoughts  = TRUE,
  include_raw       = TRUE
)

print(res)
cat("\nTHOUGHTS:\n", res$thoughts, "\n")
cat("\nDECISION CONTENT:\n", res$content, "\n")
cat("\nBetter ID:", res$better_id, "\n")
