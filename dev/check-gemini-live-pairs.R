# dev/check-gemini-live-pairs.R

# Run from the package root:
# devtools::load_all()
# source("dev/check-gemini-live-pairs.R")

library(pairwiseLLM)
library(dplyr)

cat("=== Checking API keys ===\n")
status <- check_llm_api_keys()
print(status)

if (!status$has_key[status$backend == "gemini"]) {
  stop(
    "GEMINI_API_KEY is not set. Please configure it in your environment first.\n",
    'Use `usethis::edit_r_environ()` and add:\n',
    '  GEMINI_API_KEY = "YOUR_KEY_HERE"\n',
    "Then restart R and rerun this script."
  )
}

cat("\n=== Loading example data and building small pair set ===\n")

data("example_writing_samples", package = "pairwiseLLM")

# Pick a small subset of IDs for a compact dev test
ids_small <- c("S01", "S02", "S05", "S06")

samples_small <- example_writing_samples |>
  dplyr::filter(ID %in% ids_small)

cat("Using the following sample IDs:\n")
print(samples_small |> dplyr::select(ID) |> distinct())

# Build pairs and then sample 3 of them for the dev test
pairs_small <- samples_small |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

cat("\nSmall pair tibble:\n")
print(pairs_small)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

model_name <- "gemini-3-pro-preview"

cat("\n=== Single-pair live test (include_thoughts = TRUE) ===\n")

pair1 <- pairs_small[1, ]

res_single <- gemini_compare_pair_live(
  ID1               = pair1$ID1,
  text1             = pair1$text1,
  ID2               = pair1$ID2,
  text2             = pair1$text2,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "high",      # exercise thinkingConfig
  include_thoughts  = TRUE,        # request chain-of-thought
  include_raw       = FALSE
)

print(res_single)

cat("\nTHOUGHTS (first pair):\n")
print(res_single$thoughts)

cat("\nDECISION CONTENT (first pair):\n")
print(res_single$content)

cat("\nBetter sample / ID (first pair):\n")
print(res_single |> dplyr::select(better_sample, better_id))

cat("\n=== Multi-pair live test (include_thoughts = FALSE) ===\n")

res_multi_no_thoughts <- submit_gemini_pairs_live(
  pairs             = pairs_small,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  include_thoughts  = FALSE,
  include_raw       = FALSE,
  verbose           = TRUE,
  progress          = TRUE
)

cat("\nResults (no thoughts):\n")
print(res_multi_no_thoughts)

cat("\nSummary of choices (no thoughts):\n")
print(
  res_multi_no_thoughts |>
    count(better_sample, better_id)
)

cat("\n=== Multi-pair live test (include_thoughts = TRUE) ===\n")

res_multi_with_thoughts <- submit_gemini_pairs_live(
  pairs             = pairs_small,
  model             = model_name,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "high",
  include_thoughts  = TRUE,
  include_raw       = FALSE,
  verbose           = TRUE,
  progress          = TRUE
)

cat("\nResults (with thoughts):\n")
print(
  res_multi_with_thoughts |>
    select(custom_id, better_sample, better_id, thoughts, content)
)

cat("\nNon-empty thoughts count:\n")
print(
  res_multi_with_thoughts |>
    mutate(has_thoughts = !is.na(thoughts) & nzchar(thoughts)) |>
    count(has_thoughts)
)

cat("\n=== Done: Gemini live dev tests complete ===\n")
