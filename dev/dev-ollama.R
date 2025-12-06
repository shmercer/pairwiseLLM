## -------------------------------------------------------------------
## Dev script: quick manual checks of Ollama backend
##
## This script is NOT a testthat test. It is meant to be run manually
## during development to sanity-check the Ollama backend with a few
## pairs and several local models.
##
## Suggested location: inst/dev/dev-ollama.R (or dev/dev-ollama.R)
## -------------------------------------------------------------------

# You can adjust this if your Ollama server is on a different host/port
options(pairwiseLLM.ollama_host = "http://127.0.0.1:11434")

library(pairwiseLLM)
library(dplyr)

# -------------------------------------------------------------------
# 1. Load example data and build a small set of pairs
# -------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

set.seed(123)

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

print(pairs)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Helper to print a compact summary of results
summarise_results <- function(x) {
  x |>
    count(better_id, name = "n") |>
    arrange(desc(n)) |>
    print()
}

# -------------------------------------------------------------------
# 2. Mistral: mistral-small3.2:24b (no thinking flag)
# -------------------------------------------------------------------

cat("\n=== Ollama dev check: mistral-small3.2:24b ===\n")

res_mistral <- submit_llm_pairs(
  pairs             = pairs,
  model             = "mistral-small3.2:24b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "ollama",
  verbose           = TRUE,
  status_every      = 1,
  progress          = TRUE,
  include_raw       = FALSE,
  think             = FALSE,        # not used by mistral
  num_ctx           = 8192
)

print(res_mistral |> select(custom_id, better_sample, better_id, status_code))
cat("\nSummary for mistral-small3.2:24b:\n")
summarise_results(res_mistral)

# -------------------------------------------------------------------
# 3. Qwen without thinking: qwen3:32b, think = FALSE (temp = 0)
# -------------------------------------------------------------------

cat("\n=== Ollama dev check: qwen3:32b (think = FALSE) ===\n")

res_qwen_det <- submit_llm_pairs(
  pairs             = pairs,
  model             = "qwen3:32b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "ollama",
  verbose           = TRUE,
  status_every      = 1,
  progress          = TRUE,
  include_raw       = FALSE,
  think             = FALSE,        # temperature = 0
  num_ctx           = 8192
)

print(res_qwen_det |> select(custom_id, better_sample, better_id, status_code))
cat("\nSummary for qwen3:32b (think = FALSE):\n")
summarise_results(res_qwen_det)

# -------------------------------------------------------------------
# 4. Qwen with thinking: qwen3:32b, think = TRUE (temp = 0.6)
# -------------------------------------------------------------------

cat("\n=== Ollama dev check: qwen3:32b (think = TRUE) ===\n")

res_qwen_think <- submit_llm_pairs(
  pairs             = pairs,
  model             = "qwen3:32b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "ollama",
  verbose           = TRUE,
  status_every      = 1,
  progress          = TRUE,
  include_raw       = FALSE,
  think             = TRUE,         # triggers temperature = 0.6
  num_ctx           = 8192
)

print(res_qwen_think |> select(custom_id, better_sample, better_id, status_code))
cat("\nSummary for qwen3:32b (think = TRUE):\n")
summarise_results(res_qwen_think)

# -------------------------------------------------------------------
# 5. Gemma: gemma3:27b (no thinking flag)
# -------------------------------------------------------------------

cat("\n=== Ollama dev check: gemma3:27b ===\n")

res_gemma <- submit_llm_pairs(
  pairs             = pairs,
  model             = "gemma3:27b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "ollama",
  verbose           = TRUE,
  status_every      = 1,
  progress          = TRUE,
  include_raw       = FALSE,
  think             = FALSE,        # not used by gemma
  num_ctx           = 8192
)

print(res_gemma |> select(custom_id, better_sample, better_id, status_code))
cat("\nSummary for gemma3:27b:\n")
summarise_results(res_gemma)

cat("\n=== Ollama dev check complete. ===\n")
