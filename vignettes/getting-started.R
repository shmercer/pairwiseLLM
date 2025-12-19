## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

library(pairwiseLLM)
library(dplyr)

## -----------------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")
dplyr::slice_head(example_writing_samples, n = 3)

## -----------------------------------------------------------------------------
pairs <- example_writing_samples |>
  make_pairs()

dplyr::slice_head(pairs, n = 5)

## -----------------------------------------------------------------------------
pairs_small <- sample_pairs(pairs, n_pairs = 10, seed = 123)

## -----------------------------------------------------------------------------
pairs_small <- randomize_pair_order(pairs_small, seed = 99)

## -----------------------------------------------------------------------------
td <- trait_description("overall_quality")
td

## -----------------------------------------------------------------------------
td_custom <- trait_description(
  custom_name = "Clarity",
  custom_description = "How clearly and effectively ideas are expressed."
)

## -----------------------------------------------------------------------------
tmpl <- set_prompt_template()
cat(substr(tmpl, 1, 300))

## ----eval=FALSE---------------------------------------------------------------
# set_prompt_template(file = "my_template.txt")

## ----eval=FALSE---------------------------------------------------------------
# res_live <- submit_llm_pairs(
#   pairs             = pairs_small,
#   backend           = "openai", # also "anthropic", "gemini", "together", "ollama"
#   model             = "gpt-4o",
#   trait_name        = td$name,
#   trait_description = td$description,
#   prompt_template   = tmpl
# )

## ----eval=FALSE---------------------------------------------------------------
# dplyr::slice_head(res_live, 5)

## ----eval=FALSE---------------------------------------------------------------
# # res_live: output from submit_llm_pairs()
# bt_data <- build_bt_data(res_live)
# dplyr::slice_head(bt_data, 5)

## ----eval=FALSE---------------------------------------------------------------
# # res_live: output from submit_llm_pairs()
# elo_data <- build_elo_data(res_live)

## ----eval=FALSE---------------------------------------------------------------
# bt_fit <- fit_bt_model(bt_data)

## ----eval=FALSE---------------------------------------------------------------
# summarize_bt_fit(bt_fit)

## ----eval=FALSE---------------------------------------------------------------
# elo_fit <- fit_elo_model(elo_data, runs = 5)
# elo_fit

## ----eval=FALSE---------------------------------------------------------------
# batch <- llm_submit_pairs_batch(
#   backend            = "openai",
#   model              = "gpt-4o",
#   pairs              = pairs_small,
#   trait_name         = td$name,
#   trait_description  = td$description,
#   prompt_template    = tmpl
# )

## ----eval=FALSE---------------------------------------------------------------
# res_batch <- llm_download_batch_results(batch)
# head(res_batch)

## ----eval=FALSE---------------------------------------------------------------
# # Generate a small set of pairs
# pairs_small <- example_writing_samples |>
#   make_pairs() |>
#   sample_pairs(n_pairs = 10, seed = 4321) |>
#   randomize_pair_order(seed = 8765)
# 
# td   <- trait_description("overall_quality")
# tmpl <- set_prompt_template()
# 
# # Split into two batches and include reasoning/chain-of-thought
# multi_job <- llm_submit_pairs_multi_batch(
#   pairs             = pairs_small,
#   backend           = "openai",
#   model             = "gpt-5.1",
#   trait_name        = td$name,
#   trait_description = td$description,
#   prompt_template   = tmpl,
#   n_segments        = 2,
#   output_dir        = "myjob",
#   write_registry    = TRUE,
#   include_thoughts  = TRUE
# )
# 
# # Poll and merge results.  Combined results are written to
# # "myjob/combined_results.csv" by default.
# res <- llm_resume_multi_batches(
#   jobs               = multi_job$jobs,
#   interval_seconds   = 30,
#   write_combined_csv = TRUE
# )
# 
# head(res$combined)

## -----------------------------------------------------------------------------
check_llm_api_keys()

