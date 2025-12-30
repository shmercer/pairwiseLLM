# Live + Batch smoke-test for adaptive + linking workflows using OpenAI (gpt-4o-mini)
# Uses built-in example_writing_samples (real text) from pairwiseLLM.
#
# Prereqs:
#   Sys.setenv(OPENAI_API_KEY = "sk-...")  # required
# Optional:
#   install.packages("sirt") or install.packages("BradleyTerry2")  # for real BT fitting via fit_bt_model(engine="auto")

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(tibble)
  library(dplyr)
})

stopifnot(nzchar(Sys.getenv("OPENAI_API_KEY")))

# -------------------------------------------------------------------------
# Built-in examples
# -------------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")

# Expect columns like ID + text (adjust if your dataset uses different names)
samples <- example_writing_samples
if (!all(c("ID", "text") %in% names(samples))) {
  stop("example_writing_samples must have columns: ID, text")
}

# Two small batches + a small core set
ids <- samples$ID
batches <- list(ids[1:10], ids[11:20])
core_ids <- ids[1:5]

trait <- trait_description("overall_quality")
prompt_template <- set_prompt_template()

# -------------------------------------------------------------------------
# LIVE judge (calls OpenAI chat completions directly)
# -------------------------------------------------------------------------
judge_openai_live <- function(pairs) {
  out <- submit_llm_pairs(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4o-mini",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    trait_name = trait$name,
    trait_description = trait$description,
    prompt_template = prompt_template,
    temperature = 0
  )

  # submit_llm_pairs() returns a list; bt_run_* expects the results tibble.
  if (is.list(out) && "results" %in% names(out)) {
    if ("failed_pairs" %in% names(out) && nrow(out$failed_pairs) > 0) {
      message("NOTE: ", nrow(out$failed_pairs), " failed pairs returned by provider.")
    }
    return(out$results)
  }

  out
}

# -------------------------------------------------------------------------
# BATCH judge (submits a Batch job to OpenAI, then returns results)
#   Note: this is still synchronous from R's perspective; it just exercises
#   the OpenAI Batch API under the hood.
# -------------------------------------------------------------------------
judge_openai_batch <- function(pairs) {
  batch <- llm_submit_pairs_batch(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4o-mini",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    trait_name = trait$name,
    trait_description = trait$description,
    prompt_template = prompt_template,
    temperature = 0
  )

  out <- llm_download_batch_results(batch)

  # Be robust if batch path also returns list(results=..., failed_pairs=...)
  if (is.list(out) && "results" %in% names(out)) return(out$results)
  out
}

# -------------------------------------------------------------------------
# 1) LIVE: bt_run_adaptive_core_linking (small, cheap run)
# -------------------------------------------------------------------------
out_live <- bt_run_adaptive_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "always",          # force linking path
  linking_min_n = length(core_ids),
  linking_cor_target = 0.99,
  linking_p90_abs_shift_target = 0.10,

  judge_fun = judge_openai_live,
  fit_fun = fit_bt_model,      # real fit if available; otherwise you'll see an informative error
  engine = "auto",

  round_size = 6,
  max_rounds_per_batch = 1,
  within_batch_frac = 0.6,
  core_audit_frac = 0.4,
  reliability_target = Inf,

  seed_pairs = 1,
  verbose = TRUE,
  show_progress = TRUE
)

print(out_live$batch_summary)
if (!is.null(out_live$metrics)) print(tail(out_live$metrics, 5))
print(head(out_live$results, 10))

# -------------------------------------------------------------------------
# 2) BATCH: bt_run_adaptive_core_linking (same run, but judge via Batch API)
# -------------------------------------------------------------------------
out_batch <- bt_run_adaptive_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "always",
  linking_min_n = length(core_ids),
  linking_cor_target = 0.99,
  linking_p90_abs_shift_target = 0.10,

  judge_fun = judge_openai_batch,
  fit_fun = fit_bt_model,
  engine = "auto",

  round_size = 6,
  max_rounds_per_batch = 1,
  within_batch_frac = 0.6,
  core_audit_frac = 0.4,
  reliability_target = Inf,

  seed_pairs = 1,
  verbose = TRUE,
  show_progress = TRUE
)

print(out_batch$batch_summary)
if (!is.null(out_batch$metrics)) print(tail(out_batch$metrics, 5))
print(head(out_batch$results, 10))

# -------------------------------------------------------------------------
# 3) Minimal “just the provider” checks (optional): submit a few pairs directly
# -------------------------------------------------------------------------
pairs_small <- tibble(
  ID1 = ids[1:3],
  text1 = samples$text[match(ids[1:3], ids)],
  ID2 = ids[4:6],
  text2 = samples$text[match(ids[4:6], ids)]
)

res_live_pairs <- judge_openai_live(pairs_small)
res_batch_pairs <- judge_openai_batch(pairs_small)

print(res_live_pairs)
print(res_batch_pairs)
