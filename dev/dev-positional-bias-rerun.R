# dev/dev-positional-bias-rerun.R
#
# Rerun specific failed / misconfigured batches:
#   - OpenAI gpt-5.1 (all 4 combos: no/with thinking x forward/reverse)
#   - Anthropic claude-opus-4-0 (with_thinking, reverse only)

library(pairwiseLLM)
library(dplyr)
library(readr)

## ------------------------------------------------------------------
## Shared setup: output dir, pairs, trait, template
## ------------------------------------------------------------------

out_dir <- "dev-output/positional-bias-all-models"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Recreate the same pairs as in the main dev script
set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

pairs_forward <- example_writing_samples |>
  make_pairs() |>
  randomize_pair_order(seed = 123)

# Use ALL pairs in reverse order (100%)
pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1,
  seed        = 321
)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

## ------------------------------------------------------------------
## Helper: rerun OpenAI gpt-5.1 batches (correct tag)
## ------------------------------------------------------------------

rerun_openai_gpt_5_1 <- function(include_thoughts, direction) {
  stopifnot(direction %in% c("forward", "reverse"))

  pairs <- if (direction == "forward") pairs_forward else pairs_reverse

  thinking_tag <- if (isTRUE(include_thoughts)) "with_thinking" else "no_thinking"
  safe_model   <- "gpt-5.1"

  batch_input_path <- file.path(
    out_dir,
    paste0("openai_", safe_model, "_", thinking_tag, "_", direction, "_input.jsonl")
  )
  batch_output_path <- file.path(
    out_dir,
    paste0("openai_", safe_model, "_", thinking_tag, "_", direction, "_output.jsonl")
  )
  csv_path <- file.path(
    out_dir,
    paste0("openai_", safe_model, "_", thinking_tag, "_", direction, ".csv")
  )

  message("Submitting OpenAI gpt-5.1 (", thinking_tag, ", ", direction, ") as batch...")

  pipe <- run_openai_batch_pipeline(
    pairs             = pairs,
    model             = "gpt-5.1",         # <-- correct tag
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    include_thoughts  = include_thoughts,
    batch_input_path  = batch_input_path,
    batch_output_path = batch_output_path,
    poll              = TRUE,
    interval_seconds  = 300,               # poll every 5 minutes
    timeout_seconds   = 86400              # up to 24h
  )

  if (!is.null(pipe$results) && nrow(pipe$results) > 0) {
    write_csv(pipe$results, csv_path)
    message("Saved parsed results to: ", csv_path)
  } else {
    message("No parsed results returned for OpenAI gpt-5.1 (",
            thinking_tag, ", ", direction, ").")
  }

  invisible(pipe)
}

## ------------------------------------------------------------------
## Helper: rerun Anthropic claude-opus-4-0 (with_thinking, reverse)
## ------------------------------------------------------------------

rerun_anthropic_claude_opus_4_0_with_thinking_reverse <- function() {
  pairs <- pairs_reverse

  thinking_tag <- "with_thinking"
  safe_model   <- "claude-opus-4-0"

  batch_input_path <- file.path(
    out_dir,
    paste0("anthropic_", safe_model, "_", thinking_tag, "_reverse_input.json")
  )
  batch_output_path <- file.path(
    out_dir,
    paste0("anthropic_", safe_model, "_", thinking_tag, "_reverse_output.jsonl")
  )
  csv_path <- file.path(
    out_dir,
    paste0("anthropic_", safe_model, "_", thinking_tag, "_reverse.csv")
  )

  message("Submitting Anthropic claude-opus-4-0 (with_thinking, reverse) as batch...")

  pipe <- run_anthropic_batch_pipeline(
    pairs             = pairs,
    model             = "claude-opus-4-0",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    reasoning         = "enabled",  # with_thinking
    include_thoughts  = TRUE,
    batch_input_path  = batch_input_path,
    batch_output_path = batch_output_path,
    poll              = TRUE,
    interval_seconds  = 300,        # poll every 5 minutes
    timeout_seconds   = 86400,      # up to 24h
    verbose           = TRUE
  )

  if (!is.null(pipe$results) && nrow(pipe$results) > 0) {
    write_csv(pipe$results, csv_path)
    message("Saved parsed results to: ", csv_path)
  } else {
    message("No parsed results returned for Anthropic claude-opus-4-0 (with_thinking, reverse).")
  }

  invisible(pipe)
}

## ------------------------------------------------------------------
## Execute the specific reruns
## ------------------------------------------------------------------

# OpenAI gpt-5.1 (fix model tag):
openai_gpt_5_1_no_thinking_forward  <- rerun_openai_gpt_5_1(include_thoughts = FALSE, direction = "forward")
openai_gpt_5_1_no_thinking_reverse  <- rerun_openai_gpt_5_1(include_thoughts = FALSE, direction = "reverse")
openai_gpt_5_1_with_thinking_forward <- rerun_openai_gpt_5_1(include_thoughts = TRUE,  direction = "forward")
openai_gpt_5_1_with_thinking_reverse <- rerun_openai_gpt_5_1(include_thoughts = TRUE,  direction = "reverse")

# Anthropic claude-opus-4-0 with_thinking, reverse:
anthropic_claude_opus_4_0_with_thinking_reverse <-
  rerun_anthropic_claude_opus_4_0_with_thinking_reverse()
