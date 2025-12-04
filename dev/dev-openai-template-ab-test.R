# dev/dev-openai-template-ab-test.R
#
# A/B test two alternative prompt templates on OpenAI models via batch API.
#
# - OpenAI models:
#     * gpt-4.1
#     * gpt-4.1-mini
#     * gpt-4.1-nano
#     * gpt-4o
#     * gpt-5.1
# - Thinking:
#     * "no_thinking" for all models (chat.completions, temperature=0)
#     * "with_thinking" only for gpt-5.1 (Responses API, reasoning = "low")
# - Directions:
#     * forward (alternate_pair_order)
#     * reverse (sample_reverse_pairs, 100% reversed)
#
# Templates:
#   T1: "FIRST SAMPLE / SECOND SAMPLE" template
#   T2: "SAMPLE A / SAMPLE B" template
#
# Outputs:
# - Per-run CSVs:
#     dev-output/openai-template-ab-test/openai_ab_<TID>_<model>_<thinking>_<direction>.csv
# - Summary CSV:
#     dev-output/openai-template-ab-test/openai_template_ab_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

# ---------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------

out_dir <- "dev-output/openai-template-ab-test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

td <- trait_description("overall_quality")

# ---------------------------------------------------------------------
# 1. Two candidate templates (T1 and T2)
# ---------------------------------------------------------------------

template_T1 <- "
You are an expert writing assessor.

Your task: Determine which of two writing samples demonstrates superior {TRAIT_NAME}.

{TRAIT_NAME} is defined as:
{TRAIT_DESCRIPTION}

Below are two samples. They appear in arbitrary order—neither position indicates quality.

═══════════════════════════════════════
FIRST SAMPLE:
{SAMPLE_1}

═══════════════════════════════════════
SECOND SAMPLE:
{SAMPLE_2}

═══════════════════════════════════════

ASSESSMENT PROTOCOL:

Step 1: Read both samples in their entirety.

Step 2: For each sample independently, assess the degree to which it demonstrates {TRAIT_NAME} based solely on the definition provided.

Step 3: Compare your assessments. Determine which sample shows stronger {TRAIT_NAME}.

Step 4: Select the sample with better {TRAIT_NAME}. If extremely close, choose the one with any detectable advantage. No ties are allowed.

Step 5: Verify your selection reflects the CONTENT quality, not the presentation order.

RESPONSE FORMAT:

Respond with exactly one line using this format:

<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>

if the first sample is better, OR

<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>

if the second sample is better.

Output only the XML tag with your choice. No explanations or additional text.
"

template_T2 <- "
You are an expert writing assessor evaluating student work on a specific trait.

TRAIT TO EVALUATE: {TRAIT_NAME}

DEFINITION:
{TRAIT_DESCRIPTION}

TWO SAMPLES TO COMPARE:

SAMPLE A:
{SAMPLE_1}

SAMPLE B:
{SAMPLE_2}

EVALUATION INSTRUCTIONS:

1. Read both samples completely before making any judgments.

2. Evaluate ONLY on {TRAIT_NAME} as defined above. Ignore other factors (length, grammar, formatting, topic) unless they directly impact {TRAIT_NAME}.

3. CRITICAL: The labels \"SAMPLE A\" and \"SAMPLE B\" are random assignments with no meaning. Do not let their alphabetical order or position influence your judgment.

4. Use this mental process:
   - Identify specific evidence of {TRAIT_NAME} in SAMPLE A
   - Identify specific evidence of {TRAIT_NAME} in SAMPLE B
   - Compare the QUALITY and STRENGTH of {TRAIT_NAME} in each
   - Determine which sample demonstrates BETTER {TRAIT_NAME}

5. If samples appear nearly equal, identify which shows even marginally better {TRAIT_NAME}. You must select one—ties are not permitted.

6. Before finalizing your decision, perform this check:
   \"If these samples were labeled in reverse order, would I still choose the same content as better?\"
   If your answer depends on the labels rather than the content quality, reconsider.

7. Output your decision using EXACTLY one of these two formats (nothing else):

<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>

if SAMPLE A is better, OR

<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>

if SAMPLE B is better.

Do not include explanations, reasoning, or any other text in your response.
"

templates_tbl <- tibble::tibble(
  template_id     = c("T1", "T2"),
  prompt_template = list(template_T1, template_T2)
)

# ---------------------------------------------------------------------
# 2. Build forward + reverse pairs (subset for cost)
# ---------------------------------------------------------------------

pairs_all <- example_writing_samples |>
  make_pairs()

pairs_forward <- pairs_all |>
  alternate_pair_order()

pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1.0,
  seed        = 99
)

get_pairs_for_direction <- function(direction) {
  if (identical(direction, "forward")) {
    pairs_forward
  } else if (identical(direction, "reverse")) {
    pairs_reverse
  } else {
    stop("Unknown direction: ", direction, call. = FALSE)
  }
}

# ---------------------------------------------------------------------
# 3. Model grid (OpenAI only; only gpt-5.1 gets with_thinking)
# ---------------------------------------------------------------------

openai_models <- c(
  "gpt-4.1",
  "gpt-4o",
  "gpt-5.1"
)

thinking_levels <- c("no_thinking", "with_thinking")
directions      <- c("forward", "reverse")

model_matrix <- tidyr::expand_grid(
  model     = openai_models,
  thinking  = thinking_levels,
  direction = directions
) %>%
  # Only gpt-5.1 is allowed to have thinking = "with_thinking"
  dplyr::filter(model == "gpt-5.1" | thinking == "no_thinking")

print(model_matrix)

# ---------------------------------------------------------------------
# 4. Phase 1: Submit all batches (poll = FALSE)
# ---------------------------------------------------------------------

jobs <- list()

for (t_row in seq_len(nrow(templates_tbl))) {
  template_id  <- templates_tbl$template_id[t_row]
  tmpl_string  <- templates_tbl$prompt_template[[t_row]]

  for (i in seq_len(nrow(model_matrix))) {
    row <- model_matrix[i, ]

    model     <- row$model
    thinking  <- row$thinking
    direction <- row$direction

    pairs_use <- get_pairs_for_direction(direction)

    prefix <- paste("openai_ab", template_id, model, thinking, direction, sep = "_")
    prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)

    batch_input_path  <- file.path(out_dir, paste0(prefix, "_input.jsonl"))
    batch_output_path <- file.path(out_dir, paste0(prefix, "_output.jsonl"))
    csv_path          <- file.path(out_dir, paste0(prefix, ".csv"))

    is_gpt51    <- grepl("^gpt-5\\.1", model)
    is_thinking <- identical(thinking, "with_thinking") && is_gpt51

    endpoint <- if (is_gpt51 && is_thinking) {
      "responses"
    } else {
      "chat.completions"
    }

    include_thoughts <- is_thinking

    message(
      "Submitting batch (poll = FALSE): template=", template_id,
      " | ", model, " / ", thinking, " / ", direction,
      " | endpoint=", endpoint,
      " | include_thoughts=", include_thoughts
    )

    # Arguments to run_openai_batch_pipeline
    args <- list(
      pairs             = pairs_use,
      model             = model,
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl_string,
      endpoint          = endpoint,
      batch_input_path  = batch_input_path,
      batch_output_path = batch_output_path,
      poll              = FALSE,
      include_thoughts  = include_thoughts,
      include_raw       = TRUE
    )

    # Non-thinking: enforce temperature = 0
    if (!is_thinking) {
      args$temperature <- 0
    }

    # gpt-5.1 with thinking: reasoning = "low", no explicit temperature
    if (is_thinking) {
      args$reasoning <- "low"
    }

    pipeline <- do.call(run_openai_batch_pipeline, args)

    jobs[[length(jobs) + 1L]] <- list(
      template_id       = template_id,
      model             = model,
      thinking          = thinking,
      direction         = direction,
      prefix            = prefix,
      batch_id          = pipeline$batch$id,
      batch_input_path  = pipeline$batch_input_path,
      batch_output_path = batch_output_path,
      csv_path          = csv_path,
      endpoint          = endpoint,
      include_thoughts  = include_thoughts,
      done              = FALSE,
      results           = NULL
    )
  }
}

jobs_tbl <- tibble::tibble(
  idx         = seq_along(jobs),
  template_id = vapply(jobs, `[[`, character(1), "template_id"),
  model       = vapply(jobs, `[[`, character(1), "model"),
  thinking    = vapply(jobs, `[[`, character(1), "thinking"),
  direction   = vapply(jobs, `[[`, character(1), "direction"),
  prefix      = vapply(jobs, `[[`, character(1), "prefix")
)

print(jobs_tbl)

# ---------------------------------------------------------------------
# 5. Phase 2: Poll all batches every 60 seconds, download + parse
# ---------------------------------------------------------------------

interval_seconds <- 60

is_terminal_openai <- function(status) {
  status %in% c("completed", "failed", "cancelled", "expired")
}

unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

while (length(unfinished) > 0L) {
  message("Polling ", length(unfinished), " unfinished OpenAI batch(es)...")

  for (j in unfinished) {
    job <- jobs[[j]]
    if (job$done) next

    batch  <- openai_get_batch(job$batch_id)
    status <- batch$status %||% "unknown"

    message("  [OpenAI] ", job$prefix, " status: ", status)

    if (is_terminal_openai(status)) {
      if (identical(status, "completed")) {
        openai_download_batch_output(
          batch_id = job$batch_id,
          path     = job$batch_output_path
        )
        res <- parse_openai_batch_output(job$batch_output_path)

        jobs[[j]]$results <- res
        readr::write_csv(res, job$csv_path)
        message("    -> Results written to: ", job$csv_path)

        # Quick sanity check: show a few rows with content preview
        n_show <- min(3L, nrow(res))
        if (n_show > 0L) {
          sample_rows <- res %>%
            slice_head(n = n_show) %>%
            mutate(
              has_tag         = str_detect(content, "<BETTER_SAMPLE>"),
              content_preview = str_trunc(content, 160)
            ) %>%
            select(
              custom_id,
              model,
              better_sample,
              better_id,
              has_tag,
              content_preview
            )

          print(sample_rows)
        } else {
          message("    (No rows in parsed results for ", job$prefix, ")")
        }
      }

      jobs[[j]]$done <- TRUE
    }
  }

  unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

  if (length(unfinished) > 0L) {
    message("Sleeping ", interval_seconds, " seconds before next poll...")
    Sys.sleep(interval_seconds)
  }
}

message("All OpenAI batches have reached a terminal state.")

# ---------------------------------------------------------------------
# 6. Build consistency / bias summary (per template, model, thinking)
# ---------------------------------------------------------------------

# Build lookup of results by (template_id, model, thinking, direction)
results_by_key <- list()
for (j in seq_along(jobs)) {
  key <- paste(
    jobs[[j]]$template_id,
    jobs[[j]]$model,
    jobs[[j]]$thinking,
    jobs[[j]]$direction,
    sep = "|"
  )
  results_by_key[[key]] <- jobs[[j]]$results
}

summarize_template_model <- function(template_id, model, thinking) {
  key_forward <- paste(template_id, model, thinking, "forward", sep = "|")
  key_reverse <- paste(template_id, model, thinking, "reverse", sep = "|")

  if (!key_forward %in% names(results_by_key) ||
      !key_reverse %in% names(results_by_key)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  res_forward <- results_by_key[[key_forward]]
  res_reverse <- results_by_key[[key_reverse]]

  if (is.null(res_forward) || is.null(res_reverse) ||
      !nrow(res_forward) || !nrow(res_reverse)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  # Reverse consistency
  cons <- tryCatch(
    compute_reverse_consistency(res_forward, res_reverse),
    error = function(e) {
      message("compute_reverse_consistency error for ",
              "template ", template_id, " / ", model, " / ", thinking, ": ",
              conditionMessage(e))
      NULL
    }
  )

  prop_consistent   <- NA_real_
  p_sample1_overall <- NA_real_

  if (!is.null(cons)) {
    # Extract prop_consistent from cons$summary
    if (is.list(cons) && "summary" %in% names(cons)) {
      s <- cons$summary
      if (is.data.frame(s) && "prop_consistent" %in% names(s)) {
        prop_consistent <- s$prop_consistent[1]
      }
    }

    # Positional bias: pass the full consistency object to check_positional_bias()
    bias <- tryCatch(
      check_positional_bias(cons),
      error = function(e) {
        message("check_positional_bias error for ",
                "template ", template_id, " / ", model, " / ", thinking, ": ",
                conditionMessage(e))
        NULL
      }
    )

    if (!is.null(bias) && is.list(bias) && "summary" %in% names(bias)) {
      bs <- bias$summary
      if (is.data.frame(bs) && "p_sample1_overall" %in% names(bs)) {
        p_sample1_overall <- bs$p_sample1_overall[1]
      }
    }
  }

  tibble::tibble(
    prop_consistent   = prop_consistent,
    p_sample1_overall = p_sample1_overall
  )
}

# Unique template/model/thinking combos
combos <- expand_grid(
  template_id = templates_tbl$template_id,
  model       = unique(model_matrix$model),
  thinking    = c("no_thinking", "with_thinking")
) %>%
  # Respect the actual model_matrix constraints:
  semi_join(
    model_matrix %>%
      distinct(model, thinking),
    by = c("model", "thinking")
  )

summary_tbl <- combos %>%
  rowwise() %>%
  mutate(
    summary = list(
      summarize_template_model(template_id, model, thinking)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(summary)

# ---------------------------------------------------------------------
# 7. Add thinking_config + temperature and save summary
# ---------------------------------------------------------------------

summary_tbl <- summary_tbl %>%
  mutate(
    thinking_config = case_when(
      thinking == "no_thinking" ~
        "OpenAI chat.completions (no reasoning, temp=0)",
      thinking == "with_thinking" & grepl("^gpt-5\\.1", model) ~
        "OpenAI responses (include_thoughts=TRUE, reasoning=low; no temperature param)",
      TRUE ~ NA_character_
    ),
    temperature = case_when(
      thinking == "no_thinking" ~ 0,
      thinking == "with_thinking" & grepl("^gpt-5\\.1", model) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    template_id,
    model,
    thinking,
    prop_consistent,
    p_sample1_overall,
    thinking_config,
    temperature
  )

summary_path <- file.path(out_dir, "openai_template_ab_summary.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Summary written to: ", summary_path)
