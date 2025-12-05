# dev/dev-openai-template-ab-test-4.R
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
#     dev-output/openai-template-ab-test-3/openai_ab_<TID>_<model>_<thinking>_<direction>.csv
# - Summary CSV:
#     dev-output/openai-template-ab-test-3/openai_template_ab_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

# ---------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------

out_dir <- "dev-output/openai-template-ab-test-4"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

td <- trait_description("overall_quality")

# ---------------------------------------------------------------------
# 1. Two candidate templates (T1 and T2)
# ---------------------------------------------------------------------

template_T1 <- "
You are a precise writing evaluation engine. Your goal is to assign a quality score to two writing samples and select the one with the higher score.

TRAIT: {TRAIT_NAME}
DEFINITION: {TRAIT_DESCRIPTION}

INSTRUCTIONS:
1. Read the two samples below (labeled SAMPLE_1 and SAMPLE_2).
2. Mentally assign a score from 0 to 100 to SAMPLE_1 based *strictly* on the definition above.
3. Mentally assign a score from 0 to 100 to SAMPLE_2 based *strictly* on the definition above.
4. Compare the two scores.
5. Ties are not allowed. If the scores are equal, re-evaluate the specific evidence in both text to find the differentiator.

***
SAMPLE_1
***
{SAMPLE_1}

***
SAMPLE_2
***
{SAMPLE_2}

***

DECISION PHASE:
- Which sample received the higher mental score?
- Verify: Did the order of presentation influence the score? (If yes, swap the order mentally and re-score).

REQUIRED OUTPUT:
Output only the XML tag corresponding to the higher-scoring sample.

<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
OR
<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
"

template_T2 <- "
Task: Pairwise Comparison of Writing Samples
Trait: {TRAIT_NAME}

Definition of {TRAIT_NAME}:
{TRAIT_DESCRIPTION}

System Directive: You must ignore the order of the inputs. The labels 'Passage A' and 'Passage B' are arbitrary containers. Focus only on the internal quality of the text.

-------------------------------------------------
PASSAGE A (maps to output: SAMPLE_1)
-------------------------------------------------
{SAMPLE_1}

-------------------------------------------------
PASSAGE B (maps to output: SAMPLE_2)
-------------------------------------------------
{SAMPLE_2}

-------------------------------------------------

EVALUATION CHECKLIST:
1. Review the definition of {TRAIT_NAME} provided at the top.
2. Identify the strongest evidence for this trait in Passage A.
3. Identify the strongest evidence for this trait in Passage B.
4. Determine which passage provides stronger evidence.

OUTPUT MAPPING:
- If Passage A is better, output: <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
- If Passage B is better, output: <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>

Constraint: Output ONLY the XML tag. Do not include the score, reasoning, or the text of the sample.
"

templates_tbl <- tibble::tibble(
  template_id     = c("T1", "T2"),
  prompt_template = list(template_T1, template_T2)
)

# ---------------------------------------------------------------------
# 2. Build forward + reverse pairs (all pairs)
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
