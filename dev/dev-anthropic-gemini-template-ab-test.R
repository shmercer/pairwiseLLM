# dev/dev-anthropic-gemini-template-ab-test.R
#
# A/B test five prompt templates ("test1"–"test5") on Anthropic and Gemini
# models via batch APIs using the new template system.
#
# - Providers / models:
#     * Anthropic:
#         - claude-sonnet-4-5
#         - claude-haiku-4-5
#         - claude-opus-4-5
#       * Thinking:
#         - "no_thinking": reasoning = "none", temp=0
#         - "with_thinking": reasoning = "enabled", include_thoughts=TRUE
#     * Gemini:
#         - gemini-3-pro-preview
#       * Thinking:
#         - "with_thinking" only (thinkingLevel="low", includeThoughts=TRUE)
#
# - Directions:
#     * forward (alternate_pair_order)
#     * reverse (sample_reverse_pairs, 100% reversed)
#
# - Templates:
#     * test1, test2, test3, test4, test5
#       (resolved via get_prompt_template("<id>"))
#
# Outputs:
# - Per-run CSVs:
#     dev-output/anthropic-gemini-template-ab-test/<provider>_<template>_<model>_<thinking>_<direction>.csv
# - Summary CSV:
#     dev-output/anthropic-gemini-template-ab-test/anthropic_gemini_template_ab_summary.csv
#
# Summary columns (per template_id / provider / model / thinking):
#   - prop_consistent
#   - prop_pos1
#   - p_sample1_overall
#   - thinking_config
#   - temperature

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

out_dir <- "dev-output/anthropic-gemini-template-ab-test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

# Trait description
td <- trait_description("overall_quality")

# ------------------------------------------------------------------------------
# 1. Five candidate templates from the new template system
# ------------------------------------------------------------------------------

template_ids <- paste0("test", 1:5)

templates_tbl <- tibble::tibble(
  template_id     = template_ids,
  prompt_template = lapply(template_ids, get_prompt_template)
)

print(templates_tbl)

# ------------------------------------------------------------------------------
# 2. Build forward + reverse pairs (ALL pairs)
# ------------------------------------------------------------------------------

pairs_all <- example_writing_samples %>%
  make_pairs()

# Forward: deterministic alternating SAMPLE_1 vs SAMPLE_2 order
pairs_forward <- pairs_all %>%
  alternate_pair_order()

# Reverse sequence: flip SAMPLE_1 / SAMPLE_2 for all pairs
pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1.0,
  seed        = 2002
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

# ------------------------------------------------------------------------------
# 3. Model grid (Anthropic + Gemini only)
# ------------------------------------------------------------------------------

anthropic_models <- c(
  "claude-sonnet-4-5",
  "claude-haiku-4-5",
  "claude-opus-4-5"
)

gemini_models <- c(
  "gemini-3-pro-preview"
)

thinking_levels <- c("no_thinking", "with_thinking")
directions      <- c("forward", "reverse")

anthropic_grid <- tidyr::expand_grid(
  provider  = "anthropic",
  model     = anthropic_models,
  thinking  = thinking_levels,
  direction = directions
)

# Gemini: only WITH thinking
gemini_grid <- tidyr::expand_grid(
  provider  = "gemini",
  model     = gemini_models,
  thinking  = "with_thinking",
  direction = directions
)

model_matrix <- bind_rows(
  anthropic_grid,
  gemini_grid
)

print(model_matrix)

# ------------------------------------------------------------------------------
# 4. Phase 1: Submit all batches (poll = FALSE)
# ------------------------------------------------------------------------------

jobs <- list()

for (t_row in seq_len(nrow(templates_tbl))) {
  template_id <- templates_tbl$template_id[t_row]
  tmpl_string <- templates_tbl$prompt_template[[t_row]]

  for (i in seq_len(nrow(model_matrix))) {
    row <- model_matrix[i, ]

    provider  <- row$provider
    model     <- row$model
    thinking  <- row$thinking
    direction <- row$direction

    message(
      "Submitting batch: template=", template_id,
      " | ", provider, " / ", model,
      " / ", thinking, " / ", direction
    )

    pairs_use <- get_pairs_for_direction(direction)

    prefix <- paste(provider, template_id, model, thinking, direction, sep = "_")
    prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)

    batch_input_path  <- file.path(out_dir, paste0(prefix, "_input.jsonl"))
    batch_output_path <- file.path(out_dir, paste0(prefix, "_output.jsonl"))
    csv_path          <- file.path(out_dir, paste0(prefix, ".csv"))

    is_thinking <- identical(thinking, "with_thinking")
    temperature_arg <- if (!is_thinking) 0 else NULL

    if (identical(provider, "anthropic")) {

      # Anthropic:
      # - reasoning = "none" for non-thinking (temp=0)
      # - reasoning = "enabled" for thinking (temp default, include_thoughts=TRUE)
      reasoning <- if (is_thinking) "enabled" else "none"

      if (!is_thinking) {
        pipeline <- run_anthropic_batch_pipeline(
          pairs             = pairs_use,
          model             = model,
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl_string,
          reasoning         = reasoning,
          include_thoughts  = FALSE,
          batch_input_path  = batch_input_path,
          batch_output_path = batch_output_path,
          poll              = FALSE,
          temperature       = temperature_arg,
          include_raw       = TRUE
        )
      } else {
        pipeline <- run_anthropic_batch_pipeline(
          pairs             = pairs_use,
          model             = model,
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl_string,
          reasoning         = reasoning,
          include_thoughts  = TRUE,
          batch_input_path  = batch_input_path,
          batch_output_path = batch_output_path,
          poll              = FALSE,
          include_raw       = TRUE
        )
      }

      jobs[[length(jobs) + 1L]] <- list(
        template_id       = template_id,
        provider          = provider,
        model             = model,
        thinking          = thinking,
        direction         = direction,
        prefix            = prefix,
        batch_type        = "anthropic",
        batch_id          = pipeline$batch$id,
        batch_input_path  = pipeline$batch_input_path,
        batch_output_path = batch_output_path,
        csv_path          = csv_path,
        done              = FALSE,
        results           = NULL
      )

    } else if (identical(provider, "gemini")) {

      # Gemini: only with "thinking"
      # - thinkingLevel = "low"
      # - includeThoughts = TRUE
      # - temperature left as provider default (to avoid API issues)
      req_tbl <- build_gemini_batch_requests(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl_string,
        thinking_level    = "low",
        include_thoughts  = TRUE
      )

      batch <- gemini_create_batch(
        requests    = req_tbl$request,
        model       = model,
        api_key     = Sys.getenv("GEMINI_API_KEY"),
        api_version = "v1beta"
      )

      batch_name <- batch$name %||% stop(
        "Gemini batch did not return a `name` field.",
        call. = FALSE
      )

      jobs[[length(jobs) + 1L]] <- list(
        template_id       = template_id,
        provider          = provider,
        model             = model,
        thinking          = thinking,
        direction         = direction,
        prefix            = prefix,
        batch_type        = "gemini",
        batch_name        = batch_name,
        requests_tbl      = req_tbl,
        batch_input_path  = NA_character_,  # not used for Gemini
        batch_output_path = batch_output_path,
        csv_path          = csv_path,
        done              = FALSE,
        results           = NULL
      )

    } else {
      stop("Unknown provider in model_matrix: ", provider, call. = FALSE)
    }
  }
}

jobs_tbl <- tibble::tibble(
  idx         = seq_along(jobs),
  template_id = vapply(jobs, `[[`, character(1), "template_id"),
  provider    = vapply(jobs, `[[`, character(1), "provider"),
  model       = vapply(jobs, `[[`, character(1), "model"),
  thinking    = vapply(jobs, `[[`, character(1), "thinking"),
  direction   = vapply(jobs, `[[`, character(1), "direction"),
  prefix      = vapply(jobs, `[[`, character(1), "prefix")
)

print(jobs_tbl)

# ------------------------------------------------------------------------------
# 5. Phase 2: Poll all batches periodically, download + parse
# ------------------------------------------------------------------------------

interval_seconds   <- 90      # time between full polling rounds
per_request_delay  <- 1    # short delay between individual requests (sec)

is_terminal_anthropic <- function(status) {
  status %in% c("processed", "cancelled", "expired", "failed")
}

is_terminal_gemini <- function(state) {
  state %in% c("SUCCEEDED", "FAILED", "CANCELLED", "EXPIRED")
}

unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

while (length(unfinished) > 0L) {
  message("Polling ", length(unfinished),
          " unfinished batch(es) across Anthropic/Gemini...")

  for (j in unfinished) {
    job <- jobs[[j]]
    if (job$done) {
      Sys.sleep(per_request_delay)
      next
    }

    if (identical(job$batch_type, "anthropic")) {
      batch  <- anthropic_get_batch(job$batch_id)
      status <- batch$status %||% "unknown"

      message("  [Anthropic] ", job$prefix, " status: ", status)

      if (is_terminal_anthropic(status)) {
        if (identical(status, "processed")) {
          anthropic_download_batch_results(
            batch_id = job$batch_id,
            path     = job$batch_output_path
          )

          res <- parse_anthropic_batch_output(job$batch_output_path)

          jobs[[j]]$results <- res
          readr::write_csv(res, job$csv_path)
          message("    -> Results written to: ", job$csv_path)
        }
        jobs[[j]]$done <- TRUE
      }

    } else if (identical(job$batch_type, "gemini")) {
      batch  <- gemini_get_batch(job$batch_name)
      state  <- batch$state %||% "STATE_UNSPECIFIED"

      message("  [Gemini] ", job$prefix, " state: ", state)

      if (is_terminal_gemini(state)) {
        if (identical(state, "SUCCEEDED")) {
          gemini_download_batch_results(
            name = job$batch_name,
            path = job$batch_output_path
          )

          res <- parse_gemini_batch_output(
            results_path = job$batch_output_path,
            requests_tbl = job$requests_tbl
          )

          jobs[[j]]$results <- res
          readr::write_csv(res, job$csv_path)
          message("    -> Results written to: ", job$csv_path)
        }
        jobs[[j]]$done <- TRUE
      }

    } else {
      warning("Unknown batch_type for job index ", j, call. = FALSE)
    }

    # Short delay between individual provider requests to avoid 429s
    Sys.sleep(per_request_delay)
  }

  unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

  if (length(unfinished) > 0L) {
    message("Sleeping ", interval_seconds, " seconds before next poll...")
    Sys.sleep(interval_seconds)
  }
}

message("All Anthropic/Gemini batches have reached a terminal state.")

# ------------------------------------------------------------------------------
# 6. Build consistency / bias summary (per template, provider, model, thinking)
# ------------------------------------------------------------------------------

# Map (template_id, provider, model, thinking, direction) -> results tibble
results_by_key <- list()
for (j in seq_along(jobs)) {
  key <- paste(
    jobs[[j]]$template_id,
    jobs[[j]]$provider,
    jobs[[j]]$model,
    jobs[[j]]$thinking,
    jobs[[j]]$direction,
    sep = "|"
  )
  results_by_key[[key]] <- jobs[[j]]$results
}

summarize_template_model <- function(template_id, provider, model, thinking) {
  key_forward <- paste(template_id, provider, model, thinking, "forward", sep = "|")
  key_reverse <- paste(template_id, provider, model, thinking, "reverse", sep = "|")

  if (!key_forward %in% names(results_by_key) ||
      !key_reverse %in% names(results_by_key)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      prop_sample1_overall = NA_real_, # placeholder, we'll rename below
      prop_pos1         = NA_real_
    ))
  }

  res_forward <- results_by_key[[key_forward]]
  res_reverse <- results_by_key[[key_reverse]]

  if (is.null(res_forward) || is.null(res_reverse) ||
      !nrow(res_forward) || !nrow(res_reverse)) {
    return(tibble::tibble(
      prop_consistent      = NA_real_,
      prop_sample1_overall = NA_real_,
      prop_pos1            = NA_real_
    ))
  }

  # Reverse consistency
  cons <- tryCatch(
    compute_reverse_consistency(res_forward, res_reverse),
    error = function(e) {
      message("compute_reverse_consistency error for ",
              "template=", template_id,
              " / provider=", provider,
              " / model=", model,
              " / thinking=", thinking, ": ",
              conditionMessage(e))
      NULL
    }
  )

  prop_consistent   <- NA_real_
  p_sample1_overall <- NA_real_
  prop_pos1         <- NA_real_

  if (!is.null(cons)) {
    # Extract prop_consistent from cons$summary
    if (is.list(cons) && "summary" %in% names(cons)) {
      s <- cons$summary
      if (is.data.frame(s) && "prop_consistent" %in% names(s)) {
        prop_consistent <- s$prop_consistent[1]
      }
    }

    # Positional bias on the consistency object (as in OpenAI template AB rebuild)
    bias <- tryCatch(
      check_positional_bias(cons),
      error = function(e) {
        message("check_positional_bias error for ",
                "template=", template_id,
                " / provider=", provider,
                " / model=", model,
                " / thinking=", thinking, ": ",
                conditionMessage(e))
        NULL
      }
    )

    if (!is.null(bias) && is.list(bias) && "summary" %in% names(bias)) {
      bs <- bias$summary
      if (is.data.frame(bs)) {
        # p_sample1_overall
        if ("p_sample1_overall" %in% names(bs)) {
          p_sample1_overall <- bs$p_sample1_overall[1]
        }

        # prop_pos1: use directly if present, otherwise compute from totals
        if ("prop_pos1" %in% names(bs)) {
          prop_pos1 <- bs$prop_pos1[1]
        } else if (all(c("total_pos1_wins", "total_comparisons") %in% names(bs))) {
          total_pos1_wins   <- bs$total_pos1_wins[1]
          total_comparisons <- bs$total_comparisons[1]
          if (is.finite(total_pos1_wins) &&
              is.finite(total_comparisons) &&
              total_comparisons > 0) {
            prop_pos1 <- total_pos1_wins / total_comparisons
          }
        }
      }
    }
  }

  tibble::tibble(
    prop_consistent   = prop_consistent,
    prop_pos1         = prop_pos1,
    p_sample1_overall = p_sample1_overall
  )
}

# Unique (template_id, provider, model, thinking) combos present in jobs
combos <- tibble::tibble(
  template_id = vapply(jobs, `[[`, character(1), "template_id"),
  provider    = vapply(jobs, `[[`, character(1), "provider"),
  model       = vapply(jobs, `[[`, character(1), "model"),
  thinking    = vapply(jobs, `[[`, character(1), "thinking")
) %>%
  distinct()

summary_tbl <- combos %>%
  rowwise() %>%
  mutate(
    summary = list(
      summarize_template_model(template_id, provider, model, thinking)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(summary)

# ------------------------------------------------------------------------------
# 7. Add thinking_config + temperature and save summary
# ------------------------------------------------------------------------------

summary_tbl <- summary_tbl %>%
  mutate(
    thinking_config = case_when(
      provider == "anthropic" & thinking == "no_thinking" ~
        "Anthropic Messages (reasoning=none, temp=0, max_tokens=768)",
      provider == "anthropic" & thinking == "with_thinking" ~
        "Anthropic Messages (reasoning=enabled, temp=1, max_tokens=2048, thinking_budget=1024)",
      provider == "gemini" ~
        "Gemini batchGenerateContent (thinkingLevel=Low, includeThoughts=TRUE; provider default temperature)",
      TRUE ~ NA_character_
    ),
    temperature = case_when(
      thinking == "no_thinking" ~ 0,
      provider == "anthropic" & thinking == "with_thinking" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    template_id,
    provider,
    model,
    thinking,
    prop_consistent,
    prop_pos1,
    p_sample1_overall,
    thinking_config,
    temperature
  )

summary_path <- file.path(out_dir, "anthropic_gemini_template_ab_summary.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Anthropic/Gemini template A/B summary written to: ", summary_path)
