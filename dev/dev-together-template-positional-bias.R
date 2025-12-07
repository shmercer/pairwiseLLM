# dev/dev-together-template-positional-bias.R
#
# Test positional bias for five prompt templates ("test1"–"test5")
# on Together.ai models using the new template system.
#
# Models (Together.ai):
#   - deepseek-ai/DeepSeek-R1       (default temp = 0.6)
#   - deepseek-ai/DeepSeek-V3      (default temp = 0)
#   - moonshotai/Kimi-K2-Instruct-0905 (default temp = 0)
#   - Qwen/Qwen3-235B-A22B-Instruct-2507-tput (default temp = 0)
#
# Directions:
#   - forward (alternate_pair_order)
#   - reverse (sample_reverse_pairs, 100% reversed)
#
# Templates:
#   - test1, test2, test3, test4, test5
#     (resolved via get_prompt_template("<id>"))
#
# Outputs:
#   - Per-run CSVs:
#       dev-output/together-template-positional-bias/
#         together_<template>_<model>_<direction>.csv
#   - Summary CSV:
#       dev-output/together-template-positional-bias/
#         together_template_positional_bias_summary.csv
#
# NOTE:
#   - Requires a valid TOGETHER_API_KEY in your environment.
#   - This is a DEV script; not part of testthat.

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

out_dir <- "dev-output/together-template-positional-bias"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Basic check for Together key (lightweight, dev-only)
together_key <- Sys.getenv("TOGETHER_API_KEY", unset = "")
if (identical(together_key, "")) {
  stop(
    "TOGETHER_API_KEY not set. Please set it in your environment before ",
    "running this script.",
    call. = FALSE
  )
}

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

pairs_all <- example_writing_samples |>
  make_pairs()

# Forward: deterministic alternating SAMPLE_1 vs SAMPLE_2 order
pairs_forward <- pairs_all |>
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

directions <- c("forward", "reverse")

# ------------------------------------------------------------------------------
# 3. Together model grid
# ------------------------------------------------------------------------------

together_models <- c(
  "deepseek-ai/DeepSeek-R1",
  "deepseek-ai/DeepSeek-V3",
  "moonshotai/Kimi-K2-Instruct-0905",
  "Qwen/Qwen3-235B-A22B-Instruct-2507-tput"
)

model_matrix <- tibble::tibble(model = together_models)

print(model_matrix)

# Helper: Together default temperature by model
together_default_temp <- function(model) {
  if (identical(model, "deepseek-ai/DeepSeek-R1")) {
    0.6
  } else {
    0
  }
}

# ------------------------------------------------------------------------------
# 4. Run live comparisons for all templates / models / directions
# ------------------------------------------------------------------------------

results_by_key <- list()
summary_rows   <- list()

for (t_row in seq_len(nrow(templates_tbl))) {
  template_id <- templates_tbl$template_id[t_row]
  tmpl_string <- templates_tbl$prompt_template[[t_row]]

  for (i in seq_len(nrow(model_matrix))) {
    model <- model_matrix$model[i]

    message(
      "\n====================================================\n",
      "Template=", template_id,
      " | Model=", model,
      "\n===================================================="
    )

    res_dir <- list()

    for (direction in directions) {
      message("Direction: ", direction)

      pairs_use <- get_pairs_for_direction(direction)

      prefix <- paste("together", template_id, model, direction,
                      sep = "_"
      )
      prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)

      csv_path <- file.path(out_dir, paste0(prefix, ".csv"))

      # submit_together_pairs_live() applies model-specific temperature
      # defaults internally when `temperature` is omitted:
      #   - DeepSeek-R1: 0.6
      #   - All other models: 0
      res <- submit_together_pairs_live(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl_string,
        verbose           = TRUE,
        status_every      = 10,
        progress          = TRUE,
        include_raw       = FALSE
      )

      readr::write_csv(res, csv_path)
      message("  -> Results written to: ", csv_path)

      res_dir[[direction]] <- res

      # Store by key for potential further inspection
      key <- paste(template_id, model, direction, sep = "|")
      results_by_key[[key]] <- res
    }

    # --------------------------------------------------------------------------
    # 5. Compute reverse consistency + positional bias for this template/model
    # --------------------------------------------------------------------------

    res_forward <- res_dir[["forward"]]
    res_reverse <- res_dir[["reverse"]]

    if (!is.null(res_forward) &&
        !is.null(res_reverse) &&
        nrow(res_forward) > 0L &&
        nrow(res_reverse) > 0L) {

      cons <- tryCatch(
        compute_reverse_consistency(res_forward, res_reverse),
        error = function(e) {
          message(
            "compute_reverse_consistency error for ",
            "template=", template_id,
            " / model=", model, ": ",
            conditionMessage(e)
          )
          NULL
        }
      )

      prop_consistent   <- NA_real_
      prop_pos1         <- NA_real_
      p_sample1_overall <- NA_real_

      if (!is.null(cons)) {
        # Extract prop_consistent from cons$summary, if present
        if (is.list(cons) && "summary" %in% names(cons)) {
          s <- cons$summary
          if (is.data.frame(s) && "prop_consistent" %in% names(s)) {
            prop_consistent <- s$prop_consistent[1]
          }
        }

        bias <- tryCatch(
          check_positional_bias(cons),
          error = function(e) {
            message(
              "check_positional_bias error for ",
              "template=", template_id,
              " / model=", model, ": ",
              conditionMessage(e)
            )
            NULL
          }
        )

        if (!is.null(bias) && is.list(bias) && "summary" %in% names(bias)) {
          bs <- bias$summary
          if (is.data.frame(bs)) {
            # prop_pos1: directly if present; else compute from totals
            if ("prop_pos1" %in% names(bs)) {
              prop_pos1 <- bs$prop_pos1[1]
            } else if (all(c("total_pos1_wins", "total_comparisons")
                           %in% names(bs))) {
              total_pos1_wins  <- bs$total_pos1_wins[1]
              total_comparisons <- bs$total_comparisons[1]
              if (is.finite(total_pos1_wins) &&
                  is.finite(total_comparisons) &&
                  total_comparisons > 0) {
                prop_pos1 <- total_pos1_wins / total_comparisons
              }
            }

            if ("p_sample1_overall" %in% names(bs)) {
              p_sample1_overall <- bs$p_sample1_overall[1]
            }
          }
        }
      }

      summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
        template_id       = template_id,
        model             = model,
        prop_consistent   = prop_consistent,
        prop_pos1         = prop_pos1,
        p_sample1_overall = p_sample1_overall
      )
    } else {
      summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
        template_id       = template_id,
        model             = model,
        prop_consistent   = NA_real_,
        prop_pos1         = NA_real_,
        p_sample1_overall = NA_real_
      )
    }
  }
}

# ------------------------------------------------------------------------------
# 6. Add temperature / config string and save summary
# ------------------------------------------------------------------------------

if (length(summary_rows) > 0L) {
  summary_tbl <- bind_rows(summary_rows) |>
    mutate(
      temperature = vapply(
        model,
        together_default_temp,
        numeric(1)
      ),
      thinking_config = dplyr::case_when(
        model == "deepseek-ai/DeepSeek-R1" ~
          "Together (DeepSeek-R1, default temperature=0.6, thoughts parsed into `thoughts` column)",
        TRUE ~
          "Together (chat.completions, default temperature=0)"
      )
    ) |>
    select(
      template_id,
      model,
      prop_consistent,
      prop_pos1,
      p_sample1_overall,
      thinking_config,
      temperature
    )

  summary_path <- file.path(
    out_dir,
    "together_template_positional_bias_summary.csv"
  )

  readr::write_csv(summary_tbl, summary_path)

  print(summary_tbl)
  message(
    "Together template positional bias summary written to: ",
    summary_path
  )
} else {
  message("No summary rows were generated; check for earlier errors.")
}
