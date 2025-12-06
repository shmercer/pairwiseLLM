# dev/dev-ollama-template-positional-bias.R
#
# Test positional bias for five prompt templates ("test1"–"test5")
# on local Ollama models using the new template system.
#
# Models:
#   - mistral-small3.2:24b
#   - qwen3:32b         (think = FALSE and think = TRUE)
#   - gemma3:27b
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
#       dev-output/ollama-template-positional-bias/
#         ollama_<template>_<model>_<thinking>_<direction>.csv
#   - Summary CSV:
#       dev-output/ollama-template-positional-bias/
#         ollama_template_positional_bias_summary.csv
#
# NOTE:
#   - Requires a running Ollama server and the models to be pulled:
#       ollama pull mistral-small3.2:24b
#       ollama pull qwen3:32b
#       ollama pull gemma3:27b
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

out_dir <- "dev-output/ollama-template-positional-bias"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Adjust if Ollama is on a different host/port
options(pairwiseLLM.ollama_host = "http://127.0.0.1:11434")

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
# 3. Model grid (Ollama only)
# ------------------------------------------------------------------------------

ollama_models <- c(
  "mistral-small3.2:24b",
  "qwen3:32b",
  "gemma3:27b"
)

thinking_levels <- c("no_thinking", "with_thinking")

model_matrix_raw <- tidyr::expand_grid(
  model    = ollama_models,
  thinking = thinking_levels
)

# For Qwen: both thinking configs; for others: only no_thinking
is_qwen_model <- function(m) grepl("^qwen", m, ignore.case = TRUE)

model_matrix <- model_matrix_raw |>
  rowwise() |>
  filter(
    is_qwen_model(model) |
      (!is_qwen_model(model) & identical(thinking, "no_thinking"))
  ) |>
  ungroup()

print(model_matrix)

# ------------------------------------------------------------------------------
# 4. Run live comparisons for all templates / models / thinking / directions
# ------------------------------------------------------------------------------

results_by_key <- list()
summary_rows <- list()

for (t_row in seq_len(nrow(templates_tbl))) {
  template_id <- templates_tbl$template_id[t_row]
  tmpl_string <- templates_tbl$prompt_template[[t_row]]

  for (i in seq_len(nrow(model_matrix))) {
    row <- model_matrix[i, ]

    model <- row$model
    thinking <- row$thinking

    is_qwen <- is_qwen_model(model)
    is_thinking <- identical(thinking, "with_thinking")

    # think flag for submit_llm_pairs:
    # - Qwen + "with_thinking" -> TRUE
    # - otherwise -> FALSE
    think_flag <- is_qwen && is_thinking

    message(
      "\n====================================================\n",
      "Template=", template_id,
      " | Model=", model,
      " | Thinking=", thinking,
      "\n===================================================="
    )

    # Run forward and reverse once each
    res_dir <- list()

    for (direction in directions) {
      message("Direction: ", direction)

      pairs_use <- get_pairs_for_direction(direction)

      prefix <- paste("ollama", template_id, model, thinking, direction,
        sep = "_"
      )
      prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)

      csv_path <- file.path(out_dir, paste0(prefix, ".csv"))

      res <- submit_llm_pairs(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl_string,
        backend           = "ollama",
        verbose           = TRUE,
        status_every      = 10,
        progress          = TRUE,
        include_raw       = FALSE,
        think             = think_flag,
        num_ctx           = 8192
      )

      readr::write_csv(res, csv_path)
      message("  -> Results written to: ", csv_path)

      res_dir[[direction]] <- res

      # Store by key for potential further inspection
      key <- paste(template_id, model, thinking, direction, sep = "|")
      results_by_key[[key]] <- res
    }

    # --------------------------------------------------------------------------
    # 5. Compute reverse consistency + positional bias for this combo
    # --------------------------------------------------------------------------

    res_forward <- res_dir[["forward"]]
    res_reverse <- res_dir[["reverse"]]

    if (!is.null(res_forward) && !is.null(res_reverse) &&
      nrow(res_forward) > 0L && nrow(res_reverse) > 0L) {
      cons <- tryCatch(
        compute_reverse_consistency(res_forward, res_reverse),
        error = function(e) {
          message(
            "compute_reverse_consistency error for ",
            "template=", template_id,
            " / model=", model,
            " / thinking=", thinking, ": ",
            conditionMessage(e)
          )
          NULL
        }
      )

      prop_consistent <- NA_real_
      prop_pos1 <- NA_real_
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
              " / model=", model,
              " / thinking=", thinking, ": ",
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
              total_pos1_wins <- bs$total_pos1_wins[1]
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
        template_id        = template_id,
        model              = model,
        thinking           = thinking,
        prop_consistent    = prop_consistent,
        prop_pos1          = prop_pos1,
        p_sample1_overall  = p_sample1_overall
      )
    } else {
      summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
        template_id        = template_id,
        model              = model,
        thinking           = thinking,
        prop_consistent    = NA_real_,
        prop_pos1          = NA_real_,
        p_sample1_overall  = NA_real_
      )
    }
  }
}

# ------------------------------------------------------------------------------
# 6. Add thinking_config + temperature and save summary
# ------------------------------------------------------------------------------

if (length(summary_rows) > 0L) {
  summary_tbl <- bind_rows(summary_rows) |>
    mutate(
      is_qwen = is_qwen_model(model),
      temperature = case_when(
        is_qwen & thinking == "with_thinking" ~ 0.6,
        TRUE ~ 0
      ),
      thinking_config = case_when(
        is_qwen & thinking == "with_thinking" ~
          "Ollama (Qwen, think=TRUE, temperature=0.6, num_ctx=8192)",
        is_qwen & thinking == "no_thinking" ~
          "Ollama (Qwen, think=FALSE, temperature=0, num_ctx=8192)",
        !is_qwen ~
          "Ollama (local model, temperature=0, num_ctx=8192)",
        TRUE ~ NA_character_
      )
    ) |>
    select(
      template_id,
      model,
      thinking,
      prop_consistent,
      prop_pos1,
      p_sample1_overall,
      thinking_config,
      temperature
    )

  summary_path <- file.path(
    out_dir,
    "ollama_template_positional_bias_summary.csv"
  )

  readr::write_csv(summary_tbl, summary_path)

  print(summary_tbl)
  message("Ollama template positional bias summary written to: ", summary_path)
} else {
  message("No summary rows were generated; check for earlier errors.")
}
