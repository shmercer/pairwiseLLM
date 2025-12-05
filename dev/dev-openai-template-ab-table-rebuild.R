# dev/dev-openai-template-ab-table-rebuild.R
#
# Rebuild positional-bias summary table for the OpenAI template A/B experiment
# from existing per-run CSV files.
#
# For each (template_id, model, thinking), this script:
#   - loads forward + reverse CSVs
#   - computes reverse consistency via compute_reverse_consistency()
#   - computes positional bias via check_positional_bias()
#   - extracts:
#       * prop_consistent
#       * p_sample1_overall
#       * prop_pos1 (from check_positional_bias$summary, or via
#         total_pos1_wins / total_comparisons)
#
# Output:
#   dev-output/openai-template-ab-test-3/openai_template_ab_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# Adjust this if you used a different dir
out_dir <- "dev-output/openai-template-ab-test-6"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Discover CSV files and parse metadata from filenames
# ------------------------------------------------------------------------------

# Expected filename pattern:
#   openai_ab_<TID>_<MODEL>_<THINKING>_<DIRECTION>.csv
# Example:
#   openai_ab_T1_gpt-4.1_no_thinking_forward.csv
#
# where THINKING is either "no_thinking" or "with_thinking"

all_csv <- list.files(out_dir, pattern = "^openai_ab_.*\\.csv$", full.names = TRUE)

if (!length(all_csv)) {
  stop("No CSV files found in ", out_dir,
       ". Did you already run dev-openai-template-ab-test.R?", call. = FALSE)
}

# Regex with capture groups:
#  1: template_id (e.g., T1, T2)
#  2: model (e.g., gpt-4.1, gpt-4o, gpt-5.1)
#  3: thinking ("no_thinking" or "with_thinking")
#  4: direction ("forward" or "reverse")
re <- "^openai_ab_([^_]+)_([^_]+(?:-[^_]+)*)_(no_thinking|with_thinking)_(forward|reverse)\\.csv$"

files_tbl <- tibble::tibble(path = all_csv) %>%
  mutate(
    filename = basename(path),
    matches  = str_match(filename, re)
  ) %>%
  filter(!is.na(matches[, 1])) %>%
  transmute(
    path,
    template_id = matches[, 2],
    model       = matches[, 3],
    thinking    = matches[, 4],
    direction   = matches[, 5]
  )

if (!nrow(files_tbl)) {
  stop("No CSV filenames matched the expected pattern in ", out_dir, call. = FALSE)
}

print(files_tbl)

# ------------------------------------------------------------------------------
# 2. Helper: load CSV for a given (template_id, model, thinking, direction)
# ------------------------------------------------------------------------------

load_results_csv <- function(template_id, model, thinking, direction) {
  rows <- files_tbl %>%
    filter(
      template_id == !!template_id,
      model       == !!model,
      thinking    == !!thinking,
      direction   == !!direction
    )

  if (!nrow(rows)) {
    message("Missing CSV for: template=", template_id,
            " / model=", model,
            " / thinking=", thinking,
            " / direction=", direction)
    return(NULL)
  }

  # If multiple, just use the first (should not normally happen)
  readr::read_csv(rows$path[1], show_col_types = FALSE)
}

# ------------------------------------------------------------------------------
# 3. Summarise per (template_id, model, thinking)
# ------------------------------------------------------------------------------

summarize_template_model <- function(template_id, model, thinking) {
  res_forward <- load_results_csv(template_id, model, thinking, "forward")
  res_reverse <- load_results_csv(template_id, model, thinking, "reverse")

  if (is.null(res_forward) || is.null(res_reverse) ||
      !nrow(res_forward) || !nrow(res_reverse)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_,
      prop_pos1         = NA_real_
    ))
  }

  # ------------------------------
  # Reverse consistency
  # ------------------------------
  cons <- tryCatch(
    compute_reverse_consistency(res_forward, res_reverse),
    error = function(e) {
      message("compute_reverse_consistency error for ",
              "template=", template_id, " / model=", model,
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

    # Positional bias test on the consistency object
    bias <- tryCatch(
      check_positional_bias(cons),
      error = function(e) {
        message("check_positional_bias error for ",
                "template=", template_id, " / model=", model,
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
          if (is.finite(total_pos1_wins) && is.finite(total_comparisons) &&
              total_comparisons > 0) {
            prop_pos1 <- total_pos1_wins / total_comparisons
          }
        }
      }
    }
  }

  tibble::tibble(
    prop_consistent   = prop_consistent,
    p_sample1_overall = p_sample1_overall,
    prop_pos1         = prop_pos1
  )
}

# Unique template/model/thinking combos present in the CSVs
combos <- files_tbl %>%
  distinct(template_id, model, thinking)

summary_tbl <- combos %>%
  rowwise() %>%
  mutate(
    summary = list(
      summarize_template_model(template_id, model, thinking)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(summary)

# ------------------------------------------------------------------------------
# 4. Add thinking_config + temperature and save summary
# ------------------------------------------------------------------------------

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
    prop_pos1,
    thinking_config,
    temperature
  )

summary_path <- file.path(out_dir, "openai_template_ab_summary.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Rebuilt OpenAI template A/B summary written to: ", summary_path)
