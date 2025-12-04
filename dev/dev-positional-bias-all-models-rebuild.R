# dev/dev-positional-bias-all-models-rebuild.R
#
# Rebuild positional-bias summary table from existing per-run CSV files.
#
# This script:
# - Recreates the model grid used in dev-positional-bias-all-models.R
# - For each provider/model/thinking:
#     * loads forward and reverse CSVs if available,
#     * computes reverse consistency,
#     * runs check_positional_bias() on the details,
#     * extracts prop_consistent and p_sample1_overall.
# - Rewrites dev-output/positional-bias-all-models/positional_bias_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

out_dir <- "dev-output/positional-bias-all-models"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Recreate model grid
# ------------------------------------------------------------------------------

openai_no_thinking_models <- c(
  "gpt-4.1",
  "gpt-4.1-mini",
  "gpt-4.1-nano",
  "gpt-4o"
)

openai_thinking_models <- c(
  "gpt-5.1"
)

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

openai_grid <- bind_rows(
  tidyr::expand_grid(
    provider  = "openai",
    model     = openai_no_thinking_models,
    thinking  = "no_thinking",
    direction = directions
  ),
  tidyr::expand_grid(
    provider  = "openai",
    model     = openai_thinking_models,
    thinking  = thinking_levels,
    direction = directions
  )
)

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
  openai_grid,
  anthropic_grid,
  gemini_grid
)

# Helper to reconstruct the filename prefix used in the dev script
make_prefix <- function(provider, model, thinking, direction) {
  prefix <- paste(provider, model, thinking, direction, sep = "_")
  gsub("[^A-Za-z0-9_.-]", "-", prefix)
}

# ------------------------------------------------------------------------------
# 2. Helper to load per-run CSVs
# ------------------------------------------------------------------------------

load_results_csv <- function(provider, model, thinking, direction, base_dir = out_dir) {
  prefix <- make_prefix(provider, model, thinking, direction)
  path   <- file.path(base_dir, paste0(prefix, ".csv"))

  if (!file.exists(path)) {
    message("Missing CSV for: ", provider, " / ", model, " / ", thinking,
            " / ", direction, " at ", path)
    return(NULL)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

# ------------------------------------------------------------------------------
# 3. Generic numeric extractor (same as in dev script)
# ------------------------------------------------------------------------------

extract_numeric_metric <- function(obj, name_pattern = NULL) {
  if (is.null(obj)) return(NA_real_)

  if (is.numeric(obj) && length(obj) >= 1L) {
    return(obj[1])
  }

  if (is.data.frame(obj)) {
    if (!is.null(name_pattern)) {
      num_cols   <- vapply(obj, is.numeric, logical(1))
      candidates <- names(obj)[num_cols & grepl(name_pattern, names(obj))]
      if (length(candidates) > 0L) {
        return(obj[[candidates[1]]][1])
      }
    }
    num_cols <- vapply(obj, is.numeric, logical(1))
    if (any(num_cols)) {
      first_col <- which(num_cols)[1]
      return(obj[[first_col]][1])
    }
    return(NA_real_)
  }

  if (is.list(obj)) {
    flat <- tryCatch(
      unlist(obj, recursive = TRUE, use.names = TRUE),
      error = function(e) NULL
    )
    if (is.null(flat) || !length(flat)) return(NA_real_)

    if (!is.null(name_pattern) && !is.null(names(flat))) {
      idx <- which(grepl(name_pattern, names(flat)))
      if (length(idx)) {
        val <- suppressWarnings(as.numeric(flat[idx]))
        val <- val[is.finite(val)]
        if (length(val)) return(val[1])
      }
    }

    val <- suppressWarnings(as.numeric(flat))
    val <- val[is.finite(val)]
    if (length(val)) return(val[1])
    return(NA_real_)
  }

  v <- suppressWarnings(as.numeric(obj))
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_real_)
  v[1]
}

# ------------------------------------------------------------------------------
# 4. Summarise per (provider, model, thinking) using forward + reverse CSVs
# ------------------------------------------------------------------------------

summarize_model_thinking <- function(provider, model, thinking) {
  res_forward <- load_results_csv(provider, model, thinking, "forward")
  res_reverse <- load_results_csv(provider, model, thinking, "reverse")

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
              provider, " / ", model, " / ", thinking, ": ",
              conditionMessage(e))
      NULL
    }
  )

  if (is.null(cons)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  prop_consistent <- extract_numeric_metric(cons, "prop_consistent")

  # Positional bias: check_positional_bias expects a details table
  details <- NULL
  if (is.list(cons) && "details" %in% names(cons)) {
    details <- cons$details
  }

  bias <- if (!is.null(details)) {
    tryCatch(
      check_positional_bias(details),
      error = function(e) {
        message("check_positional_bias error for ",
                provider, " / ", model, " / ", thinking, ": ",
                conditionMessage(e))
        NULL
      }
    )
  } else {
    NULL
  }

  # Extract p_sample1_overall
  p_sample1_overall <- NA_real_
  if (!is.null(bias)) {
    if (is.data.frame(bias) && "p_sample1_overall" %in% names(bias)) {
      p_sample1_overall <- bias$p_sample1_overall[1]
    } else {
      p_sample1_overall <- extract_numeric_metric(bias, "p_sample1_overall")
    }
  }

  tibble::tibble(
    prop_consistent   = prop_consistent,
    p_sample1_overall = p_sample1_overall
  )
}

# Unique provider/model/thinking combos
combos <- model_matrix %>%
  distinct(provider, model, thinking)

summary_tbl <- combos %>%
  rowwise() %>%
  mutate(
    summary = list(
      summarize_model_thinking(provider, model, thinking)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(summary)

# ------------------------------------------------------------------------------
# 5. Add thinking_config + temperature and save summary
# ------------------------------------------------------------------------------

summary_tbl <- summary_tbl %>%
  mutate(
    thinking_config = case_when(
      provider == "openai" & thinking == "no_thinking" ~
        "OpenAI chat.completions (no reasoning, temp=0)",
      provider == "openai" & thinking == "with_thinking" & model == "gpt-5.1" ~
        "OpenAI responses (include_thoughts=TRUE, effort=low; no temperature parameter)",
      provider == "openai" & thinking == "with_thinking" ~
        "OpenAI responses (include_thoughts=TRUE; no temperature parameter)",

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
    provider,
    model,
    thinking,
    prop_consistent,
    p_sample1_overall,
    thinking_config,
    temperature
  )

summary_path <- file.path(out_dir, "positional_bias_summary.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Rebuilt summary written to: ", summary_path)
