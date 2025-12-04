# dev/dev-positional-bias-all-models.R
#
# Run positional-bias experiments across multiple providers/models using
# pairwiseLLM, with:
# - all pairs from example_writing_samples
# - forward (deterministic alternating) + reverse sequences
# - batch APIs for OpenAI, Anthropic, Gemini
# - temperature = 0 for all non-thinking runs (where supported)
#
# Outputs:
# - Per-run CSVs: dev-output/positional-bias-all-models/<provider>_<model>_<thinking>_<direction>.csv
# - Summary CSV:  dev-output/positional-bias-all-models/positional_bias_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

out_dir <- "dev-output/positional-bias-all-models"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

# Trait + prompt template
td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# ------------------------------------------------------------------------------
# 1. Build forward + reverse pairs (ALL pairs)
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

# ------------------------------------------------------------------------------
# 2. Model grid
# ------------------------------------------------------------------------------

# OpenAI:
# - gpt-4.x family: no thinking / reasoning supported -> only "no_thinking"
# - gpt-5.1: test both no_thinking and with_thinking
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

# ------------------------------------------------------------------------------
# 3. Helper: choose pairs by direction
# ------------------------------------------------------------------------------

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
# 4. Submit all batches (no polling)
# ------------------------------------------------------------------------------

jobs <- list()

for (i in seq_len(nrow(model_matrix))) {
  row <- model_matrix[i, ]

  provider  <- row$provider
  model     <- row$model
  thinking  <- row$thinking
  direction <- row$direction

  message("Submitting batch: ",
          provider, " / ", model, " / ", thinking, " / ", direction)

  pairs_use <- get_pairs_for_direction(direction)

  prefix <- paste(provider, model, thinking, direction, sep = "_")
  prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)

  batch_input_path  <- file.path(out_dir, paste0(prefix, "_input.jsonl"))
  batch_output_path <- file.path(out_dir, paste0(prefix, "_output.jsonl"))
  csv_path          <- file.path(out_dir, paste0(prefix, ".csv"))

  # Decide if this run is "thinking" or not
  is_thinking <- identical(thinking, "with_thinking")

  # For non-thinking runs, we want temperature = 0 where supported.
  # - OpenAI: temperature = 0 is supported for gpt-4.x and gpt-5.1 with reasoning = none
  # - Anthropic: reasoning = "none" uses temp=0 by default; we pass temp=0 explicitly here.
  # - Gemini: only runs with thinking; we never set temp=0 for Gemini here.
  temperature_arg <- if (!is_thinking) 0 else NULL

  if (identical(provider, "openai")) {

    # For OpenAI:
    # - non-thinking: chat.completions, temperature=0
    # - with thinking: responses, include_thoughts=TRUE, NO temperature (reasoning mode)
    include_thoughts <- is_thinking

    if (!is_thinking) {
      pipeline <- run_openai_batch_pipeline(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = FALSE,
        endpoint          = "chat.completions",
        batch_input_path  = batch_input_path,
        batch_output_path = batch_output_path,
        poll              = FALSE,
        temperature       = temperature_arg
      )
    } else {
      # With thinking (responses endpoint, NO temperature param for GPT-5.1 thinking)
      pipeline <- run_openai_batch_pipeline(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_thoughts  = TRUE,   # picks responses endpoint + reasoning for gpt-5.1
        batch_input_path  = batch_input_path,
        batch_output_path = batch_output_path,
        poll              = FALSE
      )
    }

    jobs[[length(jobs) + 1L]] <- list(
      provider          = provider,
      model             = model,
      thinking          = thinking,
      direction         = direction,
      prefix            = prefix,
      batch_type        = "openai",
      batch_id          = pipeline$batch$id,
      batch_input_path  = pipeline$batch_input_path,
      batch_output_path = batch_output_path,
      csv_path          = csv_path,
      done              = FALSE,
      results           = NULL
    )

  } else if (identical(provider, "anthropic")) {

    # Anthropic:
    # - reasoning = "none" for non-thinking (temp=0, max_tokens=768)
    # - reasoning = "enabled" for thinking (temp=1, budget_tokens>=1024)
    reasoning <- if (is_thinking) "enabled" else "none"

    if (!is_thinking) {
      pipeline <- run_anthropic_batch_pipeline(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = reasoning,
        include_thoughts  = FALSE,
        batch_input_path  = batch_input_path,
        batch_output_path = batch_output_path,
        poll              = FALSE,
        temperature       = temperature_arg
      )
    } else {
      pipeline <- run_anthropic_batch_pipeline(
        pairs             = pairs_use,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = reasoning,
        include_thoughts  = TRUE,
        batch_input_path  = batch_input_path,
        batch_output_path = batch_output_path,
        poll              = FALSE
      )
    }

    jobs[[length(jobs) + 1L]] <- list(
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
    # Use thinkingLevel = "low", includeThoughts = TRUE, and leave temperature NULL
    # We bypass run_gemini_batch_pipeline so we can keep the requests_tbl for parsing.
    req_tbl <- build_gemini_batch_requests(
      pairs             = pairs_use,
      model             = model,
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      thinking_level    = "low",
      include_thoughts  = TRUE
      # temperature left as default (not 0) to avoid API issues
    )

    batch <- gemini_create_batch(
      requests     = req_tbl$request,
      model        = model,
      api_key      = Sys.getenv("GEMINI_API_KEY"),
      api_version  = "v1beta"
    )

    batch_name <- batch$name %||% stop(
      "Gemini batch did not return a `name` field.",
      call. = FALSE
    )

    jobs[[length(jobs) + 1L]] <- list(
      provider          = provider,
      model             = model,
      thinking          = thinking,
      direction         = direction,
      prefix            = prefix,
      batch_type        = "gemini",
      batch_id          = batch_name,
      batch_input_path  = NA_character_,  # we didn't write our own JSON here
      batch_output_path = batch_output_path,
      csv_path          = csv_path,
      done              = FALSE,
      results           = NULL,
      requests_tbl      = req_tbl
    )

  } else {
    stop("Unknown provider: ", provider, call. = FALSE)
  }
}

# Convert jobs list to a tibble for easier handling
jobs_tbl <- tibble::tibble(
  idx       = seq_along(jobs),
  provider  = vapply(jobs, `[[`, character(1), "provider"),
  model     = vapply(jobs, `[[`, character(1), "model"),
  thinking  = vapply(jobs, `[[`, character(1), "thinking"),
  direction = vapply(jobs, `[[`, character(1), "direction"),
  prefix    = vapply(jobs, `[[`, character(1), "prefix"),
  batch_type = vapply(jobs, `[[`, character(1), "batch_type")
)

# ------------------------------------------------------------------------------
# 5. Poll all batches concurrently (every 5 minutes)
# ------------------------------------------------------------------------------

interval_seconds <- 300  # 5 minutes

is_terminal_openai <- function(status) {
  status %in% c("completed", "failed", "cancelled", "expired")
}

is_terminal_anthropic <- function(status) {
  identical(status, "ended")
}

is_terminal_gemini <- function(state) {
  state %in% c(
    "BATCH_STATE_SUCCEEDED",
    "BATCH_STATE_FAILED",
    "BATCH_STATE_CANCELLED",
    "BATCH_STATE_EXPIRED"
  )
}

unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

while (length(unfinished) > 0L) {
  message("Polling ", length(unfinished), " unfinished batch(es)...")

  for (j in unfinished) {
    job <- jobs[[j]]

    if (job$done) next

    if (identical(job$batch_type, "openai")) {
      batch  <- openai_get_batch(job$batch_id)
      status <- batch$status %||% "unknown"

      message("  [OpenAI] ", job$prefix, " status: ", status)

      if (is_terminal_openai(status)) {
        # Download + parse if completed
        if (identical(status, "completed")) {
          openai_download_batch_output(
            batch_id = job$batch_id,
            path     = job$batch_output_path
          )
          res <- parse_openai_batch_output(job$batch_output_path)
          jobs[[j]]$results <- res
          readr::write_csv(res, job$csv_path)
        }
        jobs[[j]]$done <- TRUE
      }

    } else if (identical(job$batch_type, "anthropic")) {
      batch  <- anthropic_get_batch(job$batch_id)
      status <- batch$processing_status %||% "unknown"

      message("  [Anthropic] ", job$prefix, " status: ", status)

      if (is_terminal_anthropic(status)) {
        anthropic_download_batch_results(
          batch_id    = job$batch_id,
          output_path = job$batch_output_path
        )
        res <- parse_anthropic_batch_output(job$batch_output_path)
        jobs[[j]]$results <- res
        readr::write_csv(res, job$csv_path)
        jobs[[j]]$done <- TRUE
      }

    } else if (identical(job$batch_type, "gemini")) {
      batch <- gemini_get_batch(job$batch_id)
      state <- batch$metadata$state %||% NA_character_

      message("  [Gemini] ", job$prefix, " state: ", state)

      if (!is.na(state) && is_terminal_gemini(state)) {
        gemini_download_batch_results(
          batch        = batch,
          requests_tbl = job$requests_tbl,
          output_path  = job$batch_output_path
        )
        res <- parse_gemini_batch_output(
          results_path = job$batch_output_path,
          requests_tbl = job$requests_tbl
        )
        jobs[[j]]$results <- res
        readr::write_csv(res, job$csv_path)
        jobs[[j]]$done <- TRUE
      }

    } else {
      warning("Unknown batch_type for job ", job$prefix)
    }
  }

  unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

  if (length(unfinished) > 0L) {
    message("Sleeping ", interval_seconds, " seconds before next poll...")
    Sys.sleep(interval_seconds)
  }
}

message("All batches have reached a terminal state.")

# ------------------------------------------------------------------------------
# 6. Build summary table from in-memory results
# ------------------------------------------------------------------------------

# Helper: generic numeric extractor (same spirit as rebuild script)
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

# Build a results lookup by key
results_by_key <- list()
for (j in seq_along(jobs)) {
  key <- paste(
    jobs[[j]]$provider,
    jobs[[j]]$model,
    jobs[[j]]$thinking,
    jobs[[j]]$direction,
    sep = "|"
  )
  results_by_key[[key]] <- jobs[[j]]$results
}

# Unique provider/model/thinking combos (need forward + reverse)
combos <- model_matrix |>
  group_by(provider, model, thinking) |>
  summarize(
    has_forward = TRUE,
    has_reverse = TRUE,
    .groups = "drop"
  )

summarize_model_thinking <- function(provider, model, thinking) {
  key_forward <- paste(provider, model, thinking, "forward", sep = "|")
  key_reverse <- paste(provider, model, thinking, "reverse", sep = "|")

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
              provider, " / ", model, " / ", thinking, ": ",
              conditionMessage(e))
      NULL
    }
  )
  prop_consistent <- extract_numeric_metric(cons, "prop_consistent")

  # Positional bias: check_positional_bias expects a details table
  details <- NULL
  if (!is.null(cons) && is.list(cons) && "details" %in% names(cons)) {
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

  # Correct extraction of p_sample1_overall from check_positional_bias()
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
# 7. Add thinking_config + temperature and save summary
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
    # Temperature as controlled by this script:
    # - All non-thinking runs (OpenAI + Anthropic) use temperature=0
    # - Anthropic thinking runs use temperature=1
    # - OpenAI + Gemini thinking runs leave temperature to provider default (NA)
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
message("Summary written to: ", summary_path)
