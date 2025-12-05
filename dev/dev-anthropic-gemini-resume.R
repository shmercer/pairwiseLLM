# dev/dev-anthropic-gemini-resume.R
#
# Resume polling + parsing for the Anthropic/Gemini template A/B experiment.
#
# Assumes you previously ran dev-anthropic-gemini-template-ab-test.R
# which created a `jobs` list in memory and/or saved it to an RDS file.
#
# This script:
#   - Reloads `jobs` (from global env or RDS)
#   - Polls Anthropic + Gemini batches until they reach a terminal state
#   - Downloads & parses results for completed batches
#   - Writes per-run CSVs (if missing)
#   - Builds a summary table:
#       * prop_consistent
#       * prop_pos1
#       * p_sample1_overall
#       * thinking_config
#       * temperature
#   - Saves summary to:
#       dev-output/anthropic-gemini-template-ab-test/
#         anthropic_gemini_template_ab_summary.csv

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Setup + load `jobs`
# ------------------------------------------------------------------------------

out_dir <- "dev-output/anthropic-gemini-template-ab-test"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

jobs_rds_path <- file.path(out_dir, "jobs.rds")

# Prefer `jobs` already in the current R session; otherwise try to load from RDS
if (exists("jobs", envir = .GlobalEnv, inherits = FALSE) &&
    is.list(get("jobs", envir = .GlobalEnv))) {
  jobs <- get("jobs", envir = .GlobalEnv)
  message("Loaded `jobs` from global environment.")
} else if (file.exists(jobs_rds_path)) {
  jobs <- readRDS(jobs_rds_path)
  assign("jobs", jobs, envir = .GlobalEnv)
  message("Loaded `jobs` from RDS: ", jobs_rds_path)
} else {
  stop(
    "`jobs` object not found in global environment and no RDS at:\n  ",
    jobs_rds_path, "\n",
    "Run dev-anthropic-gemini-template-ab-test.R first, ",
    "and/or save `jobs` with saveRDS(jobs, '", jobs_rds_path, "').",
    call. = FALSE
  )
}

if (!length(jobs)) {
  stop("`jobs` is empty; nothing to resume.", call. = FALSE)
}

jobs_tbl <- tibble::tibble(
  idx         = seq_along(jobs),
  template_id = vapply(jobs, `[[`, character(1), "template_id"),
  provider    = vapply(jobs, `[[`, character(1), "provider"),
  model       = vapply(jobs, `[[`, character(1), "model"),
  thinking    = vapply(jobs, `[[`, character(1), "thinking"),
  direction   = vapply(jobs, `[[`, character(1), "direction"),
  prefix      = vapply(jobs, `[[`, character(1), "prefix"),
  batch_type  = vapply(jobs, `[[`, character(1), "batch_type"),
  done        = vapply(jobs, `[[`, logical(1),  "done")
)

print(jobs_tbl)

# ------------------------------------------------------------------------------
# 1. Polling helpers
# ------------------------------------------------------------------------------

interval_seconds  <- 90   # time between full polling rounds
per_request_delay <- 1    # short delay between individual requests (sec)

is_terminal_anthropic <- function(status) {
  # Anthropic batch status values: "in_progress", "processed", "failed",
  # "cancelled", "expired"
  status %in% c("processed", "cancelled", "expired", "failed")
}

is_terminal_gemini <- function(state) {
  # Gemini REST Batch states are stored under metadata$state and typically are:
  # "BATCH_STATE_PENDING", "BATCH_STATE_RUNNING",
  # "BATCH_STATE_SUCCEEDED", "BATCH_STATE_FAILED",
  # "BATCH_STATE_CANCELLED", "BATCH_STATE_EXPIRED"
  state %in% c(
    "BATCH_STATE_SUCCEEDED",
    "BATCH_STATE_FAILED",
    "BATCH_STATE_CANCELLED",
    "BATCH_STATE_EXPIRED"
  )
}

unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

# ------------------------------------------------------------------------------
# 2. Poll all unfinished batches, download + parse results
# ------------------------------------------------------------------------------

while (length(unfinished) > 0L) {
  message("Polling ", length(unfinished),
          " unfinished Anthropic/Gemini batch(es)...")

  for (j in unfinished) {
    job <- jobs[[j]]

    if (isTRUE(job$done)) {
      Sys.sleep(per_request_delay)
      next
    }

    if (identical(job$batch_type, "anthropic")) {
      # ------------------------------
      # Anthropic polling
      # ------------------------------
      batch <- anthropic_get_batch(job$batch_id)
      status <- if (!is.null(batch$status)) batch$status else "unknown"

      message("  [Anthropic] ", job$prefix, " status: ", status)

      if (is_terminal_anthropic(status)) {
        if (identical(status, "processed")) {
          # Download + parse only if we don't already have results
          if (is.null(job$results) || !file.exists(job$csv_path)) {
            anthropic_download_batch_results(
              batch_id = job$batch_id,
              path     = job$batch_output_path
            )

            res <- parse_anthropic_batch_output(job$batch_output_path)

            jobs[[j]]$results <- res
            readr::write_csv(res, job$csv_path)
            message("    -> Anthropic results written to: ", job$csv_path)
          } else {
            message("    -> Anthropic results already exist for: ", job$csv_path)
          }
        }

        jobs[[j]]$done <- TRUE
      }

    } else if (identical(job$batch_type, "gemini")) {
      # ------------------------------
      # Gemini polling
      # ------------------------------
      # Note: Gemini batches store state in batch$metadata$state
      batch <- gemini_get_batch(
        batch_name = job$batch_name,
        api_key    = Sys.getenv("GEMINI_API_KEY"),
        api_version = "v1beta"
      )

      state <- if (!is.null(batch$metadata$state)) {
        batch$metadata$state
      } else {
        "BATCH_STATE_UNSPECIFIED"
      }

      message("  [Gemini] ", job$prefix, " state: ", state)

      if (is_terminal_gemini(state)) {
        if (identical(state, "BATCH_STATE_SUCCEEDED")) {
          # Download + parse only if we don't already have results
          if (is.null(job$results) || !file.exists(job$csv_path)) {
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
            message("    -> Gemini results written to: ", job$csv_path)
          } else {
            message("    -> Gemini results already exist for: ", job$csv_path)
          }
        }

        jobs[[j]]$done <- TRUE
      }

    } else {
      warning("Unknown batch_type for job index ", j, call. = FALSE)
    }

    # Short delay between provider requests to reduce 429 risk
    Sys.sleep(per_request_delay)
  }

  # Update unfinished set
  unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))

  # Save updated jobs snapshot so you can resume again later if needed
  saveRDS(jobs, jobs_rds_path)

  if (length(unfinished) > 0L) {
    message("Sleeping ", interval_seconds, " seconds before next poll...")
    Sys.sleep(interval_seconds)
  }
}

message("All Anthropic/Gemini batches have reached a terminal state.")

# ------------------------------------------------------------------------------
# 3. Build consistency / positional-bias summary
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
      prop_pos1         = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  res_forward <- results_by_key[[key_forward]]
  res_reverse <- results_by_key[[key_reverse]]

  if (is.null(res_forward) || is.null(res_reverse) ||
      !nrow(res_forward) || !nrow(res_reverse)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      prop_pos1         = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  # ------------------------------
  # Reverse consistency
  # ------------------------------
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

    # Positional bias on the consistency object
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

        # prop_pos1: use directly if present, otherwise derive from counts
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
# 4. Add thinking_config + temperature and save summary
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
    prop_pos1,          # <- column order: prop_pos1 BEFORE p_sample1_overall
    p_sample1_overall,
    thinking_config,
    temperature
  )

summary_path <- file.path(out_dir, "anthropic_gemini_template_ab_summary.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Anthropic/Gemini template A/B summary written to: ", summary_path)
