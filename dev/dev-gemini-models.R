## dev/dev_gemini_models.R
##
## Dev script: test Gemini models (live + batch) with 2 pairs each.
## - Requires GEMINI_API_KEY to be set.
## - Uses example_writing_samples and the pairwiseLLM pipelines.

library(pairwiseLLM)
library(tibble)
library(dplyr)
library(purrr)

## ------------------------------------------------------------------
## Setup output directory + logging
## ------------------------------------------------------------------

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

out_dir <- "dev-output/dev-gemini-models/"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

log_file <- file.path(out_dir, paste0("dev_gemini_models_", timestamp, ".log"))

cat("Writing log to: ", log_file, "\n")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

## ------------------------------------------------------------------
## Check API key
## ------------------------------------------------------------------

if (!nzchar(Sys.getenv("GEMINI_API_KEY"))) {
  stop("GEMINI_API_KEY is not set; please export it before running this script.")
}

## ------------------------------------------------------------------
## Test configuration
## ------------------------------------------------------------------

gemini_models <- c(
  "gemini-3-pro-preview"
)

modes <- c("with_thoughts")

## Helper to make safe filenames from model + mode
safe_name <- function(x) {
  gsub("[^A-Za-z0-9._-]", "_", x)
}

## ------------------------------------------------------------------
## Build 2 pairs from example data
## ------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

# Take first 4 samples → pairs → sample 2 random pairs
pairs_all <- example_writing_samples[1:4, ] |>
  make_pairs() |>
  sample_pairs(n_pairs = 2, seed = 123) |>
  randomize_pair_order(seed = 456)

print("Pairs to use for all tests:")
print(pairs_all)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

## ------------------------------------------------------------------
## LIVE TESTS
## ------------------------------------------------------------------

live_status   <- list()
live_results  <- list()

cat("\n================ LIVE GEMINI TESTS ================\n")

for (model in gemini_models) {
  for (mode in modes) {
    include_thoughts <- identical(mode, "with_thoughts")

    cat("\n--- Live test: model = ", model,
        ", mode = ", mode,
        ", include_thoughts = ", include_thoughts, " ---\n", sep = "")

    status_row <- tibble(
      backend       = "gemini",
      model         = model,
      mode          = mode,
      success       = FALSE,
      n_pairs       = nrow(pairs_all),
      error_message = NA_character_
    )

    res_tbl <- NULL

    # We use thinking_level = "low" for all for consistency.
    # The underlying helpers are series-aware and will ignore or adapt
    # thinking config as appropriate for each model.
    try_res <- try(
      submit_gemini_pairs_live(
        pairs             = pairs_all,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "low",
        include_thoughts  = include_thoughts,
        include_raw       = TRUE,
        verbose           = TRUE,
        status_every      = 1,
        progress          = FALSE
      ),
      silent = TRUE
    )

    if (inherits(try_res, "try-error")) {
      msg <- conditionMessage(attr(try_res, "condition"))
      cat("LIVE ERROR for model ", model,
          ", mode ", mode, ": ", msg, "\n", sep = "")
      status_row$error_message <- msg
    } else {
      res_tbl <- try_res
      status_row$success <- TRUE

      cat("LIVE SUCCESS for model ", model,
          ", mode ", mode, ". Parsed rows: ", nrow(res_tbl), "\n", sep = "")

      # Save per-model live results to a CSV in dev-output/
      file_live <- file.path(
        out_dir,
        paste0(
          "live_results_gemini_",
          safe_name(model), "_",
          safe_name(mode), "_",
          timestamp,
          ".csv"
        )
      )

      # Strip raw_response list-column (can't write to CSV)
      res_csv <- res_tbl %>%
        select(-any_of("raw_response"))

      readr::write_csv(res_csv, file_live)
      cat("  Live results written to: ", file_live, "\n", sep = "")

      live_results[[paste(model, mode, sep = "|")]] <- res_tbl
    }

    live_status[[paste(model, mode, sep = "|")]] <- status_row
  }
}

live_status_tbl <- bind_rows(live_status)

summary_live_path <- file.path(
  out_dir,
  paste0("live_gemini_summary_", timestamp, ".csv")
)
readr::write_csv(live_status_tbl, summary_live_path)

cat("\nLIVE summary written to: ", summary_live_path, "\n")
print(live_status_tbl)

## ------------------------------------------------------------------
## BATCH TESTS
## ------------------------------------------------------------------

batch_status  <- list()
batch_results <- list()

cat("\n================ BATCH GEMINI TESTS ================\n")

for (model in gemini_models) {
  for (mode in modes) {
    include_thoughts <- identical(mode, "with_thoughts")

    cat("\n--- Batch test: model = ", model,
        ", mode = ", mode,
        ", include_thoughts = ", include_thoughts, " ---\n", sep = "")

    status_row <- tibble(
      backend       = "gemini",
      model         = model,
      mode          = mode,
      success       = FALSE,
      n_pairs       = nrow(pairs_all),
      error_message = NA_character_
    )

    # Write batch input/output into dev-output/
    batch_input_path  <- file.path(
      out_dir,
      paste0(
        "gemini-batch-input_",
        safe_name(model), "_",
        safe_name(mode), "_",
        timestamp,
        ".json"
      )
    )
    batch_output_path <- file.path(
      out_dir,
      paste0(
        "gemini-batch-output_",
        safe_name(model), "_",
        safe_name(mode), "_",
        timestamp,
        ".jsonl"
      )
    )

    try_batch <- try(
      run_gemini_batch_pipeline(
        pairs             = pairs_all,
        model             = model,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "low",
        batch_input_path  = batch_input_path,
        batch_output_path = batch_output_path,
        poll              = TRUE,
        interval_seconds  = 30,   # a bit shorter for dev
        timeout_seconds   = 3600, # 1 hour max per batch in dev
        api_key           = Sys.getenv("GEMINI_API_KEY"),
        api_version       = "v1beta",
        verbose           = TRUE,
        include_thoughts  = include_thoughts
      ),
      silent = TRUE
    )

    if (inherits(try_batch, "try-error")) {
      msg <- conditionMessage(attr(try_batch, "condition"))
      cat("BATCH ERROR for model ", model,
          ", mode ", mode, ": ", msg, "\n", sep = "")
      status_row$error_message <- msg
    } else {
      batch_obj <- try_batch
      res_tbl   <- batch_obj$results

      status_row$success <- TRUE

      cat("BATCH SUCCESS for model ", model,
          ", mode ", mode, ". Parsed rows: ", nrow(res_tbl), "\n", sep = "")

      # Save per-model batch results to CSV in dev-output/
      file_batch <- file.path(
        out_dir,
        paste0(
          "batch_results_gemini_",
          safe_name(model), "_",
          safe_name(mode), "_",
          timestamp,
          ".csv"
        )
      )
      readr::write_csv(res_tbl, file_batch)
      cat("  Batch results written to: ", file_batch, "\n", sep = "")

      batch_results[[paste(model, mode, sep = "|")]] <- res_tbl
    }

    batch_status[[paste(model, mode, sep = "|")]] <- status_row
  }
}

batch_status_tbl <- bind_rows(batch_status)

summary_batch_path <- file.path(
  out_dir,
  paste0("batch_gemini_summary_", timestamp, ".csv")
)
readr::write_csv(batch_status_tbl, summary_batch_path)

cat("\nBATCH summary written to: ", summary_batch_path, "\n")
print(batch_status_tbl)

cat("\nLog file (this console output) saved at: ", log_file, "\n")
