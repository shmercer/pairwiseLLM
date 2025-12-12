# dev/dev-openai-models.R
#
# Manual smoke tests for OpenAI live + batch comparison pipelines
# - Uses a small number of pairs
# - Exercises multiple chat models via chat.completions
# - Exercises gpt-5.1 and gpt-5.2 via responses (with and without thoughts)
# - Tests long date-stamped model names
#
# Requires:
#   - OPENAI_API_KEY set in your environment
#   - Network access
#
# Running this script will incur OpenAI API usage costs.

library(pairwiseLLM)
library(dplyr)
library(purrr)
library(tibble)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY is not set. Please set it before running this script.", call. = FALSE)
}

# ---------------------------------------------------------------------
# Output directory + logging
# ---------------------------------------------------------------------

out_dir <- file.path("dev-output", "dev-openai-models")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(out_dir, paste0("openai_models_", timestamp, ".log"))

cat("Writing log to:", log_file, "\n")
sink(log_file, split = TRUE)
on.exit(
  {
    sink(NULL)
  },
  add = TRUE
)

# Helper for safe file names (avoid base-pipe placeholder issues)
safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x, perl = TRUE)
  x
}

# ---------------------------------------------------------------------
# Data: small number of pairs
# ---------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")

pairs_all <- example_writing_samples |>
  make_pairs()

pairs_small <- pairs_all |>
  sample_pairs(n_pairs = 2, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

message("\n====================================================")
message("OpenAI live + batch tests")
message("Pairs: ", nrow(pairs_small))
message("====================================================")

# ---------------------------------------------------------------------
# Model grids
# ---------------------------------------------------------------------

# LIVE grid:
# - Standard chat models: use temp=0
# - Reasoning models (enabled): MUST use temp=NA (NULL)
# - Reasoning models (disabled): use temp=0 for consistency
live_grid <- tibble::tribble(
  ~model,               ~endpoint,           ~include_thoughts, ~reasoning,       ~temperature,
  "gpt-4o",             "chat.completions",  FALSE,             NA_character_,    0,
  "gpt-4o-2024-11-20",  "chat.completions",  FALSE,             NA_character_,    0,
  "gpt-4.1",            "chat.completions",  FALSE,             NA_character_,    0,
  "gpt-4.1-2025-04-14", "chat.completions",  FALSE,             NA_character_,    0,

  # GPT-5.1
  "gpt-5.1",            "responses",         TRUE,              "low",            NA_real_, # Reasoning enabled -> No temp
  "gpt-5.1",            "responses",         FALSE,             "none",           0, # Reasoning disabled -> Temp 0
  "gpt-5.1-2025-11-13", "responses",         TRUE,              "low",            NA_real_,

  # GPT-5.2
  "gpt-5.2",            "responses",         TRUE,              "low",            NA_real_,
  "gpt-5.2",            "responses",         FALSE,             "none",           0,
  "gpt-5.2-2025-12-11", "responses",         TRUE,              "low",            NA_real_
)

# BATCH grid:
# - Standard chat models: temp=0
# - Reasoning models (enabled): temp=NA
# - Reasoning models (disabled): temp=0
batch_grid <- tibble::tribble(
  ~model,               ~include_thoughts, ~endpoint,          ~reasoning,       ~temperature,
  "gpt-4o",             FALSE,             "chat.completions", NA_character_,    0,
  "gpt-4o-2024-11-20",  FALSE,             "chat.completions", NA_character_,    0,
  "gpt-4.1",            FALSE,             "chat.completions", NA_character_,    0,
  "gpt-4.1-2025-04-14", FALSE,             "chat.completions", NA_character_,    0,

  # GPT-5.1
  "gpt-5.1",            TRUE,              NA_character_,      NA_character_,    NA_real_, # Auto-selects responses/low
  "gpt-5.1",            FALSE,             "responses",        "none",           0,
  "gpt-5.1-2025-11-13", TRUE,              NA_character_,      NA_character_,    NA_real_,

  # GPT-5.2
  "gpt-5.2",            TRUE,              NA_character_,      NA_character_,    NA_real_,
  "gpt-5.2",            FALSE,             "responses",        "none",           0,
  "gpt-5.2-2025-12-11", TRUE,              NA_character_,      NA_character_,    NA_real_
)

# ---------------------------------------------------------------------
# LIVE tests
# ---------------------------------------------------------------------

message("\n====================")
message("LIVE comparisons")
message("====================")

live_status <- list()
live_results <- list()

for (i in seq_len(nrow(live_grid))) {
  row <- live_grid[i, ]

  model <- row$model
  endpoint <- row$endpoint
  include_thoughts <- isTRUE(row$include_thoughts)
  reasoning <- row$reasoning
  temperature <- row$temperature

  # Unique label for filename
  config_suffix <- if (include_thoughts) "thoughts" else "nothoughts"
  label <- paste0("live_", safe_name(model), "_", config_suffix)

  message("\n--- LIVE: ", model, " | endpoint=", endpoint, " | thoughts=", include_thoughts, " ---")

  args <- list(
    pairs             = pairs_small,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = endpoint,
    include_thoughts  = include_thoughts,
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = TRUE
  )

  # Optional arguments (avoid passing NA)
  if (!is.na(temperature)) {
    args$temperature <- temperature
  }
  if (!is.na(reasoning)) {
    args$reasoning <- reasoning
  }

  res <- tryCatch(
    do.call(submit_openai_pairs_live, args),
    error = function(e) {
      message("[ERROR] LIVE ", model, " (", endpoint, "): ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(res)) {
    live_results[[label]] <- res

    # Save a small CSV snapshot
    out_csv <- file.path(
      out_dir,
      paste0(label, "_", timestamp, ".csv")
    )
    readr::write_csv(res, out_csv)

    message("[OK] LIVE ", model, " (", endpoint, ") -> nrow=", nrow(res))
    message("Saved results to: ", out_csv)

    live_status[[label]] <- tibble::tibble(
      model    = model,
      endpoint = endpoint,
      thoughts = include_thoughts,
      ok       = TRUE,
      n_pairs  = nrow(res),
      file     = out_csv
    )
  } else {
    live_status[[label]] <- tibble::tibble(
      model    = model,
      endpoint = endpoint,
      thoughts = include_thoughts,
      ok       = FALSE,
      n_pairs  = NA_integer_,
      file     = NA_character_
    )
  }
}

live_status_tbl <- bind_rows(live_status)
message("\n=== LIVE summary ===")
print(live_status_tbl)

# ---------------------------------------------------------------------
# BATCH tests
# ---------------------------------------------------------------------

message("\n====================")
message("BATCH comparisons")
message("====================")

batch_status <- list()
batch_results <- list()

for (i in seq_len(nrow(batch_grid))) {
  row <- batch_grid[i, ]

  model <- row$model
  include_thoughts <- isTRUE(row$include_thoughts)
  endpoint <- row$endpoint
  reasoning <- row$reasoning
  temperature <- row$temperature

  config_suffix <- if (include_thoughts) "thoughts" else "nothoughts"
  label <- paste0("batch_", safe_name(model), "_", config_suffix)

  message(
    "\n--- BATCH: ", model,
    " | include_thoughts=", include_thoughts,
    " | endpoint=", ifelse(is.na(endpoint), "<auto>", endpoint),
    " ---"
  )

  batch_input_path <- tempfile("openai_batch_input_", fileext = ".jsonl")
  batch_output_path <- tempfile("openai_batch_output_", fileext = ".jsonl")

  args <- list(
    pairs             = pairs_small,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    include_thoughts  = include_thoughts,
    include_raw       = FALSE,
    batch_input_path  = batch_input_path,
    batch_output_path = batch_output_path,
    poll              = TRUE,
    interval_seconds  = 20, # less frequent polling
    timeout_seconds   = 36000 # 10 hour timeout
  )

  # Optional arguments
  if (!is.na(endpoint)) {
    args$endpoint <- endpoint
  }
  if (!is.na(temperature)) {
    args$temperature <- temperature
  }
  if (!is.na(reasoning)) {
    args$reasoning <- reasoning
  }

  pipeline <- tryCatch(
    do.call(llm_submit_pairs_batch, c(args, list(backend = "openai"))),
    error = function(e) {
      message("[ERROR] BATCH ", model, ": ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(pipeline)) {
    res <- tryCatch(llm_download_batch_results(pipeline), error = function(e) NULL)
    status <- tryCatch(pipeline$batch$status, error = function(e) NA_character_)

    if (!is.null(res)) {
      batch_results[[label]] <- res

      out_csv <- file.path(
        out_dir,
        paste0(label, "_", timestamp, ".csv")
      )
      readr::write_csv(res, out_csv)

      message("[OK] BATCH ", model, " -> status=", status)
      message("Rows: ", nrow(res))
      message("Saved results to: ", out_csv)

      batch_status[[label]] <- tibble::tibble(
        model = model,
        include_thoughts = include_thoughts,
        endpoint = ifelse(is.null(args$endpoint), "<auto>", args$endpoint),
        ok = identical(status, "completed"),
        n_pairs = nrow(res),
        batch_status = status,
        input_path = pipeline$batch_input_path,
        output_path = pipeline$batch_output_path,
        file = out_csv
      )
    } else {
      message("[WARN] BATCH ", model, ": pipeline completed but results is NULL (status=", status, ").")
      batch_status[[label]] <- tibble::tibble(
        model = model,
        include_thoughts = include_thoughts,
        endpoint = ifelse(is.null(args$endpoint), "<auto>", args$endpoint),
        ok = FALSE,
        n_pairs = NA_integer_,
        batch_status = status,
        input_path = pipeline$batch_input_path,
        output_path = pipeline$batch_output_path,
        file = NA_character_
      )
    }
  } else {
    batch_status[[label]] <- tibble::tibble(
      model = model,
      include_thoughts = include_thoughts,
      endpoint = ifelse(is.na(endpoint), "<auto>", endpoint),
      ok = FALSE,
      n_pairs = NA_integer_,
      batch_status = NA_character_,
      input_path = NA_character_,
      output_path = NA_character_,
      file = NA_character_
    )
  }
}

batch_status_tbl <- bind_rows(batch_status)
message("\n=== BATCH summary ===")
print(batch_status_tbl)

message("\nAll done. Log written to: ", log_file)
