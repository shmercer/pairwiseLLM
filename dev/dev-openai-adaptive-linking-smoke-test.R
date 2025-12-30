# Live + Batch smoke-test for adaptive + linking workflows using OpenAI backends.
#
# Purpose:
#   - Exercise end-to-end "adaptive + core-linking" runner with REAL API calls.
#   - Emit compact, actionable diagnostics when something fails early.
#
# Notes:
#   - The runners forward `...` to `fit_fun`. Do NOT pass `verbose=`; use `fit_verbose=`.
#   - This file is meant to be run interactively (not as part of tests).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(glue)
  library(readr)
  library(devtools)
})

# ---- Load local package ----
devtools::load_all()

# ---- Config ----
# OpenAI API key must be present for live/batch.
if (nchar(Sys.getenv("OPENAI_API_KEY")) == 0L) {
  stop("OPENAI_API_KEY env var is not set.", call. = FALSE)
}

# Keep the test small so you can iterate quickly.
N_CORE <- 10
BOOTSTRAP_N <- 12
ROUND_SIZE <- 8
MAX_ROUNDS <- 3

# ---- Helpers ----

# Local null-coalescing helper (avoids relying on rlang for smoke tests)
`%||%` <- function(x, y) if (is.null(x)) y else x
.ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.dbg <- function(...) message("[", .ts(), "] ", paste0(..., collapse = ""))

.short <- function(x, n = 240) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  ifelse(nchar(x) <= n, x, paste0(substr(x, 1, n), "…"))
}

.print_results_summary <- function(res, label = "results") {
  if (is.null(res)) {
    .dbg(label, ": <NULL>")
    return(invisible(NULL))
  }
  res <- tibble::as_tibble(res)

  # Basic backend validation summary (non-strict; allow missing winners so we can inspect)
  rep <- validate_backend_results(
    res,
    backend = label,
    normalize_winner = TRUE,
    strict = FALSE,
    return_report = TRUE
  )

  .dbg(label, ": n=", rep$n_rows,
       " | missing_winner=", rep$n_missing_winner,
       " | missing_id=", rep$n_missing_id,
       " | invalid_winner=", rep$n_invalid_winner)

  if ("status_code" %in% names(res)) {
    sc <- sort(table(res$status_code), decreasing = TRUE)
    .dbg(label, ": status_code=", paste(names(sc), sc, sep = ":", collapse = ", "))
  }

  if ("error_message" %in% names(res)) {
    em <- res %>%
      filter(!is.na(error_message), error_message != "") %>%
      count(error_message, sort = TRUE)
    if (nrow(em) > 0) {
      .dbg(label, ": top error_message=",
           paste0(head(em$error_message, 3), collapse = " | "))
    }
  }

  # Show a few problematic rows
  bad <- res %>%
    mutate(.missing_winner = is.na(better_id) | better_id == "") %>%
    filter(.missing_winner) %>%
    select(any_of(c("ID1", "ID2", "better_id", "better_sample", "status_code", "error_message", "content"))) %>%
    head(5)

  if (nrow(bad) > 0) {
    .dbg(label, ": example rows with missing winner:")
    print(bad %>% mutate(content = .short(content, 280)))
  }

  invisible(rep)
}

# ---- Minimal samples ----
# For repeatability, keep texts short and obviously rankable.
samples <- tibble(
  ID = sprintf("S%02d", 1:N_CORE),
  text = c(
    "A clear, well-structured answer with evidence and a conclusion.",
    "A decent answer with some structure and minor issues.",
    "An answer that is okay but vague and repetitive.",
    "A weak answer: unclear point and several grammar mistakes.",
    "Excellent: precise claims, good organization, strong reasoning.",
    "Poor: off-topic and hard to follow.",
    "Great: concise and accurate with strong support.",
    "Mediocre: some relevant points but lacks clarity.",
    "Bad: incoherent and incomplete.",
    "Very good: organized, relevant, and mostly polished."
  )
)

# ---- Prompt template (structured) ----
tmpl <- set_prompt_template(
  template = c(
    "You will compare two writing samples and decide which is better overall.",
    "",
    "<SAMPLE_1>{text1}</SAMPLE_1>",
    "",
    "<SAMPLE_2>{text2}</SAMPLE_2>",
    "",
    "Return ONLY one of the following tags:",
    "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    "or",
    "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
  )
)

# ---- Judge functions (live + batch) ----
judge_openai_live <- function(pairs) {
  t0 <- Sys.time()

  out <- submit_llm_pairs(
    pairs = pairs,
    backend = "openai_live",
    model = "gpt-4o-mini",
    prompt_template = tmpl,
    temperature = 0,
    include_raw = TRUE,     # keep raw for debugging
    status_every = 1L
  )

  .dbg("judge_openai_live: elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s")
  .print_results_summary(out$results, label = "openai_live")

  out
}

judge_openai_batch <- function(pairs) {
  t0 <- Sys.time()

  batch <- llm_submit_pairs_batch(
    pairs = pairs,
    backend = "openai_batch",
    model = "gpt-4o-mini",
    prompt_template = tmpl,
    temperature = 0
  )

  .dbg("Submitted batch: ", batch$batch_id %||% "<no batch_id>",
       " | status=", batch$status %||% "<no status>")

  # Poll a little (adjust as needed)
  Sys.sleep(20)

  res <- llm_download_batch_results(batch)
  .dbg("judge_openai_batch: elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s")
  .print_results_summary(res, label = "openai_batch")

  res
}

# ---- Runner call ----
run_one <- function(judge_fun, label) {
  .dbg("===== Running: ", label, " =====")

  out <- tryCatch(
    {
      bt_run_adaptive_core_linking(
        samples = samples,
        judge_fun = judge_fun,
        engine = "auto",
        fit_verbose = TRUE,
        round_size = ROUND_SIZE,
        init_round_size = BOOTSTRAP_N,
        max_rounds = MAX_ROUNDS,
        core_size = N_CORE,
        core_method = "pam",
        # keep linkage conservative for smoke tests
        linking = "off",
        seed_pairs = 123,
        return_diagnostics = TRUE
      )
    },
    error = function(e) {
      .dbg("ERROR in ", label, ": ", conditionMessage(e))
      .dbg("Backtrace (best-effort):")
      if (requireNamespace("rlang", quietly = TRUE)) {
        print(rlang::last_trace(drop = FALSE))
      } else {
        traceback()
      }
      stop(e)
    }
  )

  .dbg("Finished: ", label)
  .dbg("Results rows: ", nrow(out$results))
  if (!is.null(out$final_fit)) {
    .dbg("Final fit theta head:")
    print(head(out$final_fit$theta))
  }
  out
}

# ---- Execute ----
# Live (recommended first for debugging)
out_live <- run_one(judge_openai_live, "adaptive_core_linking + openai_live")

# Batch (optional; slower)
# out_batch <- run_one(judge_openai_batch, "adaptive_core_linking + openai_batch")