# Live + Batch smoke-test for adaptive + linking workflows using OpenAI backends.
#
# Purpose:
#   - Exercise end-to-end "adaptive + core-linking" runner with REAL API calls.
#   - Emit compact, actionable diagnostics when something fails early.
#
# Notes:
#   - The runners forward `...` to `fit_fun`. Do NOT pass `verbose=`; use `fit_verbose=`.
#   - This file is meant to be run interactively (not as part of tests).
#   - Outputs are written to dev/smoke_outputs/ for post-mortem debugging.

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
if (nchar(Sys.getenv("OPENAI_API_KEY")) == 0L) {
  stop("OPENAI_API_KEY env var is not set.", call. = FALSE)
}

# Keep the test small so you can iterate quickly.
N_CORE <- 10
BOOTSTRAP_N <- 12
ROUND_SIZE <- 8
MAX_ROUNDS <- 3

`%||%` <- function(x, y) if (is.null(x)) y else x
.ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.ts_compact <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
.dbg <- function(...) message("[", .ts(), "] ", paste0(..., collapse = ""))

# Output dir
out_dir <- file.path(getwd(), "dev", "smoke_outputs", paste0("smoke_", .ts_compact()))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
.dbg("Smoke outputs will be written to: ", out_dir)

write_bt_run_diagnostics <- function(out, dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, file.path(dir, paste0(name, "_out.rds")))
  if (!is.null(out$metrics)) write_csv(out$metrics, file.path(dir, paste0(name, "_metrics.csv")))
  if (!is.null(out$state)) write_csv(out$state, file.path(dir, paste0(name, "_state.csv")))
  if (!is.null(out$results)) write_csv(out$results, file.path(dir, paste0(name, "_results.csv")))
  if (!is.null(out$batch_summary)) write_csv(out$batch_summary, file.path(dir, paste0(name, "_batch_summary.csv")))
  invisible(TRUE)
}

.short <- function(x, n = 240) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  ifelse(nchar(x) <= n, x, paste0(substr(x, 1, n), "…"))
}

.print_results_summary <- function(res, backend = "openai", label = "results") {
  if (is.null(res)) {
    .dbg(label, ": <NULL>")
    return(invisible(NULL))
  }
  res <- tibble::as_tibble(res)

  n_rows <- nrow(res)
  n_missing_winner <- if ("better_id" %in% names(res)) sum(is.na(res$better_id) | res$better_id == "", na.rm = TRUE) else NA_integer_
  n_missing_id <- if (all(c("ID1","ID2") %in% names(res))) sum(is.na(res$ID1) | res$ID1 == "" | is.na(res$ID2) | res$ID2 == "", na.rm = TRUE) else NA_integer_
  n_invalid_winner <- NA_integer_
  if (all(c("ID1", "ID2", "better_id") %in% names(res))) {
    n_invalid_winner <- sum(!(res$better_id %in% c(res$ID1, res$ID2)) & !(is.na(res$better_id) | res$better_id == ""), na.rm = TRUE)
  }

  .dbg(label, ": n=", n_rows,
       " | missing_winner=", n_missing_winner,
       " | missing_id=", n_missing_id,
       " | invalid_winner=", n_invalid_winner)

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

  bad <- res %>%
    mutate(.missing_winner = ("better_id" %in% names(res)) && (is.na(better_id) | better_id == "")) %>%
    filter(.missing_winner) %>%
    select(any_of(c("ID1", "ID2", "better_id", "better_sample", "status_code", "error_message", "content"))) %>%
    head(5)

  if (nrow(bad) > 0) {
    .dbg(label, ": example rows with missing winner:")
    print(bad %>% mutate(content = .short(content, 280)))
  }

  invisible(list(
    n_rows = n_rows,
    n_missing_winner = n_missing_winner,
    n_missing_id = n_missing_id,
    n_invalid_winner = n_invalid_winner
  ))
}

# ---- Minimal samples ----
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

# ---- Trait + Prompt template (structured) ----
td <- trait_description("overall_quality")
tmpl <- get_prompt_template("default")

# ---- Judge functions (live + batch) ----
judge_openai_live <- function(pairs) {
  t0 <- Sys.time()

  out <- submit_llm_pairs(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4o-mini",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions",
    temperature = 0,
    include_raw = TRUE,
    status_every = 1L
  )

  .dbg("judge_openai_live: elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s")
  .print_results_summary(out$results, backend = "openai", label = "openai_live")
  out
}

judge_openai_batch <- function(pairs) {
  t0 <- Sys.time()

  batch <- llm_submit_pairs_batch(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4o-mini",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    temperature = 0
  )

  .dbg("Submitted batch: ", batch$batch_id %||% "<no batch_id>",
       " | status=", batch$status %||% "<no status>")

  Sys.sleep(20)

  res <- llm_download_batch_results(batch)
  .dbg("judge_openai_batch: elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s")
  .print_results_summary(res, backend = "openai", label = "openai_batch")

  res
}

# ---- Runner call ----
run_one <- function(judge_fun, label) {
  .dbg("===== Running: ", label, " =====")

  # Provide explicit core IDs to bypass select_core_set() (and any embeddings requirements).
  core_ids <- samples$ID[seq_len(min(5L, nrow(samples)))]
  new_ids <- setdiff(samples$ID, core_ids)

  # Runner expects batches to be the *new IDs introduced per batch*.
  batches <- list(new_ids)

  out <- tryCatch(
    {
      bt_run_adaptive_core_linking(
        samples = samples,
        batches = batches,
        judge_fun = judge_fun,

        core_ids = core_ids,

        engine = "auto",
        fit_verbose = TRUE,

        round_size = ROUND_SIZE,
        init_round_size = BOOTSTRAP_N,
        max_rounds_per_batch = MAX_ROUNDS,

        # Debug-friendly defaults:
        forbid_repeats = TRUE,
        balance_positions = TRUE,
        k_neighbors = Inf,
        min_judgments = NULL,

        linking = "never",
        seed_pairs = 123,
        return_diagnostics = TRUE
      )
    },
    error = function(e) {
      .dbg("ERROR in ", label, ": ", conditionMessage(e))
      .dbg("Backtrace (best-effort):")
      if (requireNamespace("rlang", quietly = TRUE)) {
        tr <- try(rlang::last_trace(drop = FALSE), silent = TRUE)
        if (inherits(tr, "try-error")) {
          traceback()
        } else {
          print(tr)
        }
      } else {
        traceback()
      }
      stop(e)
    }
  )

  .dbg("Finished: ", label)
  .dbg("Results rows: ", if (!is.null(out$results)) nrow(out$results) else NA_integer_)

  if (!is.null(out$metrics) && nrow(out$metrics) > 0L) {
    last <- tail(out$metrics, 1)
    mj <- if ("min_judgments" %in% names(last)) last$min_judgments else NA
    .dbg("Last metrics: batch=", last$batch_index %||% NA,
         " round=", last$round_index %||% NA,
         " stop_reason=", out$stop_reason %||% NA,
         " | min_judgments=", mj)
  }

  out
}

# ---- Execute ----
out_live <- run_one(judge_openai_live, "adaptive_core_linking + openai_live")

write_bt_run_diagnostics(out_live, out_dir, "openai_live")
message("\nSaved smoke test outputs to: ", out_dir)

# Batch (optional; slower)
# out_batch <- run_one(judge_openai_batch, "adaptive_core_linking + openai_batch")
# write_bt_run_diagnostics(out_batch, out_dir, "openai_batch")
