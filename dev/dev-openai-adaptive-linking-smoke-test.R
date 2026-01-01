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

devtools::load_all()

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
.dbg <- function(...) message("[", .ts(), "] ", paste0(..., collapse = ""))

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

  rep <- tryCatch(
    validate_backend_results(
      res,
      backend = backend,
      normalize_winner = TRUE,
      strict = FALSE,
      return_report = TRUE
    ),
    error = function(e) {
      .dbg(label, ": validate_backend_results() error: ", conditionMessage(e))
      list(
        n_rows = nrow(res),
        n_missing_winner = NA_integer_,
        n_missing_id = NA_integer_,
        n_invalid_winner = NA_integer_
      )
    }
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

  has_better <- "better_id" %in% names(res)
  bad <- res %>%
    mutate(.missing_winner = if (has_better) (is.na(.data$better_id) | .data$better_id == "") else TRUE) %>%
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

td <- trait_description("overall_quality")
tmpl <- get_prompt_template("default")

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

run_one <- function(judge_fun, label) {
  .dbg("===== Running: ", label, " =====")

  batches <- list(samples$ID)
  core_ids <- samples$ID[seq_len(min(5L, nrow(samples)))]

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
        if (inherits(tr, "try-error")) traceback() else print(tr)
      } else {
        traceback()
      }
      stop(e)
    }
  )

  .dbg("Finished: ", label)
  .dbg("Results rows: ", nrow(out$results))

  if (!is.null(out$final_fits) && length(out$final_fits) > 0L) {
    last_fit <- out$final_fits[[length(out$final_fits)]]
    if (!is.null(last_fit$theta)) {
      .dbg("Final fit theta head:")
      print(head(last_fit$theta))
    }
  }

  out
}

out_live <- run_one(judge_openai_live, "adaptive_core_linking + openai_live")

out_dir <- file.path(getwd(), "dev", "smoke_outputs", paste0("smoke_", format(Sys.time(), "%Y%m%d_%H%M%S")))
write_bt_run_diagnostics(out_live, out_dir, "openai_live")
message("\nSaved smoke test outputs to: ", out_dir)

# out_batch <- run_one(judge_openai_batch, "adaptive_core_linking + openai_batch")
