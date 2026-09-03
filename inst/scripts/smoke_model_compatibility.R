#!/usr/bin/env Rscript

# Opt-in, billable compatibility smoke tests. This script is not run by R CMD
# check or testthat. Example:
# PAIRWISELLM_RUN_PROVIDER_SMOKE=true \
#   Rscript inst/scripts/smoke_model_compatibility.R \
#   --mode=live --providers=openai,anthropic

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0L) return(default)
  sub(paste0("^--", name, "="), "", hit[[1L]])
}

enabled <- tolower(Sys.getenv("PAIRWISELLM_RUN_PROVIDER_SMOKE", unset = "false")) %in%
  c("1", "true", "yes", "on")
if (!enabled) {
  stop(
    paste0(
      "Provider smoke tests are billable and create external API work. Set ",
      "PAIRWISELLM_RUN_PROVIDER_SMOKE=true to opt in."
    ),
    call. = FALSE
  )
}

mode_filter <- arg_value("mode", "live")
if (!mode_filter %in% c("live", "batch", "all")) {
  stop("--mode must be live, batch, or all.", call. = FALSE)
}
provider_filter <- strsplit(arg_value("providers", "all"), ",", fixed = TRUE)[[1L]]
output_path <- arg_value(
  "output",
  file.path("tasklists", "evidence", "model-smoke-results.csv")
)

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
  library(pairwiseLLM)
}

matrix_path <- file.path("inst", "extdata", "model_smoke_matrix.csv")
smoke_matrix <- utils::read.csv(matrix_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!identical(mode_filter, "all")) {
  smoke_matrix <- smoke_matrix[smoke_matrix$mode == mode_filter, , drop = FALSE]
}
if (!identical(provider_filter, "all")) {
  smoke_matrix <- smoke_matrix[
    smoke_matrix$backend %in% tolower(provider_filter),
    ,
    drop = FALSE
  ]
}
if (nrow(smoke_matrix) == 0L) stop("No smoke-test rows matched the filters.", call. = FALSE)

pairs <- tibble::tibble(
  ID1 = "clear",
  text1 = "A direct, specific answer that fully addresses the question.",
  ID2 = "vague",
  text2 = "A vague and repetitive answer that does not address the question."
)
trait_name <- "Overall Quality"
trait_description <- "Prefer the clearer, more direct, and more responsive answer."
prompt_template <- paste(
  "The two samples are included below. Judge {TRAIT_NAME}: {TRAIT_DESCRIPTION}",
  "--- SAMPLE_1 START ---\n{SAMPLE_1}\n--- SAMPLE_1 END ---",
  "--- SAMPLE_2 START ---\n{SAMPLE_2}\n--- SAMPLE_2 END ---",
  "Return only <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> or",
  "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>.",
  sep = "\n\n"
)

run_live <- function(row) {
  common <- list(
    ID1 = pairs$ID1[[1L]], text1 = pairs$text1[[1L]],
    ID2 = pairs$ID2[[1L]], text2 = pairs$text2[[1L]],
    model = row$model_id, trait_name = trait_name,
    trait_description = trait_description, prompt_template = prompt_template,
    backend = row$backend, include_raw = FALSE
  )
  extra <- switch(
    row$test_id,
    openai_chat_live = list(endpoint = "chat.completions"),
    openai_responses_live = list(
      endpoint = "responses", reasoning = "none", include_thoughts = FALSE,
      max_output_tokens = 256
    ),
    anthropic_standard_live = list(reasoning = "none", max_tokens = 256),
    anthropic_thinking_live = list(
      reasoning = "enabled", include_thoughts = TRUE, temperature = 1,
      max_tokens = 2048, thinking_budget_tokens = 1024
    ),
    gemini_thinking_live = list(
      thinking_level = "low", include_thoughts = FALSE, max_output_tokens = 256
    ),
    vertex_standard_live = list(
      thinking_level = NULL, thinking_budget = 0L, include_thoughts = FALSE,
      max_output_tokens = 256
    ),
    together_standard_live = list(max_tokens = 512),
    stop("Unknown live test_id: ", row$test_id, call. = FALSE)
  )
  do.call(pairwiseLLM::llm_compare_pair, c(common, extra))
}

run_batch <- function(row) {
  common <- list(
    pairs = pairs, backend = row$backend, model = row$model_id,
    trait_name = trait_name, trait_description = trait_description,
    prompt_template = prompt_template, include_raw = FALSE, poll = TRUE,
    interval_seconds = 10, timeout_seconds = 3600
  )
  extra <- switch(
    row$test_id,
    openai_responses_batch = list(
      endpoint = "responses", reasoning = "none", include_thoughts = FALSE
    ),
    anthropic_standard_batch = list(
      reasoning = "none", include_thoughts = FALSE, max_tokens = 256
    ),
    gemini_thinking_batch = list(
      thinking_level = "low", include_thoughts = FALSE, max_output_tokens = 256
    ),
    stop("Unknown batch test_id: ", row$test_id, call. = FALSE)
  )
  do.call(pairwiseLLM::llm_submit_pairs_batch, c(common, extra))
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
results <- vector("list", nrow(smoke_matrix))
for (i in seq_len(nrow(smoke_matrix))) {
  row <- smoke_matrix[i, , drop = FALSE]
  started <- Sys.time()
  has_key <- nzchar(Sys.getenv(row$env_var, unset = ""))
  outcome <- if (!has_key) {
    list(status = "skipped-no-key", status_code = NA_integer_, parsed_winner = FALSE,
         error = paste(row$env_var, "is not set"))
  } else {
    tryCatch({
      value <- if (identical(row$mode, "live")) run_live(row) else run_batch(row)
      result_tbl <- if (identical(row$mode, "live")) value else value$results
      parsed <- !is.null(result_tbl) && nrow(result_tbl) > 0L &&
        "better_id" %in% names(result_tbl) && !is.na(result_tbl$better_id[[1L]])
      code <- if (!is.null(result_tbl) && "status_code" %in% names(result_tbl)) {
        as.integer(result_tbl$status_code[[1L]])
      } else {
        NA_integer_
      }
      error_text <- if (!is.null(result_tbl) && "error_message" %in% names(result_tbl) &&
        !is.na(result_tbl$error_message[[1L]])) {
        as.character(result_tbl$error_message[[1L]])
      } else if (!isTRUE(parsed)) {
        "Response did not contain a parsed winner."
      } else {
        NA_character_
      }
      status <- if (isTRUE(parsed)) {
        "passed"
      } else if (!is.na(error_text) && nzchar(error_text)) {
        "failed-error-row"
      } else {
        "failed-no-parsed-winner"
      }
      list(
        status = status, status_code = code, parsed_winner = isTRUE(parsed),
        error = error_text
      )
    }, error = function(e) {
      list(status = "failed-error", status_code = NA_integer_, parsed_winner = FALSE,
           error = conditionMessage(e))
    })
  }
  results[[i]] <- data.frame(
    row,
    package_version = as.character(utils::packageVersion("pairwiseLLM")),
    test_date = format(Sys.Date(), "%Y-%m-%d"),
    started_at_utc = format(started, tz = "UTC", usetz = TRUE),
    duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    status = outcome$status,
    status_code = outcome$status_code,
    parsed_winner = outcome$parsed_winner,
    error = outcome$error,
    check.names = FALSE
  )
  current <- do.call(rbind, results[seq_len(i)])
  utils::write.csv(current, output_path, row.names = FALSE, na = "")
  message(row$test_id, ": ", outcome$status)
}

if (any(vapply(results, function(x) grepl("^failed", x$status), logical(1)))) {
  quit(status = 1L, save = "no")
}
