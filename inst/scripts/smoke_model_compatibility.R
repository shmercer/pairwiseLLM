#!/usr/bin/env Rscript

# Opt-in, billable compatibility smoke tests. Use --list=true to inspect the
# selected matrix without credentials, network access, or billable work.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0L) return(default)
  sub(paste0("^--", name, "="), "", hit[[1L]])
}

arg_flag <- function(name, default = FALSE) {
  value <- tolower(arg_value(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false")) {
    stop("--", name, " must be true or false.", call. = FALSE)
  }
  identical(value, "true")
}

arg_number <- function(name, default, minimum = 0) {
  value <- suppressWarnings(as.numeric(arg_value(name, as.character(default))))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < minimum) {
    stop("--", name, " must be a number >= ", minimum, ".", call. = FALSE)
  }
  value
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else ""
# Rscript encodes spaces as `~+~` in the --file argument on some platforms.
script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else file.path("inst", "scripts")
source(file.path(script_dir, "smoke_model_compatibility_helpers.R"))

mode_filter <- arg_value("mode", "live")
if (!mode_filter %in% c("live", "batch", "all")) {
  stop("--mode must be live, batch, or all.", call. = FALSE)
}
provider_filter <- trimws(tolower(strsplit(
  arg_value("providers", "all"), ",", fixed = TRUE
)[[1L]]))
allow_missing_keys <- arg_flag("allow-missing-keys")
list_only <- arg_flag("list")
retry_failures <- arg_flag("retry-failures")
request_delay_seconds <- arg_number("request-delay-seconds", 1)
poll_interval_seconds <- arg_number("poll-interval-seconds", 10, 0.1)
timeout_seconds <- arg_number("timeout-seconds", 3600, 0.1)
output_path <- arg_value(
  "output", file.path("tasklists", "evidence", "model-smoke-results.csv")
)

smoke_matrix <- utils::read.csv(
  file.path("inst", "extdata", "model_smoke_matrix.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
validate_smoke_matrix(smoke_matrix)
if (!identical(mode_filter, "all")) {
  smoke_matrix <- smoke_matrix[smoke_matrix$mode == mode_filter, , drop = FALSE]
}
if (!identical(provider_filter, "all")) {
  smoke_matrix <- smoke_matrix[smoke_matrix$backend %in% provider_filter, , drop = FALSE]
}
if (!nrow(smoke_matrix)) stop("No smoke-test rows matched the filters.", call. = FALSE)

if (list_only) {
  print(smoke_matrix[c(
    "test_id", "backend", "model_id", "mode", "endpoint",
    "request_profile", "reasoning_mode"
  )], row.names = FALSE)
  quit(status = 0L, save = "no")
}

enabled <- tolower(Sys.getenv("PAIRWISELLM_RUN_PROVIDER_SMOKE", unset = "false")) %in%
  c("1", "true", "yes", "on")
if (!enabled) {
  stop(
    paste0(
      "Provider smoke tests are billable and create external API work. Set ",
      "PAIRWISELLM_RUN_PROVIDER_SMOKE=true to opt in."
    ), call. = FALSE
  )
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
  library(pairwiseLLM)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
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
  "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>.", sep = "\n\n"
)

pace <- function() {
  if (request_delay_seconds > 0) Sys.sleep(request_delay_seconds)
  invisible(NULL)
}

profile_args <- function(profile) {
  switch(
    profile,
    openai_responses_default = list(
      endpoint = "responses", include_thoughts = FALSE, max_output_tokens = 4096
    ),
    openai_chat_default = list(endpoint = "chat.completions"),
    anthropic_standard = list(reasoning = "none", max_tokens = 2048),
    anthropic_thinking = list(
      reasoning = "enabled", include_thoughts = TRUE, temperature = 1,
      max_tokens = 2048, thinking_budget_tokens = 1024
    ),
    gemini_low = list(
      thinking_level = "low", include_thoughts = FALSE, max_output_tokens = 2048
    ),
    vertex_gemini3_low = list(
      thinking_level = "low", include_thoughts = FALSE, max_output_tokens = 2048
    ),
    vertex_gemini25_no_thinking = list(
      thinking_level = NULL, thinking_budget = 0L, include_thoughts = FALSE,
      max_output_tokens = 2048
    ),
    together_standard = list(max_tokens = 4096),
    stop("Unknown request profile: ", profile, call. = FALSE)
  )
}

outcome_from_results <- function(result_tbl) {
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
  } else if (!parsed) {
    "Response did not contain a parsed winner."
  } else {
    NA_character_
  }
  list(
    status = if (parsed) "passed" else "failed-error-row",
    status_code = code, parsed_winner = parsed, error = error_text
  )
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
runtime <- smoke_matrix
runtime$package_version <- as.character(utils::packageVersion("pairwiseLLM"))
runtime$test_date <- format(Sys.Date(), "%Y-%m-%d")
runtime$started_at_utc <- NA_character_
runtime$duration_seconds <- NA_real_
runtime$status <- "pending"
runtime$status_code <- NA_integer_
runtime$parsed_winner <- FALSE
runtime$error <- NA_character_
runtime$remote_id <- NA_character_
runtime$remote_status <- NA_character_

if (file.exists(output_path)) {
  previous <- utils::read.csv(output_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (all(c("test_id", "model_id", "mode", "status") %in% names(previous))) {
    for (i in seq_len(nrow(runtime))) {
      hit <- which(previous$test_id == runtime$test_id[[i]] &
        previous$model_id == runtime$model_id[[i]] & previous$mode == runtime$mode[[i]])
      if (length(hit) == 1L) {
        resume_columns <- c(
          "started_at_utc", "duration_seconds", "status", "status_code",
          "parsed_winner", "error", "remote_id", "remote_status"
        )
        resume_columns <- intersect(resume_columns, names(previous))
        runtime[i, resume_columns] <- previous[hit, resume_columns]
      }
    }
  }
}

failure_statuses <- c(
  "failed-error", "failed-error-row", "failed-submission", "failed-remote",
  "failed-download"
)
if (retry_failures) {
  retry <- runtime$status %in% failure_statuses
  runtime$status[retry] <- "pending"
  runtime$error[retry] <- NA_character_
  runtime$remote_id[retry] <- NA_character_
  runtime$remote_status[retry] <- NA_character_
}
persist_runtime <- function() {
  utils::write.csv(runtime, output_path, row.names = FALSE, na = "")
}
persist_runtime()

live_indices <- which(runtime$mode == "live")
for (i in live_indices) {
  if (!identical(runtime$status[[i]], "pending")) next
  row <- runtime[i, , drop = FALSE]
  started <- Sys.time()
  runtime$started_at_utc[[i]] <- format(started, tz = "UTC", usetz = TRUE)
  if (!nzchar(Sys.getenv(row$env_var, unset = ""))) {
    outcome <- list(
      status = "skipped-no-key", status_code = NA_integer_, parsed_winner = FALSE,
      error = paste(row$env_var, "is not set")
    )
  } else {
    common <- list(
      ID1 = pairs$ID1[[1L]], text1 = pairs$text1[[1L]],
      ID2 = pairs$ID2[[1L]], text2 = pairs$text2[[1L]],
      model = row$model_id, trait_name = trait_name,
      trait_description = trait_description, prompt_template = prompt_template,
      backend = row$backend, include_raw = FALSE
    )
    outcome <- tryCatch({
      value <- do.call(
        pairwiseLLM::llm_compare_pair,
        c(common, profile_args(row$request_profile))
      )
      outcome_from_results(value)
    }, error = function(e) {
      list(
        status = "failed-error", status_code = NA_integer_, parsed_winner = FALSE,
        error = conditionMessage(e)
      )
    })
    pace()
  }
  runtime$duration_seconds[[i]] <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  runtime$status[[i]] <- outcome$status
  runtime$status_code[[i]] <- outcome$status_code
  runtime$parsed_winner[[i]] <- outcome$parsed_winner
  runtime$error[[i]] <- outcome$error
  persist_runtime()
  message(row$test_id, ": ", outcome$status)
}

artifact_dir <- paste0(tools::file_path_sans_ext(output_path), "-artifacts")
safe_id <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)
input_path <- function(row, extension) {
  file.path(artifact_dir, paste0(safe_id(row$test_id), "-input.", extension))
}
output_file <- function(row) {
  file.path(artifact_dir, paste0(safe_id(row$test_id), "-output.jsonl"))
}

batch_indices <- which(runtime$mode == "batch")
batch_rows <- runtime[batch_indices, , drop = FALSE]
if (nrow(batch_rows) > 0L) {
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  states <- setNames(lapply(batch_indices, function(i) {
    status <- runtime$status[[i]]
    if (identical(status, "skipped-no-key") &&
      nzchar(Sys.getenv(runtime$env_var[[i]], unset = ""))) status <- "pending"
    state <- new_batch_state(
      status, runtime$remote_id[[i]], runtime$remote_status[[i]], runtime$error[[i]]
    )
    state$status_code <- runtime$status_code[[i]]
    state$parsed_winner <- runtime$parsed_winner[[i]]
    state
  }), batch_rows$test_id)
  for (id in names(states)) {
    i <- match(id, runtime$test_id)
    if (identical(states[[id]]$status, "pending") &&
      !nzchar(Sys.getenv(runtime$env_var[[i]], unset = ""))) {
      states[[id]]$status <- "skipped-no-key"
      states[[id]]$error <- paste(runtime$env_var[[i]], "is not set")
    }
  }

  build_anthropic_requests <- function(row) {
    args <- c(list(
      pairs = pairs, model = row$model_id, trait_name = trait_name,
      trait_description = trait_description, prompt_template = prompt_template
    ), profile_args(row$request_profile))
    req_tbl <- do.call(pairwiseLLM::build_anthropic_batch_requests, args)
    lapply(seq_len(nrow(req_tbl)), function(i) {
      list(custom_id = req_tbl$custom_id[[i]], params = req_tbl$params[[i]])
    })
  }

  build_gemini_requests <- function(row) {
    do.call(pairwiseLLM::build_gemini_batch_requests, c(list(
      pairs = pairs, model = row$model_id, trait_name = trait_name,
      trait_description = trait_description, prompt_template = prompt_template
    ), profile_args(row$request_profile)))
  }

  submit_one <- function(row) {
    profile <- profile_args(row$request_profile)
    if (identical(row$backend, "openai")) {
      profile$max_output_tokens <- NULL
      req_tbl <- do.call(pairwiseLLM::build_openai_batch_requests, c(list(
        pairs = pairs, model = row$model_id, trait_name = trait_name,
        trait_description = trait_description, prompt_template = prompt_template
      ), profile))
      path <- input_path(row, "jsonl")
      pairwiseLLM::write_openai_batch_file(req_tbl, path)
      file_obj <- pairwiseLLM::openai_upload_batch_file(path)
      pace()
      batch <- pairwiseLLM::openai_create_batch(
        file_obj$id, "/v1/responses", metadata = list(smoke_test_id = row$test_id)
      )
      return(list(remote_id = batch$id, remote_status = batch$status))
    }
    if (identical(row$backend, "anthropic")) {
      requests <- build_anthropic_requests(row)
      jsonlite::write_json(
        list(requests = requests), input_path(row, "json"),
        auto_unbox = TRUE, pretty = TRUE, null = "null"
      )
      batch <- pairwiseLLM::anthropic_create_batch(requests)
      return(list(remote_id = batch$id, remote_status = batch$processing_status))
    }
    if (identical(row$backend, "gemini")) {
      req_tbl <- build_gemini_requests(row)
      jsonlite::write_json(
        list(requests = req_tbl$request), input_path(row, "json"),
        auto_unbox = TRUE, pretty = TRUE, null = "null"
      )
      batch <- pairwiseLLM::gemini_create_batch(
        req_tbl$request, row$model_id,
        display_name = paste0("pairwiseLLM-smoke-", safe_id(row$test_id))
      )
      return(list(remote_id = batch$name, remote_status = batch$metadata$state))
    }
    stop("Batch submission is not implemented for backend: ", row$backend, call. = FALSE)
  }

  poll_one <- function(row, state) {
    if (identical(row$backend, "openai")) {
      batch <- pairwiseLLM::openai_get_batch(state$remote_id)
      terminal <- batch$status %in% c("completed", "failed", "expired", "cancelled")
      return(list(
        terminal = terminal, successful = identical(batch$status, "completed"),
        remote_status = batch$status,
        error = if (terminal && !identical(batch$status, "completed")) batch$status else NA_character_
      ))
    }
    if (identical(row$backend, "anthropic")) {
      batch <- pairwiseLLM::anthropic_get_batch(state$remote_id)
      ended <- identical(batch$processing_status, "ended")
      counts <- batch$request_counts
      successful <- ended && (counts$errored %||% 0L) == 0L &&
        (counts$expired %||% 0L) == 0L && (counts$canceled %||% 0L) == 0L
      return(list(
        terminal = ended, successful = successful,
        remote_status = batch$processing_status,
        error = if (ended && !successful) "Anthropic batch ended with unsuccessful requests." else NA_character_
      ))
    }
    batch <- pairwiseLLM::gemini_get_batch(state$remote_id)
    remote_status <- batch$metadata$state %||% NA_character_
    terminal <- remote_status %in% c(
      "BATCH_STATE_SUCCEEDED", "BATCH_STATE_FAILED", "BATCH_STATE_CANCELLED",
      "BATCH_STATE_EXPIRED"
    )
    list(
      terminal = terminal, successful = identical(remote_status, "BATCH_STATE_SUCCEEDED"),
      remote_status = remote_status,
      error = if (terminal && !identical(remote_status, "BATCH_STATE_SUCCEEDED")) {
        remote_status
      } else {
        NA_character_
      }
    )
  }

  download_openai <- function(state, path) {
    batch <- pairwiseLLM::openai_get_batch(state$remote_id)
    pace()
    output_file_id <- batch$output_file_id %||% ""
    if (!nzchar(output_file_id)) stop("Completed OpenAI batch has no output file.", call. = FALSE)
    req <- pairwiseLLM:::.openai_request(paste0("/files/", output_file_id, "/content"))
    resp <- pairwiseLLM:::.openai_req_perform(req)
    writeBin(httr2::resp_body_raw(resp), path)
  }

  download_anthropic <- function(state, path) {
    batch <- pairwiseLLM::anthropic_get_batch(state$remote_id)
    pace()
    results_url <- batch$results_url %||% ""
    if (!nzchar(results_url)) stop("Completed Anthropic batch has no results URL.", call. = FALSE)
    req <- httr2::request(results_url)
    req <- httr2::req_headers(
      req, "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
    resp <- pairwiseLLM:::.anthropic_req_perform(req)
    writeBin(charToRaw(httr2::resp_body_string(resp)), path)
  }

  collect_one <- function(row, state) {
    path <- output_file(row)
    if (identical(row$backend, "openai")) {
      download_openai(state, path)
      results <- pairwiseLLM::parse_openai_batch_output(path)
    } else if (identical(row$backend, "anthropic")) {
      download_anthropic(state, path)
      results <- pairwiseLLM::parse_anthropic_batch_output(path)
    } else {
      req_tbl <- build_gemini_requests(row)
      batch <- pairwiseLLM::gemini_get_batch(state$remote_id)
      pairwiseLLM::gemini_download_batch_results(batch, req_tbl, path)
      results <- pairwiseLLM::parse_gemini_batch_output(path, req_tbl)
    }
    outcome_from_results(results)
  }

  persist_states <- function(states) {
    for (id in names(states)) {
      i <- match(id, runtime$test_id)
      runtime$status[[i]] <<- states[[id]]$status
      runtime$status_code[[i]] <<- states[[id]]$status_code
      runtime$parsed_winner[[i]] <<- states[[id]]$parsed_winner
      runtime$error[[i]] <<- states[[id]]$error
      runtime$remote_id[[i]] <<- states[[id]]$remote_id
      runtime$remote_status[[i]] <<- states[[id]]$remote_status
    }
    persist_runtime()
  }

  states <- orchestrate_smoke_batches(
    batch_rows, states, submit_one, poll_one, collect_one,
    persist = persist_states, pace = pace,
    poll_interval_seconds = poll_interval_seconds,
    timeout_seconds = timeout_seconds
  )
  persist_states(states)
}

statuses <- runtime$status
has_failures <- any(startsWith(statuses, "failed") | statuses %in% c("timed-out", "poll-error"))
has_unallowed_skips <- !allow_missing_keys && any(statuses == "skipped-no-key")
if (has_failures || has_unallowed_skips) quit(status = 1L, save = "no")
