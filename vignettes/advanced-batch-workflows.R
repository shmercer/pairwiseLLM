## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(pairwiseLLM)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

## -----------------------------------------------------------------------------
check_llm_api_keys()

## -----------------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")

td <- trait_description("overall_quality")
td

## -----------------------------------------------------------------------------
tmpl <- set_prompt_template()
cat(substr(tmpl, 1, 400), "...\n")

## -----------------------------------------------------------------------------
set.seed(123)

pairs_all <- example_writing_samples |>
  make_pairs()

n_pairs <- min(40L, nrow(pairs_all))

pairs_forward <- pairs_all |>
  sample_pairs(n_pairs = n_pairs, seed = 123) |>
  randomize_pair_order(seed = 456)

pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1.0,
  seed        = 789
)

get_pairs_for_direction <- function(direction = c("forward", "reverse")) {
  direction <- match.arg(direction)
  if (identical(direction, "forward")) {
    pairs_forward
  } else {
    pairs_reverse
  }
}

## -----------------------------------------------------------------------------
anthropic_models <- c(
  "claude-sonnet-4-5",
  "claude-haiku-4-5",
  "claude-opus-4-5"
)

gemini_models <- c(
  "gemini-3-pro-preview"
)

openai_models <- c(
  "gpt-4.1",
  "gpt-4o",
  "gpt-5.1"
)

thinking_levels <- c("no_thinking", "with_thinking")
directions <- c("forward", "reverse")

anthropic_grid <- tidyr::expand_grid(
  provider  = "anthropic",
  model     = anthropic_models,
  thinking  = thinking_levels,
  direction = directions
)

gemini_grid <- tidyr::expand_grid(
  provider  = "gemini",
  model     = gemini_models,
  thinking  = "with_thinking",
  direction = directions
)

openai_grid <- tidyr::expand_grid(
  provider  = "openai",
  model     = openai_models,
  thinking  = thinking_levels,
  direction = directions
) |>
  # For example, only allow "with_thinking" for gpt-5.1
  dplyr::filter(model == "gpt-5.1" | thinking == "no_thinking")

batch_grid <- dplyr::bind_rows(
  anthropic_grid,
  gemini_grid,
  openai_grid
)

batch_grid

## -----------------------------------------------------------------------------
templates_tbl <- tibble::tibble(
  template_id     = c("test1", "test2", "test3", "test4", "test5"),
  prompt_template = list(tmpl, tmpl, tmpl, tmpl, tmpl)
)

templates_tbl

## ----eval=FALSE---------------------------------------------------------------
# out_dir <- "dev-output/advanced-multi-batch"
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# jobs <- list()
#
# for (t_row in seq_len(nrow(templates_tbl))) {
#   template_id <- templates_tbl$template_id[t_row]
#   tmpl_string <- templates_tbl$prompt_template[[t_row]]
#
#   for (i in seq_len(nrow(batch_grid))) {
#     row <- batch_grid[i, ]
#
#     provider <- row$provider
#     model <- row$model
#     thinking <- row$thinking
#     direction <- row$direction
#
#     message(
#       "Submitting batch: template=", template_id,
#       " | ", provider, " / ", model,
#       " / ", thinking, " / ", direction
#     )
#
#     pairs_use <- get_pairs_for_direction(direction)
#     is_thinking <- identical(thinking, "with_thinking")
#
#     prefix <- paste(provider, template_id, model, thinking, direction,
#       sep = "_"
#     )
#     prefix <- gsub("[^A-Za-z0-9_.-]", "-", prefix)
#
#     batch_input_path <- file.path(out_dir, paste0(prefix, "_input.jsonl"))
#     batch_output_path <- file.path(out_dir, paste0(prefix, "_output.jsonl"))
#     csv_path <- file.path(out_dir, paste0(prefix, ".csv"))
#
#     if (identical(provider, "openai")) {
#       # OpenAI: use the helpers from the dev scripts
#       include_thoughts <- is_thinking && grepl("^gpt-5\\.1", model)
#
#       pipeline <- run_openai_batch_pipeline(
#         pairs             = pairs_use,
#         model             = model,
#         trait_name        = td$name,
#         trait_description = td$description,
#         prompt_template   = tmpl_string,
#         include_thoughts  = include_thoughts,
#         include_raw       = TRUE,
#         batch_input_path  = batch_input_path,
#         batch_output_path = batch_output_path,
#         poll              = FALSE
#       )
#
#       jobs[[length(jobs) + 1L]] <- list(
#         template_id       = template_id,
#         provider          = provider,
#         model             = model,
#         thinking          = thinking,
#         direction         = direction,
#         prefix            = prefix,
#         batch_type        = "openai",
#         batch_id          = pipeline$batch$id,
#         batch_input_path  = pipeline$batch_input_path,
#         batch_output_path = batch_output_path,
#         csv_path          = csv_path,
#         done              = FALSE,
#         results           = NULL
#       )
#     } else if (identical(provider, "anthropic")) {
#       # Anthropic: use run_anthropic_batch_pipeline()
#       reasoning <- if (is_thinking) "enabled" else "none"
#       temperature_arg <- if (!is_thinking) 0 else NULL
#
#       pipeline <- run_anthropic_batch_pipeline(
#         pairs             = pairs_use,
#         model             = model,
#         trait_name        = td$name,
#         trait_description = td$description,
#         prompt_template   = tmpl_string,
#         reasoning         = reasoning,
#         include_thoughts  = is_thinking,
#         batch_input_path  = batch_input_path,
#         batch_output_path = batch_output_path,
#         poll              = FALSE,
#         temperature       = temperature_arg,
#         include_raw       = TRUE
#       )
#
#       jobs[[length(jobs) + 1L]] <- list(
#         template_id       = template_id,
#         provider          = provider,
#         model             = model,
#         thinking          = thinking,
#         direction         = direction,
#         prefix            = prefix,
#         batch_type        = "anthropic",
#         batch_id          = pipeline$batch$id,
#         batch_input_path  = pipeline$batch_input_path,
#         batch_output_path = batch_output_path,
#         csv_path          = csv_path,
#         done              = FALSE,
#         results           = NULL
#       )
#     } else if (identical(provider, "gemini")) {
#       # Gemini: typically use low-level helpers, as in the dev scripts
#       req_tbl <- build_gemini_batch_requests(
#         pairs             = pairs_use,
#         model             = model,
#         trait_name        = td$name,
#         trait_description = td$description,
#         prompt_template   = tmpl_string,
#         thinking_level    = "low", # example
#         include_thoughts  = TRUE
#       )
#
#       batch <- gemini_create_batch(
#         requests    = req_tbl$request,
#         model       = model,
#         api_key     = Sys.getenv("GEMINI_API_KEY"),
#         api_version = "v1beta"
#       )
#
#       batch_name <- batch$name %||% stop(
#         "Gemini batch did not return a `name` field.",
#         call. = FALSE
#       )
#
#       jobs[[length(jobs) + 1L]] <- list(
#         template_id       = template_id,
#         provider          = provider,
#         model             = model,
#         thinking          = thinking,
#         direction         = direction,
#         prefix            = prefix,
#         batch_type        = "gemini",
#         batch_id          = batch_name,
#         batch_input_path  = batch_input_path,
#         batch_output_path = batch_output_path,
#         csv_path          = csv_path,
#         done              = FALSE,
#         results           = NULL
#       )
#     }
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# jobs_tbl <- tibble::tibble(
#   idx = seq_along(jobs),
#   template_id = vapply(jobs, `[[`, character(1), "template_id"),
#   provider = vapply(jobs, `[[`, character(1), "provider"),
#   model = vapply(jobs, `[[`, character(1), "model"),
#   thinking = vapply(jobs, `[[`, character(1), "thinking"),
#   direction = vapply(jobs, `[[`, character(1), "direction"),
#   prefix = vapply(jobs, `[[`, character(1), "prefix"),
#   batch_type = vapply(jobs, `[[`, character(1), "batch_type"),
#   batch_id = vapply(jobs, `[[`, character(1), "batch_id"),
#   batch_input_path = vapply(jobs, `[[`, character(1), "batch_input_path"),
#   batch_output_path = vapply(jobs, `[[`, character(1), "batch_output_path"),
#   csv_path = vapply(jobs, `[[`, character(1), "csv_path")
# )
#
# jobs_index_path <- file.path(out_dir, "batch_jobs_index.csv")
# readr::write_csv(jobs_tbl, jobs_index_path)
#
# jobs_index_path

## ----eval=FALSE---------------------------------------------------------------
# is_terminal_openai <- function(status) {
#   status %in% c("completed", "failed", "cancelled", "expired")
# }
#
# is_terminal_anthropic <- function(status) {
#   status %in% c("ended", "errored", "canceled", "expired")
# }
#
# is_terminal_gemini <- function(state) {
#   state %in% c("SUCCEEDED", "FAILED", "CANCELLED", "EXPIRED")
# }

## ----eval=FALSE---------------------------------------------------------------
# interval_seconds <- 60
# per_job_delay <- 2 # seconds between polling calls
#
# # Reload batch index
# jobs_index_path <- file.path(out_dir, "batch_jobs_index.csv")
# jobs_tbl <- readr::read_csv(jobs_index_path, show_col_types = FALSE)
#
# # Rebuild jobs list skeleton
# jobs <- purrr::pmap(
#   jobs_tbl,
#   function(idx, template_id, provider, model, thinking, direction,
#            prefix, batch_type, batch_id,
#            batch_input_path, batch_output_path, csv_path, ...) {
#     list(
#       template_id       = template_id,
#       provider          = provider,
#       model             = model,
#       thinking          = thinking,
#       direction         = direction,
#       prefix            = prefix,
#       batch_type        = batch_type,
#       batch_id          = batch_id,
#       batch_input_path  = batch_input_path,
#       batch_output_path = batch_output_path,
#       csv_path          = csv_path,
#       done              = FALSE,
#       results           = NULL
#     )
#   }
# )
#
# unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))
#
# while (length(unfinished) > 0L) {
#   message("Polling ", length(unfinished), " unfinished batch(es)...")
#
#   for (j in unfinished) {
#     job <- jobs[[j]]
#     if (job$done) next
#
#     batch_type <- job$batch_type
#
#     if (identical(batch_type, "openai")) {
#       batch <- openai_get_batch(job$batch_id)
#       status <- batch$status %||% "unknown"
#       message("  [OpenAI] ", job$prefix, " status: ", status)
#
#       if (is_terminal_openai(status)) {
#         if (identical(status, "completed")) {
#           openai_download_batch_output(
#             batch_id = job$batch_id,
#             path     = job$batch_output_path
#           )
#
#           res <- parse_openai_batch_output(job$batch_output_path)
#           jobs[[j]]$results <- res
#           readr::write_csv(res, job$csv_path)
#           message("    -> Results written to: ", job$csv_path)
#         }
#         jobs[[j]]$done <- TRUE
#       }
#     } else if (identical(batch_type, "anthropic")) {
#       batch <- anthropic_get_batch(job$batch_id)
#       status <- batch$processing_status %||% "unknown"
#       message("  [Anthropic] ", job$prefix, " status: ", status)
#
#       if (is_terminal_anthropic(status)) {
#         if (identical(status, "ended")) {
#           output_path <- anthropic_download_batch_results(
#             batch_id    = job$batch_id,
#             output_path = job$batch_output_path
#           )
#
#           res <- parse_anthropic_batch_output(
#             jsonl_path  = output_path,
#             tag_prefix  = "<BETTER_SAMPLE>",
#             tag_suffix  = "</BETTER_SAMPLE>"
#           )
#
#           jobs[[j]]$results <- res
#           readr::write_csv(res, job$csv_path)
#           message("    -> Results written to: ", job$csv_path)
#         }
#         jobs[[j]]$done <- TRUE
#       }
#     } else if (identical(batch_type, "gemini")) {
#       batch <- gemini_get_batch(job$batch_id)
#       state <- batch$state %||% "STATE_UNSPECIFIED"
#       message("  [Gemini] ", job$prefix, " state: ", state)
#
#       if (is_terminal_gemini(state)) {
#         if (identical(state, "SUCCEEDED")) {
#           raw_res <- gemini_download_batch_results(job$batch_id)
#
#           res <- parse_gemini_batch_output(
#             raw_results = raw_res,
#             tag_prefix  = "<BETTER_SAMPLE>",
#             tag_suffix  = "</BETTER_SAMPLE>"
#           )
#
#           jobs[[j]]$results <- res
#           readr::write_csv(res, job$csv_path)
#           message("    -> Results written to: ", job$csv_path)
#         }
#         jobs[[j]]$done <- TRUE
#       }
#     }
#
#     Sys.sleep(per_job_delay)
#   }
#
#   unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))
#
#   if (length(unfinished) > 0L) {
#     message("Sleeping ", interval_seconds, " seconds before next poll...")
#     Sys.sleep(interval_seconds)
#   }
# }
#
# message("All batches have reached a terminal state.")

## ----eval=FALSE---------------------------------------------------------------
# jobs_index_path <- file.path(out_dir, "batch_jobs_index.csv")
# jobs_tbl <- readr::read_csv(jobs_index_path, show_col_types = FALSE)
#
# # Rebuild jobs list as before...
# # Then:
# unfinished <- which(!vapply(jobs, `[[`, logical(1), "done"))
#
# if (length(unfinished) > 0L) {
#   message("Resuming polling for ", length(unfinished), " unfinished batch(es).")
#   # ... re-enter the polling loop ...
# } else {
#   message("All jobs are already complete.")
# }

## ----eval=FALSE---------------------------------------------------------------
#
# jobs <- purrr::pmap(batch_grid, function(provider, model, thinking, direction) {
#   pairs_use <- get_pairs_for_direction(direction)
#   llm_submit_pairs_multi_batch(
#     pairs              = pairs_use,
#     backend            = provider,
#     model              = model,
#     trait_name         = td$name,
#     trait_description  = td$description,
#     prompt_template    = tmpl,
#     n_segments         = 1,
#     output_dir         = out_dir,
#     write_registry     = TRUE,
#     include_thoughts   = (thinking == "with_thinking")
#   )$jobs[[1]]
# })

## ----eval=FALSE---------------------------------------------------------------
# results <- llm_resume_multi_batches(
#   jobs               = jobs,
#   interval_seconds   = 60,
#   write_results_csv  = TRUE,
#   write_combined_csv = TRUE
# )
# }
