## dev/dev-check-together.R
##
## Manual smoke test for Together.ai models and parsing
## ----------------------------------------------------
## - Requires TOGETHER_API_KEY in the environment
## - Uses example_writing_samples and submit_together_pairs_live()
## - Checks status codes, basic parsing, and DeepSeek-R1 <think> handling
## - Prints raw model content for at least one pair per model

library(pairwiseLLM)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(rlang) # for .data in filter()

# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

together_models <- c(
  "deepseek-ai/DeepSeek-R1",
  "deepseek-ai/DeepSeek-V3",
  "moonshotai/Kimi-K2-Instruct-0905",
  "Qwen/Qwen3-235B-A22B-Instruct-2507-tput"
)

n_pairs_per_model <- 3L
seed_pairs <- 123
seed_order <- 456

# -------------------------------------------------------------------
# Helper: check API key
# -------------------------------------------------------------------

check_together_key <- function() {
  key <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  if (identical(key, "")) {
    stop(
      "TOGETHER_API_KEY is not set. ",
      "Please export TOGETHER_API_KEY before running this script.",
      call. = FALSE
    )
  }
  invisible(key)
}

# -------------------------------------------------------------------
# Helper: safely extract raw message content from raw_response
# -------------------------------------------------------------------

extract_raw_content <- function(raw_resp) {
  if (is.null(raw_resp)) {
    return(NA_character_)
  }
  # Expect structure like: list(choices = list(list(message = list(content = "..."))))
  choices <- raw_resp$choices
  if (is.null(choices) || length(choices) < 1L) {
    return(NA_character_)
  }
  msg <- choices[[1]]$message
  if (is.null(msg)) {
    return(NA_character_)
  }
  content <- msg$content
  if (is.null(content)) {
    return(NA_character_)
  }
  as.character(content)
}

# -------------------------------------------------------------------
# Helper: basic parsing checks + raw content inspection
# -------------------------------------------------------------------

check_together_result <- function(res, model, n_expected, pairs, trait, tmpl) {
  cat("\n----------------------------------------\n")
  cat(glue("Model: {model}\n"))
  cat(glue("Rows:  {nrow(res)} (expected {n_expected})\n"))

  if (nrow(res) != n_expected) {
    warning(glue("Expected {n_expected} rows but got {nrow(res)} for {model}"))
  }

  # Status code summary
  status_tab <- res |>
    count(status_code, .drop = FALSE)

  cat("\nStatus code counts:\n")
  print(status_tab)

  # Any errors?
  if (any(!is.na(res$error_message))) {
    cat("\nRows with error_message:\n")
    print(
      res |>
        filter(!is.na(.data$error_message)) |>
        select(all_of(c("ID1", "ID2", "model", "status_code", "error_message")))
    )
  }

  # Basic parsing sanity checks
  cat("\nParsing sanity checks:\n")

  # better_sample should be NA / SAMPLE_1 / SAMPLE_2
  valid_better <- c(NA_character_, "SAMPLE_1", "SAMPLE_2")
  if (!all(res$better_sample %in% valid_better)) {
    warning("Found unexpected values in `better_sample`.")
    print(table(res$better_sample, useNA = "ifany"))
  } else {
    cat("  - `better_sample` values look valid.\n")
  }

  # better_id must be ID1, ID2, or NA
  id_values <- unique(c(res$ID1, res$ID2))
  if (!all(is.na(res$better_id) | res$better_id %in% id_values)) {
    warning("Found unexpected values in `better_id` (not ID1/ID2/NA).")
    print(table(res$better_id, useNA = "ifany"))
  } else {
    cat("  - `better_id` values look valid.\n")
  }

  # content should not be empty if status_code == 200 and no error
  good_rows <- res$status_code == 200L & is.na(res$error_message)
  if (any(good_rows)) {
    n_empty_content <- sum(good_rows & (is.na(res$content) | res$content == ""))
    if (n_empty_content > 0) {
      warning(glue("  - {n_empty_content} successful rows have empty `content`."))
    } else {
      cat("  - Successful rows have non-empty `content`.\n")
    }
  }

  # DeepSeek-R1 specific: <think> tags should be gone from content if present
  if (identical(model, "deepseek-ai/DeepSeek-R1")) {
    if (any(grepl("<think>", res$content %||% "", fixed = TRUE))) {
      warning("DeepSeek-R1: `<think>` tags still present in `content`.")
    } else {
      cat("  - DeepSeek-R1: no `<think>` tags remain in `content`.\n")
    }

    if (any(!is.na(res$thoughts))) {
      cat("  - DeepSeek-R1: `thoughts` column has non-NA entries (good).\n")
    } else {
      cat("  - DeepSeek-R1: `thoughts` column is all NA (model may not have emitted <think>).\n")
    }
  }

  # Brief summary table (as before)
  cat("\nSample of parsed results:\n")
  print(
    res |>
      select(
        all_of(c(
          "ID1", "ID2", "model", "status_code",
          "better_sample", "better_id",
          "prompt_tokens", "completion_tokens", "total_tokens"
        ))
      ) |>
      head()
  )

  # -------------------------------------------------------------------
  # Raw vs parsed content
  # -------------------------------------------------------------------

  if ("raw_response" %in% names(res)) {
    cat("\nRaw vs parsed content (first few rows):\n")

    raw_content <- map_chr(
      head(res$raw_response, 5),
      extract_raw_content
    )

    preview <- tibble::tibble(
      ID1            = head(res$ID1, 5),
      ID2            = head(res$ID2, 5),
      better_sample  = head(res$better_sample, 5),
      better_id      = head(res$better_id, 5),
      parsed_content = head(res$content, 5),
      raw_content    = raw_content
    )

    print(preview)
  } else {
    cat(
      "\n(raw_response column not present in submit_together_pairs_live() output;\n",
      " doing a single direct together_compare_pair_live() call for raw content.)\n",
      sep = ""
    )

    # Use the first pair for a direct debug call
    idx <- 1L
    dbg <- together_compare_pair_live(
      ID1               = as.character(pairs$ID1[idx]),
      text1             = as.character(pairs$text1[idx]),
      ID2               = as.character(pairs$ID2[idx]),
      text2             = as.character(pairs$text2[idx]),
      model             = model,
      trait_name        = trait$name,
      trait_description = trait$description,
      prompt_template   = tmpl,
      include_raw       = TRUE
    )

    dbg_raw <- if ("raw_response" %in% names(dbg)) {
      extract_raw_content(dbg$raw_response[[1]])
    } else {
      NA_character_
    }

    cat("\nDirect Together.ai call (first pair):\n")
    print(
      tibble::tibble(
        ID1            = dbg$ID1,
        ID2            = dbg$ID2,
        better_sample  = dbg$better_sample,
        better_id      = dbg$better_id,
        thoughts       = dbg$thoughts,
        parsed_content = dbg$content,
        raw_content    = dbg_raw
      )
    )
  }

  invisible(res)
}

# -------------------------------------------------------------------
# Helper: run a single model on n pairs
# -------------------------------------------------------------------

run_together_model <- function(model, n_pairs = n_pairs_per_model) {
  cat("\n========================================\n")
  cat(glue("Running Together.ai dev check for model: {model}\n"))
  cat("========================================\n")

  data("example_writing_samples", package = "pairwiseLLM")
  samples <- example_writing_samples

  # Build a small set of pairs
  pairs <- samples |>
    make_pairs() |>
    sample_pairs(n_pairs = n_pairs, seed = seed_pairs) |>
    randomize_pair_order(seed = seed_order)

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Let together_compare_pair_live/together backend handle temperature defaults
  res <- submit_together_pairs_live(
    pairs             = pairs,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    verbose           = TRUE,
    status_every      = 1,
    progress          = FALSE,
    include_raw       = TRUE
  )

  check_together_result(
    res,
    model      = model,
    n_expected = n_pairs,
    pairs      = pairs,
    trait      = td,
    tmpl       = tmpl
  )
}

# -------------------------------------------------------------------
# Main
# -------------------------------------------------------------------

if (interactive()) {
  cat("Checking TOGETHER_API_KEY...\n")
}
check_together_key()

results <- map(together_models, run_together_model)

cat("\n\nDone. Models checked:\n")
print(together_models)
