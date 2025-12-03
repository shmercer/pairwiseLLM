# dev/dev-positional-bias-rebuild-summary.R
#
# Rebuild positional bias summary from existing CSV files in:
#   dev-output/positional-bias-all-models

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

out_dir <- "dev-output/positional-bias-all-models"

if (!dir.exists(out_dir)) {
  stop("Output directory does not exist: ", out_dir, call. = FALSE)
}

# -------------------------------------------------------------------
# 1. Discover and parse CSV files
# -------------------------------------------------------------------

files <- list.files(out_dir, pattern = "\\.csv$", full.names = FALSE)

# Expect filenames like:
#   openai_gpt-4.1_no_thinking_forward.csv
#   anthropic_claude-opus-4-0_with_thinking_reverse.csv
#   gemini_gemini-3-pro-preview_with_thinking_forward.csv
#
# Regex:
#   ^(openai|anthropic|gemini)_(.+)_(no_thinking|with_thinking)_(forward|reverse)\\.csv$
parse_filename <- function(fname) {
  m <- regexec("^(openai|anthropic|gemini)_(.+)_(no_thinking|with_thinking)_(forward|reverse)\\.csv$",
               fname)
  regm <- regmatches(fname, m)[[1]]

  if (length(regm) != 5L) {
    return(NULL)
  }

  list(
    provider  = regm[2],
    model     = regm[3],
    thinking  = regm[4],
    direction = regm[5]
  )
}

parsed <- lapply(files, parse_filename)

keep_idx <- !vapply(parsed, is.null, logical(1))
parsed  <- parsed[keep_idx]
files   <- files[keep_idx]

if (!length(files)) {
  stop("No CSV files matched the expected naming pattern in ", out_dir, call. = FALSE)
}

meta <- tibble::tibble(
  file      = files,
  provider  = vapply(parsed, `[[`, character(1), "provider"),
  model     = vapply(parsed, `[[`, character(1), "model"),
  thinking  = vapply(parsed, `[[`, character(1), "thinking"),
  direction = vapply(parsed, `[[`, character(1), "direction")
)

# -------------------------------------------------------------------
# 2. For each (provider, model, thinking), require both forward+reverse
# -------------------------------------------------------------------

combos <- meta %>%
  group_by(provider, model, thinking) %>%
  summarize(
    has_forward = any(direction == "forward"),
    has_reverse = any(direction == "reverse"),
    .groups = "drop"
  ) %>%
  filter(has_forward, has_reverse)

if (!nrow(combos)) {
  stop("No (provider, model, thinking) combinations with both forward and reverse CSVs found.",
       call. = FALSE)
}

# -------------------------------------------------------------------
# 3. Generic numeric extractor from arbitrary object
# -------------------------------------------------------------------

extract_numeric_metric <- function(obj, name_pattern = NULL) {
  # If NULL, bail out
  if (is.null(obj)) return(NA_real_)

  # If already numeric vector
  if (is.numeric(obj) && length(obj) >= 1L) {
    return(obj[1])
  }

  # Data frame: look for column names matching pattern, then any numeric column
  if (is.data.frame(obj)) {
    if (!is.null(name_pattern)) {
      num_cols <- vapply(obj, is.numeric, logical(1))
      candidates <- names(obj)[num_cols & grepl(name_pattern, names(obj))]
      if (length(candidates) > 0L) {
        return(obj[[candidates[1]]][1])
      }
    }

    # Fallback: first numeric column
    num_cols <- vapply(obj, is.numeric, logical(1))
    if (any(num_cols)) {
      first_col <- which(num_cols)[1]
      return(obj[[first_col]][1])
    }

    return(NA_real_)
  }

  # List (incl. nested): flatten
  if (is.list(obj)) {
    flat <- tryCatch(
      unlist(obj, recursive = TRUE, use.names = TRUE),
      error = function(e) NULL
    )
    if (is.null(flat) || !length(flat)) return(NA_real_)

    # If we have names and a pattern, use that
    if (!is.null(name_pattern) && !is.null(names(flat))) {
      idx <- which(grepl(name_pattern, names(flat)))
      if (length(idx)) {
        val <- suppressWarnings(as.numeric(flat[idx]))
        val <- val[is.finite(val)]
        if (length(val)) return(val[1])
      }
    }

    # Fallback: any numeric-ish entry
    val <- suppressWarnings(as.numeric(flat))
    val <- val[is.finite(val)]
    if (length(val)) return(val[1])

    return(NA_real_)
  }

  # Anything else: try as.numeric directly
  v <- suppressWarnings(as.numeric(obj))
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_real_)
  v[1]
}

# -------------------------------------------------------------------
# 4. Helper to compute stats for a single combo
# -------------------------------------------------------------------

summarize_from_csv <- function(provider, model, thinking) {
  # Get matching rows in meta
  sub <- meta %>%
    filter(provider == !!provider,
           model    == !!model,
           thinking == !!thinking)

  fwd_row <- sub %>% filter(direction == "forward") %>% slice_head(n = 1)
  rev_row <- sub %>% filter(direction == "reverse") %>% slice_head(n = 1)

  if (!nrow(fwd_row) || !nrow(rev_row)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  fwd_path <- file.path(out_dir, fwd_row$file)
  rev_path <- file.path(out_dir, rev_row$file)

  res_forward <- readr::read_csv(fwd_path, show_col_types = FALSE)
  res_reverse <- readr::read_csv(rev_path, show_col_types = FALSE)

  if (!nrow(res_forward) || !nrow(res_reverse)) {
    return(tibble::tibble(
      prop_consistent   = NA_real_,
      p_sample1_overall = NA_real_
    ))
  }

  # --- Reverse consistency: this is the *primary* object --------------
  cons <- tryCatch(
    compute_reverse_consistency(res_forward, res_reverse),
    error = function(e) {
      message("compute_reverse_consistency error for ",
              provider, " / ", model, " / ", thinking, ": ",
              conditionMessage(e))
      NULL
    }
  )

  # prop_consistent from the consistency object (any field matching "prop_consistent")
  prop_consistent <- extract_numeric_metric(cons, "prop_consistent")

  # --- Positional bias: check_positional_bias expects a `details` data frame --
  details <- NULL
  if (!is.null(cons) && is.list(cons) && "details" %in% names(cons)) {
    details <- cons$details
  }

  bias <- if (!is.null(details)) {
    tryCatch(
      check_positional_bias(details),
      error = function(e) {
        message("check_positional_bias error for ",
                provider, " / ", model, " / ", thinking, ": ",
                conditionMessage(e))
        NULL
      }
    )
  } else {
    NULL
  }

  # p_sample1_overall from bias object (any field matching "p_sample1")
  p_sample1_overall <- extract_numeric_metric(bias, "p_sample1")

  tibble::tibble(
    prop_consistent   = prop_consistent,
    p_sample1_overall = p_sample1_overall
  )
}

# -------------------------------------------------------------------
# 5. Build base summary table
# -------------------------------------------------------------------

summary_tbl <- combos %>%
  rowwise() %>%
  mutate(
    summary = list(
      summarize_from_csv(provider, model, thinking)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(summary)

if (!nrow(summary_tbl)) {
  stop("Summary table is empty after processing all CSVs.", call. = FALSE)
}

# -------------------------------------------------------------------
# 6. Add thinking_config + temperature columns
# -------------------------------------------------------------------

summary_tbl <- summary_tbl %>%
  mutate(
    # Human-readable description of the "thinking" setup used.
    thinking_config = case_when(
      provider == "openai" & thinking == "no_thinking" ~
        "OpenAI chat.completions (no reasoning; temp=1)",

      provider == "openai" & thinking == "with_thinking" & model == "gpt-5.1" ~
        "OpenAI responses (include_thoughts=TRUE, effort=low; temp = 1)",

      provider == "openai" & thinking == "with_thinking" ~
        "OpenAI responses (include_thoughts=TRUE; temp=1)",

      provider == "anthropic" & thinking == "no_thinking" ~
        "Anthropic Messages (reasoning=none, temp=0, max_tokens=768)",

      provider == "anthropic" & thinking == "with_thinking" ~
        "Anthropic Messages (reasoning=enabled, temp=1, max_tokens=2048, thinking_budget=1024)",

      provider == "gemini" ~
        "Gemini batchGenerateContent (thinkingLevel=Low, includeThoughts=TRUE; temp=1)",

      TRUE ~ NA_character_
    ),

    # Numeric temperature as controlled by our code (NA = provider default / not explicitly set)
    temperature = case_when(
      # Anthropic defaults from build_anthropic_batch_requests()
      provider == "anthropic" & thinking == "no_thinking"   ~ 0,
      provider == "anthropic" & thinking == "with_thinking" ~ 1,

      # For OpenAI + Gemini batch runs, we typically rely on provider defaults
      # unless the dev script explicitly passed a temperature argument.
      # We mark these as NA_real_ to indicate "not controlled here".
      TRUE ~ NA_real_
    )
  ) %>%
  # Keep the summary tight and consistent
  select(
    provider,
    model,
    thinking,
    prop_consistent,
    p_sample1_overall,
    thinking_config,
    temperature
  )

# -------------------------------------------------------------------
# 7. Save + print
# -------------------------------------------------------------------

summary_path <- file.path(out_dir, "positional_bias_summary_rebuilt.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Summary rebuilt and written to: ", summary_path)


# -------------------------------------------------------------------
# 7. Save + print
# -------------------------------------------------------------------

summary_path <- file.path(out_dir, "positional_bias_summary_rebuilt.csv")
readr::write_csv(summary_tbl, summary_path)

print(summary_tbl)
message("Summary rebuilt and written to: ", summary_path)
