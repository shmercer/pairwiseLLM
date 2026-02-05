# data-raw/generate_example_writing_samples1000.R
#
# PURPOSE
# -------
# Generate a large synthetic dataset of writing samples with a *known, usable*
# latent ordering for adaptive-vs-random pairing studies, using LIVE OpenAI calls.
#
# This script implement:
#   1) Use *bucketed* quality levels (bins) rather than “1..1000 monotone”.
#      - Example: 20 levels (1..20), 50 samples per level => 1000 samples.
#      - Within-level variation creates realistic near-ties.
#      - Across-level separation creates recoverable global structure.
#   2) Keep the writing task constant so comparisons depend on quality, not topic.
#   3) Constrain length and realism to avoid saturation (“all perfect at the top”)
#      and nonsense (“unreadable at the bottom”).
#   4) Save (a) a discrete level (1..20) and (b) a centered theta_true proxy
#      so downstream evaluation can use truth-based metrics (rank recovery, RMSE).
#
# OUTPUT
# ------
# Saves an R data object into the package:
#   data/example_writing_samples1000.rda
#
# The object name is: example_writing_samples1000
# Format: tibble with columns:
#   - ID            : character, unique ID (S0001 .. S1000)
#   - text          : character, the writing sample
#   - quality_level : integer, 1..K (K = 20 by default)
#   - theta_true    : numeric, centered proxy for latent theta (based on level)
#   - prompt_id     : character, reproducibility tag for the prompt template
#   - model         : character, generator model used
#   - created_at    : POSIXct, generation timestamp (UTC)
#
# NOTES
# -----
# - This makes live API calls. You must set OPENAI_API_KEY.
# - Recommended: generate with one model, judge with another (or same model but
#   different system prompt). This script only generates.
# - After generation, you should *validate* recoverability with a small random
#   probe (e.g., 200 random pairs judged by your chosen judge model).
#
# HOW THIS IMPROVES OVER “QUALITY 1..1000”
# ----------------------------------------
# Asking an LLM to produce 1000 strictly increasing-quality texts tends to fail:
# - local inversions are common
# - high end saturates (many “very good” texts become indistinguishable)
# - spacing becomes nonlinear and unpredictable
#
# Bucketed levels avoid those issues and create a dataset that is:
# - more stable as a “known theta” proxy
# - better aligned with the strengths of adaptive pairing (resolving boundaries)
#
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
})

# -----------------------------
# 0) Safety / required API key
# -----------------------------
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY is not set. Set it with Sys.setenv(OPENAI_API_KEY='...').")
}

# -----------------------------
# 1) Configuration (edit here)
# -----------------------------

# Dataset shape
K_levels <- 20L             # number of quality bins
n_per_level <- 50L          # samples per bin
N <- K_levels * n_per_level # total samples

# Writing prompt and length constraints
writing_prompt <- "Describe a challenge you have faced and explain what you learned from it."
min_words <- 120L
max_words <- 180L

# OpenAI generation settings
# Use a *generator* model that can write creatively; temperature > 0 is helpful.
# Reasoning can be "none" or low; for generation, “reasoning” isn't typically needed.
gen_model <- "gpt-5.1"            # or "gpt-5.1" / "gpt-4o" depending on availability
endpoint <- "responses"
service_tier <- "flex"          # only applied for GPT-5 series where supported
reasoning <- "none"             # generation: usually none (or "low")
temperature <- 0.7              # encourage diversity within-level
top_p <- NULL                   # optional
max_retries_per_sample <- 3L

# Rate limiting / robustness
sleep_between_calls_sec <- 0.2  # small pause to reduce burst errors; raise if needed

# Where to save in the package tree
# Run this from the package root. If running elsewhere, adjust path.
out_rda_path <- file.path("data", "example_writing_samples1000.rda")

# A prompt template ID to track prompt evolution (helps reproducibility).
prompt_id <- "bucketed-quality-v1"

# Set a seed for deterministic *ID assignment and level ordering*.
# Note: LLM outputs are not deterministic even with a seed unless you pass an API seed.
set.seed(123)

# ----------------------------------------
# 2) Quality level guidelines (the scaffold)
# ----------------------------------------
# These guidelines are meant to create *recoverable* differences without extremes.
# We intentionally avoid:
# - nonsense at low end
# - “perfect prose” at high end
#
# Adjacent levels should be *sometimes hard to distinguish* (near ties).
# Levels several apart should be easier to distinguish (global structure).

quality_guidelines <- c(
  "1–3: severely underdeveloped; unclear purpose; frequent issues; still coherent.",
  "4–6: limited development; weak organization; noticeable issues; some clarity.",
  "7–9: basic competence; uneven development; generally understandable; some issues.",
  "10–12: adequate and clear; reasonable organization; limited depth or polish.",
  "13–15: strong; well-organized; clear and effective; good control of language.",
  "16–18: very strong; nuanced; polished; effective structure; still realistic student writing.",
  "19–20: excellent; highly effective; vivid and precise; but not flawless or over-the-top."
)

# ---------------------------------------------------------
# 3) Prompt builder for a specific quality level (1..K_levels)
# ---------------------------------------------------------
build_generation_prompt <- function(level, K_levels, n_needed, writing_prompt,
                                    min_words, max_words, quality_guidelines) {
  stopifnot(length(level) == 1L)

  paste0(
    "We are creating a dataset of writing samples with controlled differences in overall quality.\n\n",
    "There are ", K_levels, " quality levels, numbered 1 (very low quality) to ", K_levels, " (very high quality).\n",
    "Each quality level represents a RANGE of writing quality, not a single point.\n\n",
    "For EACH response you generate:\n",
    "- The writing must be internally coherent.\n",
    "- Differences in quality should come from clarity, organization, development, ",
    "precision of language, and effectiveness.\n",
    "- Do NOT mention quality levels, rubrics, or self-assessments.\n",
    "- Do NOT make the lowest levels nonsensical and do NOT make the highest levels perfect.\n",
    "- Stay within ", min_words, "–", max_words, " words.\n\n",
    "Quality level guidelines:\n",
    paste0("- ", quality_guidelines, collapse = "\n"), "\n\n",
    "TASK PROMPT:\n",
    "\"", writing_prompt, "\"\n\n",
    "Generate ", n_needed, " DIFFERENT responses for quality level ", level, ".\n",
    "Vary style, examples, and structure WITHIN the level.\n",
    "Ensure responses at adjacent levels are sometimes hard to distinguish, but ",
    "responses several levels apart are usually clearly different.\n\n",
    "OUTPUT FORMAT (important):\n",
    "Return a JSON array of strings, where each element is one full response. No extra keys.\n"
  )
}

# -----------------------------------------
# 4) OpenAI call helper (Responses endpoint)
# -----------------------------------------
# We reuse the package's internal OpenAI plumbing so auth + retry behavior stays consistent.
# This also keeps compatibility with service_tier + reasoning normalization used by pairwiseLLM.

call_openai_responses_text <- function(prompt,
                                      model,
                                      reasoning = "none",
                                      service_tier = "flex",
                                      temperature = 0.7,
                                      top_p = NULL) {

  # Normalize reasoning + service tier the same way pairwiseLLM does
  reasoning_effort <- pairwiseLLM:::normalize_openai_reasoning(
    model = model,
    reasoning = reasoning,
    include_thoughts = FALSE
  )

  st <- pairwiseLLM:::normalize_openai_service_tier(service_tier)
  if (!pairwiseLLM:::is_gpt5_series_model(model) || st %in% c("default", "auto")) {
    st <- NULL
  }

  # Build request body (matches openai_compare_pair_live behavior for /responses)
  body <- list(model = model, input = prompt)

  if (!is.null(reasoning_effort)) {
    body$reasoning <- list(effort = reasoning_effort)
  }
  if (!is.null(st)) body$service_tier <- st
  if (!is.null(temperature)) body$temperature <- temperature
  if (!is.null(top_p)) body$top_p <- top_p

  api_key <- pairwiseLLM:::.openai_api_key(NULL)

  req <- pairwiseLLM:::.openai_request("/responses", api_key) |>
    pairwiseLLM:::.openai_req_body_json(body = body)

  resp <- pairwiseLLM:::.openai_req_perform(req)
  status <- pairwiseLLM:::.openai_resp_status(resp)

  parsed <- tryCatch(
    pairwiseLLM:::.openai_resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || status >= 400L) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else paste0("HTTP ", status)
    stop("OpenAI request failed: ", msg)
  }

  # Extract concatenated "message chunks" (same logic as openai_compare_pair_live)
  message_chunks <- character(0)
  output <- parsed$output %||% list()
  if (length(output) > 0L) {
    for (out_el in output) {
      if (length(out_el$content) > 0L) {
        for (b in out_el$content) if (!is.null(b$text)) message_chunks <- c(message_chunks, b$text)
      }
    }
  }
  content <- if (length(message_chunks)) paste(message_chunks, collapse = "") else NA_character_
  if (is.na(content) || !nzchar(content)) stop("OpenAI response had empty content.")

  content
}

# ----------------------------------------
# 5) Parse model output JSON (array of strings)
# ----------------------------------------
parse_json_array_of_strings <- function(x) {
  out <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
  if (is.null(out) || !is.character(out)) return(NULL)
  if (length(out) < 1L) return(NULL)
  out
}

# ----------------------------------------
# 6) Basic word-count filter
# ----------------------------------------
word_count <- function(txt) {
  # simple whitespace split
  sum(strsplit(trimws(txt), "\\s+")[[1]] != "")
}

filter_by_length <- function(responses, min_words, max_words) {
  ok <- vapply(
    responses,
    function(t) {
      n <- word_count(t)
      isTRUE(n >= min_words && n <= max_words)
    },
    logical(1)
  )
  responses[ok]
}

# ---------------------------------------------------------
# 7) Main generation loop: level-by-level, with retries
# ---------------------------------------------------------
# Strategy:
# - For each level, request a batch (e.g., 50 items) in one call.
# - If parsing fails or too many violate length constraints, retry.
# - If still short, top-up with additional calls until we hit n_per_level
#   (bounded by max retries per sample *roughly* via capped attempts).

generate_level_samples <- function(level) {
  target <- n_per_level
  collected <- character(0)

  attempts <- 0L
  # We allow multiple calls if needed, but cap total attempts.
  # (This isn’t perfect “per sample” retrying; it’s pragmatic and keeps prompts simple.)
  max_attempts <- max(3L, ceiling(max_retries_per_sample * target / 25))

  while (length(collected) < target && attempts < max_attempts) {
    attempts <- attempts + 1L

    n_needed <- target - length(collected)
    # Request exactly what we still need; you can overshoot slightly if you prefer.
    prompt <- build_generation_prompt(
      level = level,
      K_levels = K_levels,
      n_needed = n_needed,
      writing_prompt = writing_prompt,
      min_words = min_words,
      max_words = max_words,
      quality_guidelines = quality_guidelines
    )

    cat(sprintf("Level %d/%d: requesting %d samples (attempt %d/%d)\n",
                level, K_levels, n_needed, attempts, max_attempts))

    raw <- tryCatch(
      call_openai_responses_text(
        prompt = prompt,
        model = gen_model,
        reasoning = reasoning,
        service_tier = service_tier,
        temperature = temperature,
        top_p = top_p
      ),
      error = function(e) {
        message("  OpenAI error: ", e$message)
        NULL
      }
    )

    if (is.null(raw)) {
      Sys.sleep(sleep_between_calls_sec)
      next
    }

    parsed <- parse_json_array_of_strings(raw)
    if (is.null(parsed)) {
      message("  Parse error: model did not return a JSON array of strings. Retrying.")
      Sys.sleep(sleep_between_calls_sec)
      next
    }

    parsed <- filter_by_length(parsed, min_words, max_words)
    if (length(parsed) < 1L) {
      message("  All responses failed length filter. Retrying.")
      Sys.sleep(sleep_between_calls_sec)
      next
    }

    collected <- c(collected, parsed)
    collected <- unique(collected) # avoid accidental duplicates
    Sys.sleep(sleep_between_calls_sec)
  }

  if (length(collected) < target) {
    stop(sprintf("Failed to generate enough samples for level %d (got %d / %d).",
                 level, length(collected), target))
  }

  collected[seq_len(target)]
}

# ---------------------------------------------------------
# 8) Generate the full dataset
# ---------------------------------------------------------
created_at <- as.POSIXct(Sys.time(), tz = "UTC")

all_texts <- vector("list", K_levels)
for (lvl in seq_len(K_levels)) {
  all_texts[[lvl]] <- generate_level_samples(lvl)
}

# Build tibble
example_writing_samples1000 <- tibble(
  quality_level = rep(seq_len(K_levels), each = n_per_level),
  text = unlist(all_texts, use.names = FALSE)
) |>
  mutate(
    # Stable IDs: S0001..S1000 in the final row order
    ID = sprintf("S%04d", row_number()),
    prompt_id = prompt_id,
    model = gen_model,
    created_at = created_at
  ) |>
  select(ID, text, quality_level, prompt_id, model, created_at)

# Add a centered theta proxy based on level (truth signal for evaluation).
# This is intentionally simple: theta_true increases with level, but does not pretend
# to be perfectly spaced “true quality” at the text level.
example_writing_samples1000 <- example_writing_samples1000 |>
  mutate(
    theta_true = as.numeric(scale(quality_level, center = TRUE, scale = FALSE))
  ) |>
  select(ID, text, quality_level, theta_true, prompt_id, model, created_at)

# Quick sanity checks
stopifnot(nrow(example_writing_samples1000) == N)
stopifnot(!anyDuplicated(example_writing_samples1000$ID))
stopifnot(all(!is.na(example_writing_samples1000$text)))

# ---------------------------------------------------------
# 9) Save as .rda into the package data/ directory
# ---------------------------------------------------------
dir.create(dirname(out_rda_path), recursive = TRUE, showWarnings = FALSE)

# Use compress="bzip2" to keep repo size reasonable; base R loads this fine.
save(
  example_writing_samples1000,
  file = out_rda_path,
  compress = "bzip2"
)

cat("Saved:", out_rda_path, "\n")
cat("Object:", "example_writing_samples1000", "(", nrow(example_writing_samples1000), "rows )\n")
