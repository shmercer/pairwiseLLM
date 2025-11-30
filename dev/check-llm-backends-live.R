# dev/check-llm-backends-live.R
# Quick live sanity checks for all LLM backends via llm_compare_pair()
# and submit_llm_pairs().

# --------------------------------------------------------------------
# Helper to pretty-print httr2 HTTP errors (OpenAI / Anthropic / Gemini)
# --------------------------------------------------------------------
print_http_error_details <- function(e) {
  cat("[ERROR]", conditionMessage(e), "\n\n")

  if (inherits(e, "httr2_http") && !is.null(e$resp)) {
    # We have an httr2 response object
    resp <- e$resp

    status <- tryCatch(
      httr2::resp_status(resp),
      error = function(err) NA_integer_
    )
    cat("HTTP status:", status, "\n\n")

    body_raw <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(err) NULL
    )

    if (!is.null(body_raw)) {
      cat("Raw response body:\n")
      cat(body_raw, "\n\n")
    } else {
      cat("Failed to read response body.\n\n")
    }
  } else {
    cat("No response object attached to error.\n\n")
  }
}

# --------------------------------------------------------------------
# Shared data and prompt template
# --------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:4, ]

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Convenience 3-pair tibble
pairs_3 <- tibble::tibble(
  ID1   = c(samples$ID[4], samples$ID[4], samples$ID[3]),
  text1 = c(samples$text[4], samples$text[4], samples$text[3]),
  ID2   = c(samples$ID[1], samples$ID[3], samples$ID[1]),
  text2 = c(samples$text[1], samples$text[3], samples$text[1])
)

ID1 <- samples$ID[1]
ID2 <- samples$ID[2]
text1 <- samples$text[1]
text2 <- samples$text[2]

# Small helper for checking presence of thoughts column
has_thoughts_col <- function(x) {
  "thoughts" %in% names(x)
}

# --------------------------------------------------------------------
# OpenAI: chat.completions (no reasoning / no thoughts)
# --------------------------------------------------------------------
cat("\n=== OpenAI (backend = 'openai') - chat.completions single pair ===\n\n")

openai_chat_model <- "gpt-4.1-mini"

openai_chat_single <- tryCatch(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = openai_chat_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "chat.completions",
    temperature       = 0,
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(openai_chat_single)) {
  cat("Result:\n")
  print(openai_chat_single)

  if (has_thoughts_col(openai_chat_single)) {
    cat("\n  thoughts is non-NA: ",
        !is.na(openai_chat_single$thoughts),
        "\n", sep = "")
  }

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      grepl("<BETTER_SAMPLE>", openai_chat_single$content, fixed = TRUE),
      "\n", sep = "")
  cat("  better_id: ", openai_chat_single$better_id, "\n", sep = "")
}

cat("\n=== OpenAI (backend = 'openai') - chat.completions multiple pairs ===\n")

openai_chat_multi <- tryCatch(
  submit_llm_pairs(
    pairs             = pairs_3,
    model             = openai_chat_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "chat.completions",
    temperature       = 0,
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(openai_chat_multi)) {
  cat("\nResult:\n")
  print(openai_chat_multi)

  if (has_thoughts_col(openai_chat_multi)) {
    cat("\n  thoughts is non-NA: ",
        any(!is.na(openai_chat_multi$thoughts)),
        "\n", sep = "")
  }

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      all(grepl("<BETTER_SAMPLE>",
                openai_chat_multi$content %||% "", fixed = TRUE)),
      "\n", sep = "")
  cat("  better_id: ",
      paste(openai_chat_multi$better_id, collapse = ", "),
      "\n", sep = "")
}

# --------------------------------------------------------------------
# OpenAI: responses endpoint with reasoning (gpt-5.1)
# --------------------------------------------------------------------
cat("\n=== OpenAI (backend = 'openai') - responses with reasoning (single pair) ===\n\n")

openai_resp_model <- "gpt-5.1"

openai_resp_single <- tryCatch(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = openai_resp_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "responses",
    reasoning         = "low",   # enable some internal thinking
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(openai_resp_single)) {
  cat("Result:\n")
  print(openai_resp_single)

  if (has_thoughts_col(openai_resp_single)) {
    cat("\n  thoughts is non-NA: ",
        !is.na(openai_resp_single$thoughts),
        "\n", sep = "")
  }

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      grepl("<BETTER_SAMPLE>", openai_resp_single$content, fixed = TRUE),
      "\n", sep = "")
  cat("  better_id: ", openai_resp_single$better_id, "\n", sep = "")
}

# --------------------------------------------------------------------
# Anthropic: single pair + multiple pairs (reasoning = "none")
# --------------------------------------------------------------------
cat("\n=== Anthropic (backend = 'anthropic') - single pair (no explicit thinking block) ===\n\n")

anthropic_model <- "claude-sonnet-4-5"

anth_single <- tryCatch(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = anthropic_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "anthropic",
    reasoning         = "none",   # deterministic default
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(anth_single)) {
  cat("Call succeeded; result tibble:\n")
  print(anth_single)

  # Anthropic currently does not have a dedicated `thoughts` column in the
  # package; we only inspect content and IDs here.
  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      grepl("<BETTER_SAMPLE>", anth_single$content, fixed = TRUE),
      "\n", sep = "")
  cat("  better_id: ", anth_single$better_id, "\n", sep = "")
}

cat("\n=== Anthropic (backend = 'anthropic') - multiple pairs (no explicit thinking block) ===\n\n")

anth_multi <- tryCatch(
  submit_llm_pairs(
    pairs             = pairs_3,
    model             = anthropic_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "anthropic",
    reasoning         = "none",
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(anth_multi)) {
  cat("\nResult:\n")
  print(anth_multi)

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      all(grepl("<BETTER_SAMPLE>",
                anth_multi$content %||% "", fixed = TRUE)),
      "\n", sep = "")
  cat("  better_id: ",
      paste(anth_multi$better_id, collapse = ", "),
      "\n", sep = "")
}

# --------------------------------------------------------------------
# Gemini: single pair + multiple pairs with thoughts
# --------------------------------------------------------------------
cat("\n=== Gemini (backend = 'gemini') - single pair with thoughts ===\n\n")

gemini_model <- "gemini-3-pro-preview"

gem_single <- tryCatch(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = gemini_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "gemini",
    thinking_level    = "low",   # custom Gemini arg in your backend
    include_thoughts  = TRUE,
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(gem_single)) {
  cat("Result:\n")
  print(gem_single)

  if (has_thoughts_col(gem_single)) {
    cat("\n  thoughts is non-NA: ",
        !is.na(gem_single$thoughts),
        "\n", sep = "")
  }

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      grepl("<BETTER_SAMPLE>", gem_single$content %||% "", fixed = TRUE),
      "\n", sep = "")
  cat("  better_id: ", gem_single$better_id, "\n", sep = "")

  # Extra diagnostics if non-200 status
  if (!is.na(gem_single$status_code) && gem_single$status_code != 200L) {
    cat("\n  Non-200 status detected (row 1): ",
        gem_single$status_code, "\n", sep = "")
    if (!("raw_response" %in% names(gem_single)) ||
        is.null(gem_single$raw_response[[1]])) {
      cat("  raw_response[[1]] is NULL.\n")
    } else {
      cat("  raw_response[[1]] structure (max.level = 2):\n")
      str(gem_single$raw_response[[1]], max.level = 2)
    }
  }
}

cat("\n=== Gemini (backend = 'gemini') - multiple pairs with thoughts ===\n\n")

gem_multi <- tryCatch(
  submit_llm_pairs(
    pairs             = pairs_3,
    model             = gemini_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "gemini",
    thinking_level    = "low",
    include_thoughts  = TRUE,
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    NULL
  }
)

if (!is.null(gem_multi)) {
  cat("\nResult:\n")
  print(gem_multi)

  if (has_thoughts_col(gem_multi)) {
    cat("\n  thoughts is non-NA: ",
        any(!is.na(gem_multi$thoughts)),
        "\n", sep = "")
  }

  cat("\n  content contains <BETTER_SAMPLE> tag: ",
      all(grepl("<BETTER_SAMPLE>",
                gem_multi$content %||% "", fixed = TRUE)),
      "\n", sep = "")
  cat("  better_id: ",
      paste(gem_multi$better_id, collapse = ", "),
      "\n", sep = "")

  # Extra diagnostics for non-200
  if ("status_code" %in% names(gem_multi) &&
      any(!is.na(gem_multi$status_code) & gem_multi$status_code != 200L)) {
    bad_idx <- which(!is.na(gem_multi$status_code) &
                       gem_multi$status_code != 200L)[1]
    cat("\n  Non-200 status detected (row ", bad_idx, "): ",
        gem_multi$status_code[bad_idx], "\n", sep = "")
    if (!("raw_response" %in% names(gem_multi)) ||
        is.null(gem_multi$raw_response[[bad_idx]])) {
      cat("  raw_response[[", bad_idx, "]] is NULL.\n", sep = "")
    } else {
      cat("  raw_response[[", bad_idx, "]] structure (max.level = 2):\n",
          sep = "")
      str(gem_multi$raw_response[[bad_idx]], max.level = 2)
    }
  }
}

cat("\nAll backend live checks completed.\n")
