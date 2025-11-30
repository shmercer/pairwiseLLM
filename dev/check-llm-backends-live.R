# dev/check-llm-backends-live.R
#
# Live end-to-end check of llm_compare_pair() / submit_llm_pairs()
# across backends (OpenAI, Anthropic, Gemini), including thinking /
# thoughts handling where supported.

suppressPackageStartupMessages({
  library(pairwiseLLM)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:4, ]

ID1   <- samples$ID[1]
ID2   <- samples$ID[2]
text1 <- samples$text[1]
text2 <- samples$text[2]

pairs_small <- tibble::tibble(
  ID1   = c(samples$ID[4], samples$ID[4], samples$ID[3]),
  text1 = c(samples$text[4], samples$text[4], samples$text[3]),
  ID2   = c(samples$ID[1], samples$ID[3], samples$ID[1]),
  text2 = c(samples$text[1], samples$text[3], samples$text[1])
)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

print_section <- function(title) {
  cat("\n", strrep("=", 3), " ", title, " ", strrep("=", 3), "\n\n", sep = "")
}

summarise_result <- function(res, show_thoughts_snippet = FALSE) {
  print(res)

  has_thoughts <- "thoughts" %in% names(res)
  thoughts_non_na <- has_thoughts && any(!is.na(res$thoughts))

  cat("\n  thoughts is non-NA: ", thoughts_non_na, "\n", sep = "")

  contents <- res$content
  contents[is.na(contents)] <- ""
  has_better_tag <- any(grepl("<BETTER_SAMPLE>", contents, fixed = TRUE))

  cat("\n  content contains <BETTER_SAMPLE> tag: ", has_better_tag, "\n", sep = "")
  cat("  better_id: ", res$better_id[1] %||% NA_character_, "\n", sep = "")

  if (has_thoughts && thoughts_non_na && show_thoughts_snippet) {
    t1 <- res$thoughts[1]
    cat("\n  thoughts[1] (truncated to 300 chars):\n")
    if (nchar(t1) > 300) {
      cat(substr(t1, 1, 300), "...\n", sep = "")
    } else {
      cat(t1, "\n", sep = "")
    }
  }

  if ("status_code" %in% names(res) && any(!is.na(res$status_code) & res$status_code != 200L)) {
    idx <- which(!is.na(res$status_code) & res$status_code != 200L)[1]
    cat("\n  Non-200 status detected (row ", idx, "): ", res$status_code[idx], "\n", sep = "")
    if ("error_message" %in% names(res)) {
      em <- res$error_message[idx]
      if (!is.na(em)) {
        cat("  error_message (truncated):\n")
        if (nchar(em) > 300) {
          cat(substr(em, 1, 300), "...\n", sep = "")
        } else {
          cat(em, "\n", sep = "")
        }
      }
    }
    if ("raw_response" %in% names(res)) {
      raw1 <- res$raw_response[[idx]]
      if (is.null(raw1)) {
        cat("  raw_response[[", idx, "]] is NULL.\n", sep = "")
      } else {
        cat("  raw_response[[", idx, "]] top-level names: ",
            paste(names(raw1), collapse = ", "), "\n", sep = "")
      }
    }
  }

  cat("\n")
}

handle_http_error <- function(expr) {
  tryCatch(
    expr,
    `httr2_http` = function(e) {
      cat("[ERROR] ", conditionMessage(e), "\n\n", sep = "")
      resp <- tryCatch(e$resp, error = function(...) NULL)
      if (!is.null(resp)) {
        status <- tryCatch(httr2::resp_status(resp), error = function(...) NA_integer_)
        cat("  Status from response: ", status, "\n", sep = "")
        body_raw <- tryCatch(httr2::resp_body_string(resp), error = function(...) NULL)
        if (!is.null(body_raw)) {
          cat("  Raw body (truncated to 800 chars):\n")
          if (nchar(body_raw) > 800) {
            cat(substr(body_raw, 1, 800), "...\n", sep = "")
          } else {
            cat(body_raw, "\n", sep = "")
          }
        }
      } else {
        cat("No response object attached to error.\n")
      }
      NULL
    },
    error = function(e) {
      cat("[ERROR] ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )
}

# =====================================================================
# OpenAI: chat.completions (no thoughts)
# =====================================================================

print_section("OpenAI (backend = 'openai') - chat.completions single pair")

res_oa_chat_single <- handle_http_error(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = "gpt-4.1-mini",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "chat.completions",
    temperature       = 0,
    include_raw       = TRUE
  )
)

if (!is.null(res_oa_chat_single)) {
  cat("Result:\n")
  summarise_result(res_oa_chat_single, show_thoughts_snippet = FALSE)
}

print_section("OpenAI (backend = 'openai') - chat.completions multiple pairs")

res_oa_chat_multi <- handle_http_error(
  submit_llm_pairs(
    pairs             = pairs_small,
    model             = "gpt-4.1-mini",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "chat.completions",
    temperature       = 0,
    verbose           = TRUE,
    status_every      = 2,
    progress          = TRUE,
    include_raw       = TRUE
  )
)

if (!is.null(res_oa_chat_multi)) {
  cat("Result:\n")
  summarise_result(res_oa_chat_multi, show_thoughts_snippet = FALSE)
}

# =====================================================================
# OpenAI: responses + reasoning/thoughts
# =====================================================================

print_section("OpenAI (backend = 'openai') - responses with reasoning (single pair)")

res_oa_resp_single <- handle_http_error(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = "gpt-5.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "openai",
    endpoint          = "responses",
    reasoning         = "low",     # maps to effort = "low", summary = "auto"
    temperature       = NULL,
    top_p             = NULL,
    logprobs          = NULL,
    include_raw       = TRUE
  )
)

if (!is.null(res_oa_resp_single)) {
  cat("Result:\n")
  summarise_result(res_oa_resp_single, show_thoughts_snippet = TRUE)
}

# =====================================================================
# Anthropic: extended thinking (reasoning = 'enabled')
# =====================================================================

print_section("Anthropic (backend = 'anthropic') - single pair with thoughts")

res_anth_single <- handle_http_error(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "anthropic",
    reasoning         = "enabled",
    max_tokens             = 2048,
    thinking_budget_tokens = 1024,
    include_raw            = TRUE
  )
)

if (!is.null(res_anth_single)) {
  cat("Result:\n")
  summarise_result(res_anth_single, show_thoughts_snippet = TRUE)
}

print_section("Anthropic (backend = 'anthropic') - multiple pairs with thoughts")

res_anth_multi <- handle_http_error(
  submit_llm_pairs(
    pairs             = pairs_small,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "anthropic",
    reasoning         = "enabled",
    max_tokens             = 2048,
    thinking_budget_tokens = 1024,
    verbose           = TRUE,
    status_every      = 1,
    progress          = TRUE,
    include_raw       = TRUE
  )
)

if (!is.null(res_anth_multi)) {
  cat("Result:\n")
  summarise_result(res_anth_multi, show_thoughts_snippet = TRUE)
}

# =====================================================================
# Gemini: thinking + thoughts
# =====================================================================

print_section("Gemini (backend = 'gemini') - single pair with thoughts")

res_gem_single <- handle_http_error(
  llm_compare_pair(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "gemini",
    thinking_level    = "low",
    include_thoughts  = TRUE,
    include_raw       = TRUE
  )
)

if (!is.null(res_gem_single)) {
  cat("Result:\n")
  summarise_result(res_gem_single, show_thoughts_snippet = TRUE)
}

print_section("Gemini (backend = 'gemini') - multiple pairs with thoughts")

res_gem_multi <- handle_http_error(
  submit_llm_pairs(
    pairs             = pairs_small,
    model             = "gemini-3-pro-preview",
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
  )
)

if (!is.null(res_gem_multi)) {
  cat("Result:\n")
  summarise_result(res_gem_multi, show_thoughts_snippet = TRUE)
}

cat("All backend live checks completed.\n")
