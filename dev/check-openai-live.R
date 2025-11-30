# dev/check-openai-live.R
#
# Manual smoke test for OpenAI live pairwise comparisons
# - Verifies chat.completions path (no thoughts)
# - Tries responses path with reasoning summaries (thoughts + content split)
#
# If your API key / account does not have access to the Responses endpoint
# or reasoning models, the responses section will print a warning and skip.
#
# Requires:
#   - OPENAI_API_KEY set in your environment
#   - Network access
#
# This script will incur OpenAI API cost for the chosen models.

library(pairwiseLLM)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY is not set. Please set it before running this script.", call. = FALSE)
}

# -------------------------------------------------------------------
# Config
# -------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")

# Take two short samples
samples <- example_writing_samples[1:2, ]

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

ID1   <- samples$ID[1]
ID2   <- samples$ID[2]
text1 <- samples$text[1]
text2 <- samples$text[2]

# You can adjust these as desired
chat_model    <- "gpt-4.1"
reason_model  <- "gpt-5.1"  # change if your account has access to a different reasoning model
reason_effort <- "low"

DO_CHAT      <- TRUE
DO_RESPONSES <- TRUE

# -------------------------------------------------------------------
# 1) Chat completions endpoint (no thoughts)
# -------------------------------------------------------------------
if (DO_CHAT) {
  cat("\n=== OpenAI live: chat.completions (no thoughts) ===\n")

  res_chat <- openai_compare_pair_live(
    ID1               = ID1,
    text1             = text1,
    ID2               = ID2,
    text2             = text2,
    model             = chat_model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "chat.completions",
    temperature       = 0,
    include_raw       = TRUE
  )

  print(res_chat[, c(
    "custom_id", "model", "object_type",
    "status_code", "error_message",
    "thoughts", "content",
    "better_sample", "better_id",
    "prompt_tokens", "completion_tokens", "total_tokens"
  )])

  cat("\nChat completions check:\n")
  cat("  thoughts column is NA: ",
      is.na(res_chat$thoughts), "\n")
  cat("  content contains <BETTER_SAMPLE> tag: ",
      grepl("<BETTER_SAMPLE>", res_chat$content, fixed = TRUE), "\n")
  cat("  better_id: ", res_chat$better_id, "\n")
}

# -------------------------------------------------------------------
# 2) Responses endpoint with reasoning summaries (thoughts + content)
# -------------------------------------------------------------------
if (DO_RESPONSES) {
  cat("\n=== OpenAI live: responses (reasoning + thoughts) ===\n")

  res_resp <- tryCatch(
    openai_compare_pair_live(
      ID1               = ID1,
      text1             = text1,
      ID2               = ID2,
      text2             = text2,
      model             = reason_model,
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning         = reason_effort,
      include_thoughts  = TRUE,
      include_raw       = TRUE
    ),
    error = function(e) {
      cat("\n[WARNING] Responses + reasoning test failed:\n  ",
          conditionMessage(e), "\n", sep = "")

      if (inherits(e, "httr2_http")) {
        cat(
          "This often means:\n",
          "  - Your API key/account does not have access to the Responses endpoint,\n",
          "  - The chosen model (", reason_model, ") is not available to you,\n",
          "  - Or the model does not support the requested reasoning settings.\n",
          "You can try updating `reason_model` or disabling DO_RESPONSES.\n",
          sep = ""
        )
      }

      return(NULL)
    }
  )

  if (!is.null(res_resp)) {
    print(res_resp[, c(
      "custom_id", "model", "object_type",
      "status_code", "error_message",
      "thoughts", "content",
      "better_sample", "better_id",
      "prompt_tokens", "completion_tokens", "total_tokens"
    )])

    cat("\nResponses + reasoning check:\n")
    cat("  thoughts is non-NA: ",
        !is.na(res_resp$thoughts), "\n")
    cat("  content contains <BETTER_SAMPLE> tag: ",
        grepl("<BETTER_SAMPLE>", res_resp$content, fixed = TRUE), "\n")
    cat("  better_id: ", res_resp$better_id, "\n")

    if (!is.na(res_resp$thoughts)) {
      cat("\n--- Thoughts (reasoning summary, first 500 chars) ---\n")
      cat(substr(res_resp$thoughts, 1, 500), "\n")
    }
  } else {
    cat("\nSkipping further Responses checks due to error above.\n")
  }
}

cat("\nDone.\n")
