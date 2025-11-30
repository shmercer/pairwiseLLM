# dev/debug-gemini-live.R
#
# Focused debugging for Gemini live calls via llm_compare_pair().

library(pairwiseLLM)

`%||%` <- function(x, y) if (is.null(x)) y else x

print_http_error_details <- function(e) {
  cat("[ERROR] ", conditionMessage(e), "\n", sep = "")

  cat("\nCondition class:\n")
  print(class(e))

  cat("\nCondition structure (str, max.level = 1):\n")
  utils::str(e, max.level = 1)
  cat("\n")

  if (inherits(e, "httr2_http")) {
    resp <- e$response %||% NULL
    if (is.null(resp)) {
      cat("No response object attached to httr2 error.\n")
      return(invisible(NULL))
    }

    cat("\n=== HTTP Status ===\n")
    print(httr2::resp_status(resp))

    cat("\n=== Raw Response Body ===\n")
    raw_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(err) paste0("<failed to read body: ", conditionMessage(err), ">")
    )
    cat(raw_body, "\n\n")

    parsed <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = function(err) NULL
    )
    if (!is.null(parsed)) {
      cat("=== Parsed Response Body (str, max.level = 2) ===\n")
      utils::str(parsed, max.level = 2)
      cat("\n")
    }
  }

  invisible(NULL)
}

# -------------------------------------------------------------------
# Minimal test call
# -------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]
td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

ID1 <- samples$ID[1]
ID2 <- samples$ID[2]

cat("=== Gemini debug: single llm_compare_pair() call ===\n\n")

res <- tryCatch(
  llm_compare_pair(
    ID1               = ID1,
    text1             = samples$text[1],
    ID2               = ID2,
    text2             = samples$text[2],
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    backend           = "gemini",
    include_raw       = TRUE,
    include_thoughts  = TRUE
  ),
  error = function(e) {
    print_http_error_details(e)
    return(NULL)
  }
)

if (!is.null(res)) {
  cat("Call returned tibble:\n")
  print(res)

  if ("status_code" %in% names(res)) {
    cat("\nStatus code: ", res$status_code[1], "\n", sep = "")
  }

  if ("error_message" %in% names(res)) {
    cat("Error message (truncated):\n")
    cat(substr(res$error_message[1], 1, 300), "\n")
  }

  if ("raw_response" %in% names(res)) {
    cat("\nraw_response[[1]] structure (max.level = 2):\n")
    utils::str(res$raw_response[[1]], max.level = 2)
  } else {
    cat("\nNo raw_response column present.\n")
  }
}

cat("\nDone.\n")
