# dev/debug-anthropic-thinking.R
#
# Debug Anthropic "extended thinking" responses and how they map into
# pairwiseLLM. This script:
#   * Calls anthropic_compare_pair_live() with reasoning = "enabled"
#   * Prints the result tibble
#   * If available, prints the `thoughts` column
#   * Inspects raw_response$content to show thinking vs text blocks

cat("=== Anthropic extended thinking debug ===\n\n")

suppressPackageStartupMessages({
  library(pairwiseLLM)
})

# --------------------------------------------------------------------
# Small helper to pretty-print httr2 HTTP errors (if we ever get one)
# --------------------------------------------------------------------
print_httr2_error <- function(e) {
  cat("[HTTR2 HTTP ERROR]\n")
  cat("Condition class:\n")
  print(class(e))
  cat("\nCondition message:\n")
  cat(conditionMessage(e), "\n\n")

  # Try to pull out any attached response
  resp <- tryCatch(e$resp, error = function(...) NULL)
  if (is.null(resp)) {
    cat("No response object attached to httr2 error.\n\n")
    return(invisible(NULL))
  }

  cat("Raw response status (if available):\n")
  status <- tryCatch(httr2::resp_status(resp), error = function(...) NA_integer_)
  print(status)
  cat("\n")

  body_raw <- tryCatch(httr2::resp_body_string(resp), error = function(...) NULL)
  if (!is.null(body_raw)) {
    cat("Raw response body (truncated to 1000 chars):\n")
    if (nchar(body_raw) > 1000) {
      cat(substr(body_raw, 1, 1000), "...\n\n", sep = "")
    } else {
      cat(body_raw, "\n\n")
    }
  } else {
    cat("Could not read response body as string.\n\n")
  }

  invisible(NULL)
}

# --------------------------------------------------------------------
# Pick a simple pair from example data
# --------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

ID1   <- samples$ID[1]
ID2   <- samples$ID[2]
text1 <- samples$text[1]
text2 <- samples$text[2]

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# You can tweak the model here if needed:
model <- "claude-sonnet-4-5"

cat("Using model: ", model, "\n", sep = "")
cat("Comparing IDs: ", ID1, " vs ", ID2, "\n\n", sep = "")

# --------------------------------------------------------------------
# Make a single Anthropic call with reasoning = "enabled"
# --------------------------------------------------------------------
res <- tryCatch(
  {
    anthropic_compare_pair_live(
      ID1               = ID1,
      text1             = text1,
      ID2               = ID2,
      text2             = text2,
      model             = model,
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      reasoning         = "enabled",
      # Reasonable defaults for debugging
      max_tokens             = 2048,
      thinking_budget_tokens = 1024,
      include_raw            = TRUE
    )
  },
  `httr2_http` = function(e) {
    print_httr2_error(e)
    return(NULL)
  },
  error = function(e) {
    cat("[ERROR] ", conditionMessage(e), "\n", sep = "")
    return(NULL)
  }
)

if (is.null(res)) {
  cat("Call returned NULL due to an error. See details above.\n")
  quit(save = "no")
}

cat("Call succeeded; result tibble:\n")
print(res)
cat("\n")

# --------------------------------------------------------------------
# If/when anthropic_compare_pair_live grows a `thoughts` column, show it
# --------------------------------------------------------------------
if ("thoughts" %in% names(res)) {
  cat("=== thoughts column (res$thoughts) ===\n")
  print(res$thoughts)
  cat("\n")
} else {
  cat("No `thoughts` column present in result (anthropic_compare_pair_live\n",
      "currently only returns `content`).\n\n", sep = "")
}

# --------------------------------------------------------------------
# Inspect raw_response to see thinking vs text blocks
# --------------------------------------------------------------------
if (!("raw_response" %in% names(res))) {
  cat("No raw_response column present; cannot inspect content blocks.\n")
  quit(save = "no")
}

raw <- res$raw_response[[1]]

if (is.null(raw)) {
  cat("raw_response[[1]] is NULL; no body captured.\n")
  quit(save = "no")
}

cat("=== Raw response top-level keys ===\n")
print(names(raw))
cat("\n")

content_blocks <- raw$content

if (is.null(content_blocks)) {
  cat("raw$content is NULL; nothing to inspect.\n")
  quit(save = "no")
}

cat("=== Raw content blocks (type + truncated payload) ===\n")
for (i in seq_along(content_blocks)) {
  blk <- content_blocks[[i]]
  blk_type <- blk$type %||% NA_character_
  cat(sprintf("Block %d: type = %s\n", i, blk_type))

  if (identical(blk_type, "thinking")) {
    think_text <- blk$thinking %||% ""
    cat("  thinking (truncated 300 chars):\n")
    if (nchar(think_text) > 300) {
      cat("  ", substr(think_text, 1, 300), "...\n", sep = "")
    } else {
      cat("  ", think_text, "\n", sep = "")
    }
  } else if (identical(blk_type, "redacted_thinking")) {
    red_data <- blk$data %||% ""
    cat("  redacted_thinking (truncated 120 chars):\n")
    if (nchar(red_data) > 120) {
      cat("  ", substr(red_data, 1, 120), "...\n", sep = "")
    } else {
      cat("  ", red_data, "\n", sep = "")
    }
  } else if (identical(blk_type, "text")) {
    txt <- blk$text %||% ""
    cat("  text (truncated 300 chars):\n")
    if (nchar(txt) > 300) {
      cat("  ", substr(txt, 1, 300), "...\n", sep = "")
    } else {
      cat("  ", txt, "\n", sep = "")
    }
  } else {
    # Other block types: tool_use, tool_result, etc.
    cat("  (non-thinking, non-text block)\n")
  }

  cat("\n")
}

# --------------------------------------------------------------------
# Also synthesise a combined reasoning string from thinking blocks only
# (so we can later decide how to populate a `thoughts` column)
# --------------------------------------------------------------------
thinking_pieces <- character(0)
text_pieces     <- character(0)

for (blk in content_blocks) {
  if (!is.null(blk$type) && blk$type == "thinking") {
    thinking_pieces <- c(thinking_pieces, as.character(blk$thinking %||% ""))
  } else if (!is.null(blk$type) && blk$type == "redacted_thinking") {
    # You might want to include or exclude these in thoughts; here we mark them.
    thinking_pieces <- c(
      thinking_pieces,
      sprintf("[[REDACTED_THINKING %d chars]]", nchar(blk$data %||% ""))
    )
  } else if (!is.null(blk$type) && blk$type == "text") {
    text_pieces <- c(text_pieces, as.character(blk$text %||% ""))
  }
}

combined_thoughts <- if (length(thinking_pieces)) {
  paste(thinking_pieces, collapse = "\n")
} else {
  NA_character_
}

combined_text <- if (length(text_pieces)) {
  paste(text_pieces, collapse = "")
} else {
  NA_character_
}

cat("=== Synthesised thinking/text (for future `thoughts` implementation) ===\n")
cat("Combined thinking (from `thinking` / `redacted_thinking` blocks):\n")
if (is.na(combined_thoughts)) {
  cat("  <none>\n\n")
} else {
  if (nchar(combined_thoughts) > 600) {
    cat(substr(combined_thoughts, 1, 600), "...\n\n", sep = "")
  } else {
    cat(combined_thoughts, "\n\n")
  }
}

cat("Combined text (from `text` blocks):\n")
if (is.na(combined_text)) {
  cat("  <none>\n\n")
} else {
  if (nchar(combined_text) > 600) {
    cat(substr(combined_text, 1, 600), "...\n\n", sep = "")
  } else {
    cat(combined_text, "\n\n")
  }
}

cat("Done.\n")
