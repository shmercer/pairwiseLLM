# dev/debug-openai-thoughts.R

library(pairwiseLLM)

# Safe helper, same as in the package
`%||%` <- function(x, y) if (is.null(x)) y else x

# Extract any reasoning summary text from a raw OpenAI /v1/responses body
extract_openai_reasoning_summary_raw <- function(raw) {
  if (is.null(raw)) return(NA_character_)

  reasoning_chunks <- character(0)

  # 1) Preferred: reasoning output item (type = "reasoning") with a summary
  output <- raw$output %||% list()
  if (length(output) > 0L) {
    for (out_el in output) {
      if (!is.null(out_el$type) && identical(out_el$type, "reasoning")) {
        rs <- out_el$summary
        if (!is.null(rs) && length(rs) > 0L) {
          if (is.list(rs) && !is.data.frame(rs)) {
            for (s in rs) {
              if (!is.null(s$text)) {
                reasoning_chunks <- c(
                  reasoning_chunks,
                  as.character(s$text %||% "")
                )
              }
            }
          } else if (is.data.frame(rs) && "text" %in% names(rs)) {
            reasoning_chunks <- c(
              reasoning_chunks,
              as.character(rs$text)
            )
          }
        }
      }
    }
  }

  # 2) Back-compat: top-level body$reasoning$summary$text
  if (!length(reasoning_chunks) &&
      !is.null(raw$reasoning) &&
      !is.null(raw$reasoning$summary)) {

    rs <- raw$reasoning$summary

    if (is.list(rs) && !is.null(rs$text)) {
      reasoning_chunks <- c(
        reasoning_chunks,
        as.character(rs$text %||% "")
      )
    }
    # If rs is just "auto"/"detailed", we ignore it on purpose.
  }

  if (!length(reasoning_chunks)) {
    return(NA_character_)
  }

  paste(reasoning_chunks, collapse = " ")
}

# --------------------------------------------------------------------
# Example live check (adjust IDs/texts/trait as needed)
# --------------------------------------------------------------------

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

res <- openai_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "responses",
  reasoning         = "low",
  include_thoughts  = TRUE,
  include_raw       = TRUE
)

cat("=== Parsed thoughts column ===\n")
print(res$thoughts)

cat("\n=== Extracted reasoning summary from raw_response ===\n")
raw <- res$raw_response[[1]]
summary_text <- extract_openai_reasoning_summary_raw(raw)
print(summary_text)

cat("\n=== Raw reasoning + output structure (truncated) ===\n")
str(list(
  reasoning = raw$reasoning,
  output    = raw$output
), max.level = 3)
