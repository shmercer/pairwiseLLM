# Internal parsing helpers shared across backends

# Extract a winner token ("SAMPLE_1"/"SAMPLE_2") from model text output.
# This is intentionally tolerant to whitespace/newlines/case and minor variants.
.extract_better_sample <- function(content,
                                   tag_prefix = "<BETTER_SAMPLE>",
                                   tag_suffix = "</BETTER_SAMPLE>") {
  if (length(content) != 1L || is.na(content) || !nzchar(content)) {
    return(NA_character_)
  }

  # Escape regex metacharacters in tag strings
  esc <- function(x) gsub("([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\1", x, perl = TRUE)
  pre <- esc(tag_prefix)
  suf <- esc(tag_suffix)

  # Primary: look for tag-wrapped winner with flexible whitespace, case-insensitive
  pat <- paste0("(?is)", pre, "\\s*(SAMPLE\\s*[_-]?\\s*1|SAMPLE\\s*[_-]?\\s*2)\\s*", suf)
  m <- regexpr(pat, content, perl = TRUE)

  if (!is.na(m[1]) && m[1] != -1L) {
    match_txt <- regmatches(content, m)
    token <- sub(paste0("(?is)", pre, "\\s*"), "", match_txt, perl = TRUE)
    token <- sub(paste0("(?is)\\s*", suf, ".*$"), "", token, perl = TRUE)
    token <- toupper(token)
    token <- gsub("[\\s-]+", "_", token)
    token <- gsub("__+", "_", token)
    if (grepl("1", token)) {
      return("SAMPLE_1")
    }
    if (grepl("2", token)) {
      return("SAMPLE_2")
    }
  }

  # Fallback: allow bare tokens without tags
  if (grepl("(?i)SAMPLE\\s*[_-]?\\s*1", content, perl = TRUE)) {
    return("SAMPLE_1")
  }
  if (grepl("(?i)SAMPLE\\s*[_-]?\\s*2", content, perl = TRUE)) {
    return("SAMPLE_2")
  }

  NA_character_
}
