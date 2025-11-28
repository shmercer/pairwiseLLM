#' Get or set a prompt template for pairwise comparisons
#'
#' This function returns a default prompt template that includes
#' placeholders for the trait name, trait description, and two
#' writing samples. Any custom template must contain the
#' placeholders \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}},
#' \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' @param template Optional character string containing a custom template.
#'   If \code{NULL}, a default template is returned.
#' @param file Optional path to a text file containing a template.
#'   Ignored if \code{template} is not \code{NULL}.
#'
#' @return A character string containing the prompt template.
#'
#' @examples
#' # Get the default template
#' tmpl <- set_prompt_template()
#' cat(substr(tmpl, 1, 200), "...\n")
#'
#' # Using the template with example writing samples
#' data("example_writing_samples")
#' td <- trait_description("overall_quality")
#'
#' text1 <- example_writing_samples$text[1]
#' text2 <- example_writing_samples$text[2]
#'
#' prompt <- build_prompt(
#'   template   = tmpl,
#'   trait_name = td$name,
#'   trait_desc = td$description,
#'   text1      = text1,
#'   text2      = text2
#' )
#' cat(substr(prompt, 1, 200), "...\n")
#'
#' @export
set_prompt_template <- function(template = NULL,
                                file = NULL) {
  if (!is.null(template)) {
    return(.validate_template(template))
  }

  if (!is.null(file)) {
    template_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    return(.validate_template(template_text))
  }

  default <- "
You are an expert writing assessor.

Your task is to decide which of two student writing samples shows BETTER {TRAIT_NAME}.

Definition of {TRAIT_NAME}:
{TRAIT_DESCRIPTION}

INSTRUCTIONS:
1. Read BOTH samples carefully.

2. Evaluate the samples ONLY on {TRAIT_NAME}, according to the definition above.
   Do NOT consider length, formatting, grammar, topic relevance, or any other
   aspect unless it directly affects {TRAIT_NAME}.

3. The labels SAMPLE_1 and SAMPLE_2 are arbitrary. They do NOT indicate quality.
   SAMPLE_1 could have been SAMPLE_2 and vice versa.

3a. Before deciding, remind yourself explicitly that SAMPLE_1 and SAMPLE_2 might
    have been presented in the opposite order. Your decision MUST NOT depend on
    their position.

3b. After reading both samples, PAUSE and reconsider which sample is truly better
    on {TRAIT_NAME}, independent of position.

4. You MUST choose exactly one sample. If the samples seem equal, choose the one
   that is even slightly better on {TRAIT_NAME}. Do NOT output ties.

5. Decide which sample has BETTER QUALITY on {TRAIT_NAME}.
   Think silently. Do NOT reveal your reasoning.

6. After making your judgment, respond EXACTLY with ONE of the following lines:

   <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>

   OR

   <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>

6a. Before responding, VERIFY that the tag you are about to output matches
    your internal decision about which sample is better.

IMPORTANT:
- Output EXACTLY one of the two lines above.
- Do NOT add explanations, reasoning, punctuation, or extra text.
- Do NOT include chain-of-thought or justification.

SAMPLE 1:
{SAMPLE_1}

SAMPLE 2:
{SAMPLE_2}
"

  .validate_template(default)
}

# Internal helper: check that required placeholders are present
.validate_template <- function(template) {
  required <- c("{TRAIT_DESCRIPTION}", "{SAMPLE_1}", "{SAMPLE_2}")
  missing <- required[!vapply(required, grepl, logical(1), x = template, fixed = TRUE)]

  if (length(missing) > 0) {
    stop(
      "Template is missing required placeholder(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  template
}

#' Build a concrete LLM prompt from a template
#'
#' This function takes a prompt template (typically from
#' \code{\link{set_prompt_template}}), a trait name and description,
#' and two writing samples, and fills in the required placeholders.
#'
#' The template must contain the placeholders:
#' \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}},
#' \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' @param template Character string containing the prompt template.
#' @param trait_name Character scalar giving a short label for the trait
#'   (e.g., "Overall Quality").
#' @param trait_desc Character scalar giving the full definition of the trait.
#' @param text1 Character scalar containing the text for SAMPLE_1.
#' @param text2 Character scalar containing the text for SAMPLE_2.
#'
#' @return A single character string containing the completed prompt.
#'
#' @examples
#' tmpl <- set_prompt_template()
#' td   <- trait_description("overall_quality")
#' prompt <- build_prompt(
#'   template   = tmpl,
#'   trait_name = td$name,
#'   trait_desc = td$description,
#'   text1      = "This is sample 1.",
#'   text2      = "This is sample 2."
#' )
#' cat(substr(prompt, 1, 200), "...\n")
#'
#' @export
build_prompt <- function(template,
                         trait_name,
                         trait_desc,
                         text1,
                         text2) {
  if (!is.character(template) || length(template) != 1L) {
    stop("`template` must be a single character string.", call. = FALSE)
  }
  if (!is.character(trait_name) || length(trait_name) != 1L) {
    stop("`trait_name` must be a single character string.", call. = FALSE)
  }
  if (!is.character(trait_desc) || length(trait_desc) != 1L) {
    stop("`trait_desc` must be a single character string.", call. = FALSE)
  }
  if (!is.character(text1) || length(text1) != 1L) {
    stop("`text1` must be a single character string.", call. = FALSE)
  }
  if (!is.character(text2) || length(text2) != 1L) {
    stop("`text2` must be a single character string.", call. = FALSE)
  }

  out <- template
  out <- gsub("{TRAIT_NAME}",        trait_name, out, fixed = TRUE)
  out <- gsub("{TRAIT_DESCRIPTION}", trait_desc, out,   fixed = TRUE)
  out <- gsub("{SAMPLE_1}",          text1,      out,   fixed = TRUE)
  out <- gsub("{SAMPLE_2}",          text2,      out,   fixed = TRUE)

  out
}
