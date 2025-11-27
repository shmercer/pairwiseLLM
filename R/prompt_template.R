#' Get or set a prompt template for pairwise comparisons
#'
#' This function returns a default prompt template that includes
#' placeholders for the trait description and two writing samples.
#' The placeholders \code{{TRAIT_DESCRIPTION}}, \code{{SAMPLE_1}},
#' and \code{{SAMPLE_2}} must be present in any custom template.
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
#' trait <- trait_description("overall_quality")
#' text1 <- example_writing_samples$text[1]
#' text2 <- example_writing_samples$text[2]
#' prompt <- build_prompt(tmpl, trait_desc = trait, text1 = text1, text2 = text2)
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

Your task is to decide which of two student writing samples shows BETTER {TRAIT_DESCRIPTION}.

INSTRUCTIONS:
1. Read BOTH samples carefully.
2. Compare the samples only on {TRAIT_DESCRIPTION}.
3. Ignore the order in which the samples are presented; do NOT automatically prefer SAMPLE_1.
4. If the samples are equally strong or weak, choose the one that is *slightly* better on {TRAIT_DESCRIPTION}.
5. Think briefly about which sample is better on {TRAIT_DESCRIPTION}.
6. Then respond EXACTLY in one line using ONE of the following formats:
   <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
   or
   <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>

Do not include any other text.

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

#' Build a prompt for a pair of samples
#'
#' This function substitutes a trait description and two writing samples
#' into a prompt template created by \code{\link{set_prompt_template}}.
#' The template must contain the placeholders
#' \code{{TRAIT_DESCRIPTION}}, \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' @param template A character string containing a prompt template.
#'   Typically this is the result of \code{\link{set_prompt_template}()}.
#' @param trait_desc A character string with the trait description to
#'   insert in place of \code{{TRAIT_DESCRIPTION}}.
#' @param text1 A character string containing the first writing sample.
#'   This is substituted for \code{{SAMPLE_1}}.
#' @param text2 A character string containing the second writing sample.
#'   This is substituted for \code{{SAMPLE_2}}.
#'
#' @return A single character string containing the completed prompt,
#'   ready to be sent to an LLM.
#'
#' @examples
#' # Get a default template and a built-in trait description
#' tmpl  <- set_prompt_template()
#' trait <- trait_description("overall_quality")
#'
#' text1 <- "This is the first example writing sample."
#' text2 <- "This is the second example writing sample."
#'
#' prompt <- build_prompt(tmpl, trait_desc = trait, text1 = text1, text2 = text2)
#' cat(substr(prompt, 1, 200), "...\n")
#'
#' # Use the built-in example writing samples
#' data("example_writing_samples")
#' prompt2 <- build_prompt(
#'   tmpl,
#'   trait_desc = trait,
#'   text1      = example_writing_samples$text[1],
#'   text2      = example_writing_samples$text[2]
#' )
#' cat(substr(prompt2, 1, 200), "...\n")
#'
#' @export
build_prompt <- function(template,
                         trait_desc,
                         text1,
                         text2) {
  out <- template
  out <- gsub("{TRAIT_DESCRIPTION}", trait_desc, out, fixed = TRUE)
  out <- gsub("{SAMPLE_1}",        text1,      out, fixed = TRUE)
  out <- gsub("{SAMPLE_2}",        text2,      out, fixed = TRUE)
  out
}
