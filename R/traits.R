#' Get a trait description string
#'
#' This function returns a trait description string that can be inserted
#' into LLM prompts. It provides a small set of built-in examples and
#' can also return custom user-specified descriptions directly.
#'
#' @param name A character string identifying a built-in trait
#'   description. Current options include:
#'   \itemize{
#'     \item \code{"overall_quality"}: a simple, generic definition of overall quality.
#'     \item \code{"organization"}: a simple, generic definition of organization.
#'   }
#'   This argument is ignored if \code{custom} is not \code{NULL}.
#' @param custom An optional character string containing a fully
#'   specified trait description. If provided, this string is returned
#'   as-is.
#'
#' @return A character string with the trait description.
#'
#' @examples
#' # Use a built-in description
#' trait_description("overall_quality")
#'
#' # Provide your own description
#' trait_description(custom = "Clarity of ideas and argumentation.")
#'
#' @export
trait_description <- function(name = c("overall_quality", "organization"),
                              custom = NULL) {
  if (!is.null(custom)) {
    return(custom)
  }

  name <- match.arg(name)

  traits <- list(
    overall_quality = paste(
      "Overall quality of the writing, considering how well ideas are",
      "expressed, how clearly the writing is organized, and how",
      "effective the language and conventions are."
    ),
    organization = paste(
      "Organization of the writing, including the clarity of the",
      "introduction and conclusion, the logical flow of ideas, and",
      "the use of transitions between sentences and paragraphs."
    )
  )

  traits[[name]]
}
