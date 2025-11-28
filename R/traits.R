#' Get a trait name and description for prompts
#'
#' This helper returns both a short display name and a longer
#' description for a scoring trait. These can be inserted into
#' the prompt template via the \code{{TRAIT_NAME}} and
#' \code{{TRAIT_DESCRIPTION}} placeholders.
#'
#' @param name Character identifier for a built-in trait. One of
#'   \code{"overall_quality"} or \code{"organization"}.
#'   Ignored if \code{custom_description} is supplied.
#' @param custom_name Optional short label to use when supplying a
#'   \code{custom_description}. Defaults to "Custom trait" if
#'   \code{custom_description} is provided but \code{custom_name}
#'   is \code{NULL}.
#' @param custom_description Optional full-text definition of a
#'   custom trait. When supplied, built-in \code{name} values are
#'   ignored and this text is returned instead.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{name}{Short display label for the trait (e.g., "Overall Quality").}
#'   \item{description}{Full-text definition of the trait, suitable for
#'         inclusion in the prompt.}
#' }
#'
#' @examples
#' td <- trait_description("overall_quality")
#' td$name
#' td$description
#'
#' custom_td <- trait_description(
#'   custom_name = "Ideas",
#'   custom_description = "Quality and development of ideas in the writing."
#' )
#' custom_td$name
#' custom_td$description
#'
#' @export
trait_description <- function(name = c("overall_quality", "organization"),
                              custom_name = NULL,
                              custom_description = NULL) {
  # Custom trait path
  if (!is.null(custom_description)) {
    nm <- if (!is.null(custom_name)) custom_name else "Custom trait"
    return(list(
      name        = nm,
      description = custom_description
    ))
  }

  # Built-in traits
  name <- match.arg(name)

  trait_name <- switch(
    name,
    overall_quality = "Overall Quality",
    organization    = "Organization"
  )

  trait_desc <- switch(
    name,
    overall_quality =
      "Overall quality of the writing, considering how well ideas are expressed, how clearly the writing is organized, and how effective the language and conventions are.",
    organization =
      "How clearly the writing is organized, including logical sequencing of ideas and effective use of transitions."
  )

  list(
    name        = trait_name,
    description = trait_desc
  )
}
