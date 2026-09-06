#' Gemini parameter normalization helpers
#'
#' @keywords internal
#' @noRd
normalize_gemini_service_tier <- function(service_tier) {
  if (is.null(service_tier)) {
    return(NULL)
  }

  if (!is.character(service_tier) || length(service_tier) != 1L || is.na(service_tier)) {
    rlang::abort("`service_tier` must be NULL or a non-missing character scalar.")
  }

  if (identical(service_tier, "standard")) {
    return(NULL)
  }

  allowed <- c("flex", "priority")
  if (!service_tier %in% allowed) {
    rlang::abort(
      paste0(
        "`service_tier` must be one of NULL, \"standard\", \"flex\", or ",
        "\"priority\" for the Gemini Developer API."
      )
    )
  }

  service_tier
}
