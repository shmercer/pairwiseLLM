#' Internal: resolve API key with helpful guidance
#'
#' This helper is used by backend-specific functions such as
#' `.openai_api_key()` and `.anthropic_api_key()` to retrieve an API key from
#' either an explicit argument or an environment variable. If no key can be
#' found, it throws an informative error describing how to set the key.
#'
#' @param api_key Optional key value supplied directly by the user. If this is a
#'   non-empty character scalar, it is returned as-is.
#' @param env_var Name of the environment variable to use when `api_key` is
#'   `NULL` or empty.
#' @param service Human-readable service name used in the error message (for
#'   example `"OpenAI"` or `"Anthropic"`).
#'
#' @keywords internal
#' @noRd
.get_api_key <- function(api_key = NULL, env_var, service = env_var) {
  # 1) Use explicit api_key if supplied and non-empty
  if (!is.null(api_key) &&
      is.character(api_key) &&
      length(api_key) == 1L &&
      nzchar(api_key)) {
    return(api_key)
  }

  # 2) Fall back to environment variable
  key <- Sys.getenv(env_var, unset = "")
  if (nzchar(key)) {
    return(key)
  }

  # 3) Informative error if nothing was found
  stop(
    "No API key found for ", service, ".\n",
    "Please either:\n",
    "  • Set the ", env_var, " environment variable, or\n",
    "  • Supply `api_key` explicitly to the function you are calling.\n\n",
    "Common ways to set a key in R:\n",
    "  - For the current session only:\n",
    "      Sys.setenv(", env_var, ' = "YOUR_KEY_HERE")\n',
    "  - Persistently (recommended):\n",
    "      usethis::edit_r_environ()\n",
    "    and add a line such as:\n",
    "      ", env_var, ' = "YOUR_KEY_HERE"\n\n',
    "After editing .Renviron, restart R (Session → Restart R in RStudio) ",
    "so the change takes effect.",
    call. = FALSE
  )
}
