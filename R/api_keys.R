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
    "  - Set the ", env_var, " environment variable, or\n",
    "  - Supply `api_key` explicitly to the function you are calling.\n\n",
    "Common ways to set a key in R:\n",
    "  - For the current session only:\n",
    "      Sys.setenv(", env_var, ' = "YOUR_KEY_HERE")\n',
    "  - Persistently (recommended):\n",
    "      usethis::edit_r_environ()\n",
    "    and add a line such as:\n",
    "      ", env_var, ' = "YOUR_KEY_HERE"\n\n',
    "After editing .Renviron, restart R (Session -> Restart R in RStudio) ",
    "so the change takes effect.",
    call. = FALSE
  )
}

#' Check configured API keys for LLM backends
#'
#' This function inspects the current R session for configured API keys
#' used by pairwiseLLM. It checks for known environment variables such as
#' `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, and `GEMINI_API_KEY`, and returns
#' a small tibble summarising which keys are available.
#'
#' It does **not** print or return the key values themselves - only whether
#' each key is present. This makes it safe to run in logs, scripts, and
#' shared environments.
#'
#' @param verbose Logical; if `TRUE` (default), prints a human-readable
#'   summary to the console describing which keys are set and how to
#'   configure missing ones.
#'
#' @return A tibble (data frame) with one row per backend and columns:
#' \describe{
#'   \item{backend}{Short backend identifier, e.g. `"openai"`, `"anthropic"`, `"gemini"`.}
#'   \item{service}{Human-readable service name, e.g. `"OpenAI"`, `"Anthropic"`, `"Google Gemini"`.}
#'   \item{env_var}{Name of the environment variable that is checked.}
#'   \item{has_key}{Logical flag indicating whether the key is set and non-empty.}
#' }
#'
#' @examples
#' \dontrun{
#'   # In an interactive session, quickly check which keys are configured:
#'   check_llm_api_keys()
#'
#'   # In non-interactive scripts, you can disable messages and just use the result:
#'   status <- check_llm_api_keys(verbose = FALSE)
#'   status
#' }
#'
#' @export
check_llm_api_keys <- function(verbose = TRUE) {
  # Known backends and their primary env vars
  backends <- c("openai", "anthropic", "gemini")
  services <- c("OpenAI", "Anthropic", "Google Gemini")
  env_vars <- c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GEMINI_API_KEY")

  values <- vapply(
    env_vars,
    function(v) Sys.getenv(v, unset = ""),
    FUN.VALUE = character(1L)
  )
  has_key <- nzchar(values)

  res <- tibble::tibble(
    backend = backends,
    service = services,
    env_var = env_vars,
    has_key = has_key
  )

  if (isTRUE(verbose)) {
    any_set  <- any(has_key)
    all_set  <- all(has_key)
    none_set <- !any_set

    if (all_set) {
      message(
        "All known LLM API keys are set: ",
        paste(env_vars, collapse = ", "),
        "."
      )
    } else if (none_set) {
      message(
        "No LLM API keys are currently set for known backends:\n",
        "  - OpenAI:         OPENAI_API_KEY\n",
        "  - Anthropic:      ANTHROPIC_API_KEY\n",
        "  - Google Gemini:  GEMINI_API_KEY\n",
        "\n",
        "Use `usethis::edit_r_environ()` to add the keys persistently, e.g.:\n",
        '  OPENAI_API_KEY   = "YOUR_OPENAI_KEY_HERE"\n',
        '  ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY_HERE"\n',
        '  GEMINI_API_KEY    = "YOUR_GEMINI_KEY_HERE"'
      )
    } else {
      message("Some LLM API keys are not set:")
      for (i in seq_along(env_vars)) {
        if (has_key[i]) {
          message(
            "- ", services[i], " (", backends[i], "): ",
            env_vars[i], " is set."
          )
        } else {
          msg <- paste0(
            "- ", services[i], " (", backends[i], "): ", env_vars[i], " is not set.\n",
            "  Set it for this session with:\n",
            "    Sys.setenv(", env_vars[i], ' = "YOUR_KEY_HERE")\n',
            "  Or persistently via:\n",
            "    usethis::edit_r_environ()\n",
            "  and add a line like:\n",
            "    ", env_vars[i], ' = "YOUR_KEY_HERE"'
          )
          message(msg)
        }
      }
    }
  }

  res
}

#' Internal: Google Gemini API key helper
#'
#' This is a thin wrapper around `.get_api_key()` for the Google Gemini backend.
#' It looks for a `GEMINI_API_KEY` environment variable by default and can be
#' overridden explicitly via the `api_key` argument.
#'
#' @param api_key Optional character scalar. If `NULL` or an empty string, the
#'   helper falls back to `Sys.getenv("GEMINI_API_KEY")`.
#'
#' @keywords internal
.gemini_api_key <- function(api_key = NULL) {
  .get_api_key(
    api_key = api_key,
    env_var = "GEMINI_API_KEY",
    service = "Google Gemini"
  )
}
