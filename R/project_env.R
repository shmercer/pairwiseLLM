#' Print project-level .Renviron instructions for pairwiseLLM
#'
#' These helpers make it easier to keep configuration project-specific (rather
#' than global) by printing instructions you can copy into a local `.Renviron`
#' file.
#'
#' Two common settings are:
#' - `RETICULATE_PYTHON` for the Python executable used by the `reticulate`
#'   package (used by the sentence-transformers embedding engine).
#' - `PAIRWISELLM_EMBEDDINGS_CACHE_DIR` for a persistent embeddings cache
#'   directory used by [compute_text_embeddings()].
#'
#' After updating a `.Renviron`, restart R so the new environment variables are
#' picked up.
#'
#' @param python Path to a Python executable.
#' @param cache_dir Directory path to store cached embeddings.
#' @param file Path to the project `.Renviron` file (used for display in the
#'   printed instructions). Defaults to `.Renviron` in the current working
#'   directory.
#' @param overwrite Logical. Ignored (kept for backward compatibility).
#'
#' @return The `KEY="value"` line to add to the `.Renviron` file (invisibly).
#'
#' @examples
#' \dontrun{
#' # Print instructions for project-specific reticulate python:
#' set_project_reticulate_python("/path/to/python")
#'
#' # Print instructions for persisting embeddings caching in this project:
#' set_project_embeddings_cache_dir("./.cache/pairwiseLLM")
#' }
#'
#' @name project_env_helpers
NULL

.project_env_entry <- function(key, value) {
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("`key` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("`value` must be a single non-empty string.", call. = FALSE)
  }
  # Use a stable quoting style across platforms so instructions are
  # copy/pasteable and tests behave consistently.
  paste0(key, "=", shQuote(value, type = "cmd"))
}

.print_project_env_instructions <- function(entry, file = ".Renviron") {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    file <- ".Renviron"
  }
  msg <- paste0(
    "To configure pairwiseLLM for this project, add the following line to ",
    sQuote(file), ":\n\n",
    "  ", entry, "\n\n",
    "Then restart R so the environment variable is picked up."
  )
  message(msg)
  invisible(entry)
}

#' Set project-level RETICULATE_PYTHON
#'
#' Prints a `RETICULATE_PYTHON="..."` line you can add to a project `.Renviron`.
#'
#' @inheritParams project_env_helpers
#' @param python Path to a Python executable.
#'
#' @export
set_project_reticulate_python <- function(python, file = ".Renviron", overwrite = FALSE) {
  if (!missing(overwrite) && isTRUE(overwrite)) {
    # kept for backward compatibility; no file writing is performed
  }
  entry <- .project_env_entry("RETICULATE_PYTHON", python)
  .print_project_env_instructions(entry, file = file)
}

#' Set project-level PAIRWISELLM_EMBEDDINGS_CACHE_DIR
#'
#' Prints a `PAIRWISELLM_EMBEDDINGS_CACHE_DIR="..."` line you can add to a
#' project `.Renviron`. When set, [compute_text_embeddings()] uses this value as
#' the default cache directory when `cache_dir = NULL`.
#'
#' @inheritParams project_env_helpers
#' @param cache_dir Directory path.
#'
#' @export
set_project_embeddings_cache_dir <- function(cache_dir, file = ".Renviron", overwrite = FALSE) {
  if (!missing(overwrite) && isTRUE(overwrite)) {
    # kept for backward compatibility; no file writing is performed
  }
  entry <- .project_env_entry("PAIRWISELLM_EMBEDDINGS_CACHE_DIR", cache_dir)
  .print_project_env_instructions(entry, file = file)
}
