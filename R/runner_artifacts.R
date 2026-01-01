# ---- runner artifacts (internal) ----

#' Internal: ensure a directory exists
#'
#' Creates `path` (recursively) if it does not exist. If `path` is `NULL`, `NA`,
#' or empty, this function is a no-op.
#'
#' @param path Directory path.
#'
#' @return Invisibly, `path` (as character) if created/exists, otherwise `NULL`.
#' @keywords internal
.ensure_dir <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  path <- as.character(path)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}


#' Internal: write a CSV safely
#'
#' Writes a data frame/tibble to `path` without erroring when `x` is `NULL` or
#' empty.
#'
#' - If `x` is `NULL`, this is a no-op (returns invisibly).
#' - If `x` is a 0-row data frame/tibble, a header-only CSV is written.
#'
#' @param x A data frame or tibble (or `NULL`).
#' @param path File path.
#'
#' @return Invisibly, the path written to (or `NULL` if `x` is `NULL`).
#' @keywords internal
.write_csv_safe <- function(x, path) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty file path.", call. = FALSE)
  }
  path <- as.character(path)
  .ensure_dir(dirname(path))

  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  utils::write.csv(x, file = path, row.names = FALSE)
  invisible(path)
}
