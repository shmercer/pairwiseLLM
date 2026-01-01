# Internal helpers for writing runner artifacts safely.
#
# These are intentionally unexported and undocumented in the public help index.

#' @noRd
.ensure_dir <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  if (dir.exists(path)) {
    return(invisible(TRUE))
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(dir.exists(path))
}

#' @noRd
.write_csv_safe <- function(x, path) {
  if (is.null(x)) {
    return(invisible(FALSE))
  }
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty string.", call. = FALSE)
  }

  # Create parent directory
  .ensure_dir(dirname(path))

  # Coerce to data.frame (tibble is fine)
  if (!inherits(x, "data.frame")) {
    x <- as.data.frame(x)
  }

  # Write header-only CSV for 0-row frames (keeps schema visible).
  utils::write.csv(x, file = path, row.names = FALSE)
  invisible(TRUE)
}
