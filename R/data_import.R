#' Read writing samples from a data frame
#'
#' This function extracts ID and text columns from a data frame and
#' enforces that IDs are unique. By default, it assumes the first
#' column is the ID and the second column is the text.
#'
#' @param df A data frame or tibble containing at least two columns.
#' @param id_col Column specifying the IDs. Can be a column name (string)
#'   or a column index (integer). Defaults to 1.
#' @param text_col Column specifying the writing samples (character).
#'   Can be a column name or index. Defaults to 2.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{ID}: character ID for each sample
#'     \item \code{text}: character string of the writing sample
#'   }
#'   Any remaining columns in \code{df} are retained unchanged.
#'
#' @examples
#' df <- data.frame(
#'   StudentID = c("S1", "S2"),
#'   Response = c("This is sample 1.", "This is sample 2."),
#'   Grade = c(8, 9),
#'   stringsAsFactors = FALSE
#' )
#'
#' samples <- read_samples_df(df, id_col = "StudentID", text_col = "Response")
#' samples
#'
#' # Using the built-in example dataset
#' data("example_writing_samples")
#' samples2 <- read_samples_df(
#'   example_writing_samples[, c("ID", "text")],
#'   id_col   = "ID",
#'   text_col = "text"
#' )
#' head(samples2)
#'
#' @export
read_samples_df <- function(df,
                            id_col = 1,
                            text_col = 2) {
  df <- tibble::as_tibble(df)

  id_col <- tidyselect::vars_pull(names(df), {{ id_col }})
  text_col <- tidyselect::vars_pull(names(df), {{ text_col }})

  out <- df
  out$ID <- as.character(df[[id_col]])
  out$text <- as.character(df[[text_col]])

  # Reorder columns: ID, text, then everything else except original
  # ID/text columns
  keep_cols <- c("ID", "text")
  other_cols <- setdiff(names(out), c(keep_cols, id_col, text_col))
  out <- out[, c(keep_cols, other_cols), drop = FALSE]

  # Check uniqueness of IDs
  dup_ids <- out$ID[duplicated(out$ID)]
  if (length(dup_ids) > 0) {
    stop(
      "Duplicate IDs detected in data: ",
      paste(unique(dup_ids), collapse = ", "),
      call. = FALSE
    )
  }

  out
}

#' Read writing samples from a directory of .txt files
#'
#' This function reads all text files in a directory and uses the
#' filename (without extension) as the sample ID and the file contents
#' as the text.
#'
#' @param path Directory containing .txt files.
#' @param pattern A regular expression used to match file names.
#'   Defaults to \code{"\\\\.txt$"}, meaning all files ending in \code{.txt}.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{ID}: filename without extension
#'     \item \code{text}: file contents as a single character string
#'   }
#'
#' @examples
#' # Create a temporary directory with sample text files
#' samples_dir <- tempfile()
#' dir.create(samples_dir)
#'
#' writeLines("This is sample A.", file.path(samples_dir, "A.txt"))
#' writeLines("This is sample B.", file.path(samples_dir, "B.txt"))
#'
#' # Read samples into a tibble
#' samples <- read_samples_dir(samples_dir)
#'
#' samples
#'
#' @export
read_samples_dir <- function(path = ".",
                             pattern = "\\.txt$") {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No files matching pattern found in '", path, "'.", call. = FALSE)
  }

  ids <- tools::file_path_sans_ext(basename(files))

  # Check uniqueness of IDs
  dup_ids <- ids[duplicated(ids)]
  if (length(dup_ids) > 0) {
    stop(
      "Duplicate IDs detected from filenames: ",
      paste(unique(dup_ids), collapse = ", "),
      call. = FALSE
    )
  }

  texts <- vapply(
    files,
    FUN = function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
    FUN.VALUE = character(1L)
  )

  tibble::tibble(
    ID   = ids,
    text = texts
  )
}
