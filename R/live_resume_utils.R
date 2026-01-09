# Internal utilities for live submit_* functions (resume + type stability)
# These are intentionally not exported.

.coerce_live_submit_types <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }

  # ID-like columns can be mis-guessed as numeric when read from CSV.
  id_cols <- c(
    "custom_id", "ID1", "ID2", "better_id", "better_sample",
    "model", "object_type"
  )

  txt_cols <- c("error_message", "thoughts", "content")
  num_cols <- c("prompt_tokens", "completion_tokens", "total_tokens")

  for (nm in intersect(id_cols, names(df))) df[[nm]] <- as.character(df[[nm]])
  for (nm in intersect(txt_cols, names(df))) df[[nm]] <- as.character(df[[nm]])

  if ("status_code" %in% names(df)) {
    df$status_code <- suppressWarnings(as.integer(df$status_code))
  }
  for (nm in intersect(num_cols, names(df))) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))

  df
}

.read_existing_live_results <- function(save_path, verbose = TRUE) {
  if (!.require_pkg("readr")) {
    stop("The 'readr' package is required for incremental saving. Please install it.", call. = FALSE)
  }
  # `show_col_types` is not available in older readr versions; keep compatibility.
  if ("show_col_types" %in% names(formals(readr::read_csv))) {
    out <- readr::read_csv(save_path, show_col_types = FALSE)
  } else {
    out <- readr::read_csv(save_path)
  }
  .coerce_live_submit_types(out)
}

.require_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
