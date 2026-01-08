# Internal helpers for pair handling

#' @keywords internal
#
# NOTE: The separator is part of the public-facing schema in several
# runners/tests (e.g., diagnostics tables). Use a simple, stable delimiter.
.unordered_pair_key <- function(id1, id2, sep = "-") {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  lo <- pmin(id1, id2)
  hi <- pmax(id1, id2)
  paste0(lo, sep, hi)
}

#' Deterministic direction label relative to the unordered key
#'
#' Returns "forward" when (id1,id2) are in the same order as
#' \code{.unordered_pair_key(id1,id2)} (i.e., id1 <= id2 lexicographically),
#' otherwise "reverse".
#'
#' @keywords internal
.pair_direction <- function(id1, id2) {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  dplyr::if_else(id1 <= id2, "forward", "reverse")
}

#' Normalize a prior-pairs data frame to ID1/ID2 schema
#'
#' @keywords internal
.normalize_existing_pairs <- function(existing_pairs, err_arg = "existing_pairs") {
  if (is.null(existing_pairs)) {
    return(tibble::tibble(ID1 = character(0), ID2 = character(0)))
  }

  x <- tibble::as_tibble(existing_pairs)

  if (all(c("ID1", "ID2") %in% names(x))) {
    return(tibble::tibble(ID1 = as.character(x$ID1), ID2 = as.character(x$ID2)))
  }

  if (all(c("object1", "object2") %in% names(x))) {
    return(tibble::tibble(ID1 = as.character(x$object1), ID2 = as.character(x$object2)))
  }

  stop("`", err_arg, "` must have columns ID1/ID2 or object1/object2.", call. = FALSE)
}

#' Keep first occurrence of each unordered pair
#'
#' @keywords internal
.distinct_unordered_pairs <- function(pairs, id1_col = "ID1", id2_col = "ID2", sep = "-") {
  pairs <- tibble::as_tibble(pairs)
  if (!all(c(id1_col, id2_col) %in% names(pairs))) {
    stop("`pairs` must contain columns: `", id1_col, "` and `", id2_col, "`.", call. = FALSE)
  }
  key <- .unordered_pair_key(pairs[[id1_col]], pairs[[id2_col]], sep = sep)
  pairs[!duplicated(key), , drop = FALSE]
}

#' Add text1/text2 columns for pairs
#'
#' Internal alias for the exported \code{add_pair_texts()}.
#'
#' @keywords internal
.add_pair_texts <- function(pairs, samples, id1_col = "ID1", id2_col = "ID2") {
  add_pair_texts(pairs = pairs, samples = samples, id1_col = id1_col, id2_col = id2_col)
}


#' Ensure `custom_id` exists and is unique
#'
#' If `pairs$custom_id` is missing or empty, it is synthesized as
#' `<prefix>_<ID1>_vs_<ID2>`. If user-supplied `custom_id` values are present,
#' they are preserved.
#'
#' @keywords internal
.ensure_custom_id <- function(pairs, prefix = "LIVE") {
  pairs <- tibble::as_tibble(pairs)
  if (!all(c("ID1", "ID2") %in% names(pairs))) {
    stop("`pairs` must contain columns: ID1, ID2.", call. = FALSE)
  }

  if (!"custom_id" %in% names(pairs)) {
    pairs$custom_id <- NA_character_
  }

  missing <- is.na(pairs$custom_id) | !nzchar(pairs$custom_id)
  if (any(missing)) {
    pairs$custom_id[missing] <- sprintf(
      "%s_%s_vs_%s",
      prefix,
      as.character(pairs$ID1[missing]),
      as.character(pairs$ID2[missing])
    )
  }

  if (anyDuplicated(pairs$custom_id)) {
    stop("pairs$custom_id must be unique per request.", call. = FALSE)
  }

  pairs
}
