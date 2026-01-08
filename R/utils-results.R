# Internal helpers for validating judge results and normalizing winners

#' Escape regex metacharacters in a literal string
#'
#' @keywords internal
.escape_regex <- function(x) {
  out <- as.character(x)

  # Escape backslash first
  out <- gsub("\\\\", "\\\\\\\\", out)

  # Escape other regex metacharacters
  specials <- c(".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?", "-")
  for (ch in specials) {
    out <- gsub(ch, paste0("\\\\", ch), out, fixed = TRUE)
  }

  out
}

#' Normalize common better_id/winner encodings to literal IDs
#'
#' @keywords internal
.normalize_better_id <- function(better_id, ID1, ID2) {
  n <- length(better_id)

  # Make sure we have character and trim
  b <- trimws(as.character(better_id))
  # Strip surrounding quotes
  b <- gsub("^[\"']+|[\"']+$", "", b)

  # Treat NA-like tokens as NA
  tok_upper <- toupper(b)
  tok_compact <- gsub("[^A-Z0-9]+", "", tok_upper)
  missing_tokens <- c("", "NA", "NAN", "NULL", "NONE", "N/A", "<NA>", "TIE", "EQUAL", "MISSING")
  miss_compact <- gsub("[^A-Z0-9]+", "", missing_tokens)
  is_missing <- is.na(b) | tok_compact %in% miss_compact

  out <- rep(NA_character_, n)
  out[!is_missing] <- b[!is_missing]

  # Exact match to ID1/ID2
  exact1 <- !is_missing & (out == ID1)
  exact2 <- !is_missing & (out == ID2)
  out[exact1] <- ID1[exact1]
  out[exact2] <- ID2[exact2]

  # Remaining need mapping
  remaining <- !is_missing & !(out == ID1 | out == ID2)
  if (!any(remaining)) {
    return(out)
  }

  idx <- which(remaining)
  tok_full <- toupper(trimws(out[idx]))
  tok_full2 <- gsub("[^A-Z0-9]+", "", tok_full)

  # Positional label detection (exact or substring)
  pos1 <- tok_full2 %in% c("SAMPLE1", "ID1", "POS1", "LEFT", "L", "1", "A", "FIRST", "TRUE") |
    grepl("SAMPLE1", tok_full2, fixed = TRUE) |
    grepl("ID1", tok_full2, fixed = TRUE) |
    grepl("POS1", tok_full2, fixed = TRUE)

  pos2 <- tok_full2 %in% c("SAMPLE2", "ID2", "POS2", "RIGHT", "R", "2", "0", "B", "SECOND", "FALSE") |
    grepl("SAMPLE2", tok_full2, fixed = TRUE) |
    grepl("ID2", tok_full2, fixed = TRUE) |
    grepl("POS2", tok_full2, fixed = TRUE)

  only1 <- pos1 & !pos2
  only2 <- pos2 & !pos1

  out[idx[only1]] <- ID1[idx[only1]]
  out[idx[only2]] <- ID2[idx[only2]]

  # If still unresolved, try detecting mention of exactly one ID in free text ("Winner: A")
  unresolved <- idx[!(only1 | only2)]
  if (length(unresolved) > 0L) {
    for (i in unresolved) {
      s <- as.character(out[i])
      id1 <- ID1[i]
      id2 <- ID2[i]

      m1 <- grepl(paste0("\\b", .escape_regex(id1), "\\b"), s, ignore.case = TRUE)
      m2 <- grepl(paste0("\\b", .escape_regex(id2), "\\b"), s, ignore.case = TRUE)

      if (m1 && !m2) out[i] <- id1
      if (m2 && !m1) out[i] <- id2
    }
  }

  out
}

#' Validate judge output tibble
#'
#' @keywords internal

# Drop any historical rows a judge function may have echoed back.
# This is a defensive safety-net for adaptive loops: the judge should return only
# results for the current request, but older runners / user-provided judges may
# accidentally include cumulative history.
#
# @keywords internal
.filter_judge_results_to_request <- function(res, requested_pairs) {
  res <- tibble::as_tibble(res)
  requested_pairs <- tibble::as_tibble(requested_pairs)

  use_custom_id <- "custom_id" %in% names(requested_pairs) && "custom_id" %in% names(res)
  if (use_custom_id) {
    req_key <- requested_pairs$custom_id
    got_key <- res$custom_id
    key_name <- "custom_id"
  } else if (all(c("ID1", "ID2") %in% names(requested_pairs)) && all(c("ID1", "ID2") %in% names(res))) {
    req_key <- paste(requested_pairs$ID1, requested_pairs$ID2, sep = "\r")
    got_key <- paste(res$ID1, res$ID2, sep = "\r")
    key_name <- "ID1,ID2"
  } else {
    return(res)
  }

  req_key <- unique(stats::na.omit(req_key))
  got_key <- unique(stats::na.omit(got_key))

  extra <- setdiff(got_key, req_key)
  if (length(extra) > 0L) {
    warning(
      "judge_fun returned rows not in current request; dropping extras. ",
      "This can happen if a runner returns cumulative results. (key: ",
      key_name,
      ")",
      call. = FALSE
    )
    if (use_custom_id) {
      res <- dplyr::filter(res, .data$custom_id %in% req_key)
    } else {
      res <- dplyr::filter(res, paste(.data$ID1, .data$ID2, sep = "\r") %in% req_key)
    }
  }

  if (use_custom_id) {
    if (anyDuplicated(res$custom_id)) {
      warning(
        "judge_fun returned duplicate keys within this round; keeping first occurrence. (key: custom_id)",
        call. = FALSE
      )
      res <- dplyr::distinct(res, .data$custom_id, .keep_all = TRUE)
    }
  } else {
    key_vec <- paste(res$ID1, res$ID2, sep = "\r")
    if (anyDuplicated(key_vec)) {
      warning(
        "judge_fun returned duplicate keys within this round; keeping first occurrence. (key: ID1,ID2)",
        call. = FALSE
      )
      res <- dplyr::distinct(res, .data$ID1, .data$ID2, .keep_all = TRUE)
    }
  }

  res
}

.validate_judge_results <- function(res, ids, judge_col = NULL) {
  res <- tibble::as_tibble(res)

  required <- c("ID1", "ID2", "better_id")
  missing <- setdiff(required, names(res))
  if (length(missing) > 0L) {
    stop("`judge_fun` must return columns: ", paste(required, collapse = ", "), call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L || !nzchar(judge_col)) {
      stop("`judge` must be a non-empty character scalar.", call. = FALSE)
    }
    if (!judge_col %in% names(res)) {
      stop("`judge_fun` must include a `", judge_col, "` column when `judge` is provided.", call. = FALSE)
    }
  }

  # Coerce to character + trim IDs
  res$ID1 <- trimws(as.character(res$ID1))
  res$ID2 <- trimws(as.character(res$ID2))
  res$better_id <- as.character(res$better_id)

  if (any(is.na(res$ID1)) || any(is.na(res$ID2)) || any(res$ID1 == "") || any(res$ID2 == "")) {
    stop("`ID1` and `ID2` must be non-missing and non-empty in results.", call. = FALSE)
  }
  if (any(res$ID1 == res$ID2)) {
    stop("`judge_fun` returned pairs with ID1 == ID2.", call. = FALSE)
  }
  if (any(!(res$ID1 %in% ids)) || any(!(res$ID2 %in% ids))) {
    stop("`judge_fun` returned IDs not present in `samples$ID`.", call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (any(is.na(res[[judge_col]]))) {
      stop("`judge_fun` output has missing values in judge column: ", judge_col, call. = FALSE)
    }
    res[[judge_col]] <- as.character(res[[judge_col]])
  }

  res$better_id <- .normalize_better_id(res$better_id, res$ID1, res$ID2)

  bad <- !is.na(res$better_id) & !(res$better_id == res$ID1 | res$better_id == res$ID2)
  if (any(bad)) {
    bad_idx <- which(bad)
    examples <- unique(as.character(res$better_id[bad_idx]))
    examples <- examples[!is.na(examples)]
    examples <- utils::head(examples, 3)

    stop(
      "`judge_fun` returned `better_id` values that are not equal to ID1 or ID2.",
      if (length(examples) > 0L) paste0(" Examples: ", paste(examples, collapse = ", ")) else "",
      call. = FALSE
    )
  }

  res
}
