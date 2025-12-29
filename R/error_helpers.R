# ---- error helpers (internal) ----

#' Internal: abort with an actionable resume/checkpoint mismatch error
#'
#' @keywords internal
.abort_checkpoint_mismatch <- function(field, expected, actual, hint = NULL) {
  expected_str <- paste(utils::capture.output(utils::str(expected, max.level = 2)), collapse = "\n")
  actual_str <- paste(utils::capture.output(utils::str(actual, max.level = 2)), collapse = "\n")

  msg <- paste0(
    "Resume checkpoint does not match `", field, "`.\n",
    "Requested:\n", expected_str, "\n",
    "Checkpoint:\n", actual_str
  )
  if (!is.null(hint) && nzchar(as.character(hint))) {
    msg <- paste0(msg, "\nHint: ", as.character(hint))
  }
  stop(msg, call. = FALSE)
}

#' Internal: abort with diagnostics when a BT fit cannot be computed
#'
#' @keywords internal
.abort_no_fit <- function(stage, results, ids = NULL, judge_col = NULL, hint = NULL) {
  results <- tibble::as_tibble(results)

  n_results <- nrow(results)
  n_non_missing_better_id <- if ("better_id" %in% names(results)) sum(!is.na(results$better_id)) else NA_integer_

  ids_in_results <- character(0)
  if (all(c("ID1", "ID2") %in% names(results))) {
    ids_in_results <- unique(c(as.character(results$ID1), as.character(results$ID2)))
    ids_in_results <- ids_in_results[!is.na(ids_in_results)]
  }
  n_unique_ids_in_results <- length(ids_in_results)

  n_ids_expected <- if (!is.null(ids)) length(unique(as.character(ids))) else NA_integer_

  judge_info <- ""
  if (!is.null(judge_col)) {
    if (judge_col %in% names(results)) {
      j <- results[[judge_col]]
      judge_info <- paste0(
        "\n- judge_col=", judge_col,
        ": n_unique=", length(unique(j)),
        ", n_na=", sum(is.na(j))
      )
    } else {
      judge_info <- paste0("\n- judge_col=", judge_col, ": <not present>")
    }
  }

  msg <- paste0(
    "No BT fit could be computed at stage `", as.character(stage), "`.\n",
    "Diagnostics:\n",
    "- n_results=", n_results, "\n",
    "- n_non_missing_better_id=", n_non_missing_better_id, "\n",
    "- n_unique_ids_in_results=", n_unique_ids_in_results,
    if (!is.na(n_ids_expected)) paste0(" (expected ", n_ids_expected, ")") else "",
    judge_info,
    "\n\n",
    "Debugging tips:\n",
    "- Check missing winners: table(is.na(results$better_id))\n",
    "- Validate IDs: setdiff(unique(c(results$ID1, results$ID2)), ids)\n",
    "- If many invalid/duplicate results, inspect raw judge outputs.\n"
  )

  if (!is.null(hint) && nzchar(as.character(hint))) {
    msg <- paste0(msg, "\nHint: ", as.character(hint))
  }

  stop(msg, call. = FALSE)
}
