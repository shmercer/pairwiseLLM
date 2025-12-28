#' Summarize a pairwise results table
#'
#' @description
#' \code{results_summary()} computes quick, human-friendly diagnostics for a
#' pairwise results table (typically the output of a judging backend or a runner).
#'
#' It is designed to be \emph{non-blocking} by default: in non-strict mode it
#' reports counts of missing or invalid winners instead of erroring. When
#' \code{strict = TRUE}, the function calls \code{\link{validate_pairwise_results}}
#' to enforce validity (while still allowing rows with missing winners).
#'
#' @details
#' The summary is returned as a list with up to three components:
#' \describe{
#'   \item{\code{overall}}{One-row data frame with coverage and basic validity counts
#'   (rows, unique IDs, unique unordered pairs, missing winners, invalid winners).}
#'   \item{\code{reverse}}{If \code{compute_reverse = TRUE} and both pair orderings are
#'   present, a list returned by \code{\link{compute_reverse_consistency}}. Note that
#'   reverse-consistency is computed using only rows with a valid winner. Pairs where
#'   the per-direction majority vote is tied are excluded by
#'   \code{\link{compute_reverse_consistency}}.}
#'   \item{\code{judge}}{If \code{judge_col} is provided and present, a list containing
#'   per-judge summaries from \code{\link{judge_summary}} and flags for missing judge
#'   values.}
#' }
#'
#' @param results A data frame or tibble of pairwise results.
#' @param id1_col,id2_col Column names giving the first and second item IDs.
#' @param winner_col Column name giving the winner ID (i.e., the better item).
#' @param judge_col Optional column name identifying the judge/model/annotator.
#' @param compute_reverse Logical; if \code{TRUE}, attempt to compute forward-vs-reverse
#'   consistency when both orderings exist.
#' @param normalize_winner Logical; if \code{TRUE}, attempt to normalize common winner
#'   tokens (e.g., \code{"SAMPLE_1"}, \code{"SAMPLE_2"}, \code{"1"}, \code{"2"}) into
#'   the corresponding IDs. Unsupported tokens remain invalid.
#' @param strict Logical; if \code{TRUE}, validate inputs using
#'   \code{\link{validate_pairwise_results}} (errors on invalid winners and other
#'   violations). If \code{FALSE} (default), the function is report-only.
#'
#' @return
#' A list with components:
#' \describe{
#'   \item{\code{overall}}{A one-row data frame of overall counts.}
#'   \item{\code{reverse}}{A list (or \code{NULL}) with a \code{summary} data frame and
#'   \code{details}, as produced by \code{\link{compute_reverse_consistency}}.}
#'   \item{\code{judge}}{A list (or \code{NULL}) containing per-judge summaries.}
#' }
#'
#' @examples
#' res <- tibble::tibble(
#'   ID1 = c("A", "B", "A", "B"),
#'   ID2 = c("B", "A", "C", "C"),
#'   better_id = c("A", "A", "C", NA_character_)
#' )
#'
#' # Non-strict: report missing/invalid winners without erroring
#' s <- results_summary(res, compute_reverse = TRUE, strict = FALSE)
#' s$overall
#'
#' # Strict: enforce validity (still allows missing winners)
#' s2 <- results_summary(res, compute_reverse = TRUE, strict = TRUE)
#'
#' @seealso
#' \code{\link{validate_pairwise_results}}, \code{\link{judge_summary}},
#' \code{\link{compute_reverse_consistency}}
#'
#' @export
results_summary <- function(results,
                            id1_col = "ID1",
                            id2_col = "ID2",
                            winner_col = "better_id",
                            judge_col = NULL,
                            compute_reverse = TRUE,
                            normalize_winner = FALSE,
                            strict = FALSE) {
  # Basic column checks
  need <- c(id1_col, id2_col, winner_col)
  miss <- setdiff(need, names(results))
  if (length(miss) > 0L) {
    stop(
      "`results` must contain columns: ", paste(need, collapse = ", "),
      ". Missing: ", paste(miss, collapse = ", "), "."
    )
  }

  # Strict path: normalize + validate winners; allow missing winner rows.
  res_use <- results
  if (isTRUE(strict)) {
    res_use <- validate_pairwise_results(
      results,
      id1_col = id1_col,
      id2_col = id2_col,
      winner_col = winner_col,
      judge_col = judge_col,
      normalize_winner = normalize_winner,
      allow_missing_winner = TRUE,
      return_report = FALSE
    )
  }

  id1 <- as.character(res_use[[id1_col]])
  id2 <- as.character(res_use[[id2_col]])

  win <- res_use[[winner_col]]
  win <- as.character(win)

  # Non-strict normalization (best-effort). Strict mode already normalized.
  if (!isTRUE(strict) && isTRUE(normalize_winner)) {
    # Only normalize the most common tokens.
    wlow <- tolower(win)
    is_s1 <- !is.na(wlow) & wlow %in% c("sample_1", "s1", "1")
    is_s2 <- !is.na(wlow) & wlow %in% c("sample_2", "s2", "2")
    win[is_s1] <- id1[is_s1]
    win[is_s2] <- id2[is_s2]
  }

  # Missing and invalid winners (non-strict reports these)
  missing_winner <- is.na(win) | win == ""
  valid_winner <- !missing_winner & (win == id1 | win == id2)
  invalid_winner <- !missing_winner & !valid_winner

  # Coverage stats
  all_ids <- unique(c(id1, id2))
  key <- paste(pmin(id1, id2), pmax(id1, id2), sep = "||")
  out <- list()

  out$overall <- tibble::tibble(
    n_rows = nrow(res_use),
    n_unique_ids = length(all_ids),
    n_unique_unordered_pairs = length(unique(key)),
    n_missing_winner = sum(missing_winner, na.rm = TRUE),
    n_invalid_winner = sum(invalid_winner, na.rm = TRUE)
  )

  # Reverse consistency (only for rows with valid winners)
  if (isTRUE(compute_reverse)) {
    ok <- valid_winner
    if (any(ok)) {
      dfv <- tibble::as_tibble(res_use[ok, , drop = FALSE])
      # Ensure columns are named as expected by compute_reverse_consistency()
      if (!(id1_col == "ID1" && id2_col == "ID2" && winner_col == "better_id")) {
        dfv <- dplyr::rename(
          dfv,
          ID1 = !!rlang::sym(id1_col),
          ID2 = !!rlang::sym(id2_col),
          better_id = !!rlang::sym(winner_col)
        )
      }
      # Split by a deterministic rule to produce one "main" and one "reverse" table
      is_main <- dfv$ID1 <= dfv$ID2
      main_df <- dfv[is_main, , drop = FALSE]
      rev_df <- dfv[!is_main, , drop = FALSE]
      if (nrow(main_df) > 0L && nrow(rev_df) > 0L) {
        out$reverse <- compute_reverse_consistency(main_df, rev_df)
      } else {
        out$reverse <- list(
          summary = tibble::tibble(n_pairs = 0L, prop_consistent = NA_real_),
          details = tibble::tibble(),
          mean_signed = NA_real_
        )
      }
    } else {
      out$reverse <- list(
        summary = tibble::tibble(n_pairs = 0L, prop_consistent = NA_real_),
        details = tibble::tibble(),
        mean_signed = NA_real_
      )
    }
  }

  # Per-judge summary (only if judge_col present)
  if (!is.null(judge_col) && judge_col %in% names(res_use)) {
    judges <- res_use[[judge_col]]
    has_missing_judge <- anyNA(judges)

    per_judge <- NULL
    if (!has_missing_judge) {
      tmp <- tibble::as_tibble(res_use)
      tmp <- dplyr::rename(tmp, judge = !!rlang::sym(judge_col))
      per_judge <- judge_summary(tmp, judge_col = "judge", compute_reverse = compute_reverse)
    }

    out$judge <- list(
      judge_col = judge_col,
      has_missing_judge = has_missing_judge,
      per_judge = per_judge
    )
  }

  out
}
