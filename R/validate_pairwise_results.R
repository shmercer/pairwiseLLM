#' Validate and normalize pairwise comparison results
#'
#' This helper checks that a results table contains the expected columns for
#' pairwise comparisons (two IDs plus a winner/better ID). Optionally, it
#' validates that IDs come from a known set, enforces no self-comparisons,
#' validates a judge column, and normalizes common winner tokens (e.g., "SAMPLE_1")
#' into explicit IDs.
#'
#' @param results A data frame containing pairwise comparison results.
#' @param id1_col,id2_col Column names for the two item IDs. Defaults are \code{"ID1"} and \code{"ID2"}.
#' @param winner_col Column name for the winner/better ID. Default \code{"better_id"}.
#' @param judge_col Optional column name for judge/annotator identifier. If provided, it must exist
#'   in \code{results} and must not contain missing values.
#' @param ids Optional character vector of valid IDs. If provided, all non-missing values in
#'   \code{id1_col} and \code{id2_col} must be present in \code{ids}.
#' @param allow_missing_winner Logical; if \code{TRUE}, missing \code{winner_col} values are allowed.
#' @param allow_self Logical; if \code{FALSE} (default), rows with \code{id1_col == id2_col} error.
#' @param normalize_winner Logical; if \code{TRUE} (default), normalize common winner tokens into explicit IDs.
#' @param return_report Logical; if \code{TRUE}, return a list with \code{$data} and \code{$report}.
#'
#' @return By default, a tibble with the same columns as \code{results} (with normalized/coerced types).
#'   If \code{return_report=TRUE}, a list with elements:
#'   \itemize{
#'     \item \code{data}: the normalized results tibble
#'     \item \code{report}: a list of summary counts (missing winners, invalid winners, self-comparisons, etc.)
#'   }
#'
#' @examples
#' res <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "C"), better_id = c("A", "SAMPLE_2"))
#' validate_pairwise_results(res)
#'
#' @export
validate_pairwise_results <- function(results,
                                      id1_col = "ID1",
                                      id2_col = "ID2",
                                      winner_col = "better_id",
                                      judge_col = NULL,
                                      ids = NULL,
                                      allow_missing_winner = TRUE,
                                      allow_self = FALSE,
                                      normalize_winner = TRUE,
                                      return_report = FALSE) {
  results <- tibble::as_tibble(results)

  if (!is.character(id1_col) || length(id1_col) != 1L || is.na(id1_col) || !nzchar(id1_col)) {
    stop("`id1_col` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(id2_col) || length(id2_col) != 1L || is.na(id2_col) || !nzchar(id2_col)) {
    stop("`id2_col` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(winner_col) || length(winner_col) != 1L || is.na(winner_col) || !nzchar(winner_col)) {
    stop("`winner_col` must be a non-empty character scalar.", call. = FALSE)
  }

  required <- c(id1_col, id2_col, winner_col)
  missing <- setdiff(required, names(results))
  if (length(missing) > 0L) {
    stop("`results` must contain columns: ", paste(required, collapse = ", "), call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L || is.na(judge_col) || !nzchar(judge_col)) {
      stop("`judge_col` must be a non-empty character scalar when provided.", call. = FALSE)
    }
    if (!judge_col %in% names(results)) {
      stop("`results` must include a `", judge_col, "` column when `judge_col` is provided.", call. = FALSE)
    }
  }

  id1 <- results[[id1_col]]
  id2 <- results[[id2_col]]
  winner <- results[[winner_col]]

  id1 <- if (is.factor(id1)) as.character(id1) else as.character(id1)
  id2 <- if (is.factor(id2)) as.character(id2) else as.character(id2)

  if (!is.null(ids)) {
    ids <- as.character(ids)
    if (length(ids) < 1L || anyNA(ids) || any(ids == "")) {
      stop("`ids` must be a character vector of non-missing, non-empty IDs.", call. = FALSE)
    }
    bad1 <- !is.na(id1) & !(id1 %in% ids)
    bad2 <- !is.na(id2) & !(id2 %in% ids)
    if (any(bad1 | bad2)) {
      stop("`results` contains IDs not present in `ids`.", call. = FALSE)
    }
  }

  # Self-comparisons
  self <- !is.na(id1) & !is.na(id2) & (id1 == id2)
  if (any(self) && !isTRUE(allow_self)) {
    stop("`results` contains self-comparisons where `", id1_col, " == ", id2_col, "`.", call. = FALSE)
  }

  # Judge column validation
  if (!is.null(judge_col)) {
    if (any(is.na(results[[judge_col]]))) {
      stop("`results` has missing values in judge column: ", judge_col, call. = FALSE)
    }
    results[[judge_col]] <- as.character(results[[judge_col]])
  }

  # Winner normalization + validation
  if (!isTRUE(allow_missing_winner) && any(is.na(winner))) {
    stop("`results` has missing values in winner column: ", winner_col, call. = FALSE)
  }

  winner <- if (is.factor(winner)) as.character(winner) else as.character(winner)

  if (isTRUE(normalize_winner)) {
    # Internal helper normalizes tokens like SAMPLE_1/SAMPLE_2 (and variants) into explicit IDs
    winner <- .normalize_better_id(winner, id1, id2)
  }

  invalid <- !is.na(winner) & !(winner == id1 | winner == id2)
  if (any(invalid)) {
    bad_idx <- which(invalid)
    examples <- unique(as.character(winner[bad_idx]))
    examples <- examples[!is.na(examples)]
    examples <- utils::head(examples, 5)
    stop(
      "`results` winner column must match `", id1_col, "` or `", id2_col,
      "` (or be missing). Examples of invalid winners: ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }

  results[[id1_col]] <- id1
  results[[id2_col]] <- id2
  results[[winner_col]] <- winner

  report <- list(
    n_rows = nrow(results),
    n_missing_winner = sum(is.na(winner)),
    n_invalid_winner = sum(invalid),
    n_self_comparisons = sum(self),
    n_missing_id1 = sum(is.na(id1)),
    n_missing_id2 = sum(is.na(id2))
  )

  # Unique unordered pair count (ignoring judge)
  ok_pair <- !is.na(id1) & !is.na(id2)
  if (any(ok_pair)) {
    a <- pmin(id1[ok_pair], id2[ok_pair])
    b <- pmax(id1[ok_pair], id2[ok_pair])
    report$n_unique_unordered_pairs <- length(unique(paste0(a, "\u001f", b)))
  } else {
    report$n_unique_unordered_pairs <- 0L
  }

  if (isTRUE(return_report)) {
    return(list(data = results, report = report))
  }

  results
}
