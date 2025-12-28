#' Validate and summarize backend-parsed pairwise results
#'
#' @description
#' \code{validate_backend_results()} is a convenience wrapper for validating the
#' output of a judging backend (LLM or human) after parsing, and for producing a
#' compact report about parse/validation health.
#'
#' Unlike \code{\link{validate_pairwise_results}}, this function defaults to a
#' \emph{report-only} mode that does not error on invalid winners. This is useful
#' when you want to measure backend quality (e.g., parse failures) without
#' interrupting a pipeline. Set \code{strict = TRUE} to enforce validity.
#'
#' @details
#' The function checks for required ID and winner columns, counts missing/invalid
#' winners, and (when present) counts backend error indicators such as
#' \code{status_code} and \code{error_message}. It can optionally validate judge
#' columns (e.g., model name) via \code{judge_col}.
#'
#' Winner validation follows the same rule as \code{\link{validate_pairwise_results}}:
#' non-missing winners must exactly match \code{ID1} or \code{ID2}. If
#' \code{normalize_winner = TRUE}, the function also recognizes common winner tokens
#' such as \code{"SAMPLE_1"}, \code{"SAMPLE_2"}, \code{"1"}, and \code{"2"} and maps
#' them to the corresponding IDs.
#'
#' @param results A data frame or tibble of parsed backend outputs.
#' @param backend Optional backend label to include in the report (e.g., \code{"openai_live"}).
#' @param id1_col,id2_col Column names for the first and second item IDs.
#' @param winner_col Column name for the winner ID.
#' @param judge_col Optional column identifying the judge/model/annotator.
#' @param normalize_winner Logical; if \code{TRUE}, normalize common winner tokens.
#' @param strict Logical; if \code{TRUE}, enforce validity and error on violations
#'   (using \code{\link{validate_pairwise_results}}). If \code{FALSE} (default),
#'   produce a report without erroring on invalid winners.
#' @param return_report Logical; if \code{TRUE}, return a list with \code{data} and
#'   \code{report}. If \code{FALSE}, return the (possibly normalized) data.
#'
#' @return
#' If \code{return_report = FALSE}, returns a tibble/data.frame (possibly with
#' normalized winners when \code{normalize_winner = TRUE}).
#'
#' If \code{return_report = TRUE}, returns a list with:
#' \describe{
#'   \item{\code{data}}{The validated/normalized data (same rows as input).}
#'   \item{\code{report}}{A list containing counts and flags: total rows, missing/invalid
#'   winners, missing IDs, and (when present) backend error counts.}
#' }
#'
#' @examples
#' res <- tibble::tibble(
#'   ID1 = c("A", "A", "B"),
#'   ID2 = c("B", "C", "C"),
#'   better_id = c("SAMPLE_1", "2", NA_character_),
#'   status_code = c(200L, 200L, 500L),
#'   error_message = c(NA, NA, "Server error")
#' )
#'
#' # Report-only mode (default): does not error on invalid rows
#' out <- validate_backend_results(
#'   res,
#'   backend = "demo",
#'   normalize_winner = TRUE,
#'   return_report = TRUE
#' )
#' out$report$n_invalid_winner
#'
#' # Strict mode: enforce validity (errors on invalid winners)
#' \dontrun{
#' validate_backend_results(res, strict = TRUE)
#' }
#'
#' @seealso \code{\link{validate_pairwise_results}}, \code{\link{results_summary}}
#' @export
validate_backend_results <- function(results,
                                     backend = NULL,
                                     id1_col = "ID1",
                                     id2_col = "ID2",
                                     winner_col = "better_id",
                                     judge_col = NULL,
                                     normalize_winner = FALSE,
                                     strict = FALSE,
                                     return_report = FALSE) {
  # Basic column checks (also works when `results` is not a data.frame)
  need <- c(id1_col, id2_col, winner_col)
  miss <- setdiff(need, names(results))
  if (length(miss) > 0L) {
    stop(
      "`results` must contain columns: ", paste(need, collapse = ", "),
      ". Missing: ", paste(miss, collapse = ", "), "."
    )
  }

  res_use <- tibble::as_tibble(results)

  # Strict mode uses validate_pairwise_results (still allowing missing winners)
  if (isTRUE(strict)) {
    res_use <- validate_pairwise_results(
      res_use,
      id1_col = id1_col,
      id2_col = id2_col,
      winner_col = winner_col,
      judge_col = judge_col,
      normalize_winner = normalize_winner,
      allow_missing_winner = TRUE,
      return_report = FALSE
    )
    report <- list(
      backend = backend,
      n_rows = nrow(res_use),
      n_missing_id = sum(is.na(res_use[[id1_col]]) | is.na(res_use[[id2_col]])),
      n_missing_winner = sum(is.na(res_use[[winner_col]]) | res_use[[winner_col]] == ""),
      n_invalid_winner = 0L
    )
  } else {
    # Non-strict: compute invalid/missing counts without erroring.
    id1 <- as.character(res_use[[id1_col]])
    id2 <- as.character(res_use[[id2_col]])
    win <- as.character(res_use[[winner_col]])

    if (isTRUE(normalize_winner)) {
      wlow <- tolower(win)
      is_s1 <- !is.na(wlow) & wlow %in% c("sample_1", "s1", "1")
      is_s2 <- !is.na(wlow) & wlow %in% c("sample_2", "s2", "2")
      win[is_s1] <- id1[is_s1]
      win[is_s2] <- id2[is_s2]
      res_use[[winner_col]] <- win
    }

    missing_winner <- is.na(win) | win == ""
    valid_winner <- !missing_winner & (win == id1 | win == id2)
    invalid_winner <- !missing_winner & !valid_winner

    report <- list(
      backend = backend,
      n_rows = nrow(res_use),
      n_missing_id = sum(is.na(id1) | is.na(id2)),
      n_missing_winner = sum(missing_winner, na.rm = TRUE),
      n_invalid_winner = sum(invalid_winner, na.rm = TRUE)
    )

    # Judge missingness flag (if requested and present)
    if (!is.null(judge_col) && judge_col %in% names(res_use)) {
      report$has_missing_judge <- anyNA(res_use[[judge_col]])
    }
  }

  # Backend error indicators (optional)
  if ("status_code" %in% names(res_use)) {
    sc <- suppressWarnings(as.integer(res_use$status_code))
    report$n_status_ge_400 <- sum(!is.na(sc) & sc >= 400L)
  }
  if ("error_message" %in% names(res_use)) {
    em <- res_use$error_message
    report$n_error_message <- sum(!is.na(em) & as.character(em) != "")
  }

  if (isTRUE(return_report)) {
    return(list(data = res_use, report = report))
  }
  res_use
}

#' @keywords internal
.apply_backend_validation_to_submit_output <- function(output,
                                                       backend,
                                                       validate = FALSE,
                                                       validate_strict = FALSE,
                                                       normalize_winner = FALSE,
                                                       id1_col = "ID1",
                                                       id2_col = "ID2",
                                                       winner_col = "better_id",
                                                       judge_col = NULL) {
  if (!isTRUE(validate)) {
    return(output)
  }

  if (!is.list(output) || is.null(output$results)) {
    stop("`output` must be a list with a `results` element.", call. = FALSE)
  }

  vr <- validate_backend_results(
    output$results,
    backend = backend,
    id1_col = id1_col,
    id2_col = id2_col,
    winner_col = winner_col,
    judge_col = judge_col,
    normalize_winner = normalize_winner,
    strict = isTRUE(validate_strict),
    return_report = TRUE
  )

  output$results <- vr$data
  output$validation_report <- vr$report
  output
}
