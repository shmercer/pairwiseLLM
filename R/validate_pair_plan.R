#' Validate a proposed pair plan
#'
#' @description
#' \code{validate_pair_plan()} checks a data frame of \emph{proposed} (unjudged)
#' pairs before sending them to a judge (human or LLM). It is intended to catch
#' common issues early (self-pairs, duplicates, missing IDs, IDs not in a known
#' set) and to provide lightweight diagnostics on coverage and position balance.
#'
#' @details
#' The function can be used in two styles:
#' \itemize{
#'   \item \strong{Strict validation} (\code{strict = TRUE}): any violation errors.
#'   \item \strong{Report-only} (\code{strict = FALSE}): violations trigger warnings
#'     and (optionally) a report can be returned via \code{return_report = TRUE}.
#' }
#'
#' Duplicate checking is performed on \emph{unordered} pairs using a key computed
#' from \code{pmin(ID1, ID2)} and \code{pmax(ID1, ID2)}, so both \code{(A, B)} and
#' \code{(B, A)} count as duplicates unless \code{allow_duplicates = TRUE}.
#'
#' @param pairs A data frame or tibble containing proposed pairs.
#' @param id1_col,id2_col Column names for the first and second item IDs.
#' @param ids Optional character vector of allowed IDs. If provided, all IDs in
#'   \code{pairs} must be contained in \code{ids}.
#' @param allow_self Logical; if \code{FALSE} (default), error/warn when \code{ID1 == ID2}.
#' @param allow_duplicates Logical; if \code{FALSE} (default), error/warn when the same
#'   unordered pair appears more than once.
#' @param strict Logical; if \code{TRUE}, violations throw an error; if \code{FALSE},
#'   violations produce warnings.
#' @param return_report Logical; if \code{TRUE}, return a list with \code{data} and
#'   \code{report}. If \code{FALSE}, return \code{pairs} (invisibly).
#'
#' @return
#' If \code{return_report = FALSE}, returns \code{pairs} (invisibly).
#' If \code{return_report = TRUE}, returns a list with:
#' \describe{
#'   \item{\code{data}}{The input \code{pairs} (unchanged).}
#'   \item{\code{report}}{A list of diagnostics, including counts of violations,
#'   per-ID appearance counts, and simple position-balance summaries.}
#' }
#'
#' @examples
#' pairs <- tibble::tibble(
#'   ID1 = c("A", "A", "B"),
#'   ID2 = c("B", "C", "A")
#' )
#'
#' # Report-only: returns a report and warns if needed
#' out <- validate_pair_plan(pairs, return_report = TRUE)
#' out$report$n_rows
#'
#' # Strict mode: errors on self-pairs / duplicates / invalid IDs
#' pairs2 <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("A", "B"))
#' \dontrun{
#' validate_pair_plan(pairs2, strict = TRUE)
#' }
#'
#' @export
validate_pair_plan <- function(pairs,
                               id1_col = "ID1",
                               id2_col = "ID2",
                               ids = NULL,
                               allow_self = FALSE,
                               allow_duplicates = FALSE,
                               strict = FALSE,
                               return_report = FALSE) {
  cols_needed <- c(id1_col, id2_col)
  missing_cols <- setdiff(cols_needed, names(pairs))
  if (length(missing_cols) > 0L) {
    stop(
      "`pairs` must contain columns: ", paste(cols_needed, collapse = ", "),
      ". Missing: ", paste(missing_cols, collapse = ", "), "."
    )
  }

  id1 <- pairs[[id1_col]]
  id2 <- pairs[[id2_col]]

  if (anyNA(id1) || anyNA(id2)) {
    msg <- "Proposed pairs have missing IDs in ID columns."
    if (isTRUE(strict)) stop(msg) else warning(msg, call. = FALSE)
  }

  id1_chr <- as.character(id1)
  id2_chr <- as.character(id2)

  # IDs membership check (if provided)
  if (!is.null(ids)) {
    ids <- as.character(ids)
    bad1 <- setdiff(unique(id1_chr), ids)
    bad2 <- setdiff(unique(id2_chr), ids)
    bad <- unique(c(bad1, bad2))
    if (length(bad) > 0L) {
      msg <- paste0(
        "Proposed pairs contain IDs not present in `ids`: ",
        paste(utils::head(bad, 10), collapse = ", "),
        if (length(bad) > 10) " ..."
      )
      if (isTRUE(strict)) stop(msg) else warning(msg, call. = FALSE)
    }
  }

  # Self pairs
  is_self <- !is.na(id1_chr) & !is.na(id2_chr) & (id1_chr == id2_chr)
  n_self <- sum(is_self, na.rm = TRUE)
  if (!isTRUE(allow_self) && n_self > 0L) {
    msg <- paste0("Proposed pairs include ", n_self, " self-comparisons (ID1 == ID2).")
    if (isTRUE(strict)) stop(msg) else warning(msg, call. = FALSE)
  }

  # Duplicate unordered pairs
  key <- paste(pmin(id1_chr, id2_chr), pmax(id1_chr, id2_chr), sep = "||")
  dup_idx <- duplicated(key)
  n_dups <- sum(dup_idx, na.rm = TRUE)
  if (!isTRUE(allow_duplicates) && n_dups > 0L) {
    msg <- paste0("Proposed pairs include ", n_dups, " duplicate unordered pairs.")
    if (isTRUE(strict)) stop(msg) else warning(msg, call. = FALSE)
  }

  # Diagnostics / report
  all_ids <- unique(c(id1_chr, id2_chr))
  counts_any <- table(c(id1_chr, id2_chr))

  # Position balance
  n_pos1 <- table(id1_chr)
  n_pos2 <- table(id2_chr)
  ids_union <- sort(unique(c(names(n_pos1), names(n_pos2))))
  n1 <- as.integer(n_pos1[ids_union])
  n1[is.na(n1)] <- 0L
  n2 <- as.integer(n_pos2[ids_union])
  n2[is.na(n2)] <- 0L
  denom <- n1 + n2
  pos1_rate <- ifelse(denom > 0, n1 / denom, NA_real_)

  position_balance <- tibble::tibble(
    ID = ids_union,
    n_pos1 = n1,
    n_pos2 = n2,
    pos1_rate = pos1_rate
  )

  report <- list(
    n_rows = nrow(pairs),
    n_unique_ids = length(all_ids),
    n_unique_unordered_pairs = length(unique(key)),
    # aliases for compatibility / ergonomics
    n_unique_pairs_unordered = length(unique(key)),
    n_self_pairs = n_self,
    n_duplicate_unordered_pairs = n_dups,
    per_id_counts = tibble::tibble(
      id = names(counts_any),
      n_appearances = as.integer(counts_any)
    ),
    position_balance = position_balance,
    per_id_min = if (length(counts_any) > 0L) min(as.integer(counts_any)) else 0L,
    per_id_median = if (length(counts_any) > 0L) stats::median(as.integer(counts_any)) else 0,
    per_id_max = if (length(counts_any) > 0L) max(as.integer(counts_any)) else 0L
  )

  if (isTRUE(return_report)) {
    return(list(data = pairs, report = report))
  }

  invisible(pairs)
}
