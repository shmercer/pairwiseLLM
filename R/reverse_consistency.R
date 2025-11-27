#' Compute consistency between forward and reverse pair comparisons
#'
#' Given two data frames of pairwise comparison results (one for
#' the "forward" ordering of pairs, one for the "reverse" ordering),
#' this function identifies pairs that were evaluated in both orders
#' and computes the proportion of consistent judgments.
#'
#' Consistency is defined at the level of IDs: a pair is consistent
#' if the same ID is selected as better in both data frames. This
#' assumes that each result data frame contains at least the columns
#' \code{ID1}, \code{ID2}, and \code{better_id}, where
#' \code{better_id} is the ID of the better sample (not
#' "SAMPLE_1"/"SAMPLE_2").
#'
#' @param main_results A data frame or tibble containing pairwise
#'   comparison results for the "forward" ordering of pairs, with
#'   columns \code{ID1}, \code{ID2}, and \code{better_id}.
#' @param reverse_results A data frame or tibble containing results
#'   for the corresponding "reverse" ordering, with the same column
#'   requirements.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{summary}: a tibble with one row and columns
#'       \code{n_pairs}, \code{n_consistent}, and \code{prop_consistent}.
#'     \item \code{details}: a tibble with one row per overlapping pair,
#'       including columns \code{key}, \code{ID1_main}, \code{ID2_main},
#'       \code{ID1_rev}, \code{ID2_rev}, \code{better_id_main},
#'       \code{better_id_rev}, and \code{is_consistent}.
#'   }
#'   Pairs for which \code{better_id} is \code{NA} in either data
#'   frame are excluded from the consistency calculation.
#'
#' @examples
#' # Simple synthetic example
#' main <- tibble::tibble(
#'   ID1       = c("S1", "S1", "S2"),
#'   ID2       = c("S2", "S3", "S3"),
#'   better_id = c("S1", "S3", "S2")
#' )
#'
#' rev <- tibble::tibble(
#'   ID1       = c("S2", "S3", "S3"),
#'   ID2       = c("S1", "S1", "S2"),
#'   better_id = c("S1", "S3", "S2")
#' )
#'
#' rc <- compute_reverse_consistency(main, rev)
#' rc$summary
#'
#' # Using the example writing pairs: reverse the first 10 pairs
#' data("example_writing_pairs")
#' main2 <- example_writing_pairs[1:10, ]
#' rev2  <- main2
#' rev2$ID1 <- main2$ID2
#' rev2$ID2 <- main2$ID1
#' rc2 <- compute_reverse_consistency(main2, rev2)
#' rc2$summary
#'
#' @export
compute_reverse_consistency <- function(main_results,
                                        reverse_results) {
  main_results    <- tibble::as_tibble(main_results)
  reverse_results <- tibble::as_tibble(reverse_results)

  required_cols <- c("ID1", "ID2", "better_id")

  if (!all(required_cols %in% names(main_results))) {
    stop(
      "`main_results` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(required_cols %in% names(reverse_results))) {
    stop(
      "`reverse_results` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Construct an order-invariant key for each pair
  main <- main_results
  main$key <- paste(
    pmin(main$ID1, main$ID2),
    pmax(main$ID1, main$ID2),
    sep = "||"
  )
  main <- dplyr::distinct(main, key, .keep_all = TRUE)
  main <- dplyr::select(
    main,
    key,
    ID1_main       = ID1,
    ID2_main       = ID2,
    better_id_main = better_id
  )

  rev <- reverse_results
  rev$key <- paste(
    pmin(rev$ID1, rev$ID2),
    pmax(rev$ID1, rev$ID2),
    sep = "||"
  )
  rev <- dplyr::distinct(rev, key, .keep_all = TRUE)
  rev <- dplyr::select(
    rev,
    key,
    ID1_rev       = ID1,
    ID2_rev       = ID2,
    better_id_rev = better_id
  )

  joined <- dplyr::inner_join(main, rev, by = "key")

  # Drop cases with missing better_id in either direction
  if (nrow(joined) > 0L) {
    keep <- !is.na(joined$better_id_main) & !is.na(joined$better_id_rev)
    joined <- joined[keep, , drop = FALSE]
    joined$is_consistent <- joined$better_id_main == joined$better_id_rev
  } else {
    joined$is_consistent <- logical(0)
  }

  n_pairs      <- nrow(joined)
  n_consistent <- if (n_pairs > 0L) sum(joined$is_consistent) else 0L
  prop_consistent <- if (n_pairs > 0L) n_consistent / n_pairs else NA_real_

  summary_tbl <- tibble::tibble(
    n_pairs         = n_pairs,
    n_consistent    = n_consistent,
    prop_consistent = prop_consistent
  )

  list(
    summary = summary_tbl,
    details = joined
  )
}
