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

#' Check positional bias and bootstrap consistency reliability
#'
#' This function diagnoses positional bias in LLM-based paired comparison data
#' and provides a bootstrapped confidence interval for the overall consistency
#' of forward vs. reverse comparisons.
#'
#' It is designed to work with the output of
#' \code{\link{compute_reverse_consistency}}, but will also accept a tibble
#' that looks like its \code{$details} component.
#'
#' @param consistency Either:
#'   \itemize{
#'     \item A list returned by \code{compute_reverse_consistency()} that
#'           contains a \code{$details} tibble; or
#'     \item A tibble/data frame with columns
#'           \code{key}, \code{ID1_main}, \code{ID2_main},
#'           \code{better_id_main}, \code{ID1_rev}, \code{ID2_rev},
#'           \code{better_id_rev}, and \code{is_consistent}.
#'   }
#' @param n_boot Integer, number of bootstrap resamples for estimating the
#'   distribution of the overall consistency proportion. Default is 1000.
#' @param conf_level Confidence level for the bootstrap interval. Default is
#'   0.95.
#' @param seed Optional integer seed for reproducible bootstrapping. If
#'   \code{NULL} (default), the current RNG state is used.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{summary}{A tibble with:
#'       \itemize{
#'         \item \code{n_pairs}: number of unordered pairs
#'         \item \code{prop_consistent}: observed proportion of consistent pairs
#'         \item \code{boot_mean}: mean of bootstrap consistency proportions
#'         \item \code{boot_lwr}, \code{boot_upr}: bootstrap confidence interval
#'         \item \code{p_sample1_main}: p-value from a binomial test for the
#'               null hypothesis that SAMPLE_1 wins 50\% of the time in the
#'               main (forward) comparisons
#'         \item \code{p_sample1_rev}: analogous p-value for the reverse
#'               comparisons
#'         \item \code{p_sample1_overall}: p-value from a binomial test for
#'               the null that position 1 wins 50\% of the time across
#'               \emph{all} (forward + reverse) comparisons
#'         \item \code{total_pos1_wins}: total number of wins by position 1
#'               across forward + reverse comparisons
#'         \item \code{total_comparisons}: total number of valid forward +
#'               reverse comparisons included in the overall test
#'         \item \code{n_inconsistent}: number of pairs with inconsistent
#'               forward vs. reverse outcomes
#'         \item \code{n_inconsistent_pos1_bias}: among inconsistent pairs, how
#'               many times the winner is in position 1 in both directions
#'         \item \code{n_inconsistent_pos2_bias}: analogous for position 2
#'       }
#'     }
#'     \item{details}{The input \code{details} tibble augmented with:
#'       \itemize{
#'         \item \code{winner_pos_main}: \code{"pos1"} or \code{"pos2"} (or
#'               \code{NA}) indicating which position won in the main direction
#'         \item \code{winner_pos_rev}: analogous for the reversed direction
#'         \item \code{is_pos1_bias}: logical; \code{TRUE} if the pair is
#'               inconsistent and position 1 wins in both directions
#'         \item \code{is_pos2_bias}: analogous for position 2
#'       }
#'     }
#'   }
#'
#' @export
check_positional_bias <- function(consistency,
                                  n_boot     = 1000,
                                  conf_level = 0.95,
                                  seed       = NULL) {

  # ---- Extract the details tibble safely ----
  if (!inherits(consistency, "data.frame") &&
      is.list(consistency) &&
      "details" %in% names(consistency)) {
    details <- consistency[["details"]]
  } else {
    details <- consistency
  }

  details <- tibble::as_tibble(details)

  required <- c(
    "key",
    "ID1_main", "ID2_main", "better_id_main",
    "ID1_rev",  "ID2_rev",  "better_id_rev",
    "is_consistent"
  )
  missing <- setdiff(required, names(details))
  if (length(missing) > 0L) {
    stop(
      "`details` must contain columns: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }

  n_pairs <- nrow(details)
  if (n_pairs == 0L) {
    stop("`details` has zero rows; nothing to diagnose.", call. = FALSE)
  }

  # Winner positions
  details <- details %>%
    dplyr::mutate(
      winner_pos_main = dplyr::case_when(
        better_id_main == ID1_main ~ "pos1",
        better_id_main == ID2_main ~ "pos2",
        TRUE                       ~ NA_character_
      ),
      winner_pos_rev = dplyr::case_when(
        better_id_rev == ID1_rev ~ "pos1",
        better_id_rev == ID2_rev ~ "pos2",
        TRUE                     ~ NA_character_
      )
    )

  # Overall consistency
  prop_consistent <- mean(details$is_consistent)

  # Bootstrap CI
  if (!is.null(seed)) {
    set.seed(seed)
  }

  boot_props <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n_pairs, size = n_pairs, replace = TRUE)
    boot_props[b] <- mean(details$is_consistent[idx])
  }

  alpha     <- 1 - conf_level
  boot_lwr  <- stats::quantile(boot_props, probs = alpha / 2)
  boot_upr  <- stats::quantile(boot_props, probs = 1 - alpha / 2)
  boot_mean <- mean(boot_props)

  # Positional bias: how often does SAMPLE_1 (position 1) win?
  # -- forward (main) direction --
  wins_sample1_main <- sum(details$better_id_main == details$ID1_main, na.rm = TRUE)
  n_valid_main      <- sum(
    !is.na(details$better_id_main) &
      (details$better_id_main == details$ID1_main |
         details$better_id_main == details$ID2_main)
  )

  p_sample1_main <- if (n_valid_main > 0L) {
    stats::binom.test(wins_sample1_main, n_valid_main, p = 0.5)$p.value
  } else {
    NA_real_
  }

  # -- reverse direction --
  wins_sample1_rev <- sum(details$better_id_rev == details$ID1_rev, na.rm = TRUE)
  n_valid_rev      <- sum(
    !is.na(details$better_id_rev) &
      (details$better_id_rev == details$ID1_rev |
         details$better_id_rev == details$ID2_rev)
  )

  p_sample1_rev <- if (n_valid_rev > 0L) {
    stats::binom.test(wins_sample1_rev, n_valid_rev, p = 0.5)$p.value
  } else {
    NA_real_
  }

  # ---- NEW: overall position-1 bias test (forward + reverse combined) ----
  total_pos1_wins   <- wins_sample1_main + wins_sample1_rev
  total_comparisons <- n_valid_main + n_valid_rev

  p_sample1_overall <- if (total_comparisons > 0L) {
    stats::binom.test(total_pos1_wins, total_comparisons, p = 0.5)$p.value
  } else {
    NA_real_
  }

  # Inconsistent pairs & positional bias counts
  inconsistent <- details %>%
    dplyr::filter(!is_consistent)

  n_inconsistent <- nrow(inconsistent)

  if (n_inconsistent > 0L) {
    inconsistent <- inconsistent %>%
      dplyr::mutate(
        is_pos1_bias = winner_pos_main == "pos1" & winner_pos_rev == "pos1",
        is_pos2_bias = winner_pos_main == "pos2" & winner_pos_rev == "pos2"
      )

    n_pos1_bias <- sum(inconsistent$is_pos1_bias, na.rm = TRUE)
    n_pos2_bias <- sum(inconsistent$is_pos2_bias, na.rm = TRUE)

    # Merge flags back into main details tibble
    details <- details %>%
      dplyr::left_join(
        inconsistent %>%
          dplyr::select(key, is_pos1_bias, is_pos2_bias),
        by = "key"
      )
  } else {
    details$is_pos1_bias <- FALSE
    details$is_pos2_bias <- FALSE
    n_pos1_bias <- 0L
    n_pos2_bias <- 0L
  }

  summary <- tibble::tibble(
    n_pairs                  = n_pairs,
    prop_consistent          = prop_consistent,
    boot_mean                = boot_mean,
    boot_lwr                 = as.numeric(boot_lwr),
    boot_upr                 = as.numeric(boot_upr),
    p_sample1_main           = p_sample1_main,
    p_sample1_rev            = p_sample1_rev,
    p_sample1_overall        = p_sample1_overall,
    total_pos1_wins          = total_pos1_wins,
    total_comparisons        = total_comparisons,
    n_inconsistent           = n_inconsistent,
    n_inconsistent_pos1_bias = n_pos1_bias,
    n_inconsistent_pos2_bias = n_pos2_bias
  )

  list(
    summary = summary,
    details = details
  )
}
