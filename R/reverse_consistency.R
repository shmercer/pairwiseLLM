#' Compute consistency between forward and reverse pair comparisons
#'
#' Given two data frames of pairwise comparison results (one for
#' the "forward" ordering of pairs, one for the "reverse" ordering),
#' this function identifies unordered pairs that were evaluated in both
#' directions and computes the proportion of consistent judgments.
#'
#' Consistency is defined at the level of IDs: a pair is consistent
#' if the same ID is selected as better in both directions. This function
#' assumes each input contains columns \code{ID1}, \code{ID2}, and
#' \code{better_id}, where \code{better_id} is the ID of the better sample
#' (not "SAMPLE_1"/"SAMPLE_2").
#'
#' \strong{Per-key majority agreement (duplicates supported).}
#' If a pair appears multiple times in \code{main_results} and/or
#' \code{reverse_results} (e.g., submitted twice), this function aggregates
#' each unordered pair key separately in each direction and takes the
#' \emph{majority} \code{better_id}. If there is a tie for the majority
#' winner within a direction, that direction's majority winner is set to
#' \code{NA} and the key is excluded from the consistency calculation.
#'
#' The output \code{details} contains exactly one row per unordered pair key,
#' which keeps it compatible with \code{\link{check_positional_bias}}.
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
#'       Here, \code{n_pairs} counts unordered pair keys with a non-missing
#'       majority winner in both directions.
#'     \item \code{details}: a tibble with one row per unordered pair key,
#'       including columns \code{key}, \code{ID1_main}, \code{ID2_main},
#'       \code{ID1_rev}, \code{ID2_rev}, \code{better_id_main},
#'       \code{better_id_rev}, and \code{is_consistent}. Additional columns
#'       provide vote counts and tie flags.
#'   }
#'
#' @examples
#' main <- tibble::tibble(
#'   ID1       = c("A", "A", "X"),
#'   ID2       = c("B", "B", "Y"),
#'   better_id = c("A", "B", "X") # duplicate A-B with disagreement
#' )
#' rev <- tibble::tibble(
#'   ID1       = c("B"),
#'   ID2       = c("A"),
#'   better_id = c("A")
#' )
#' compute_reverse_consistency(main, rev)$summary
#'
#' @export
compute_reverse_consistency <- function(main_results, reverse_results) {
  # Define variables (for R CMD check notes)
  ID1 <- NULL
  ID2 <- NULL
  better_id <- NULL
  key <- NULL
  .tmp <- NULL

  ID1_main <- NULL
  ID2_main <- NULL
  better_id_main <- NULL
  n_main_votes <- NULL
  n_main_A <- NULL
  n_main_B <- NULL
  is_main_tie <- NULL

  ID1_rev <- NULL
  ID2_rev <- NULL
  better_id_rev <- NULL
  n_rev_votes <- NULL
  n_rev_A <- NULL
  n_rev_B <- NULL
  is_rev_tie <- NULL

  # ---- validate inputs early (so we error cleanly, not inside dplyr) ----
  validate_required_cols <- function(df, name) {
    req <- c("ID1", "ID2", "better_id")
    miss <- setdiff(req, names(df))
    if (length(miss) > 0L) {
      stop(
        "`", name, "` must contain columns: ", paste(req, collapse = ", "),
        call. = FALSE
      )
    }
  }

  main_results <- tibble::as_tibble(main_results)
  reverse_results <- tibble::as_tibble(reverse_results)
  validate_required_cols(main_results, "main_results")
  validate_required_cols(reverse_results, "reverse_results")

  # Helper: majority vote over a vector; ties => NA
  majority_vote <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(NA_character_)
    }
    tab <- table(x)
    mx <- max(tab)
    winners <- names(tab)[tab == mx]
    if (length(winners) != 1L) {
      return(NA_character_)
    }
    winners[[1]]
  }

  # Helper: choose the most common presented ordering (ID1, ID2) for a key.
  # If tied, take the first in sorted order (stable, deterministic).
  most_common_order <- function(id1, id2) {
    ord <- paste(as.character(id1), as.character(id2), sep = "||")
    if (length(ord) == 0L) {
      return(c(NA_character_, NA_character_))
    }
    tab <- table(ord)
    if (length(tab) == 0L) {
      return(c(NA_character_, NA_character_))
    }
    mx <- max(tab)
    winners <- sort(names(tab)[tab == mx])
    if (length(winners) == 0L) {
      return(c(NA_character_, NA_character_))
    }
    parts <- strsplit(winners[[1]], "\\|\\|")[[1]]
    c(parts[[1]], parts[[2]])
  }

  # Add order-invariant key and drop rows where better_id is NA (per direction)
  main <- main_results |>
    dplyr::mutate(
      key = paste(
        pmin(as.character(ID1), as.character(ID2)),
        pmax(as.character(ID1), as.character(ID2)),
        sep = "||"
      )
    ) |>
    dplyr::filter(!is.na(better_id))

  rev <- reverse_results |>
    dplyr::mutate(
      key = paste(
        pmin(as.character(ID1), as.character(ID2)),
        pmax(as.character(ID1), as.character(ID2)),
        sep = "||"
      )
    ) |>
    dplyr::filter(!is.na(better_id))

  # If either direction has no usable votes, return a clean empty result
  if (nrow(main) == 0L || nrow(rev) == 0L) {
    summary_tbl <- tibble::tibble(
      n_pairs = 0L,
      n_consistent = 0L,
      prop_consistent = NA_real_
    )
    return(list(summary = summary_tbl, details = tibble::tibble()))
  }

  # Summarize each direction to ONE row per key
  main_sum <- main |>
    dplyr::group_by(key) |>
    dplyr::summarise(
      better_id_main = majority_vote(better_id),
      .tmp = list(most_common_order(ID1, ID2)),
      ID1_main = .tmp[[1]][1],
      ID2_main = .tmp[[1]][2],
      n_main_votes = dplyr::n(),
      n_main_A = sum(better_id == ID1_main, na.rm = TRUE),
      n_main_B = sum(better_id == ID2_main, na.rm = TRUE),
      is_main_tie = is.na(better_id_main) & n_main_votes > 0,
      .groups = "drop"
    ) |>
    dplyr::select(-.tmp)

  rev_sum <- rev |>
    dplyr::group_by(key) |>
    dplyr::summarise(
      better_id_rev = majority_vote(better_id),
      .tmp = list(most_common_order(ID1, ID2)),
      ID1_rev = .tmp[[1]][1],
      ID2_rev = .tmp[[1]][2],
      n_rev_votes = dplyr::n(),
      n_rev_A = sum(better_id == ID1_rev, na.rm = TRUE),
      n_rev_B = sum(better_id == ID2_rev, na.rm = TRUE),
      is_rev_tie = is.na(better_id_rev) & n_rev_votes > 0,
      .groups = "drop"
    ) |>
    dplyr::select(-.tmp)

  joined <- dplyr::inner_join(main_sum, rev_sum, by = "key")

  if (nrow(joined) > 0L) {
    keep <- !is.na(joined$better_id_main) & !is.na(joined$better_id_rev)
    joined <- joined[keep, , drop = FALSE]
    joined$is_consistent <- joined$better_id_main == joined$better_id_rev
  } else {
    joined$is_consistent <- logical(0)
  }

  n_pairs <- nrow(joined)
  n_consistent <- if (n_pairs > 0L) sum(joined$is_consistent, na.rm = TRUE) else 0L
  prop_consistent <- if (n_pairs > 0L) n_consistent / n_pairs else NA_real_

  summary_tbl <- tibble::tibble(
    n_pairs = n_pairs,
    n_consistent = n_consistent,
    prop_consistent = prop_consistent
  )

  list(summary = summary_tbl, details = joined)
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
#' bias <- check_positional_bias(rc)
#' bias$summary
#'
#' @export
check_positional_bias <- function(consistency,
                                  n_boot = 1000,
                                  conf_level = 0.95,
                                  seed = NULL) {
  # Define variables
  is_consistent <- NULL
  is_pos1_bias <- NULL
  is_pos2_bias <- NULL
  key <- NULL
  winner_pos_main <- NULL
  winner_pos_rev <- NULL

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
    "ID1_rev", "ID2_rev", "better_id_rev",
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
  details <- details |>
    dplyr::mutate(
      winner_pos_main = dplyr::case_when(
        better_id_main == ID1_main ~ "pos1",
        better_id_main == ID2_main ~ "pos2",
        TRUE ~ NA_character_
      ),
      winner_pos_rev = dplyr::case_when(
        better_id_rev == ID1_rev ~ "pos1",
        better_id_rev == ID2_rev ~ "pos2",
        TRUE ~ NA_character_
      )
    )

  # Overall consistency
  prop_consistent <- mean(details$is_consistent)

  # Bootstrap CI
  boot_props <- .with_seed_restore(
    seed,
    f = function() {
      boot_props <- numeric(n_boot)
      for (b in seq_len(n_boot)) {
        idx <- sample.int(n_pairs, size = n_pairs, replace = TRUE)
        boot_props[b] <- mean(details$is_consistent[idx])
      }
      boot_props
    },
    arg_name = "seed"
  )

  alpha <- 1 - conf_level
  boot_lwr <- stats::quantile(boot_props, probs = alpha / 2)
  boot_upr <- stats::quantile(boot_props, probs = 1 - alpha / 2)
  boot_mean <- mean(boot_props)

  # Positional bias: how often does position 1 win?
  wins_sample1_main <- sum(details$better_id_main == details$ID1_main, na.rm = TRUE)
  n_valid_main <- sum(
    !is.na(details$better_id_main) &
      (details$better_id_main == details$ID1_main |
        details$better_id_main == details$ID2_main)
  )

  p_sample1_main <- if (n_valid_main > 0L) {
    stats::binom.test(wins_sample1_main, n_valid_main, p = 0.5)$p.value
  } else {
    NA_real_
  }

  wins_sample1_rev <- sum(details$better_id_rev == details$ID1_rev, na.rm = TRUE)
  n_valid_rev <- sum(
    !is.na(details$better_id_rev) &
      (details$better_id_rev == details$ID1_rev |
        details$better_id_rev == details$ID2_rev)
  )

  p_sample1_rev <- if (n_valid_rev > 0L) {
    stats::binom.test(wins_sample1_rev, n_valid_rev, p = 0.5)$p.value
  } else {
    NA_real_
  }

  total_pos1_wins <- wins_sample1_main + wins_sample1_rev
  total_comparisons <- n_valid_main + n_valid_rev

  p_sample1_overall <- if (total_comparisons > 0L) {
    stats::binom.test(total_pos1_wins, total_comparisons, p = 0.5)$p.value
  } else {
    NA_real_
  }

  # NEW: signed bias summary (numeric scalar)
  prop_pos1_wins_overall <- if (total_comparisons > 0L) {
    total_pos1_wins / total_comparisons
  } else {
    NA_real_
  }
  mean_signed <- if (is.finite(prop_pos1_wins_overall)) {
    prop_pos1_wins_overall - 0.5
  } else {
    NA_real_
  }

  # Inconsistent pairs & positional bias counts
  inconsistent <- details |>
    dplyr::filter(!is_consistent)

  n_inconsistent <- nrow(inconsistent)

  if (n_inconsistent > 0L) {
    inconsistent <- inconsistent |>
      dplyr::mutate(
        is_pos1_bias = winner_pos_main == "pos1" & winner_pos_rev == "pos1",
        is_pos2_bias = winner_pos_main == "pos2" & winner_pos_rev == "pos2"
      )

    n_pos1_bias <- sum(inconsistent$is_pos1_bias, na.rm = TRUE)
    n_pos2_bias <- sum(inconsistent$is_pos2_bias, na.rm = TRUE)

    details <- details |>
      dplyr::left_join(
        inconsistent |>
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
    n_pairs = n_pairs,
    prop_consistent = prop_consistent,
    boot_mean = boot_mean,
    boot_lwr = as.numeric(boot_lwr),
    boot_upr = as.numeric(boot_upr),
    p_sample1_main = p_sample1_main,
    p_sample1_rev = p_sample1_rev,
    p_sample1_overall = p_sample1_overall,
    total_pos1_wins = total_pos1_wins,
    total_comparisons = total_comparisons,
    prop_pos1_wins_overall = prop_pos1_wins_overall,
    mean_signed = mean_signed,
    n_inconsistent = n_inconsistent,
    n_inconsistent_pos1_bias = n_pos1_bias,
    n_inconsistent_pos2_bias = n_pos2_bias
  )

  list(
    summary = summary,
    details = details,
    mean_signed = mean_signed
  )
}
