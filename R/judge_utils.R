#' Summarize pairwise comparison results by judge
#'
#' This helper produces descriptive statistics about a results table
#' grouped by a judge/model identifier column.
#'
#' The input should follow the standard pairwiseLLM results schema
#' with columns \code{ID1}, \code{ID2}, and \code{better_id} (the winning ID).
#' The judge column is specified via \code{judge_col}.
#'
#' In addition to basic counts (valid rows, invalid winners, etc.), this
#' helper reports a quick positional check (how often position 1 wins), and
#' an optional forward-vs-reverse consistency check when both orderings are
#' present for some unordered pairs.
#'
#' @param results A data frame or tibble with columns \code{ID1}, \code{ID2},
#'   and \code{better_id}. Additional columns are allowed.
#' @param judge_col Character scalar giving the column name that identifies
#'   the judge/model.
#' @param compute_reverse Logical. If \code{TRUE} (default), attempt to compute
#'   forward-vs-reverse consistency within each judge by splitting the rows into
#'   a "main" direction (lexicographically smaller ID in \code{ID1}) and a
#'   "reverse" direction (lexicographically larger ID in \code{ID1}). If no
#'   unordered pairs appear in both directions, the reverse-consistency fields
#'   are returned as \code{NA}.
#'
#' @return A list with two tibbles:
#' \describe{
#'   \item{by_judge}{One row per judge with descriptive statistics.}
#'   \item{overall}{One-row tibble with the same statistics computed across
#'   all judges combined.}
#' }
#'
#' @examples
#' res <- tibble::tibble(
#'   ID1 = c("A", "B", "A", "C"),
#'   ID2 = c("B", "A", "C", "A"),
#'   better_id = c("A", "A", "C", "A"),
#'   judge = c("m1", "m1", "m2", "m2")
#' )
#' out <- judge_summary(res, judge_col = "judge")
#' out$by_judge
#'
#' @export
judge_summary <- function(results, judge_col = "judge", compute_reverse = TRUE) {
  # Define vars for R CMD check notes
  ID1 <- NULL
  ID2 <- NULL
  better_id <- NULL

  results <- tibble::as_tibble(results)

  if (!is.character(judge_col) || length(judge_col) != 1L || is.na(judge_col) || nchar(judge_col) == 0L) {
    stop("`judge_col` must be a non-empty character scalar.", call. = FALSE)
  }

  required <- c("ID1", "ID2", "better_id", judge_col)
  missing <- setdiff(required, names(results))
  if (length(missing) > 0L) {
    stop("`results` must contain columns: ", paste(required, collapse = ", "), call. = FALSE)
  }

  # Coerce to character for consistent comparisons
  results <- dplyr::mutate(
    results,
    ID1 = as.character(.data$ID1),
    ID2 = as.character(.data$ID2),
    better_id = as.character(.data$better_id),
    .judge = as.character(.data[[judge_col]])
  )

  # Flag validity
  results <- dplyr::mutate(
    results,
    is_missing_winner = is.na(.data$better_id) | .data$better_id == "",
    is_invalid_winner = !.data$is_missing_winner & !(.data$better_id %in% c(.data$ID1, .data$ID2)),
    is_valid = !.data$is_missing_winner & !.data$is_invalid_winner,
    pos1_wins = dplyr::if_else(.data$is_valid & .data$better_id == .data$ID1, 1L, 0L)
  )

  summarize_one <- function(df) {
    n_total <- nrow(df)
    n_missing_winner <- sum(df$is_missing_winner, na.rm = TRUE)
    n_invalid_winner <- sum(df$is_invalid_winner, na.rm = TRUE)
    n_valid <- sum(df$is_valid, na.rm = TRUE)

    ids <- unique(c(df$ID1, df$ID2))
    n_unique_items <- length(ids[!is.na(ids)])

    # unordered pair keys
    id_min <- pmin(df$ID1, df$ID2)
    id_max <- pmax(df$ID1, df$ID2)
    key <- paste(id_min, id_max, sep = "||")
    n_unique_pairs <- dplyr::n_distinct(key)

    pos1_win_rate <- if (n_valid == 0L) NA_real_ else mean(df$pos1_wins[df$is_valid] == 1L)

    rc_n_pairs <- NA_integer_
    rc_prop_consistent <- NA_real_

    if (isTRUE(compute_reverse)) {
      # Reverse-consistency should be computed on valid winner rows only.
      dfv <- df[df$is_valid %in% TRUE, , drop = FALSE]

      # Split rows into "main" and "reverse" based on lexicographic ordering
      is_main <- dfv$ID1 <= dfv$ID2
      main_df <- dfv[is_main, , drop = FALSE]
      rev_df <- dfv[!is_main, , drop = FALSE]

      # compute_reverse_consistency expects ID1/ID2/better_id columns
      if (nrow(main_df) > 0L && nrow(rev_df) > 0L) {
        rc <- compute_reverse_consistency(
          main_results = dplyr::select(main_df, ID1, ID2, better_id),
          reverse_results = dplyr::select(rev_df, ID1, ID2, better_id)
        )
        rc_n_pairs <- as.integer(rc$summary$n_pairs[[1]])
        rc_prop_consistent <- as.numeric(rc$summary$prop_consistent[[1]])
      }
    }

    tibble::tibble(
      n_total = as.integer(n_total),
      n_valid = as.integer(n_valid),
      n_missing_winner = as.integer(n_missing_winner),
      n_invalid_winner = as.integer(n_invalid_winner),
      prop_valid = if (n_total == 0L) NA_real_ else n_valid / n_total,
      n_unique_items = as.integer(n_unique_items),
      n_unique_pairs = as.integer(n_unique_pairs),
      pos1_win_rate = as.numeric(pos1_win_rate),
      rc_n_pairs = rc_n_pairs,
      rc_prop_consistent = rc_prop_consistent
    )
  }

  by_judge <- results |>
    dplyr::group_by(.data$.judge) |>
    dplyr::group_modify(~ summarize_one(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(judge = .data$.judge) |>
    dplyr::select(-dplyr::all_of(".judge"))

  overall <- summarize_one(results)

  list(by_judge = by_judge, overall = overall)
}


#' Summarize judge fit diagnostics from a Bradley--Terry fit
#'
#' This helper extracts and summarizes judge-level fit diagnostics
#' (e.g., infit/outfit) when using a modeling engine that provides them
#' (notably \pkg{sirt} via \code{\link[sirt]{btm}}).
#'
#' It is designed to work with the object returned by \code{\link{fit_bt_model}},
#' but will also accept a data frame/tibble that already looks like a judge-fit
#' table (with columns \code{judge}, \code{infit}, and \code{outfit}).
#'
#' @param fit Either a list returned by \code{\link{fit_bt_model}} (recommended),
#'   or a data frame/tibble with judge fit columns.
#' @param fit_bounds Numeric vector of length 2 giving acceptable bounds for
#'   \code{infit} and \code{outfit}. Default is \code{c(0.7, 1.3)}.
#' @param top_n Integer. Number of worst judges (by max of infit/outfit deviation)
#'   to return in the \code{worst_judges} field of the summary. Default is 5.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{One-row tibble with overall judge-fit summary statistics.}
#'   \item{details}{A tibble with one row per judge, including a \code{is_misfit}
#'   flag.}
#' }
#'
#' @examples
#' fit <- list(
#'   diagnostics = list(
#'     judge_fit = tibble::tibble(
#'       judge = c("mA", "mB"),
#'       infit = c(1.0, 1.6),
#'       outfit = c(1.1, 1.0)
#'     )
#'   )
#' )
#' judge_fit_summary(fit)$summary
#'
#' @export
judge_fit_summary <- function(fit, fit_bounds = c(0.7, 1.3), top_n = 5L) {
  if (!is.numeric(fit_bounds) || length(fit_bounds) != 2L || any(!is.finite(fit_bounds))) {
    stop("`fit_bounds` must be a numeric vector of length 2 with finite values.", call. = FALSE)
  }
  lower <- min(fit_bounds)
  upper <- max(fit_bounds)

  top_n <- as.integer(top_n)
  if (is.na(top_n) || top_n < 0L) {
    stop("`top_n` must be a non-negative integer.", call. = FALSE)
  }

  extract_judge_fit <- function(x) {
    # case 1: already a data frame
    if (inherits(x, "data.frame")) {
      return(tibble::as_tibble(x))
    }

    # case 2: fit_bt_model() result
    if (is.list(x) && !is.null(x$diagnostics) && is.list(x$diagnostics) && !is.null(x$diagnostics$judge_fit)) {
      return(tibble::as_tibble(x$diagnostics$judge_fit))
    }

    # case 3: raw sirt::btm output
    if (is.list(x) && !is.null(x$fit_judges)) {
      return(tibble::as_tibble(x$fit_judges))
    }

    NULL
  }

  jf <- extract_judge_fit(fit)

  if (is.null(jf)) {
    summary <- tibble::tibble(
      has_judge_fit = FALSE,
      n_judges = 0L,
      n_misfit = 0L,
      misfit_prop = NA_real_,
      infit_mean = NA_real_,
      outfit_mean = NA_real_,
      infit_max = NA_real_,
      outfit_max = NA_real_,
      worst_judges = list(character())
    )
    details <- tibble::tibble()
    return(list(summary = summary, details = details))
  }

  jf <- tibble::as_tibble(jf)

  required <- c("judge", "infit", "outfit")
  miss <- setdiff(required, names(jf))
  if (length(miss) > 0L) {
    stop("Judge fit table must contain columns: ", paste(required, collapse = ", "), call. = FALSE)
  }

  # Coerce to atomic numeric
  details <- jf |>
    dplyr::mutate(
      judge = as.character(.data$judge),
      infit = as.double(unname(.data$infit)),
      outfit = as.double(unname(.data$outfit))
    ) |>
    dplyr::mutate(
      is_misfit = (.data$infit < lower | .data$infit > upper) | (.data$outfit < lower | .data$outfit > upper),
      deviation = pmax(abs(.data$infit - 1), abs(.data$outfit - 1), na.rm = TRUE)
    )

  n_judges <- nrow(details)
  n_misfit <- sum(details$is_misfit %in% TRUE, na.rm = TRUE)
  misfit_prop <- if (n_judges == 0L) NA_real_ else n_misfit / n_judges

  infit_mean <- if (n_judges == 0L) NA_real_ else mean(details$infit, na.rm = TRUE)
  outfit_mean <- if (n_judges == 0L) NA_real_ else mean(details$outfit, na.rm = TRUE)
  infit_max <- if (n_judges == 0L) NA_real_ else max(details$infit, na.rm = TRUE)
  outfit_max <- if (n_judges == 0L) NA_real_ else max(details$outfit, na.rm = TRUE)

  worst <- details |>
    dplyr::arrange(dplyr::desc(.data$deviation)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(dplyr::all_of("judge"))

  summary <- tibble::tibble(
    has_judge_fit = TRUE,
    n_judges = as.integer(n_judges),
    n_misfit = as.integer(n_misfit),
    misfit_prop = as.numeric(misfit_prop),
    infit_mean = as.numeric(infit_mean),
    outfit_mean = as.numeric(outfit_mean),
    infit_max = as.numeric(infit_max),
    outfit_max = as.numeric(outfit_max),
    worst_judges = list(as.character(worst))
  )

  details <- details |>
    dplyr::select(dplyr::all_of(c("judge", "infit", "outfit", "is_misfit", "deviation")))

  list(summary = summary, details = details)
}
