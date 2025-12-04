#' Build EloChoice comparison data from pairwise results
#'
#' This function converts pairwise comparison results into the two-column
#' format used by the \pkg{EloChoice} package: one column for the winner and
#' one for the loser of each trial.
#'
#' It assumes that the input contains columns \code{ID1}, \code{ID2}, and
#' \code{better_id}, where \code{better_id} is the ID of the better sample.
#' Rows where \code{better_id} does not match either \code{ID1} or \code{ID2}
#' (including \code{NA}) are excluded.
#'
#' @param results A data frame or tibble with columns \code{ID1},
#'   \code{ID2}, and \code{better_id}.
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item \code{winner}: ID of the winning sample
#'     \item \code{loser}: ID of the losing sample
#'   }
#'   Rows with invalid or missing \code{better_id} are dropped.
#'
#' @examples
#' results <- tibble::tibble(
#'   ID1       = c("S1", "S1", "S2", "S3"),
#'   ID2       = c("S2", "S3", "S3", "S4"),
#'   better_id = c("S1", "S3", "S2", "S4")
#' )
#'
#' elo_data <- build_elo_data(results)
#' elo_data
#'
#' @export
build_elo_data <- function(results) {
  results <- tibble::as_tibble(results)

  required_cols <- c("ID1", "ID2", "better_id")
  if (!all(required_cols %in% names(results))) {
    stop(
      "`results` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Winner = better_id, loser = the other ID (otherwise NA)
  winner <- ifelse(
    results$better_id == results$ID1, results$ID1,
    ifelse(results$better_id == results$ID2, results$ID2, NA_character_)
  )

  loser <- ifelse(
    results$better_id == results$ID1, results$ID2,
    ifelse(results$better_id == results$ID2, results$ID1, NA_character_)
  )

  keep <- !is.na(winner) & !is.na(loser)

  tibble::tibble(
    winner = winner[keep],
    loser  = loser[keep]
  )
}

#' Fit an EloChoice model to pairwise comparison data
#'
#' This function fits an Elo-based paired-comparison model using the
#' \pkg{EloChoice} package. It is intended to complement
#' \code{\link{fit_bt_model}} by providing an alternative scoring framework
#' based on Elo ratings rather than Bradley–Terry models.
#'
#' The input \code{elo_data} must contain two columns:
#' \enumerate{
#'   \item \code{winner}: ID of the winning sample in each pairwise trial
#'   \item \code{loser}: ID of the losing sample in each trial
#' }
#' These can be created from standard pairwise comparison output using
#' \code{\link{build_elo_data}}.
#'
#' @details
#' Internally, this function calls:
#' \itemize{
#'   \item \code{\link[EloChoice]{elochoice}} — to estimate Elo ratings using
#'         repeated randomization of trial order;
#'   \item \code{\link[EloChoice]{reliability}} — to compute unweighted and
#'         weighted reliability indices as described in Clark et al. (2018).
#' }
#'
#' If the \pkg{EloChoice} package is not installed, a helpful error message
#' is shown telling the user how to install it.
#'
#' The returned object mirrors the structure of \code{\link{fit_bt_model}}
#' for consistency across scoring engines:
#' \itemize{
#'   \item \code{engine} — always \code{"EloChoice"}.
#'   \item \code{fit} — the raw \code{"elochoice"} object returned by
#'         \code{EloChoice::elochoice()}.
#'   \item \code{elo} — a tibble with columns:
#'         \itemize{
#'           \item \code{ID}: sample identifier
#'           \item \code{elo}: estimated Elo rating
#'         }
#'         (Unlike Bradley–Terry models, EloChoice does not provide
#'         standard errors for these ratings, so none are returned.)
#'   \item \code{reliability} — the mean unweighted reliability index
#'         (mean proportion of “upsets” across randomizations).
#'   \item \code{reliability_weighted} — the mean weighted reliability index
#'         (weighted version of the upset measure).
#' }
#'
#' @param elo_data A data frame or tibble containing \code{winner} and
#'   \code{loser} columns. Typically produced using
#'   \code{\link{build_elo_data}}.
#' @param runs Integer number of randomizations to use in
#'   \code{EloChoice::elochoice}. Default is 5.
#' @param ... Additional arguments passed to
#'   \code{EloChoice::elochoice()}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{engine}{Character scalar identifying the scoring engine
#'         (\code{"EloChoice"}).}
#'   \item{fit}{The \code{"elochoice"} model object.}
#'   \item{elo}{A tibble with columns \code{ID} and \code{elo}.}
#'   \item{reliability}{Numeric scalar: mean unweighted reliability index.}
#'   \item{reliability_weighted}{Numeric scalar: mean weighted reliability index.}
#' }
#'
#' @references
#' Clark AP, Howard KL, Woods AT, Penton-Voak IS, Neumann C (2018).
#' "Why rate when you could compare? Using the 'EloChoice' package to assess
#' pairwise comparisons of perceived physical strength."
#' \emph{PLOS ONE}, 13(1), e0190393.
#' \doi{10.1371/journal.pone.0190393}.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("EloChoice", quietly = TRUE)) {
#'   data("example_writing_pairs", package = "pairwiseLLM")
#'
#'   elo_data <- build_elo_data(example_writing_pairs)
#'
#'   fit <- fit_elo_model(elo_data, runs = 5)
#'   fit$elo
#'   fit$reliability
#'   fit$reliability_weighted
#' }
#' }
#'
#' @export
fit_elo_model <- function(elo_data, runs = 5, ...) {
  # Helpful message if EloChoice is not installed
  if (!requireNamespace("EloChoice", quietly = TRUE)) {
    stop(
      "Package 'EloChoice' must be installed to use `fit_elo_model()`. ",
      "Install it with:\n  install.packages(\"EloChoice\")",
      call. = FALSE
    )
  }

  elo_data <- as.data.frame(elo_data)

  required_cols <- c("winner", "loser")
  missing <- setdiff(required_cols, names(elo_data))
  if (length(missing) > 0L) {
    stop(
      "`elo_data` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate runs: type and length before numeric comparison
  if (!is.numeric(runs) || length(runs) != 1L || runs <= 0) {
    stop("`runs` must be a single positive numeric value.", call. = FALSE)
  }
  runs <- as.integer(runs)

  # -------------------------------------------------------------------
  # Fit EloChoice model
  # -------------------------------------------------------------------
  fit <- EloChoice::elochoice(
    winner = elo_data$winner,
    loser  = elo_data$loser,
    runs   = runs,
    ...
  )

  # -------------------------------------------------------------------
  # Extract Elo ratings from ratmat
  # ratmat: rows = randomizations, cols = stimuli
  # -------------------------------------------------------------------
  ratings_mat <- fit$ratmat
  if (is.null(ratings_mat) || !is.matrix(ratings_mat)) {
    stop(
      "EloChoice::elochoice() output does not contain a valid `ratmat` matrix.",
      call. = FALSE
    )
  }

  ids <- colnames(ratings_mat)
  if (is.null(ids) || all(!nzchar(ids))) {
    # This would be unusual; EloChoice normally sets colnames from stimulus IDs
    ids <- as.character(seq_len(ncol(ratings_mat)))
  }

  elo_scores <- colMeans(ratings_mat, na.rm = TRUE)

  elo_tbl <- tibble::tibble(
    ID  = as.character(ids),
    elo = as.numeric(elo_scores)
  )

  # -------------------------------------------------------------------
  # Reliability: unweighted + weighted indices
  # -------------------------------------------------------------------
  rel <- EloChoice::reliability(fit)

  reliability_unweighted <- NA_real_
  reliability_weighted   <- NA_real_

  if (is.data.frame(rel) && nrow(rel) > 0L) {
    # Per docs: first column = unweighted, second = weighted
    # typically named upset and upset.wgt
    if ("upset" %in% names(rel)) {
      reliability_unweighted <- mean(rel$upset, na.rm = TRUE)
    } else {
      # fallback to first numeric column if names ever change
      num_cols <- vapply(rel, is.numeric, logical(1))
      if (any(num_cols)) {
        reliability_unweighted <- mean(rel[[which(num_cols)[1]]], na.rm = TRUE)
      }
    }

    if ("upset.wgt" %in% names(rel)) {
      reliability_weighted <- mean(rel$upset.wgt, na.rm = TRUE)
    } else {
      num_cols <- vapply(rel, is.numeric, logical(1))
      if (sum(num_cols) >= 2L) {
        reliability_weighted <- mean(rel[[which(num_cols)[2]]], na.rm = TRUE)
      }
    }
  }

  list(
    engine               = "EloChoice",
    fit                  = fit,
    elo                  = elo_tbl,
    reliability          = reliability_unweighted,
    reliability_weighted = reliability_weighted
  )
}
