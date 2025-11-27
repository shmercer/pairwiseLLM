#' Create all unordered pairs of writing samples
#'
#' Given a data frame of samples with columns \code{ID} and \code{text},
#' this function generates all unordered pairs (combinations) of samples.
#' Each pair appears exactly once, with \code{ID1} < \code{ID2} in
#' lexicographic order.
#'
#' @param samples A tibble or data frame with columns \code{ID} and \code{text}.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{ID1}, \code{text1}
#'     \item \code{ID2}, \code{text2}
#'   }
#'
#' @examples
#' samples <- tibble::tibble(
#'   ID   = c("S1", "S2", "S3"),
#'   text = c("Sample 1", "Sample 2", "Sample 3")
#' )
#'
#' pairs_all <- make_pairs(samples)
#' pairs_all
#'
#' @export
make_pairs <- function(samples) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must have columns 'ID' and 'text'.", call. = FALSE)
  }

  ids <- samples$ID
  n   <- length(ids)

  if (n < 2) {
    stop("At least two samples are required to create pairs.", call. = FALSE)
  }

  comb <- utils::combn(ids, 2, simplify = FALSE)

  out <- purrr::map_dfr(
    comb,
    function(pair_ids) {
      id1 <- pair_ids[[1]]
      id2 <- pair_ids[[2]]
      text1 <- samples$text[samples$ID == id1][1]
      text2 <- samples$text[samples$ID == id2][1]
      tibble::tibble(
        ID1   = id1,
        text1 = text1,
        ID2   = id2,
        text2 = text2
      )
    }
  )

  out
}

#' Randomly sample pairs of writing samples
#'
#' This function samples a subset of rows from a pairs data frame
#' returned by \code{\link{make_pairs}}. You can specify either the
#' proportion of pairs to retain (\code{pair_pct}), the absolute number
#' of pairs (\code{n_pairs}), or both (in which case the minimum of the
#' two is used).
#'
#' @param pairs A tibble with columns \code{ID1}, \code{text1},
#'   \code{ID2}, and \code{text2}.
#' @param pair_pct Proportion of pairs to sample (between 0 and 1).
#'   Defaults to 1 (all pairs).
#' @param n_pairs Optional integer specifying the maximum number of
#'   pairs to sample.
#' @param seed Optional integer seed for reproducible sampling.
#'
#' @return A tibble containing the sampled rows of \code{pairs}.
#'
#' @examples
#' samples <- tibble::tibble(
#'   ID   = c("S1", "S2", "S3", "S4"),
#'   text = paste("Sample", 1:4)
#' )
#' pairs_all <- make_pairs(samples)
#'
#' # Sample 50% of all pairs
#' sample_pairs(pairs_all, pair_pct = 0.5, seed = 123)
#'
#' # Sample exactly 3 pairs
#' sample_pairs(pairs_all, n_pairs = 3, seed = 123)
#'
#' @export
sample_pairs <- function(pairs,
                         pair_pct = 1,
                         n_pairs  = NULL,
                         seed     = NULL) {
  pairs <- tibble::as_tibble(pairs)
  n <- nrow(pairs)
  if (n == 0L) return(pairs)

  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit({ .Random.seed <<- old_seed }, add = TRUE)
    set.seed(seed)
  }

  pair_pct <- max(min(pair_pct, 1), 0)  # clamp

  n_from_pct <- floor(pair_pct * n)
  if (is.null(n_pairs)) {
    k <- n_from_pct
  } else {
    k <- min(n_from_pct, n_pairs)
  }
  k <- max(min(k, n), 0)

  if (k == n) {
    return(pairs)
  }

  idx <- sample.int(n, size = k)
  pairs[idx, , drop = FALSE]
}

#' Generate reversed pairs for consistency checks
#'
#' Given a data frame of unordered pairs, this function samples a subset
#' of the rows and returns the same pairs in reversed order (i.e.,
#' swapping \code{ID1} with \code{ID2} and \code{text1} with \code{text2}).
#' These reversed pairs can be submitted separately to the LLM to assess
#' the consistency of pairwise judgments.
#'
#' @param pairs A tibble with columns \code{ID1}, \code{text1},
#'   \code{ID2}, and \code{text2}.
#' @param reverse_pct Proportion of rows in \code{pairs} to reverse
#'   (between 0 and 1). Defaults to 0.
#' @param n_reverse Optional integer specifying the maximum number of
#'   reversed pairs to generate.
#' @param seed Optional integer seed for reproducible sampling.
#'
#' @return A tibble of reversed pairs with columns:
#'   \itemize{
#'     \item \code{ID1}, \code{text1} (originally ID2/text2)
#'     \item \code{ID2}, \code{text2} (originally ID1/text1)
#'   }
#'
#' @examples
#' samples <- tibble::tibble(
#'   ID   = c("S1", "S2", "S3"),
#'   text = c("Sample 1", "Sample 2", "Sample 3")
#' )
#' pairs_all <- make_pairs(samples)
#'
#' # Reverse 50% of the pairs
#' rev_pairs <- sample_reverse_pairs(pairs_all, reverse_pct = 0.5, seed = 123)
#' rev_pairs
#'
#' @export
sample_reverse_pairs <- function(pairs,
                                 reverse_pct = 0,
                                 n_reverse   = NULL,
                                 seed        = NULL) {
  pairs <- tibble::as_tibble(pairs)
  n <- nrow(pairs)
  if (n == 0L) return(pairs[0, , drop = FALSE])

  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit({ .Random.seed <<- old_seed }, add = TRUE)
    set.seed(seed)
  }

  reverse_pct <- max(min(reverse_pct, 1), 0)

  n_from_pct <- floor(reverse_pct * n)
  if (is.null(n_reverse)) {
    k <- n_from_pct
  } else {
    k <- min(n_from_pct, n_reverse)
  }
  k <- max(min(k, n), 0)

  if (k == 0L) {
    return(pairs[0, , drop = FALSE])
  }

  idx <- sample.int(n, size = k)
  sel <- pairs[idx, , drop = FALSE]

  tibble::tibble(
    ID1   = sel$ID2,
    text1 = sel$text2,
    ID2   = sel$ID1,
    text2 = sel$text1
  )
}
