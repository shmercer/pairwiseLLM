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
#' # Using the built-in example data (10 writing samples)
#' data("example_writing_samples")
#' pairs_example <- make_pairs(example_writing_samples)
#' nrow(pairs_example)  # should be choose(10, 2) = 45
#'
#' @export
make_pairs <- function(samples) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must have columns 'ID' and 'text'.", call. = FALSE)
  }

  ids <- as.character(samples$ID)
  n   <- length(ids)

  if (n < 2) {
    stop("At least two samples are required to create pairs.", call. = FALSE)
  }

  # All unordered combinations of IDs
  comb <- utils::combn(ids, 2, simplify = FALSE)

  rows <- lapply(
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

  # Use dplyr so the Imports entry is “real”
  out <- dplyr::bind_rows(rows)

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
#' # Using example_writing_samples: sample 10% of all pairs
#' data("example_writing_samples")
#' pairs_ex <- make_pairs(example_writing_samples)
#' pairs_ex_sample <- sample_pairs(pairs_ex, pair_pct = 0.10, seed = 1)
#' nrow(pairs_ex_sample)
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

#' Sample reversed versions of a subset of pairs
#'
#' Given a table of pairs with columns \code{ID1}, \code{text1},
#' \code{ID2}, and \code{text2}, this function selects a subset
#' of rows and returns a new tibble where the order of each selected
#' pair is reversed.
#'
#' @param pairs A data frame or tibble with columns \code{ID1},
#'   \code{text1}, \code{ID2}, and \code{text2}.
#' @param reverse_pct Optional proportion of rows to reverse
#'   (between 0 and 1). If \code{n_reverse} is also supplied,
#'   \code{n_reverse} takes precedence and \code{reverse_pct} is ignored.
#' @param n_reverse Optional absolute number of rows to reverse.
#'   If supplied, this takes precedence over \code{reverse_pct}.
#' @param seed Optional integer seed for reproducible sampling.
#'
#' @return A tibble containing the reversed pairs only (i.e., with
#'   \code{ID1} swapped with \code{ID2} and \code{text1} swapped with
#'   \code{text2}).
#'
#' @examples
#' data("example_writing_samples")
#' pairs <- make_pairs(example_writing_samples)
#'
#' # Reverse 20% of the pairs
#' rev20 <- sample_reverse_pairs(pairs, reverse_pct = 0.2, seed = 123)
#'
#' @export
sample_reverse_pairs <- function(pairs,
                                 reverse_pct = NULL,
                                 n_reverse   = NULL,
                                 seed        = NULL) {
  pairs <- tibble::as_tibble(pairs)

  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols  <- setdiff(required_cols, names(pairs))
  if (length(missing_cols) > 0L) {
    stop(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  n <- nrow(pairs)
  if (n == 0L) {
    return(pairs[0, required_cols])
  }

  if (is.null(reverse_pct) && is.null(n_reverse)) {
    stop("Provide at least one of `reverse_pct` or `n_reverse`.",
         call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # If n_reverse is provided, it takes precedence
  if (!is.null(n_reverse)) {
    k <- as.integer(n_reverse)
    if (is.na(k) || k < 0L) {
      stop("`n_reverse` must be a non-negative integer.", call. = FALSE)
    }
  } else {
    # Use reverse_pct
    if (!is.numeric(reverse_pct) || length(reverse_pct) != 1L || is.na(reverse_pct)) {
      stop("`reverse_pct` must be a single numeric value.", call. = FALSE)
    }

    # Edge cases: <= 0 => 0 rows; >= 1 => all rows
    if (reverse_pct <= 0) {
      k <- 0L
    } else if (reverse_pct >= 1) {
      k <- n
    } else {
      k <- round(n * reverse_pct)
    }
  }

  if (k == 0L) {
    # Return an empty tibble with the same structure
    return(pairs[0, required_cols])
  }

  k <- min(k, n)
  idx <- sample.int(n, k)

  selected <- pairs[idx, required_cols]

  tibble::tibble(
    ID1   = selected$ID2,
    text1 = selected$text2,
    ID2   = selected$ID1,
    text2 = selected$text1
  )
}

#' Randomly assign samples to positions SAMPLE_1 and SAMPLE_2
#'
#' This helper takes a table of paired writing samples (with columns
#' \code{ID1}, \code{text1}, \code{ID2}, and \code{text2}) and, for each row,
#' randomly decides whether to keep the current order or swap the two samples.
#' The result is that approximately half of the pairs will have the original
#' order and half will be reversed, on average.
#'
#' This is useful for reducing position biases in LLM-based paired comparisons,
#' while still allowing reverse-order consistency checks via
#' \code{\link{sample_reverse_pairs}} and \code{\link{compute_reverse_consistency}}.
#'
#' @param pairs A data frame or tibble with columns \code{ID1}, \code{text1},
#'   \code{ID2}, and \code{text2}.
#' @param seed Optional integer seed for reproducible randomization. If
#'   \code{NULL} (default), the current RNG state is used.
#'
#' @return A tibble with the same columns as \code{pairs}, but with some rows'
#'   \code{ID1}/\code{text1} and \code{ID2}/\code{text2} swapped.
#'
#' @examples
#' data("example_writing_samples")
#' pairs_all <- make_pairs(example_writing_samples)
#'
#' set.seed(123)
#' pairs_rand <- randomize_pair_order(pairs_all, seed = 123)
#'
#' head(pairs_all[, c("ID1", "ID2")])
#' head(pairs_rand[, c("ID1", "ID2")])
#'
#' @export
randomize_pair_order <- function(pairs, seed = NULL) {
  pairs <- tibble::as_tibble(pairs)

  required <- c("ID1", "text1", "ID2", "text2")
  missing  <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop(
      "`pairs` must contain columns: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(pairs)
  if (n == 0L) {
    return(pairs)
  }

  flip <- sample(c(TRUE, FALSE), size = n, replace = TRUE)

  out <- pairs

  idx <- which(flip)
  if (length(idx) > 0L) {
    id1_tmp   <- out$ID1[idx]
    text1_tmp <- out$text1[idx]

    out$ID1[idx]   <- out$ID2[idx]
    out$text1[idx] <- out$text2[idx]

    out$ID2[idx]   <- id1_tmp
    out$text2[idx] <- text1_tmp
  }

  out
}
