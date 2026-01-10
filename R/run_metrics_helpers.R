#' Count unique unordered pairs for runner metrics
#'
#' Runner functions in pairwiseLLM report pair-count metrics each round. This helper
#' computes schema-stable counts using a consistent definition across runners.
#'
#' Semantics:
#' \itemize{
#'   \item \code{n_pairs_total}: number of unique \emph{unordered} pairs observed so far,
#'   optionally restricted to pairs where both endpoints lie in \code{ids}.
#'   \item \code{n_pairs_new}: number of unique \emph{unordered} pairs observed so far
#'   that involve at least one ID in \code{new_ids} (and, if \code{ids} is supplied,
#'   also lie within \code{ids}).
#' }
#'
#' @param results A data frame with columns \code{ID1} and \code{ID2}.
#' @param ids Optional character vector defining the relevant ID universe. If
#'   supplied, counts only include pairs where both endpoints are in \code{ids}.
#' @param new_ids Optional character vector of newly introduced IDs (core-linking
#'   context). If supplied, \code{n_pairs_new} counts unique unordered pairs that
#'   touch at least one ID in \code{new_ids}.
#'
#' @return A tibble with integer columns \code{n_pairs_total} and \code{n_pairs_new}.
#'   If \code{new_ids} is \code{NULL}, \code{n_pairs_new} is returned as \code{NA}.
#'
#' @examples
#' results <- tibble::tibble(
#'   ID1 = c("A", "B", "B", "A"),
#'   ID2 = c("B", "A", "C", "B")
#' )
#'
#' # Unique unordered edges across all observed pairs
#' bt_count_unique_pairs(results)
#'
#' # Count edges that involve at least one newly introduced ID
#' bt_count_unique_pairs(results, new_ids = "C")
#'
#' # Restrict counting to a particular ID universe
#' bt_count_unique_pairs(results, ids = c("A", "B"))
#'
#' @export
bt_count_unique_pairs <- function(results, ids = NULL, new_ids = NULL) {
  res <- tibble::as_tibble(results)

  if (!all(c("ID1", "ID2") %in% names(res))) {
    stop("`results` must have columns ID1 and ID2.", call. = FALSE)
  }

  id1 <- as.character(res$ID1)
  id2 <- as.character(res$ID2)

  keep <- !is.na(id1) & !is.na(id2) & nzchar(id1) & nzchar(id2)

  ids_u <- NULL
  if (!is.null(ids)) {
    ids_u <- unique(as.character(ids))
    ids_u <- ids_u[!is.na(ids_u) & nzchar(ids_u)]
    if (length(ids_u) == 0L) ids_u <- NULL
  }

  if (!is.null(ids_u)) {
    keep <- keep & (id1 %in% ids_u) & (id2 %in% ids_u)
  }

  key_all <- .unordered_pair_key(id1, id2)

  n_pairs_total <- if (any(keep)) {
    as.integer(length(unique(key_all[keep])))
  } else {
    0L
  }

  n_pairs_new <- NA_integer_
  if (!is.null(new_ids)) {
    new_u <- unique(as.character(new_ids))
    new_u <- new_u[!is.na(new_u) & nzchar(new_u)]

    if (length(new_u) == 0L) {
      n_pairs_new <- 0L
    } else {
      keep_new <- keep & ((id1 %in% new_u) | (id2 %in% new_u))
      n_pairs_new <- if (any(keep_new)) {
        as.integer(length(unique(key_all[keep_new])))
      } else {
        0L
      }
    }
  }

  tibble::tibble(
    n_pairs_total = as.integer(n_pairs_total),
    n_pairs_new = as.integer(n_pairs_new)
  )
}
