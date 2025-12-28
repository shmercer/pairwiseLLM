#' Validate and align an embeddings matrix
#'
#' Validates that an embeddings object is a numeric matrix and (optionally)
#' aligns/reorders its rows to match a set of item IDs.
#'
#' This helper is useful for "bring your own embeddings" workflows:
#' pre-compute embeddings however you like, then call \code{validate_embeddings()}
#' before passing them into \code{select_core_set()}.
#'
#' @param embeddings A numeric matrix of embeddings with one row per item.
#' @param ids Optional character vector of IDs for the target items. If supplied:
#'   \itemize{
#'     \item If \code{rownames(embeddings)} are present, they must contain all
#'       \code{ids} and will be used to align/reorder rows to \code{ids}.
#'       Extra rows are allowed.
#'     \item If \code{rownames(embeddings)} are not present, then
#'       \code{nrow(embeddings)} must equal \code{length(ids)} and rows are
#'       assumed to be in the same order as \code{ids}.
#'   }
#' @param allow_na Logical; allow missing values in \code{embeddings}. Default
#'   \code{FALSE}.
#' @param arg_name Character; used in error messages to name the argument.
#'
#' @return A numeric matrix. If \code{ids} are provided and rownames are present,
#'   the returned matrix is reordered to match \code{ids}.
#'
#' @examples
#' ids <- c("a", "b", "c")
#' emb <- matrix(rnorm(3 * 4), ncol = 4)
#' rownames(emb) <- ids
#' emb2 <- validate_embeddings(emb, ids = ids)
#' stopifnot(identical(rownames(emb2), ids))
#'
#' @export
validate_embeddings <- function(embeddings,
                                ids = NULL,
                                allow_na = FALSE,
                                arg_name = "embeddings") {
  if (missing(embeddings) || is.null(embeddings)) {
    stop(sprintf("`%s` must be provided.", arg_name), call. = FALSE)
  }
  if (!is.matrix(embeddings)) {
    stop(sprintf("`%s` must be a matrix.", arg_name), call. = FALSE)
  }
  if (!is.numeric(embeddings)) {
    stop(sprintf("`%s` must be a numeric matrix.", arg_name), call. = FALSE)
  }
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na)) {
    stop("`allow_na` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!isTRUE(allow_na) && anyNA(embeddings)) {
    stop(sprintf("`%s` must not contain missing values.", arg_name), call. = FALSE)
  }

  if (is.null(ids)) {
    return(embeddings)
  }

  if (!is.character(ids) || anyNA(ids) || any(ids == "") || length(ids) < 1L) {
    stop("`ids` must be a non-missing character vector.", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop("`ids` must not contain duplicates.", call. = FALSE)
  }

  rn <- rownames(embeddings)
  if (!is.null(rn) && length(rn) > 0L) {
    if (anyNA(rn) || any(rn == "")) {
      stop(sprintf("`%s` rownames must not contain missing/empty IDs.", arg_name), call. = FALSE)
    }
    if (anyDuplicated(rn)) {
      stop(sprintf("`%s` rownames must not contain duplicates.", arg_name), call. = FALSE)
    }
    idx <- match(ids, rn)
    if (anyNA(idx)) {
      stop(sprintf("`%s` rownames must contain all `ids`.", arg_name), call. = FALSE)
    }
    return(embeddings[idx, , drop = FALSE])
  }

  if (nrow(embeddings) != length(ids)) {
    stop(sprintf("`%s` must have nrow == length(ids) when rownames are not provided.", arg_name), call. = FALSE)
  }
  embeddings
}
