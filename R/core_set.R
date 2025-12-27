#' Select a core set of items for BT linking
#'
#' Selects a representative "core bank" of samples from a larger pool, intended
#' to be reused across waves/batches for linking Bradley-Terry (BT) scales.
#'
#' This function does not run any LLM calls. It selects items using one of:
#' \itemize{
#'   \item \code{"embeddings"}: k-means clustering on a supplied embedding matrix,
#'     then chooses one medoid (nearest-to-centroid item) per cluster.
#'   \item \code{"token_stratified"}: picks items spaced across the distribution
#'     of word counts (fast fallback when embeddings are not available).
#'   \item \code{"random"}: uniform random sample without replacement.
#' }
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' @param core_size Integer number of core items to select. If \code{NULL},
#'   uses \code{core_pct * nrow(samples)} (clamped to \code{[2, n]}).
#' @param core_pct Proportion used when \code{core_size} is \code{NULL}.
#'   Must be in \code{(0, 1]}.
#' @param method Selection method: \code{"embeddings"}, \code{"token_stratified"},
#'   or \code{"random"}.
#' @param embeddings Optional numeric matrix of embeddings (rows correspond to
#'   samples). Required when \code{method = "embeddings"}.
#'   If \code{rownames(embeddings)} are present, they must contain all sample IDs
#'   and will be used to align rows. Otherwise \code{nrow(embeddings)} must equal
#'   \code{nrow(samples)} and rows are assumed to be in the same order as
#'   \code{samples}.
#' @param distance Distance used within embeddings selection. For \code{"cosine"},
#'   embeddings are L2-normalized and Euclidean distance is applied.
#' @param seed Optional integer seed. When provided, RNG state is restored to its
#'   prior value (or returned to "uninitialized" if it was missing).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{ID}: selected core IDs
#'   \item \code{method}: selection method
#'   \item \code{core_rank}: 1..core_size
#'   \item \code{word_count}: word count (only populated for token_stratified)
#'   \item \code{cluster}: k-means cluster label (only for embeddings)
#'   \item \code{centroid_dist}: squared distance to cluster centroid (embeddings)
#' }
#'
#' @examples
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' # Token-stratified (no embeddings needed)
#' core_len <- select_core_set(example_writing_samples, core_size = 4, method = "token_stratified")
#' core_len
#'
#' # Embeddings-based (user supplies embeddings matrix)
#' set.seed(1)
#' emb <- matrix(rnorm(nrow(example_writing_samples) * 8), ncol = 8)
#' rownames(emb) <- example_writing_samples$ID
#' core_emb <- select_core_set(
#'   example_writing_samples,
#'   core_size = 5,
#'   method = "embeddings",
#'   embeddings = emb,
#'   seed = 1
#' )
#' core_emb
#'
#' @export
select_core_set <- function(samples,
                            core_size = NULL,
                            core_pct = 0.10,
                            method = c("embeddings", "token_stratified", "random"),
                            embeddings = NULL,
                            distance = c("cosine", "euclidean"),
                            seed = NULL) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must have columns 'ID' and 'text'.", call. = FALSE)
  }

  ids <- as.character(samples$ID)
  n <- length(ids)
  if (n < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

  method <- match.arg(method)
  distance <- match.arg(distance)

  if (!is.numeric(core_pct) || length(core_pct) != 1L || is.na(core_pct) || core_pct <= 0 || core_pct > 1) {
    stop("`core_pct` must be a single numeric value in (0, 1].", call. = FALSE)
  }

  if (is.null(core_size)) {
    k <- as.integer(round(core_pct * n))
  } else {
    k <- as.integer(core_size)
  }
  if (is.na(k) || k < 2L) stop("`core_size` must be an integer >= 2.", call. = FALSE)
  if (k > n) stop("`core_size` cannot exceed nrow(samples).", call. = FALSE)

  out <- .with_seed_restore(
    seed,
    f = function() {
      if (identical(method, "random")) {
        sel <- sample.int(n, size = k, replace = FALSE)
        out_ids <- ids[sel]

        return(tibble::tibble(
          ID = out_ids,
          method = method,
          core_rank = seq_along(out_ids),
          word_count = NA_integer_,
          cluster = NA_integer_,
          centroid_dist = NA_real_
        ))
      }

      if (identical(method, "token_stratified")) {
        wc <- .count_words(samples$text)
        ord <- order(wc, ids)

        pos <- unique(as.integer(round(seq.int(1L, n, length.out = k))))
        pos <- pos[pos >= 1L & pos <= n]

        # nocov start
        if (length(pos) < k) {
          remaining <- setdiff(seq_len(n), pos)
          need <- k - length(pos)
          if (need > 0L && length(remaining) > 0L) {
            pos <- c(pos, sample(remaining, size = min(need, length(remaining)), replace = FALSE))
          }
        }
        # nocov end

        pos <- pos[seq_len(min(k, length(pos)))]
        sel <- ord[pos]

        # nocov start
        if (length(sel) < k) {
          # top up deterministically from the middle-out if something weird happened
          remaining <- setdiff(seq_len(n), sel)
          topup <- remaining[seq_len(min(k - length(sel), length(remaining)))]
          sel <- c(sel, topup)
        }
        # nocov end

        out_ids <- ids[sel]
        out_wc <- wc[sel]

        return(tibble::tibble(
          ID = out_ids,
          method = method,
          core_rank = seq_along(out_ids),
          word_count = as.integer(out_wc),
          cluster = NA_integer_,
          centroid_dist = NA_real_
        ))
      }

      # embeddings
      if (is.null(embeddings)) {
        stop("`embeddings` must be provided when `method = 'embeddings'`.", call. = FALSE)
      }
      emb <- .align_embeddings(embeddings, ids)

      if (identical(distance, "cosine")) {
        emb <- .l2_normalize_rows(emb)
      }

      km <- tryCatch(
        stats::kmeans(emb, centers = k, nstart = 10, iter.max = 50),
        error = function(e) {
          stop("k-means failed while selecting core items: ", conditionMessage(e), call. = FALSE)
        }
      )

      centers <- km$centers
      cl <- as.integer(km$cluster)

      chosen <- integer(0)
      chosen_cluster <- integer(0)
      chosen_dist <- numeric(0)

      for (j in seq_len(k)) {
        idx <- which(cl == j)
        # nocov start
        if (length(idx) == 0L) next
        # nocov end

        diffs <- emb[idx, , drop = FALSE] - matrix(centers[j, ], nrow = length(idx), ncol = ncol(emb), byrow = TRUE)
        d2 <- rowSums(diffs * diffs)

        w <- which.min(d2)
        chosen <- c(chosen, idx[w])
        chosen_cluster <- c(chosen_cluster, j)
        chosen_dist <- c(chosen_dist, d2[w])
      }

      # Defensive: if any cluster was empty (rare), top up from remaining points
      # nocov start
      if (length(chosen) < k) {
        remaining <- setdiff(seq_len(n), chosen)
        need <- k - length(chosen)
        if (need > 0L && length(remaining) > 0L) {
          top <- remaining[seq_len(min(need, length(remaining)))]
          chosen <- c(chosen, top)
          chosen_cluster <- c(chosen_cluster, rep(NA_integer_, length(top)))
          chosen_dist <- c(chosen_dist, rep(NA_real_, length(top)))
        }
      }
      # nocov end

      chosen <- chosen[seq_len(k)]
      chosen_cluster <- chosen_cluster[seq_len(k)]
      chosen_dist <- chosen_dist[seq_len(k)]

      out_ids <- ids[chosen]

      tibble::tibble(
        ID = out_ids,
        method = method,
        core_rank = seq_along(out_ids),
        word_count = NA_integer_,
        cluster = chosen_cluster,
        centroid_dist = as.numeric(chosen_dist)
      )
    },
    arg_name = "seed"
  )

  out
}


.count_words <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  n <- length(x)
  out <- integer(n)
  for (i in seq_len(n)) {
    m <- gregexpr("\\S+", x[i], perl = TRUE)[[1]]
    out[i] <- if (length(m) == 1L && m[1] == -1L) 0L else length(m)
  }
  out
}

.align_embeddings <- function(embeddings, ids) {
  if (!is.matrix(embeddings)) stop("`embeddings` must be a matrix.", call. = FALSE)
  if (!is.numeric(embeddings)) stop("`embeddings` must be a numeric matrix.", call. = FALSE)

  rn <- rownames(embeddings)
  if (!is.null(rn) && all(!is.na(rn)) && all(rn != "")) {
    idx <- match(ids, rn)
    if (anyNA(idx)) stop("`embeddings` rownames must contain all `samples$ID`.", call. = FALSE)
    return(embeddings[idx, , drop = FALSE])
  }

  if (nrow(embeddings) != length(ids)) {
    stop("`embeddings` must have nrow == nrow(samples) when rownames are not provided.", call. = FALSE)
  }

  embeddings
}

.l2_normalize_rows <- function(x) {
  # normalize each row; leave all-zero rows as-is
  norms <- sqrt(rowSums(x * x))
  nz <- norms > 0
  x[nz, ] <- x[nz, , drop = FALSE] / norms[nz]
  x
}
