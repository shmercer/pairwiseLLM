#' Select a core set of items for BT linking
#'
#' Selects a representative "core bank" of samples from a larger pool, intended
#' to be reused across waves/batches for linking Bradley-Terry (BT) scales.
#'
#' This function does not run any LLM calls. It selects items using one of:
#' \itemize{
#'   \item \code{"auto"} / \code{"embeddings"}: clustering on a supplied embedding
#'     matrix, then chooses one medoid per cluster. By default, uses PAM for
#'     smaller datasets and CLARA for larger datasets.
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
#' @param method Selection method: \code{"auto"}, \code{"pam"}, \code{"clara"},
#'   \code{"embeddings"} (alias for \code{"auto"}), \code{"token_stratified"},
#'   or \code{"random"}.
#' @param embeddings Optional numeric matrix of embeddings (rows correspond to
#'   samples). Required when \code{method} uses embeddings
#'   (\code{method} in \code{c("auto","pam","clara","embeddings")}).
#'   If \code{rownames(embeddings)} are present, they must contain all sample IDs
#'   and will be used to align rows. Otherwise \code{nrow(embeddings)} must equal
#'   \code{nrow(samples)} and rows are assumed to be in the same order as
#'   \code{samples}.
#' @param distance Distance used within embeddings selection. For \code{"cosine"},
#'   embeddings are L2-normalized and Euclidean distance is applied.
#' @param clara_threshold Integer. When \code{method = "auto"} (or \code{"embeddings"}),
#'   uses PAM when \code{nrow(samples) <= clara_threshold} and CLARA otherwise.
#' @param clara_samples Integer number of CLARA samples (passed to
#'   \code{cluster::clara}).
#' @param clara_sampsize Optional integer CLARA subsample size (passed to
#'   \code{cluster::clara}). If \code{NULL}, uses a conservative heuristic.
#' @param seed Optional integer seed. When provided, RNG state is restored to its
#'   prior value (or returned to "uninitialized" if it was missing).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{ID}: selected core IDs
#'   \item \code{method}: selection method
#'   \item \code{core_rank}: 1..core_size
#'   \item \code{word_count}: word count (only populated for token_stratified)
#'   \item \code{cluster}: cluster label (only for embeddings-based methods)
#'   \item \code{centroid_dist}: squared distance from the selected medoid to the
#'     centroid of its cluster (embeddings-based methods)
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
                            method = c("auto", "pam", "clara", "embeddings", "token_stratified", "random"),
                            embeddings = NULL,
                            distance = c("cosine", "euclidean"),
                            clara_threshold = 2000L,
                            clara_samples = 5L,
                            clara_sampsize = NULL,
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
  # Backward-compatible alias: "embeddings" behaves like "auto".
  method_cluster <- method
  if (identical(method_cluster, "embeddings")) method_cluster <- "auto"
  distance <- match.arg(distance)

  # Back-compat: previous versions used method = "embeddings".
  if (identical(method, "embeddings")) method <- "auto"

  if (!is.numeric(clara_threshold) || length(clara_threshold) != 1L || is.na(clara_threshold) || clara_threshold < 2) {
    stop("`clara_threshold` must be a single integer >= 2.", call. = FALSE)
  }
  clara_threshold <- as.integer(clara_threshold)

  if (!is.numeric(clara_samples) || length(clara_samples) != 1L || is.na(clara_samples) || clara_samples < 1) {
    stop("`clara_samples` must be a single integer >= 1.", call. = FALSE)
  }
  clara_samples <- as.integer(clara_samples)

  if (!is.null(clara_sampsize)) {
    if (!is.numeric(clara_sampsize) || length(clara_sampsize) != 1L || is.na(clara_sampsize) || clara_sampsize < 2) {
      stop("`clara_sampsize` must be NULL or a single integer >= 2.", call. = FALSE)
    }
    clara_sampsize <- as.integer(clara_sampsize)
  }

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

      # embeddings / clustering-based core selection
      if (is.null(embeddings)) {
        stop("`embeddings` must be provided when `method` selects from embeddings.", call. = FALSE)
      }
      emb <- .align_embeddings(embeddings, ids)

      if (identical(distance, "cosine")) {
        emb <- .l2_normalize_rows(emb)
      }

      sel <- tryCatch(
        .select_medoids_from_embeddings(
          emb,
          k = k,
          method = method_cluster,
          clara_threshold = clara_threshold,
          clara_samples = clara_samples,
          clara_sampsize = clara_sampsize
        ),
        error = function(e) {
          stop("Embedding clustering failed while selecting core items: ", conditionMessage(e), call. = FALSE)
        }
      )

      chosen <- as.integer(sel$medoids)
      cl <- as.integer(sel$clustering)
      chosen_cluster <- cl[chosen]

      # Distance from medoid to its cluster centroid (squared Euclidean).
      chosen_dist <- rep(NA_real_, length(chosen))
      for (i in seq_along(chosen)) {
        j <- chosen_cluster[i]
        idx <- which(cl == j)
        # nocov start
        if (length(idx) == 0L) next
        # nocov end
        centroid <- colMeans(emb[idx, , drop = FALSE])
        diffs <- emb[chosen[i], ] - centroid
        chosen_dist[i] <- sum(diffs * diffs)
      }

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


.select_medoids_from_embeddings <- function(emb,
                                            k,
                                            method = c("auto", "pam", "clara"),
                                            clara_threshold = 2000L,
                                            clara_samples = 5L,
                                            clara_sampsize = NULL) {
  method <- match.arg(method)
  n <- nrow(emb)
  if (k < 2L || k > n) stop("`k` must be between 2 and nrow(emb).", call. = FALSE)

  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' is required for embeddings-based core selection.", call. = FALSE)
  }

  if (identical(method, "auto")) {
    if (n <= clara_threshold) {
      return(.select_medoids_pam(emb, k = k))
    }
    return(.select_medoids_clara(emb, k = k, samples = clara_samples, sampsize = clara_sampsize))
  }

  if (identical(method, "pam")) {
    return(.select_medoids_pam(emb, k = k))
  }
  .select_medoids_clara(emb, k = k, samples = clara_samples, sampsize = clara_sampsize)
}

.select_medoids_pam <- function(emb, k) {
  fit <- cluster::pam(emb, k = k, metric = "euclidean")
  list(medoids = fit$id.med, clustering = fit$clustering)
}

.select_medoids_clara <- function(emb, k, samples = 5L, sampsize = NULL) {
  n <- nrow(emb)
  if (is.null(sampsize)) {
    # A conservative heuristic similar to cluster defaults; ensure >= k
    sampsize <- min(n, max(40L + 2L * k, k + 1L))
  }
  fit <- cluster::clara(emb, k = k, metric = "euclidean", samples = samples, sampsize = sampsize)
  list(medoids = fit$i.med, clustering = fit$clustering)
}
