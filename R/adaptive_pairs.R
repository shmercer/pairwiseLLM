#' Select adaptive pairs for the next round of comparisons
#'
#' This helper proposes a new set of pairs for a round-based adaptive workflow.
#' It uses current BT estimates (\code{theta} and \code{se}) to prioritize pairs
#' that are expected to be most informative (roughly: pairs with predicted win
#' probability near 0.5) while also prioritizing items that have not yet been
#' judged \code{min_judgments} times.
#'
#' Candidate pairs are generated efficiently by sorting items by \code{theta} and
#' considering only \code{k_neighbors} adjacent items for each item. This avoids
#' enumerating all \code{N*(N-1)/2} pairs and scales to large \code{N}.
#'
#' The function can also:
#' \itemize{
#'   \item forbid repeated pairings (unordered) across rounds,
#'   \item balance positions (ID1 vs ID2) to reduce positional bias,
#'   \item accept existing pairs in either \code{ID1/ID2} or \code{object1/object2} format.
#' }
#'
#' @param samples A tibble/data frame with columns \code{ID} and \code{text}.
#' @param theta A tibble/data frame with columns \code{ID}, \code{theta}, and \code{se}
#'   (typically \code{bt_fit$theta} from \code{\link{fit_bt_model}}).
#' @param existing_pairs Optional data frame containing previously judged pairs.
#'   Supported formats:
#'   \itemize{
#'     \item \code{ID1}, \code{ID2} (e.g., pairs table or LLM results)
#'     \item \code{object1}, \code{object2} (e.g., BT data)
#'   }
#'   If \code{NULL} (default), the function assumes no prior pairs.
#' @param n_pairs Integer number of new pairs to return for the next round.
#' @param k_neighbors Integer number of adjacent neighbors (in sorted-theta order).
#'   Use \code{NULL} or \code{Inf} to consider all neighbors.
#'   to consider for each item when generating candidate pairs. Default is 10.
#' @param min_judgments Integer minimum desired number of judgments per item.
#'   Items below this threshold are prioritized. Default is 12.
#' @param forbid_repeats Logical; if \code{TRUE} (default), do not return pairs
#'   that have already appeared in \code{existing_pairs} (unordered).
#' @param balance_positions Logical; if \code{TRUE} (default), orient each selected
#'   pair so that items with more historical appearances in position 1 (ID1) are
#'   more likely to be placed in position 2 (ID2), and vice versa.
#' @param embedding_neighbors Optional embedding-based neighbor lists used to
#'   augment candidate generation. This can be either:
#'   \itemize{
#'     \item a named list mapping each ID to a character vector of neighbor IDs, or
#'     \item a matrix/data.frame with \code{rownames} equal to IDs and neighbor IDs
#'       stored in columns.
#'   }
#'   When provided, these candidates are added on top of the theta-neighborhood
#'   candidates controlled by \code{k_neighbors}.
#' @param embed_far_k Integer; number of additional "far" candidates to sample
#'   per item (uniformly at random) in addition to theta/embedding neighbors.
#' @param seed Optional integer seed for reproducibility. If \code{NULL} (default),
#'   the current RNG state is used and not modified.
#'
#' @return A tibble with columns \code{ID1}, \code{text1}, \code{ID2}, \code{text2}.
#'   Extra columns are not returned, to keep the output directly compatible with
#'   \code{\link{submit_llm_pairs}}.
#'
#' @examples
#' samples <- tibble::tibble(
#'   ID = c("A", "B", "C", "D"),
#'   text = paste("Sample", c("A", "B", "C", "D"))
#' )
#'
#' theta <- tibble::tibble(
#'   ID = c("A", "B", "C", "D"),
#'   theta = c(0.0, 0.1, 2.0, 2.1),
#'   se = c(0.5, 0.5, 0.3, 0.3)
#' )
#'
#' # First round: no existing pairs
#' select_adaptive_pairs(samples, theta, n_pairs = 2, seed = 1)
#'
#' # Later: forbid repeats against existing pairs
#' existing <- tibble::tibble(ID1 = "A", ID2 = "B")
#' select_adaptive_pairs(samples, theta, existing_pairs = existing, n_pairs = 2, seed = 1)
#'
#' @export
select_adaptive_pairs <- function(samples,
                                  theta,
                                  existing_pairs = NULL,
                                  embedding_neighbors = NULL,
                                  n_pairs,
                                  k_neighbors = 10,
                                  min_judgments = 12,
                                  forbid_repeats = TRUE,
                                  balance_positions = TRUE,
                                  embed_far_k = 0,
                                  seed = NULL) {
  samples <- tibble::as_tibble(samples)

  required_s <- c("ID", "text")
  missing_s <- setdiff(required_s, names(samples))
  if (length(missing_s) > 0L) {
    stop(
      "`samples` must contain columns: ",
      paste(required_s, collapse = ", "),
      call. = FALSE
    )
  }

  theta <- tibble::as_tibble(theta)
  required_t <- c("ID", "theta", "se")
  missing_t <- setdiff(required_t, names(theta))
  if (length(missing_t) > 0L) {
    stop(
      "`theta` must contain columns: ",
      paste(required_t, collapse = ", "),
      call. = FALSE
    )
  }

  n_pairs <- as.integer(n_pairs)
  if (is.na(n_pairs) || n_pairs < 0L) {
    stop("`n_pairs` must be a non-negative integer.", call. = FALSE)
  }

  # Allow NULL / Inf as a convenience for "all neighbors".
  if (is.null(k_neighbors) || isTRUE(is.infinite(k_neighbors))) {
    k_neighbors <- Inf
  }
  if (!is.numeric(k_neighbors) || length(k_neighbors) != 1L || is.na(k_neighbors)) {
    stop("`k_neighbors` must be a positive integer, or NULL/Inf for all neighbors.", call. = FALSE)
  }
  # If finite, require >= 1.
  if (is.finite(k_neighbors)) {
    if (k_neighbors < 1) {
      stop("`k_neighbors` must be positive (>= 1), or NULL/Inf for all neighbors.", call. = FALSE)
    }
    # Ensure integer-like.
    if (abs(k_neighbors - round(k_neighbors)) > 1e-12) {
      stop("`k_neighbors` must be an integer (or NULL/Inf for all neighbors).", call. = FALSE)
    }
    k_neighbors <- as.integer(k_neighbors)
  }

  # Allow NULL to disable the minimum-judgments heuristic.
  if (is.null(min_judgments)) min_judgments <- 0L
  min_judgments <- as.integer(min_judgments)
  if (is.na(min_judgments) || min_judgments < 0L) {
    stop("`min_judgments` must be NULL or a non-negative integer.", call. = FALSE)
  }

  # Normalize IDs to character
  samples <- dplyr::mutate(samples, ID = as.character(.data$ID), text = as.character(.data$text))
  theta <- dplyr::mutate(theta, ID = as.character(.data$ID))

  ids <- unique(samples$ID)
  if (length(ids) < 2L) {
    stop("At least two samples are required to select pairs.", call. = FALSE)
  }

  # Early return: n_pairs == 0
  if (n_pairs == 0L) {
    return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
  }

  # Wrap all stochastic steps so we do not permanently change RNG state
  out <- .with_seed_restore(
    seed,
    f = function() {
      # Join theta/se onto all samples; fill missing estimates (cold-start/new items)
      joined <- dplyr::left_join(
        dplyr::select(samples, dplyr::all_of(c("ID", "text"))),
        dplyr::select(theta, dplyr::all_of(c("ID", "theta", "se"))),
        by = "ID"
      )

      se_fill <- suppressWarnings(max(joined$se, na.rm = TRUE))
      if (!is.finite(se_fill)) {
        se_fill <- 1
      } else {
        se_fill <- se_fill * 2
      }

      joined <- dplyr::mutate(
        joined,
        theta = dplyr::if_else(is.na(.data$theta), 0, as.double(.data$theta)),
        se = dplyr::if_else(is.na(.data$se), se_fill, as.double(.data$se))
      )

      # ------------------------------------------------------------------
      # Existing pairs: counts + repeat prevention
      # ------------------------------------------------------------------
      existing_key <- character()
      pos1_counts <- stats::setNames(integer(length(ids)), ids)
      pos2_counts <- stats::setNames(integer(length(ids)), ids)
      total_counts <- stats::setNames(integer(length(ids)), ids)

      ex <- .normalize_existing_pairs(existing_pairs, err_arg = "existing_pairs")

      if (nrow(ex) > 0L) {
        ex1 <- ex$ID1
        ex2 <- ex$ID2

        keep1 <- ex1 %in% ids
        keep2 <- ex2 %in% ids

        # position counts only for known ids
        if (any(keep1)) {
          tab1 <- table(ex1[keep1])
          pos1_counts[names(tab1)] <- pos1_counts[names(tab1)] + as.integer(tab1)
        }
        if (any(keep2)) {
          tab2 <- table(ex2[keep2])
          pos2_counts[names(tab2)] <- pos2_counts[names(tab2)] + as.integer(tab2)
        }

        # total counts (unordered)
        all_ids <- c(ex1[keep1], ex2[keep2])
        if (length(all_ids) > 0L) {
          tab_all <- table(all_ids)
          total_counts[names(tab_all)] <- total_counts[names(tab_all)] + as.integer(tab_all)
        }

        # repeat-prevention key (unordered)
        existing_key <- unique(.unordered_pair_key(ex1, ex2))
      }

      # ------------------------------------------------------------------
      # Candidate generation: neighbors in theta-sorted order, optionally
      # augmented by embedding-based neighbor lists.
      # ------------------------------------------------------------------
      ord <- order(joined$theta, joined$ID, na.last = TRUE)
      joined <- joined[ord, , drop = FALSE]

      id_vec <- joined$ID
      th_vec <- as.double(joined$theta)
      se_vec <- as.double(joined$se)

      # map counts into sorted order
      tot_vec <- as.integer(total_counts[id_vec])
      p1_vec <- as.integer(pos1_counts[id_vec])
      p2_vec <- as.integer(pos2_counts[id_vec])

      n <- length(id_vec)
      k_neighbors2 <- min(k_neighbors, max(n - 1L, 1L))

      # Normalize embedding neighbor input to a named list(ID -> character vector)
      embed_nbrs <- NULL
      if (!is.null(embedding_neighbors)) {
        if (is.list(embedding_neighbors) && !is.null(names(embedding_neighbors))) {
          embed_nbrs <- embedding_neighbors
        } else if (is.matrix(embedding_neighbors) || is.data.frame(embedding_neighbors)) {
          emb_mat <- as.matrix(embedding_neighbors)
          rn <- rownames(emb_mat)
          if (is.null(rn) || length(rn) == 0L) {
            stop("`embedding_neighbors` must be a named list or a matrix with rownames = IDs.", call. = FALSE)
          }
          embed_nbrs <- lapply(seq_len(nrow(emb_mat)), function(ii) as.character(emb_mat[ii, ]))
          names(embed_nbrs) <- rn
        } else {
          stop("`embedding_neighbors` must be NULL, a named list, or a matrix/data.frame.", call. = FALSE)
        }

        missing_ids <- setdiff(ids, names(embed_nbrs))
        if (length(missing_ids) > 0L) {
          stop("`embedding_neighbors` is missing IDs: ", paste(utils::head(missing_ids, 10), collapse = ", "), call. = FALSE)
        }
      }

      if (!is.numeric(embed_far_k) || length(embed_far_k) != 1L || is.na(embed_far_k) || embed_far_k < 0) {
        stop("`embed_far_k` must be a single non-negative integer.", call. = FALSE)
      }
      embed_far_k <- as.integer(embed_far_k)

      cand_i <- integer()
      cand_j <- integer()
      cand_score <- double()

      for (i in seq_len(n)) {
        # 1) theta-neighborhood candidates
        js <- integer()
        hi <- min(n, i + k_neighbors2)
        if (hi > i) {
          js <- c(js, (i + 1L):hi)
        }

        # 2) embedding-neighborhood candidates (mapped to current theta-sorted indices)
        if (!is.null(embed_nbrs)) {
          this_id <- id_vec[[i]]
          nbr_ids <- embed_nbrs[[this_id]]
          if (!is.null(nbr_ids) && length(nbr_ids) > 0L) {
            nbr_ids <- nbr_ids[!is.na(nbr_ids) & nzchar(nbr_ids)]
            if (length(nbr_ids) > 0L) {
              nbr_idx <- match(nbr_ids, id_vec)
              nbr_idx <- nbr_idx[!is.na(nbr_idx) & nbr_idx != i]
              if (length(nbr_idx) > 0L) {
                js <- c(js, nbr_idx)
              }
            }
          }
        }

        # 3) optional far candidates to preserve global mixing
        if (embed_far_k > 0L) {
          # Sample from indices strictly after i so we score each unordered pair once.
          base_pool <- if (i < n) (i + 1L):n else integer(0)
          pool <- setdiff(base_pool, js)
          if (length(pool) > 0L) {
            js <- c(js, sample(pool, size = min(embed_far_k, length(pool)), replace = FALSE))
          }
        }

        js <- unique(js)
        js <- js[js != i]
        if (length(js) == 0L) next

        for (j in js) {
          # Only score each unordered pair once; keep upper triangle by index.
          if (j <= i) next
          if (isTRUE(forbid_repeats) && length(existing_key) > 0L) {
            kkey <- .unordered_pair_key(id_vec[[i]], id_vec[[j]])
            if (kkey %in% existing_key) next
          }

          # predicted probability / information (max at 0.5)
          d <- th_vec[[i]] - th_vec[[j]]
          p <- 1 / (1 + exp(-d))
          info <- p * (1 - p)

          need_i <- max(0, min_judgments - tot_vec[[i]])
          need_j <- max(0, min_judgments - tot_vec[[j]])

          score <- info * (se_vec[[i]] + se_vec[[j]]) * (1 + need_i + need_j)

          cand_i <- c(cand_i, i)
          cand_j <- c(cand_j, j)
          cand_score <- c(cand_score, score)
        }
      }

      if (length(cand_score) == 0L) {
        return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
      }

      o <- order(cand_score, decreasing = TRUE)
      cand_i <- cand_i[o]
      cand_j <- cand_j[o]

      take <- min(n_pairs, length(cand_i))
      cand_i <- cand_i[seq_len(take)]
      cand_j <- cand_j[seq_len(take)]

      # ------------------------------------------------------------------
      # Orient pairs (position balancing) + construct output
      # ------------------------------------------------------------------
      out_ID1 <- character(take)
      out_ID2 <- character(take)

      # mutable counts while building this round
      p1 <- p1_vec
      p2 <- p2_vec

      for (k in seq_len(take)) {
        i <- cand_i[[k]]
        j <- cand_j[[k]]

        a <- id_vec[[i]]
        b <- id_vec[[j]]

        if (isTRUE(balance_positions)) {
          imb_a <- p1[[i]] - p2[[i]]
          imb_b <- p1[[j]] - p2[[j]]

          if (imb_a > imb_b) {
            # a has "too many" as ID1; place it as ID2
            out_ID1[[k]] <- b
            out_ID2[[k]] <- a
            p1[[j]] <- p1[[j]] + 1L
            p2[[i]] <- p2[[i]] + 1L
          } else if (imb_a < imb_b) {
            out_ID1[[k]] <- a
            out_ID2[[k]] <- b
            p1[[i]] <- p1[[i]] + 1L
            p2[[j]] <- p2[[j]] + 1L
          } else {
            # tie: randomize
            if (stats::runif(1) < 0.5) {
              out_ID1[[k]] <- a
              out_ID2[[k]] <- b
              p1[[i]] <- p1[[i]] + 1L
              p2[[j]] <- p2[[j]] + 1L
            } else {
              out_ID1[[k]] <- b
              out_ID2[[k]] <- a
              p1[[j]] <- p1[[j]] + 1L
              p2[[i]] <- p2[[i]] + 1L
            }
          }
        } else {
          out_ID1[[k]] <- a
          out_ID2[[k]] <- b
        }
      }

      .add_pair_texts(
        tibble::tibble(ID1 = out_ID1, ID2 = out_ID2),
        samples = samples
      )
    },
    arg_name = "seed"
  )

  out
}

# Compute a simple cosine-similarity neighbor list from an embeddings matrix.
#
# This is intentionally lightweight (no ANN dependency) and intended for
# moderate n (e.g., up to a few thousand) where a single dense similarity
# computation is acceptable.
#
# @keywords internal
.compute_embedding_neighbors <- function(embeddings, ids, k = 30L, normalize = TRUE) {
  if (is.null(embeddings)) stop("`embeddings` must not be NULL.", call. = FALSE)
  ids <- as.character(ids)
  if (!is.character(ids) || anyNA(ids) || any(ids == "") || length(ids) < 2L) {
    stop("`ids` must be a non-missing character vector with length >= 2.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 0) {
    stop("`k` must be a single non-negative integer.", call. = FALSE)
  }
  k <- as.integer(k)
  if (k == 0L) {
    out <- rep(list(character()), length(ids))
    names(out) <- ids
    return(out)
  }

  emb <- validate_embeddings(embeddings, ids = ids, arg_name = "embeddings")
  emb <- as.matrix(emb)
  storage.mode(emb) <- "double"

  if (isTRUE(normalize)) {
    norms <- sqrt(rowSums(emb^2))
    norms[norms == 0] <- 1
    emb <- emb / norms
  }

  # Cosine similarity matrix.
  sim <- emb %*% t(emb)
  diag(sim) <- -Inf

  kk <- min(k, length(ids) - 1L)
  nbrs <- vector("list", length(ids))
  names(nbrs) <- ids
  for (i in seq_along(ids)) {
    o <- order(sim[i, ], decreasing = TRUE)
    nbrs[[i]] <- ids[o[seq_len(kk)]]
  }
  nbrs
}
