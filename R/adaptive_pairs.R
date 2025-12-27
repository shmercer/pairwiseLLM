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
#' @param k_neighbors Integer number of adjacent neighbors (in sorted-theta order)
#'   to consider for each item when generating candidate pairs. Default is 10.
#' @param min_judgments Integer minimum desired number of judgments per item.
#'   Items below this threshold are prioritized. Default is 12.
#' @param forbid_repeats Logical; if \code{TRUE} (default), do not return pairs
#'   that have already appeared in \code{existing_pairs} (unordered).
#' @param balance_positions Logical; if \code{TRUE} (default), orient each selected
#'   pair so that items with more historical appearances in position 1 (ID1) are
#'   more likely to be placed in position 2 (ID2), and vice versa.
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
                                  n_pairs,
                                  k_neighbors = 10,
                                  min_judgments = 12,
                                  forbid_repeats = TRUE,
                                  balance_positions = TRUE,
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

  k_neighbors <- as.integer(k_neighbors)
  if (is.na(k_neighbors) || k_neighbors < 1L) {
    stop("`k_neighbors` must be a positive integer.", call. = FALSE)
  }

  min_judgments <- as.integer(min_judgments)
  if (is.na(min_judgments) || min_judgments < 0L) {
    stop("`min_judgments` must be a non-negative integer.", call. = FALSE)
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
      # Candidate generation: k-nearest neighbors in theta-sorted order
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

      cand_i <- integer()
      cand_j <- integer()
      cand_score <- double()

      for (i in seq_len(n)) {
        hi <- min(n, i + k_neighbors2)
        if (hi <= i) next
        js <- (i + 1L):hi

        for (j in js) {
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
