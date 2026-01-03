# -------------------------------------------------------------------------
# Adaptive pairing pipeline internals
# -------------------------------------------------------------------------

#' @keywords internal
.ap_gen_candidates <- function(id_vec,
                               th_vec,
                               se_vec,
                               tot_vec,
                               k_neighbors2,
                               embed_nbrs = NULL,
                               embed_far_k = 0L) {
  n <- length(id_vec)
  if (n == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      source = character()
    ))
  }

  embed_far_k <- as.integer(embed_far_k)
  if (is.na(embed_far_k) || embed_far_k < 0L) {
    stop("`embed_far_k` must be a single non-negative integer.", call. = FALSE)
  }

  out_i <- integer()
  out_j <- integer()
  out_source <- character()
  out_anchor <- integer()
  out_embed_rank <- integer()

  for (i in seq_len(n)) {
    # 1) theta-neighborhood candidates
    js_theta <- integer()
    lo <- max(1L, i - k_neighbors2)
    hi <- min(n, i + k_neighbors2)
    if (lo <= hi) {
      js_theta <- seq.int(lo, hi)
      js_theta <- js_theta[js_theta != i]
    }

    # 2) embedding neighbor candidates
    js_embed <- integer()
    if (!is.null(embed_nbrs)) {
      nbr_ids <- embed_nbrs[[id_vec[[i]]]]
      if (!is.null(nbr_ids) && length(nbr_ids) > 0L) {
        nbr_ids <- nbr_ids[!is.na(nbr_ids) & nzchar(nbr_ids)]
        if (length(nbr_ids) > 0L) {
          nbr_idx <- match(nbr_ids, id_vec)
          nbr_idx <- nbr_idx[!is.na(nbr_idx) & nbr_idx != i]
          if (length(nbr_idx) > 0L) {
            js_embed <- nbr_idx
          }
        }
      }
    }

    # 3) optional far candidates (uses current RNG state; caller controls seed)
    js_far <- integer()
    if (embed_far_k > 0L) {
      base_pool <- if (i < n) (i + 1L):n else integer(0)
      pool <- setdiff(base_pool, unique(c(js_theta, js_embed)))
      if (length(pool) > 0L) {
        js_far <- sample(pool, size = min(embed_far_k, length(pool)), replace = FALSE)
      }
    }

    # Preserve prior relative priority order: theta -> embed -> far.
    js_all <- c(js_theta, js_embed, js_far)
    if (length(js_all) == 0L) next

    # dedupe by index, keep first occurrence
    js_unique <- integer()
    src_unique <- character()
    embed_rank_unique <- integer()
    seen <- logical(n)

    for (jj in seq_along(js_all)) {
      j <- js_all[[jj]]
      if (is.na(j) || j < 1L || j > n) next
      if (j == i) next
      if (seen[[j]]) next
      seen[[j]] <- TRUE

      js_unique <- c(js_unique, j)
      if (jj <= length(js_theta)) {
        src_unique <- c(src_unique, "theta")
        embed_rank_unique <- c(embed_rank_unique, NA_integer_)
      } else if (jj <= (length(js_theta) + length(js_embed))) {
        src_unique <- c(src_unique, "embed")
        embed_rank_unique <- c(embed_rank_unique, match(j, js_embed))
      } else {
        src_unique <- c(src_unique, "far")
        embed_rank_unique <- c(embed_rank_unique, NA_integer_)
      }
    }

    if (length(js_unique) == 0L) next

    # Score each unordered pair once: keep upper triangle by index.
    keep <- js_unique > i
    if (!any(keep)) next

    out_i <- c(out_i, rep.int(i, sum(keep)))
    out_j <- c(out_j, js_unique[keep])
    out_source <- c(out_source, src_unique[keep])
    out_anchor <- c(out_anchor, rep.int(i, sum(keep)))
    out_embed_rank <- c(out_embed_rank, embed_rank_unique[keep])
  }

  key <- .unordered_pair_key(id_vec[out_i], id_vec[out_j])
  embed_hit <- out_source == "embed"

  tibble::tibble(
    i_idx = as.integer(out_i),
    j_idx = as.integer(out_j),
    anchor_idx = as.integer(out_anchor),
    pair_key = as.character(key),
    source = as.character(out_source),
    embed_hit = as.logical(embed_hit),
    embed_rank = as.integer(out_embed_rank),
    directed = FALSE
  )
}

#' @keywords internal
.ap_gen_repeat_reverse <- function(existing_completed,
                                   id_vec,
                                   repeat_cap = 1L) {
  existing_completed <- tibble::as_tibble(existing_completed)

  if (nrow(existing_completed) == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical(),
      score_info = numeric(),
      score_need = numeric(),
      score_embed = numeric(),
      score_total = numeric()
    ))
  }

  if (!all(c("ID1", "ID2") %in% names(existing_completed))) {
    stop("`existing_completed` must contain columns ID1 and ID2.", call. = FALSE)
  }

  repeat_cap <- as.integer(repeat_cap)
  if (is.na(repeat_cap) || repeat_cap < 0L) {
    stop("`repeat_cap` must be a non-negative integer.", call. = FALSE)
  }

  # For this PR, we only schedule one reverse repeat per unordered pair,
  # so eligibility is pairs with exactly one completed judgment.
  ex <- dplyr::mutate(
    existing_completed,
    ID1 = as.character(.data$ID1),
    ID2 = as.character(.data$ID2),
    pair_key = .unordered_pair_key(.data$ID1, .data$ID2),
    direction = .pair_direction(.data$ID1, .data$ID2)
  )

  key_n <- dplyr::count(ex, .data$pair_key, name = "n_completed")
  eligible_keys <- dplyr::filter(key_n, .data$n_completed == 1L)

  if (nrow(eligible_keys) == 0L || repeat_cap == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical(),
      score_info = numeric(),
      score_need = numeric(),
      score_embed = numeric(),
      score_total = numeric()
    ))
  }

  ex1 <- dplyr::semi_join(ex, eligible_keys, by = "pair_key")
  ex1 <- dplyr::group_by(ex1, .data$pair_key)
  ex1 <- dplyr::slice_head(ex1, n = 1L)
  ex1 <- dplyr::ungroup(ex1)

  # Reverse orientation of the single completed judgment by swapping positions.
  id1_rev <- as.character(ex1$ID2)
  id2_rev <- as.character(ex1$ID1)

  i_idx <- match(id1_rev, id_vec)
  j_idx <- match(id2_rev, id_vec)

  ok <- !is.na(i_idx) & !is.na(j_idx) & i_idx != j_idx

  if (!any(ok)) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical(),
      score_info = numeric(),
      score_need = numeric(),
      score_embed = numeric(),
      score_total = numeric()
    ))
  }

  tibble::tibble(
    i_idx = as.integer(i_idx[ok]),
    j_idx = as.integer(j_idx[ok]),
    anchor_idx = as.integer(i_idx[ok]),
    pair_key = as.character(ex1$pair_key[ok]),
    source = "repeat_reverse",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = TRUE,
    score_info = NA_real_,
    score_need = NA_real_,
    score_embed = 0,
    score_total = Inf
  )
}

#' @keywords internal
.ap_score_candidates <- function(cand_tbl,
                                 th_vec,
                                 se_vec,
                                 tot_vec,
                                 min_judgments,
                                 w_embed = 1,
                                 embed_score_mode = "rank_decay") {
  cand_tbl <- tibble::as_tibble(cand_tbl)

  w_embed <- as.double(w_embed)
  if (!is.finite(w_embed) || length(w_embed) != 1L) {
    stop("`w_embed` must be a single finite numeric value.", call. = FALSE)
  }

  embed_score_mode <- match.arg(embed_score_mode, choices = c("rank_decay", "binary_neighbor"))

  if (nrow(cand_tbl) == 0L) {
    cand_tbl$score_info <- numeric(0)
    cand_tbl$score_need <- numeric(0)
    cand_tbl$score_embed <- numeric(0)
    cand_tbl$score_total <- numeric(0)
    return(cand_tbl)
  }

  i <- cand_tbl$i_idx
  j <- cand_tbl$j_idx

  d <- th_vec[i] - th_vec[j]
  p <- 1 / (1 + exp(-d))
  info <- p * (1 - p)

  need_i <- pmax(0, min_judgments - tot_vec[i])
  need_j <- pmax(0, min_judgments - tot_vec[j])

  score_total <- info * (se_vec[i] + se_vec[j]) * (1 + need_i + need_j)

  score_embed <- numeric(length(score_total))
  if (w_embed != 0) {
    if (embed_score_mode == "binary_neighbor") {
      score_embed <- ifelse(cand_tbl$source == "embed", 1, 0)
    } else {
      # rank_decay
      embed_rank <- cand_tbl$embed_rank
      if (is.null(embed_rank)) {
        embed_rank <- rep(NA_integer_, nrow(cand_tbl))
      }
      score_embed <- ifelse(
        cand_tbl$source == "embed" & !is.na(embed_rank),
        1 / (1 + as.double(embed_rank)),
        0
      )
    }
  }

  score_total <- score_total + w_embed * score_embed

  dplyr::mutate(
    cand_tbl,
    score_info = as.double(info),
    score_need = as.double(need_i + need_j),
    score_embed = as.double(score_embed),
    score_total = as.double(score_total)
  )
}

#' @keywords internal
.ap_apply_constraints <- function(cand_tbl,
                                  id_vec,
                                  existing_counts = integer(),
                                  existing_dir = character(),
                                  existing_counts_all = integer(),
                                  forbid_unordered = TRUE,
                                  repeat_policy = "none",
                                  repeat_cap = 1L,
                                  # legacy/internal (pre-PR6) args kept for backwards compatibility
                                  existing_key = NULL,
                                  forbid_repeats = NULL) {
  cand_tbl <- tibble::as_tibble(cand_tbl)

  # If legacy `forbid_repeats` is provided, it also controls whether we forbid
  # selecting unordered repeats from the non-repeat candidate pool.
  if (!is.null(forbid_repeats)) {
    forbid_unordered <- isTRUE(forbid_repeats)
  }

  # Legacy/internal args (pre-PR6): map forbid_repeats -> repeat_policy, and existing_key -> existing_counts.
  if (!is.null(forbid_repeats)) {
    if (!is.logical(forbid_repeats) || length(forbid_repeats) != 1L || is.na(forbid_repeats)) {
      stop("`forbid_repeats` must be TRUE/FALSE (or NULL).", call. = FALSE)
    }
    repeat_policy <- if (isTRUE(forbid_repeats)) "none" else "reverse_only"
  }

  if (!is.null(existing_key) && length(existing_counts) == 0L) {
    existing_key <- unique(as.character(existing_key))
    existing_key <- existing_key[!is.na(existing_key) & nzchar(existing_key)]
    if (length(existing_key) > 0L) {
      existing_counts <- stats::setNames(rep.int(1L, length(existing_key)), existing_key)
      if (length(existing_dir) == 0L) {
        existing_dir <- stats::setNames(rep(NA_character_, length(existing_key)), existing_key)
      }
    }
  }

  # Accept a convenience alias used by other entrypoints/tests.
  # `forbid_unordered` means: do not plan repeats *and* forbid selecting
  # unordered repeats from the non-repeat candidate pool.
  repeat_policy <- match.arg(repeat_policy, c("none", "reverse_only", "forbid_unordered"))
  if (identical(repeat_policy, "forbid_unordered")) {
    forbid_unordered <- TRUE
    repeat_policy <- "none"
  }
  repeat_cap <- as.integer(repeat_cap)
  if (is.na(repeat_cap) || repeat_cap < 0L) {
    stop("`repeat_cap` must be a non-negative integer.", call. = FALSE)
  }

  if (nrow(cand_tbl) == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical(),
      score_info = numeric(),
      score_need = numeric(),
      score_embed = numeric(),
      score_total = numeric()
    ))
  }

  n <- length(id_vec)

  # valid indices + no self-pairs
  cand_tbl <- dplyr::filter(
    cand_tbl,
    !is.na(.data$i_idx),
    !is.na(.data$j_idx),
    .data$i_idx >= 1L,
    .data$j_idx >= 1L,
    .data$i_idx <= n,
    .data$j_idx <= n,
    .data$i_idx != .data$j_idx
  )

  if (nrow(cand_tbl) == 0L) {
    return(cand_tbl)
  }

  if (!"pair_key" %in% names(cand_tbl)) {
    cand_tbl$pair_key <- .unordered_pair_key(id_vec[cand_tbl$i_idx], id_vec[cand_tbl$j_idx])
  }


  # Apply repeat policy.
  # - `existing_counts_all` encodes whether an unordered pair has *ever* appeared
  #   in `existing_pairs` (presence-only).
  # - `existing_counts` encodes the number of *completed* judgments (rows with a
  #   non-missing winner), used to implement `repeat_policy = 'reverse_only'`.

  # Presence-only counts: fall back to completed counts if the caller did not
  # provide `existing_counts_all`.
  existing_any <- existing_counts_all
  if (length(existing_any) == 0L && length(existing_counts) > 0L) {
    existing_any <- existing_counts
  }

  if (repeat_policy == "none") {
    # Forbid selecting any unordered pair that has ever appeared.
    if (length(existing_any) > 0L) {
      n_any <- existing_any[cand_tbl$pair_key]
      n_any[is.na(n_any)] <- 0L
      cand_tbl <- cand_tbl[n_any == 0L, , drop = FALSE]
    }
  } else {
    # reverse_only

    # By default, we forbid selecting unordered repeats from the non-repeat
    # candidate pool. (Repeats are only planned via the explicit repeat bucket.)
    if (isTRUE(forbid_unordered) && length(existing_any) > 0L) {
      n_any <- existing_any[cand_tbl$pair_key]
      n_any[is.na(n_any)] <- 0L
      is_repeat_source_any <- cand_tbl$source == "repeat_reverse"
      cand_tbl <- cand_tbl[(n_any == 0L) | is_repeat_source_any, , drop = FALSE]
    }

    if (nrow(cand_tbl) > 0L) {
      max_done <- as.integer(repeat_cap + 1L)
      n_done <- if (length(existing_counts) > 0L) existing_counts[cand_tbl$pair_key] else integer(nrow(cand_tbl))
      n_done[is.na(n_done)] <- 0L

      # For pairs with exactly one completed judgment, we allow a single reverse repeat
      # from the explicit repeat bucket (source == 'repeat_reverse').
      cand_dir <- .pair_direction(id_vec[cand_tbl$i_idx], id_vec[cand_tbl$j_idx])
      base_dir <- existing_dir[cand_tbl$pair_key]
      want_dir <- ifelse(base_dir == "forward", "reverse",
        ifelse(base_dir == "reverse", "forward", NA_character_)
      )

      is_repeat_source <- cand_tbl$source == "repeat_reverse"

      keep_new <- n_done == 0L
      keep_repeat <- (n_done == 1L) & is_repeat_source & !is.na(want_dir) & (cand_dir == want_dir)

      keep <- (keep_new | keep_repeat) & (n_done < max_done)
      cand_tbl <- cand_tbl[keep, , drop = FALSE]
    }
  }

  if (nrow(cand_tbl) == 0L) {
    return(tibble::as_tibble(cand_tbl))
  }

  # ensure uniqueness of unordered pairs
  cand_tbl <- cand_tbl[!duplicated(cand_tbl$pair_key), , drop = FALSE]

  tibble::as_tibble(cand_tbl)
}

#' @keywords internal
.ap_select_pairs_from_scored <- function(scored_tbl,
                                         n_pairs,
                                         embed_quota_frac = 0.25,
                                         embed_sources = "embed",
                                         repeat_quota_n = 0L,
                                         repeat_sources = "repeat_reverse") {
  n_pairs <- as.integer(n_pairs)
  scored_tbl <- tibble::as_tibble(scored_tbl)

  if (nrow(scored_tbl) == 0L || is.na(n_pairs) || n_pairs <= 0L) {
    return(scored_tbl[0, , drop = FALSE])
  }

  embed_quota_frac <- as.double(embed_quota_frac)
  if (!is.finite(embed_quota_frac) || embed_quota_frac < 0 || embed_quota_frac > 1) {
    stop("`embed_quota_frac` must be a single number in [0, 1].", call. = FALSE)
  }

  repeat_quota_n <- as.integer(repeat_quota_n)
  if (is.na(repeat_quota_n) || repeat_quota_n < 0L) {
    stop("`repeat_quota_n` must be a non-negative integer.", call. = FALSE)
  }

  if (!"pair_key" %in% names(scored_tbl)) {
    stop("`scored_tbl` must contain a `pair_key` column.", call. = FALSE)
  }

  # Base ordering for non-repeat candidates.
  scored_tbl <- dplyr::arrange(scored_tbl, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)

  take_keys <- character()
  scored_tbl_nonrep <- dplyr::filter(scored_tbl, !(.data$source %in% repeat_sources))

  # --- 1) Repeat bucket (deterministic) ---
  if (repeat_quota_n > 0L) {
    rep_tbl <- dplyr::filter(scored_tbl, .data$source %in% repeat_sources)
    if (nrow(rep_tbl) > 0L) {
      rep_tbl <- dplyr::arrange(rep_tbl, .data$pair_key, .data$i_idx, .data$j_idx)
      take_rep <- min(repeat_quota_n, nrow(rep_tbl))
      take_keys <- c(take_keys, rep_tbl$pair_key[seq_len(take_rep)])
    }
  }

  # --- 2) Embedding quota (minimum share) ---
  remaining_after_repeat <- n_pairs - length(take_keys)
  if (remaining_after_repeat <= 0L) {
    idx <- match(take_keys, scored_tbl$pair_key)
    idx <- idx[!is.na(idx)]
    return(scored_tbl[idx, , drop = FALSE])
  }

  n_embed_min <- ceiling(embed_quota_frac * n_pairs)
  n_embed_min <- as.integer(min(n_embed_min, n_pairs))

  take_keys2 <- take_keys
  if (n_embed_min > 0L) {
    embed_tbl <- dplyr::filter(scored_tbl_nonrep, .data$source %in% embed_sources)
    embed_tbl <- embed_tbl[!(embed_tbl$pair_key %in% take_keys2), , drop = FALSE]
    if (nrow(embed_tbl) > 0L) {
      take_embed <- min(n_embed_min, nrow(embed_tbl))
      take_keys2 <- c(take_keys2, embed_tbl$pair_key[seq_len(take_embed)])
    }
  }

  # --- 3) Fill remainder by score ---
  remaining <- n_pairs - length(take_keys2)
  if (remaining > 0L) {
    rest_tbl <- scored_tbl_nonrep[!(scored_tbl_nonrep$pair_key %in% take_keys2), , drop = FALSE]
    take_rest <- min(remaining, nrow(rest_tbl))
    if (take_rest > 0L) {
      take_keys2 <- c(take_keys2, rest_tbl$pair_key[seq_len(take_rest)])
    }
  }

  idx <- match(take_keys2, scored_tbl$pair_key)
  idx <- idx[!is.na(idx)]
  scored_tbl[idx, , drop = FALSE]
}

#' @keywords internal
.ap_orient_pairs <- function(selected_tbl,
                             id_vec,
                             samples,
                             p1_vec,
                             p2_vec,
                             balance_positions = TRUE) {
  selected_tbl <- tibble::as_tibble(selected_tbl)
  take <- nrow(selected_tbl)
  if (take == 0L) {
    return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
  }

  out_ID1 <- character(take)
  out_ID2 <- character(take)

  # mutable counts while building this round
  p1 <- p1_vec
  p2 <- p2_vec

  for (k in seq_len(take)) {
    i <- selected_tbl$i_idx[[k]]
    j <- selected_tbl$j_idx[[k]]

    a <- id_vec[[i]]
    b <- id_vec[[j]]

    is_directed <- FALSE
    if ("directed" %in% names(selected_tbl)) {
      is_directed <- isTRUE(selected_tbl$directed[[k]])
    }

    if (isTRUE(is_directed)) {
      # Honor requested orientation (used for reverse-only repeats).
      out_ID1[[k]] <- a
      out_ID2[[k]] <- b
      p1[[i]] <- p1[[i]] + 1L
      p2[[j]] <- p2[[j]] + 1L
      next
    }

    if (isTRUE(balance_positions)) {
      imb_a <- p1[[i]] - p2[[i]]
      imb_b <- p1[[j]] - p2[[j]]

      if (imb_a > imb_b) {
        out_ID1[[k]] <- b
        out_ID2[[k]] <- a
        p1[[j]] <- p1[[j]] + 1L
        p2[[i]] <- p2[[i]] + 1L
      } else if (imb_b > imb_a) {
        out_ID1[[k]] <- a
        out_ID2[[k]] <- b
        p1[[i]] <- p1[[i]] + 1L
        p2[[j]] <- p2[[j]] + 1L
      } else {
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

  out_tbl <- tibble::tibble(ID1 = out_ID1, ID2 = out_ID2)
  .add_pair_texts(out_tbl, samples = samples)
}

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
#' @param embedding_neighbors Optional embedding-based neighbor lists used to
#'   augment candidate generation. This can be either:
#'   \itemize{
#'     \item a named list mapping each ID to a character vector of neighbor IDs, or
#'     \item a matrix/data.frame with \code{rownames} equal to IDs and neighbor IDs
#'       stored in columns.
#'   }
#'   When provided, these candidates are added on top of the theta-neighborhood
#'   candidates controlled by \code{k_neighbors}.
#' @param n_pairs Integer number of new pairs to return for the next round.
#' @param k_neighbors Integer number of adjacent neighbors (in sorted-theta order).
#'   Use \code{NULL} or \code{Inf} to consider all neighbors. Default is 10.
#' @param min_judgments Integer minimum desired number of judgments per item.
#'   Items below this threshold are prioritized. Default is 12.
#' @param forbid_repeats Logical; if \code{TRUE} (default), do not return pairs
#'   that have already appeared in \code{existing_pairs} (unordered).
#' @param balance_positions Logical; if \code{TRUE} (default), orient each selected
#'   pair so that items with more historical appearances in position 1 (ID1) are
#'   more likely to be placed in position 2 (ID2), and vice versa.
#' @param embed_far_k Integer; number of additional "far" candidates to sample
#'   per item (uniformly at random) in addition to theta/embedding neighbors.
#' @param embed_quota_frac Numeric in \code{[0, 1]}. Minimum fraction of selected pairs
#'   that should come from embedding-neighbor candidates when
#'   \code{embedding_neighbors} is provided. Set to 0 to disable the quota.
#'   Default is 0.25.
#' @param candidate_pool_cap Optional cap on the candidate pool size after
#'   scoring/constraints. Use \code{Inf} (default) for no cap.
#' @param per_anchor_cap Optional cap on the number of candidates retained per
#'   anchor item (in theta-sorted order) after scoring/constraints. Use
#'   \code{Inf} (default) for no cap.
#' @param w_embed Numeric weight for an embedding-neighbor bonus term in the
#'   scoring function. Default is 1 (embeddings influence both candidate
#'   generation and scoring when available).
#' @param embed_score_mode Character; how to compute the embedding bonus when
#'   \code{w_embed != 0}. Options are \code{"binary_neighbor"} and
#'   \code{"rank_decay"}.
#' @param return_internal Logical; if \code{TRUE}, return a list with
#'   \code{$pairs}, \code{$diagnostics}, and intermediate candidate tables.
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
                                  repeat_policy = "reverse_only",
                                  repeat_cap = 1L,
                                  repeat_frac = 0.05,
                                  repeat_n = NULL,
                                  repeat_guard_min_degree = 1L,
                                  repeat_guard_largest_component_frac = 0.90,
                                  forbid_repeats = NULL,
                                  balance_positions = TRUE,
                                  embed_far_k = 0,
                                  embed_quota_frac = 0.25,
                                  candidate_pool_cap = Inf,
                                  per_anchor_cap = Inf,
                                  w_embed = 1,
                                  embed_score_mode = "rank_decay",
                                  return_internal = FALSE,
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
  if (is.finite(k_neighbors)) {
    if (k_neighbors < 1) {
      stop("`k_neighbors` must be positive (>= 1), or NULL/Inf for all neighbors.", call. = FALSE)
    }
    if (abs(k_neighbors - round(k_neighbors)) > 1e-12) {
      stop("`k_neighbors` must be an integer (or NULL/Inf for all neighbors).", call. = FALSE)
    }
    k_neighbors <- as.integer(k_neighbors)
  }

  if (is.null(min_judgments)) min_judgments <- 0L
  min_judgments <- as.integer(min_judgments)
  if (is.na(min_judgments) || min_judgments < 0L) {
    stop("`min_judgments` must be NULL or a non-negative integer.", call. = FALSE)
  }

  # --- repeat policy (PR6) ---
  # `forbid_repeats` is a soft-deprecated alias: TRUE => repeat_policy='none'
  # FALSE => repeat_policy='reverse_only'.
  if (!is.null(forbid_repeats)) {
    if (!is.logical(forbid_repeats) || length(forbid_repeats) != 1L || is.na(forbid_repeats)) {
      stop("`forbid_repeats` must be TRUE/FALSE (or NULL).", call. = FALSE)
    }
    if (isTRUE(forbid_repeats)) {
      repeat_policy <- "none"
    } else {
      repeat_policy <- "reverse_only"
    }
  }

  # Accept a convenience alias used by other entrypoints/tests.
  # "forbid_unordered" means: do not plan repeats *and* forbid selecting
  # unordered repeats from the candidate pool.
  repeat_policy <- match.arg(repeat_policy, c("none", "reverse_only", "forbid_unordered"))

  if (repeat_policy == "forbid_unordered") {
    # Reuse legacy/internal guard for forbidding unordered repeats.
    # We intentionally avoid warning here because callers explicitly requested
    # the forbidding behavior via repeat_policy.
    forbid_repeats <- TRUE
    repeat_policy <- "none"
  }

  repeat_cap <- as.integer(repeat_cap)
  if (is.na(repeat_cap) || repeat_cap < 0L) {
    stop("`repeat_cap` must be a non-negative integer.", call. = FALSE)
  }

  repeat_frac <- as.double(repeat_frac)
  if (!is.finite(repeat_frac) || length(repeat_frac) != 1L || repeat_frac < 0 || repeat_frac > 1) {
    stop("`repeat_frac` must be a single number in [0, 1].", call. = FALSE)
  }

  if (!is.null(repeat_n)) {
    if (!is.numeric(repeat_n) || length(repeat_n) != 1L || is.na(repeat_n) || repeat_n < 0) {
      stop("`repeat_n` must be NULL or a single non-negative integer.", call. = FALSE)
    }
    repeat_n <- as.integer(repeat_n)
  }

  repeat_guard_min_degree <- as.integer(repeat_guard_min_degree)
  if (is.na(repeat_guard_min_degree) || repeat_guard_min_degree < 0L) {
    stop("`repeat_guard_min_degree` must be a non-negative integer.", call. = FALSE)
  }

  repeat_guard_largest_component_frac <- as.double(repeat_guard_largest_component_frac)
  if (!is.finite(repeat_guard_largest_component_frac) || length(repeat_guard_largest_component_frac) != 1L ||
    repeat_guard_largest_component_frac < 0 || repeat_guard_largest_component_frac > 1) {
    stop("`repeat_guard_largest_component_frac` must be a single number in [0, 1].", call. = FALSE)
  }

  embed_quota_frac <- as.double(embed_quota_frac)
  if (!is.finite(embed_quota_frac) || length(embed_quota_frac) != 1L || embed_quota_frac < 0 || embed_quota_frac > 1) {
    stop("`embed_quota_frac` must be a single number in [0, 1].", call. = FALSE)
  }

  .validate_cap <- function(x, arg_name) {
    if (isTRUE(is.infinite(x))) {
      return(Inf)
    }
    x <- as.double(x)
    if (!is.finite(x) || length(x) != 1L || x < 0) {
      stop("`", arg_name, "` must be a single non-negative integer (or Inf).", call. = FALSE)
    }
    if (abs(x - round(x)) > 1e-12) {
      stop("`", arg_name, "` must be an integer (or Inf).", call. = FALSE)
    }
    as.integer(x)
  }

  candidate_pool_cap <- .validate_cap(candidate_pool_cap, "candidate_pool_cap")
  per_anchor_cap <- .validate_cap(per_anchor_cap, "per_anchor_cap")

  embed_score_mode <- match.arg(embed_score_mode, choices = c("rank_decay", "binary_neighbor"))

  if (!is.logical(return_internal) || length(return_internal) != 1L || is.na(return_internal)) {
    stop("`return_internal` must be TRUE or FALSE.", call. = FALSE)
  }

  samples <- dplyr::mutate(samples, ID = as.character(.data$ID), text = as.character(.data$text))
  theta <- dplyr::mutate(theta, ID = as.character(.data$ID))

  ids <- unique(samples$ID)
  if (length(ids) < 2L) {
    stop("At least two samples are required to select pairs.", call. = FALSE)
  }

  if (n_pairs == 0L) {
    return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
  }

  res <- .with_seed_restore(
    seed,
    f = function() {
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

        if (any(keep1)) {
          tab1 <- table(ex1[keep1])
          pos1_counts[names(tab1)] <- pos1_counts[names(tab1)] + as.integer(tab1)
        }
        if (any(keep2)) {
          tab2 <- table(ex2[keep2])
          pos2_counts[names(tab2)] <- pos2_counts[names(tab2)] + as.integer(tab2)
        }

        all_ids <- c(ex1[keep1], ex2[keep2])
        if (length(all_ids) > 0L) {
          tab_all <- table(all_ids)
          total_counts[names(tab_all)] <- total_counts[names(tab_all)] + as.integer(tab_all)
        }

        existing_key <- unique(.unordered_pair_key(ex1, ex2))
      }

      # Existing-pair bookkeeping
      # - For repeat_policy == "none", we forbid any unordered pair that appears in `existing_pairs`,
      #   regardless of whether a winner column exists.
      # - For repeat_policy == "reverse_only", we only count completed judgments (rows with non-missing
      #   `better_id` when present) and track the direction of the first completed judgment.
      existing_counts_all <- integer()
      existing_counts <- integer()
      existing_dir <- character()
      existing_completed <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())

      if (!is.null(existing_pairs)) {
        ex_full <- tibble::as_tibble(existing_pairs)
        if (all(c("object1", "object2") %in% names(ex_full)) && !all(c("ID1", "ID2") %in% names(ex_full))) {
          ex_full <- dplyr::rename(ex_full, ID1 = "object1", ID2 = "object2")
        }

        if (all(c("ID1", "ID2") %in% names(ex_full))) {
          ex_full <- dplyr::mutate(ex_full,
            ID1 = as.character(.data$ID1),
            ID2 = as.character(.data$ID2)
          )
          ex_full <- dplyr::filter(ex_full, .data$ID1 %in% ids, .data$ID2 %in% ids)

          if (nrow(ex_full) > 0L) {
            key_all <- unique(.unordered_pair_key(ex_full$ID1, ex_full$ID2))
            existing_counts_all <- stats::setNames(rep.int(1L, length(key_all)), key_all)
          }

          has_winner <- "better_id" %in% names(ex_full)
          existing_completed <- dplyr::transmute(
            ex_full,
            ID1 = .data$ID1,
            ID2 = .data$ID2,
            better_id = if (has_winner) as.character(.data$better_id) else NA_character_
          )
          if (has_winner) {
            existing_completed <- dplyr::filter(existing_completed, !is.na(.data$better_id), nzchar(.data$better_id))
          } else {
            existing_completed <- existing_completed[0, , drop = FALSE]
          }

          if (nrow(existing_completed) > 0L) {
            tmp <- dplyr::mutate(existing_completed,
              pair_key = .unordered_pair_key(.data$ID1, .data$ID2),
              direction = .pair_direction(.data$ID1, .data$ID2)
            )
            key_n <- dplyr::count(tmp, .data$pair_key, name = "n_completed")
            existing_counts <- stats::setNames(as.integer(key_n$n_completed), key_n$pair_key)

            one_key <- dplyr::filter(key_n, .data$n_completed == 1L)$pair_key
            if (length(one_key) > 0L) {
              dir_tbl <- dplyr::filter(tmp, .data$pair_key %in% one_key)
              dir_tbl <- dplyr::group_by(dir_tbl, .data$pair_key)
              dir_tbl <- dplyr::slice_head(dir_tbl, n = 1)
              dir_tbl <- dplyr::ungroup(dir_tbl)
              existing_dir <- stats::setNames(as.character(dir_tbl$direction), dir_tbl$pair_key)
            }
          }
        }
      }

      # Repeat guard (graph health)
      graph_state <- .graph_state_from_pairs(ex, ids = ids)
      repeat_guard_passed <- (graph_state$metrics$degree_min[[1]] >= repeat_guard_min_degree) &&
        (graph_state$metrics$largest_component_frac[[1]] >= repeat_guard_largest_component_frac)

      # Compute repeat quota for this round (deterministic)
      repeat_quota_n <- 0L
      if (repeat_policy == "reverse_only") {
        if (is.null(repeat_n)) {
          repeat_quota_n <- as.integer(floor(repeat_frac * n_pairs))
        } else {
          repeat_quota_n <- as.integer(repeat_n)
        }
        repeat_quota_n <- max(repeat_quota_n, 0L)
        repeat_quota_n <- min(repeat_quota_n, n_pairs)
        if (!isTRUE(repeat_guard_passed)) {
          repeat_quota_n <- 0L
        }
      }

      ord <- order(joined$theta, joined$ID, na.last = TRUE)
      joined <- joined[ord, , drop = FALSE]

      id_vec <- joined$ID

      # Repeat candidate bucket.
      #
      # We always compute eligibility for diagnostics, but only *add* repeat
      # candidates to the scoring pool when the repeat guard passes and the
      # quota is non-zero.
      repeat_cand_tbl <- .ap_gen_repeat_reverse(existing_completed, id_vec = id_vec, repeat_cap = repeat_cap)
      n_repeat_eligible <- nrow(repeat_cand_tbl)

      th_vec <- as.double(joined$theta)
      se_vec <- as.double(joined$se)

      tot_vec <- as.integer(total_counts[id_vec])
      p1_vec <- as.integer(pos1_counts[id_vec])
      p2_vec <- as.integer(pos2_counts[id_vec])

      n <- length(id_vec)
      k_neighbors2 <- min(k_neighbors, max(n - 1L, 1L))

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
          stop(
            "`embedding_neighbors` is missing IDs: ",
            paste(utils::head(missing_ids, 10), collapse = ", "),
            call. = FALSE
          )
        }
      }

      if (!is.numeric(embed_far_k) || length(embed_far_k) != 1L || is.na(embed_far_k) || embed_far_k < 0) {
        stop("`embed_far_k` must be a single non-negative integer.", call. = FALSE)
      }
      embed_far_k <- as.integer(embed_far_k)

      cand_tbl_raw <- .ap_gen_candidates(
        id_vec = id_vec,
        th_vec = th_vec,
        se_vec = se_vec,
        tot_vec = tot_vec,
        k_neighbors2 = k_neighbors2,
        embed_nbrs = embed_nbrs,
        embed_far_k = embed_far_k
      )

      scored_tbl <- .ap_score_candidates(
        cand_tbl = cand_tbl_raw,
        th_vec = th_vec,
        se_vec = se_vec,
        tot_vec = tot_vec,
        min_judgments = min_judgments,
        w_embed = w_embed,
        embed_score_mode = embed_score_mode
      )

      include_repeats <- repeat_policy == "reverse_only" &&
        isTRUE(repeat_guard_passed) &&
        repeat_quota_n > 0L &&
        n_repeat_eligible > 0L

      scored_tbl_all <- if (isTRUE(include_repeats)) {
        dplyr::bind_rows(scored_tbl, repeat_cand_tbl)
      } else {
        scored_tbl
      }

      constrained_tbl <- .ap_apply_constraints(
        cand_tbl = scored_tbl_all,
        id_vec = id_vec,
        # PR6: constraint logic depends on the repeat policy.
        # - repeat_policy == 'none' forbids any unordered pair that has ever appeared
        #   in existing_pairs (presence-only).
        # - repeat_policy == 'reverse_only' uses counts of completed judgments
        #   (and direction of the first completed judgment) to allow exactly one
        #   reverse repeat when n_completed == 1.
        existing_counts = existing_counts,
        existing_dir = existing_dir,
        existing_counts_all = existing_counts_all,
        forbid_unordered = if (is.null(forbid_repeats)) TRUE else isTRUE(forbid_repeats),
        repeat_policy = repeat_policy,
        repeat_cap = repeat_cap
      )

      capped_tbl <- constrained_tbl
      if (is.finite(per_anchor_cap)) {
        capped_tbl <- dplyr::arrange(capped_tbl, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)
        capped_tbl <- dplyr::group_by(capped_tbl, .data$anchor_idx)
        capped_tbl <- dplyr::slice_head(capped_tbl, n = per_anchor_cap)
        capped_tbl <- dplyr::ungroup(capped_tbl)
      }
      if (is.finite(candidate_pool_cap)) {
        capped_tbl <- dplyr::arrange(capped_tbl, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)
        capped_tbl <- dplyr::slice_head(capped_tbl, n = candidate_pool_cap)
      }

      selected_tbl <- .ap_select_pairs_from_scored(
        scored_tbl = capped_tbl,
        n_pairs = n_pairs,
        embed_quota_frac = embed_quota_frac,
        embed_sources = "embed",
        repeat_quota_n = repeat_quota_n,
        repeat_sources = "repeat_reverse"
      )

      pairs_tbl <- .ap_orient_pairs(
        selected_tbl = selected_tbl,
        id_vec = id_vec,
        samples = samples,
        p1_vec = p1_vec,
        p2_vec = p2_vec,
        balance_positions = balance_positions
      )

      # Track planned reverse-repeat pairs for downstream runner diagnostics.
      #
      # Planned repeats are the subset of selected pairs that came from the explicit
      # repeat candidate bucket. We mark them using the unordered pair key so that any
      # downstream re-ordering (e.g., positional balancing) is handled robustly.
      planned_repeat <- tibble::tibble(ID1 = character(), ID2 = character(), pair_key = character())
      if (repeat_policy == "reverse_only" && nrow(pairs_tbl) > 0L) {
        repeat_keys <- character(0)
        if (nrow(selected_tbl) > 0L) {
          repeat_rows <- dplyr::filter(selected_tbl, .data$source == "repeat_reverse")
          if (nrow(repeat_rows) > 0L) {
            repeat_keys <- unique(as.character(repeat_rows$pair_key))
          }
        }
        if (length(repeat_keys) > 0L) {
          planned_repeat <- dplyr::mutate(
            pairs_tbl,
            pair_key = .unordered_pair_key(.data$ID1, .data$ID2)
          )
          planned_repeat <- dplyr::filter(planned_repeat, .data$pair_key %in% repeat_keys)
          planned_repeat <- dplyr::select(planned_repeat, "ID1", "ID2", "pair_key")
        }
      }
      attr(pairs_tbl, "planned_repeat_pairs") <- planned_repeat

      embed_hit_rate <- NA_real_
      if (!is.null(embed_nbrs) && nrow(pairs_tbl) > 0L) {
        hits <- mapply(
          function(a, b) {
            (b %in% embed_nbrs[[a]]) || (a %in% embed_nbrs[[b]])
          },
          pairs_tbl$ID1,
          pairs_tbl$ID2
        )
        embed_hit_rate <- mean(as.logical(hits))
      }

      diagnostics <- tibble::tibble(
        n_candidates_raw = nrow(cand_tbl_raw),
        n_candidates_scored = nrow(scored_tbl),
        n_candidates_after_constraints = nrow(constrained_tbl),
        n_candidates_after_caps = nrow(capped_tbl),
        n_selected = nrow(selected_tbl),
        n_repeat_eligible = as.integer(n_repeat_eligible),
        n_repeat_planned = nrow(planned_repeat),
        repeat_policy = as.character(repeat_policy),
        repeat_cap = as.integer(repeat_cap),
        repeat_frac = as.double(repeat_frac),
        repeat_n = ifelse(is.null(repeat_n), NA_integer_, as.integer(repeat_n)),
        repeat_quota_n = as.integer(repeat_quota_n),
        repeat_guard_passed = as.logical(repeat_guard_passed),
        repeat_guard_min_degree = as.integer(repeat_guard_min_degree),
        repeat_guard_largest_component_frac = as.double(repeat_guard_largest_component_frac),
        graph_degree_min = as.double(graph_state$metrics$degree_min[[1]]),
        graph_largest_component_frac = as.double(graph_state$metrics$largest_component_frac[[1]]),
        n_selected_theta = sum(selected_tbl$source == "theta"),
        n_selected_embed = sum(selected_tbl$source == "embed"),
        n_selected_far = sum(selected_tbl$source == "far"),
        embed_neighbor_hit_rate = embed_hit_rate,
        embed_quota_frac = embed_quota_frac,
        candidate_pool_cap = candidate_pool_cap,
        per_anchor_cap = per_anchor_cap,
        w_embed = w_embed,
        embed_score_mode = embed_score_mode
      )

      internals <- NULL
      if (isTRUE(return_internal)) {
        internals <- list(
          raw = cand_tbl_raw,
          scored = scored_tbl,
          constrained = constrained_tbl,
          capped = capped_tbl,
          selected = selected_tbl
        )
      }

      list(pairs = pairs_tbl, diagnostics = diagnostics, candidates = internals)
    },
    arg_name = "seed"
  )

  if (isTRUE(return_internal)) {
    return(res)
  }

  out <- res$pairs
  attr(out, "pairing_diagnostics") <- res$diagnostics
  if (!is.null(attr(out, "planned_repeat_pairs"))) {
    # keep
  } else if ("planned_repeat_pairs" %in% names(res$diagnostics)) {
    attr(out, "planned_repeat_pairs") <- res$diagnostics$planned_repeat_pairs[[1]]
  }
  out
}

#' Compute a simple cosine-similarity neighbor list from an embeddings matrix.
#'
#' This is intentionally lightweight (no ANN dependency) and intended for
#' moderate n (e.g., up to a few thousand) where a single dense similarity
#' computation is acceptable.
#'
#' @keywords internal
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
