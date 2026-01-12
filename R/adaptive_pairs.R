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
                               embed_far_k = 0L,
                               hash_round = 0L,
                               hash_salt = "pairwiseLLM") {
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

    # 3) optional far candidates (deterministic; does not depend on R RNG)
    js_far <- integer()
    if (embed_far_k > 0L) {
      base_pool <- if (i < n) (i + 1L):n else integer(0)
      pool <- setdiff(base_pool, unique(c(js_theta, js_embed)))
      if (length(pool) > 0L) {
        pool_ids <- id_vec[pool]
        h <- .hash_key_u32(
          id1 = rep.int(id_vec[[i]], length(pool_ids)),
          id2 = pool_ids,
          round = hash_round,
          salt = paste0(hash_salt, "|far|", i)
        )
        ord_far <- order(h, pool_ids, na.last = TRUE)
        take_far <- min(embed_far_k, length(pool))
        js_far <- pool[ord_far[seq_len(take_far)]]
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

## Generate bridge/repair candidates across components using embedding neighbors.
##
## This helper proposes cross-component edges based on `embedding_neighbors`.
## It is deterministic: component priority, then neighbor rank, then IDs.
.ap_gen_bridge_candidates <- function(id_vec,
                                      embed_nbrs,
                                      component_id,
                                      max_neighbors_per_anchor = 20L) {
  id_vec <- as.character(id_vec)
  if (length(id_vec) < 2L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical()
    ))
  }

  if (is.null(embed_nbrs) || !is.list(embed_nbrs) || is.null(names(embed_nbrs))) {
    stop("`embed_nbrs` must be a named list of neighbor IDs.", call. = FALSE)
  }

  max_neighbors_per_anchor <- as.integer(max_neighbors_per_anchor)
  if (is.na(max_neighbors_per_anchor) || max_neighbors_per_anchor < 0L) {
    stop("`max_neighbors_per_anchor` must be a non-negative integer.", call. = FALSE)
  }
  if (max_neighbors_per_anchor == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical()
    ))
  }

  comp <- component_id
  if (!is.null(names(comp))) {
    comp <- comp[id_vec]
  }
  comp <- as.integer(comp)
  if (length(comp) != length(id_vec) || anyNA(comp)) {
    # Defensive: treat each node as its own component.
    comp <- seq_along(id_vec)
  }

  comp_sizes <- as.integer(table(comp))
  comp_ids <- as.integer(names(table(comp)))
  comp_tbl <- tibble::tibble(
    component_id = comp_ids,
    n_nodes = comp_sizes
  ) %>%
    dplyr::arrange(dplyr::desc(.data$n_nodes), .data$component_id)

  comp_rank <- stats::setNames(seq_len(nrow(comp_tbl)), as.character(comp_tbl$component_id))
  comp_rank_vec <- unname(comp_rank[as.character(comp)])

  # Anchor order: prioritize larger components, then stable lexicographic ID order.
  anchor_tbl <- tibble::tibble(
    anchor_idx = seq_along(id_vec),
    ID = id_vec,
    comp_rank = as.integer(comp_rank_vec)
  ) %>%
    dplyr::arrange(.data$comp_rank, .data$ID)

  out_i <- integer(0)
  out_j <- integer(0)
  out_anchor <- integer(0)
  out_rank <- integer(0)

  for (k in seq_len(nrow(anchor_tbl))) {
    i <- as.integer(anchor_tbl$anchor_idx[[k]])
    id_i <- id_vec[[i]]
    nbrs <- embed_nbrs[[id_i]]
    nbrs <- as.character(nbrs)
    nbrs <- nbrs[!is.na(nbrs) & nzchar(nbrs) & (nbrs %in% id_vec)]
    if (length(nbrs) == 0L) next

    j_all <- match(nbrs, id_vec)
    ok <- !is.na(j_all) & (comp[j_all] != comp[[i]])
    if (!any(ok)) next

    keep_pos <- which(ok)
    if (length(keep_pos) > max_neighbors_per_anchor) {
      keep_pos <- keep_pos[seq_len(max_neighbors_per_anchor)]
    }

    j_idx <- j_all[keep_pos]
    # Use original neighbor-list position as a deterministic rank.
    rank_vec <- keep_pos

    i_idx <- pmin(i, j_idx)
    j_idx2 <- pmax(i, j_idx)

    out_i <- c(out_i, as.integer(i_idx))
    out_j <- c(out_j, as.integer(j_idx2))
    out_anchor <- c(out_anchor, rep.int(i, length(j_idx2)))
    out_rank <- c(out_rank, as.integer(rank_vec))
  }

  if (length(out_i) == 0L) {
    return(tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical()
    ))
  }

  cand <- tibble::tibble(
    i_idx = as.integer(out_i),
    j_idx = as.integer(out_j),
    anchor_idx = as.integer(out_anchor),
    embed_rank = as.integer(out_rank)
  ) %>%
    dplyr::filter(.data$i_idx != .data$j_idx)

  cand <- dplyr::mutate(
    cand,
    ID1 = id_vec[.data$i_idx],
    ID2 = id_vec[.data$j_idx],
    pair_key = .unordered_pair_key(.data$ID1, .data$ID2),
    source = "bridge",
    embed_hit = TRUE,
    directed = FALSE
  )

  # Deduplicate unordered pairs deterministically.
  cand <- dplyr::arrange(cand, .data$embed_rank, .data$ID1, .data$ID2)
  cand <- cand[!duplicated(cand$pair_key), , drop = FALSE]

  dplyr::select(
    cand,
    "i_idx",
    "j_idx",
    "anchor_idx",
    "pair_key",
    "source",
    "embed_hit",
    "embed_rank",
    "directed"
  )
}

#' Generate deterministic controlled-random candidates
#'
#' This last-resort generator produces a broad set of candidate pairs when
#' normal candidate generation (theta neighborhood / embedding neighbors) yields
#' nothing, but eligible pairs may still exist. It does not use R's RNG.
#'
#' Strategy:
#' - deterministically permute IDs via a stable hash
#' - generate within-window pairs along the permuted ordering
#'
#' @param id_vec Character vector of IDs.
#' @param n_pairs Target number of pairs to select.
#' @param round_key Integer-ish scalar used to vary ordering across rounds.
#' @param salt Character scalar used to vary ordering (e.g., derived from `seed`).
#'
#' @return Tibble with the standard candidate columns.
#'
#' @keywords internal
.ap_gen_controlled_random_candidates <- function(id_vec,
                                                 n_pairs,
                                                 round_key = 0L,
                                                 salt = "pairwiseLLM") {
  id_vec <- as.character(id_vec)
  n <- length(id_vec)
  n_pairs <- as.integer(n_pairs)

  if (n < 2L || is.na(n_pairs) || n_pairs <= 0L) {
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

  round_key <- as.integer(round_key)
  if (is.na(round_key)) round_key <- 0L
  salt <- as.character(salt)
  if (is.na(salt) || !nzchar(salt)) salt <- "pairwiseLLM"

  # Heuristic: create a reasonably broad candidate pool without generating all
  # O(n^2) pairs.
  window <- ceiling((10 * n_pairs) / n)
  window <- max(window, 10L)
  window <- min(window, n - 1L)

  id_hash <- .stable_hash_u32(paste(id_vec, round_key, salt, sep = "\u001F"))
  perm <- order(id_hash, id_vec, na.last = TRUE)

  # Generate window pairs along permuted order.
  out_i <- integer(0)
  out_j <- integer(0)
  out_anchor <- integer(0)

  for (p in seq_len(n - 1L)) {
    i_idx <- perm[[p]]
    max_off <- min(window, n - p)
    if (max_off < 1L) next # nocov
    js <- perm[(p + 1L):(p + max_off)]
    out_i <- c(out_i, rep.int(i_idx, length(js)))
    out_j <- c(out_j, js)
    out_anchor <- c(out_anchor, rep.int(i_idx, length(js)))
  }

  if (length(out_i) == 0L) { # nocov start
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
    )) # nocov end
  }

  pk <- .unordered_pair_key(id_vec[out_i], id_vec[out_j])

  tibble::tibble(
    i_idx = as.integer(out_i),
    j_idx = as.integer(out_j),
    anchor_idx = as.integer(out_anchor),
    pair_key = as.character(pk),
    source = "random",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = FALSE,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = 0
  )
}

#' Enforce a per-node cap on a scored candidate table
#'
#' Applied as a deterministic greedy filter in score order.
#'
#' @keywords internal
.ap_enforce_per_node_cap <- function(scored_tbl,
                                     id_vec,
                                     per_node_cap = 1L) {
  scored_tbl <- tibble::as_tibble(scored_tbl)

  per_node_cap <- as.integer(per_node_cap)
  if (is.na(per_node_cap) || per_node_cap < 1L || nrow(scored_tbl) == 0L) {
    return(scored_tbl)
  }

  scored_tbl <- dplyr::arrange(scored_tbl, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)

  n <- length(id_vec)
  counts <- integer(n)
  keep <- logical(nrow(scored_tbl))

  for (k in seq_len(nrow(scored_tbl))) {
    i <- as.integer(scored_tbl$i_idx[[k]])
    j <- as.integer(scored_tbl$j_idx[[k]])

    if (counts[[i]] < per_node_cap && counts[[j]] < per_node_cap) {
      keep[[k]] <- TRUE
      counts[[i]] <- counts[[i]] + 1L
      counts[[j]] <- counts[[j]] + 1L
    }
  }

  scored_tbl[keep, , drop = FALSE]
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
  repeat_policy <- match.arg(repeat_policy, c("allow", "none", "reverse_only", "forbid_unordered"))
  if (identical(repeat_policy, "allow")) repeat_policy <- "none"
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
.ap_map_idx_to_ids <- function(cand_tbl,
                               id_vec,
                               err_ctx = "select_adaptive_pairs") {
  cand_tbl <- tibble::as_tibble(cand_tbl)
  id_vec <- as.character(id_vec)

  if (is.null(id_vec) || length(id_vec) == 0L) {
    stop(
      "Index mapping mismatch (", err_ctx, "): `id_vec` is NULL/empty.",
      call. = FALSE
    )
  }

  if (!all(c("i_idx", "j_idx") %in% names(cand_tbl))) {
    stop(
      "Index mapping mismatch (", err_ctx, "): candidate table must include i_idx and j_idx.",
      call. = FALSE
    )
  }

  i_idx <- suppressWarnings(as.integer(cand_tbl$i_idx))
  j_idx <- suppressWarnings(as.integer(cand_tbl$j_idx))
  n <- length(id_vec)

  bad <- which(
    is.na(i_idx) | is.na(j_idx) |
      i_idx < 1L | j_idx < 1L |
      i_idx > n | j_idx > n
  )
  if (length(bad) > 0L) {
    stop(
      "Index mapping mismatch (", err_ctx, "): i_idx/j_idx exceed length(id_vec) ",
      "(possible ID order corruption).",
      call. = FALSE
    )
  }

  id1 <- id_vec[i_idx]
  id2 <- id_vec[j_idx]

  if (anyNA(id1) || anyNA(id2)) {
    stop(
      "Index mapping mismatch (", err_ctx, "): NA after mapping indices to IDs.",
      call. = FALSE
    )
  }

  dplyr::mutate(
    cand_tbl,
    ID1 = as.character(id1),
    ID2 = as.character(id2)
  )
}

#' Internal: choose an ID mapping vector (defensive helper)
#'
#' @keywords internal
.ap_choose_id_map_vec <- function(id_vec, graph_state) {
  id_vec <- as.character(id_vec %||% character(0))
  if (length(id_vec) > 0L) {
    return(id_vec)
  }

  gs_ids <- graph_state$ids %||% character(0)
  gs_ids <- as.character(gs_ids)
  if (length(gs_ids) > 0L) {
    return(gs_ids)
  }

  character(0)
}

#' Internal: pick lowest-degree pairs from a candidate pool
#'
#' @keywords internal
.ap_pick_low_degree <- function(pool, deg_sum, n_pick) {
  pool <- tibble::as_tibble(pool)
  n_pick <- as.integer(n_pick)
  if (n_pick <= 0L || nrow(pool) == 0L) {
    return(pool[0, , drop = FALSE])
  }

  # If we don't have indices, fall back to deterministic slice_head.
  if (!all(c("i_idx", "j_idx") %in% names(pool))) {
    return(dplyr::slice_head(pool, n = n_pick))
  }

  deg_sum <- as.double(deg_sum)
  deg_sum_pool <- deg_sum[pool$i_idx] + deg_sum[pool$j_idx]
  if (length(deg_sum_pool) != nrow(pool)) {
    return(dplyr::slice_head(pool, n = n_pick))
  }

  # Prefer low degree, break ties by high score_total, then indices.
  if ("score_total" %in% names(pool)) {
    ord <- order(deg_sum_pool, dplyr::desc(pool$score_total), pool$i_idx, pool$j_idx, na.last = TRUE)
  } else {
    ord <- order(deg_sum_pool, pool$i_idx, pool$j_idx, na.last = TRUE)
  }

  dplyr::slice_head(pool[ord, , drop = FALSE], n = n_pick)
}

#' @keywords internal
.ap_orient_pairs <- function(selected_tbl,
                             id_vec,
                             samples,
                             p1_vec,
                             p2_vec,
                             balance_positions = TRUE,
                             hash_round = 0L,
                             hash_salt = "pairwiseLLM") {
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
        # PR9.3: deterministic tie-breaker (no RNG dependence).
        pk <- .unordered_pair_key(a, b)
        coin <- .stable_hash_u32(paste(pk, hash_round, hash_salt, k, sep = "\u001F"))
        if ((coin %% 2) < 1) {
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
#' @param repeat_policy Character repeat planning policy. Options:
#'   \itemize{
#'     \item \code{"none"}: do not plan repeat checks.
#'     \item \code{"reverse_only"}: plan a subset of opposite-direction repeats for
#'       eligible unordered pairs (A,B).
#'     \item \code{"forbid_unordered"}: convenience alias that behaves like the
#'       legacy \code{forbid_repeats = TRUE} (no planned repeats and forbids
#'       selecting unordered repeats from the candidate pool).
#'   }
#' @param repeat_cap Non-negative integer cap on the number of planned repeat
#'   pairs per unordered pair key. For \code{repeat_policy = "reverse_only"}, each
#'   unordered pair is eligible for at most \code{repeat_cap} planned reverse
#'   repeats.
#' @param repeat_frac Numeric in \code{[0, 1]}. Target fraction of the requested
#'   \code{n_pairs} that should be reserved for repeat checks (when eligible
#'   repeat pairs exist).
#' @param repeat_n Optional non-negative integer. If provided, overrides
#'   \code{repeat_frac} and targets this many planned repeat pairs.
#' @param repeat_guard_min_degree Integer. Guard for enabling repeat planning:
#'   do not plan repeats until the minimum graph degree across IDs is at least
#'   this value.
#' @param repeat_guard_largest_component_frac Numeric in \code{[0, 1]}. Guard for
#'   enabling repeat planning: do not plan repeats until the largest connected
#'   component contains at least this fraction of IDs.
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
#' @param explore_frac Fraction of pairs to reserve for exploration (e.g., bridge/low-degree) before exploitation.
#' @param graph_state Optional graph state from `.graph_state_from_pairs()` to avoid recomputation.
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
#'   The returned tibble may include attributes used for diagnostics:
#'   \itemize{
#'     \item \code{attr(out, "planned_repeat_pairs")}: a tibble of repeat pairs
#'       planned under \code{repeat_policy} (may be empty).
#'     \item \code{attr(out, "pairing_diagnostics")}: a one-row tibble of pairing
#'       counts/caps/guard outcomes, including fallback state
#'       (\code{fallback_path}, \code{fallback_trigger}), selected-pair counts by source, and
#'       graph-health metrics before/after planning
#'       (\code{degree_min_before/after}, \code{largest_component_frac_before/after}).
#'   }
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
                                  explore_frac = 0,
                                  graph_state = NULL,
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
  repeat_policy <- match.arg(repeat_policy, c("allow", "none", "reverse_only", "forbid_unordered"))
  if (identical(repeat_policy, "allow")) repeat_policy <- "none"

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

      # ---- diagnostics defaults -------------------------------------------
      # Keep diagnostics scalar so we always return a 1-row tibble.
      fallback_path <- NA_character_
      fallback_trigger <- NA_character_
      n_pairs_source_random <- 0L
      embed_neighbor_hit_rate <- NA_real_
      repeat_guard_passed <- NA
      repeat_guard_reason <- NA_character_
      repeat_quota_n <- 0L
      n_repeat_eligible <- 0L
      n_repeat_planned <- 0L

      # Repeat guard (graph health)
      graph_state <- if (!is.null(graph_state)) graph_state else .graph_state_from_pairs(ex, ids = ids)

      # Defensive normalization: some callers (e.g., checkpoint resume) may
      # provide a graph_state whose vectors are present but unnamed. Downstream
      # logic matches by names for component_id / degree.
      if (!is.null(graph_state$component_id) && is.null(names(graph_state$component_id))) {
        if (!is.null(graph_state$ids) && length(graph_state$component_id) == length(graph_state$ids)) {
          names(graph_state$component_id) <- graph_state$ids
        } else if (!is.null(graph_state$degree) && !is.null(names(graph_state$degree)) &&
          length(graph_state$component_id) == length(graph_state$degree)) {
          names(graph_state$component_id) <- names(graph_state$degree)
        }
      }
      if (!is.null(graph_state$degree) && is.null(names(graph_state$degree)) &&
        !is.null(graph_state$ids) && length(graph_state$degree) == length(graph_state$ids)) {
        names(graph_state$degree) <- graph_state$ids
      }
      deg_min <- if (!is.null(graph_state$metrics) && nrow(graph_state$metrics) > 0L) graph_state$metrics$degree_min[[1]] else 0
      lcf <- if (!is.null(graph_state$metrics) && nrow(graph_state$metrics) > 0L) graph_state$metrics$largest_component_frac[[1]] else 0
      graph_unhealthy <- (deg_min < repeat_guard_min_degree) || (lcf < repeat_guard_largest_component_frac)
      repeat_guard_passed <- isTRUE((deg_min >= repeat_guard_min_degree) && (lcf >= repeat_guard_largest_component_frac))

      # Compute repeat quota for this round (deterministic)
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

      # PR9.3: stable deterministic pseudo-random salt/round identifiers for
      # any controlled-random ordering. This avoids reliance on R's RNG
      # implementation for determinism across R versions.
      hash_round <- as.integer(nrow(ex))
      if (is.na(hash_round)) hash_round <- 0L # nocov
      hash_salt <- "pairwiseLLM"
      if (!is.null(seed) && length(seed) == 1L && !is.na(seed)) {
        hash_salt <- paste0(hash_salt, "|", as.character(seed))
      }

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
        embed_far_k = embed_far_k,
        hash_round = hash_round,
        hash_salt = hash_salt
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


      explore_frac <- as.double(explore_frac)
      if (is.na(explore_frac) || explore_frac < 0 || explore_frac > 1) {
        stop("`explore_frac` must be between 0 and 1.", call. = FALSE)
      }

      n_explore_target <- as.integer(floor(n_pairs * explore_frac))
      explore_selected_tbl <- capped_tbl[0, , drop = FALSE]

      # IMPORTANT: If repeat candidates are present (repeat_policy == 'reverse_only'),
      # exploration selection must not bypass the repeat quota. Repeat candidates
      # are injected into the scoring/candidate pool (source == 'repeat_reverse')
      # and would otherwise be eligible for exploration (low-degree preference),
      # even though repeat_quota_n is only enforced inside .ap_select_pairs_from_scored.
      #
      # Strategy:
      #   1) Prefer non-repeat candidates for exploration.
      #   2) If there is remaining exploration quota and remaining repeat quota,
      #      allow repeat candidates into exploration, but cap them to the
      #      remaining repeat quota.
      #   3) Decrement repeat_quota_n passed to the exploit selector by the
      #      number of repeat pairs already selected in exploration.
      repeat_source_name <- "repeat_reverse"
      explore_repeat_cap <- if (repeat_policy == "reverse_only") as.integer(repeat_quota_n) else 0L
      if (is.na(explore_repeat_cap) || explore_repeat_cap < 0L) explore_repeat_cap <- 0L # nocov


      # Workstream D: determine required component-bridge quota up-front.
      # If the graph is disconnected, we allocate an explicit bridge quota even
      # when `explore_frac == 0` (bridging is non-negotiable for graph health).
      n_components <- 1L
      if (!is.null(graph_state$metrics) && nrow(graph_state$metrics) > 0L &&
        "n_components" %in% names(graph_state$metrics)) {
        n_components <- as.integer(graph_state$metrics$n_components[[1]])
        if (is.na(n_components) || n_components < 1L) n_components <- 1L
      }

      n_bridge_target <- 0L
      if (n_components > 1L) {
        n_bridge_target <- as.integer(min(ceiling(0.5 * n_pairs), n_components - 1L))
        n_bridge_target <- max(0L, n_bridge_target)
      }

      if (n_bridge_target > 0L) {
        n_explore_target <- as.integer(max(n_explore_target, n_bridge_target))
      }

      if (n_explore_target > 0L) {
        # Candidate tables are index-based (i_idx/j_idx). Some paths omit ID1/ID2;
        # add them here so component/degree lookups and ordering are robust.
        if (!all(c("ID1", "ID2") %in% names(capped_tbl)) &&
          all(c("i_idx", "j_idx") %in% names(capped_tbl))) {
          # IMPORTANT (Workstream G): never overwrite the theta-ordered `id_vec`.
          # Candidate indices were created against that ordering, and downstream
          # orientation uses it as well.
          id_map_vec <- .ap_choose_id_map_vec(id_vec, graph_state)

          capped_tbl <- .ap_map_idx_to_ids(
            cand_tbl = capped_tbl,
            id_vec = id_map_vec,
            err_ctx = "select_adaptive_pairs exploration"
          )
        }

        # Workstream D: explicit component-bridging exploration when disconnected.
        bridge_selected_tbl <- capped_tbl[0, , drop = FALSE]

        if (n_bridge_target > 0L) {
          # Ensure exploration budget can accommodate required bridges.
          n_explore_target <- as.integer(max(n_explore_target, n_bridge_target))

          bridge_raw <- .ap_make_component_bridge_pairs(
            graph_state = graph_state,
            id_vec_theta = id_vec,
            n_bridge = n_bridge_target
          )

          if (nrow(bridge_raw) > 0L) {
            bridge_constrained <- .ap_apply_constraints(
              cand_tbl = bridge_raw,
              id_vec = id_vec,
              existing_counts = existing_counts,
              existing_counts_all = existing_counts_all,
              existing_dir = existing_dir,
              forbid_unordered = if (is.null(forbid_repeats)) TRUE else isTRUE(forbid_repeats),
              repeat_policy = repeat_policy,
              repeat_cap = repeat_cap
            )

            if (nrow(bridge_constrained) > 0L && "target_component_id" %in% names(bridge_constrained)) {
              bridge_best <- dplyr::group_by(bridge_constrained, .data$target_component_id)
              bridge_best <- dplyr::slice_head(bridge_best, n = 1L)
              bridge_best <- dplyr::ungroup(bridge_best)
              bridge_best <- dplyr::arrange(
                bridge_best,
                .data$target_component_size,
                .data$target_component_id,
                .data$i_idx,
                .data$j_idx
              )
              bridge_selected_tbl <- utils::head(bridge_best, n_bridge_target)
            } else if (nrow(bridge_constrained) > 0L) {
              bridge_selected_tbl <- utils::head(bridge_constrained, n_bridge_target)
            }
          }
        }

        capped_tbl_explore <- capped_tbl
        if (nrow(bridge_selected_tbl) > 0L && "pair_key" %in% names(capped_tbl_explore)) {
          capped_tbl_explore <- dplyr::filter(
            capped_tbl_explore,
            !(.data$pair_key %in% bridge_selected_tbl$pair_key)
          )
        }

        # Prefer low-degree endpoints (exploration), stable tie-break.
        degree <- graph_state$degree
        deg1 <- degree[match(capped_tbl_explore$ID1, names(degree))]
        deg2 <- degree[match(capped_tbl_explore$ID2, names(degree))]
        deg1[is.na(deg1)] <- 0
        deg2[is.na(deg2)] <- 0
        deg_sum <- deg1 + deg2
        capped_tbl_explore$deg_sum <- deg_sum

        # This flag MUST come from locals if present; never assume a symbol exists.
        # Backward-compat: earlier prototypes controlled this behavior via
        # internal flags rather than a formal argument. Use string-based lookup
        # to avoid NOTE about undefined globals in R CMD check.
        explore_across_components_flag <- FALSE
        # This behavior was only reachable in earlier internal prototypes
        # where these flags were defined as locals in the same frame.
        # The exported API does not expose them and the lookup uses
        # inherits = FALSE, so callers cannot enable it. Keep the code for
        # backward compatibility but exclude from coverage.
        # nocov start
        val_now <- get0("explore_across_components_now", ifnotfound = NULL, inherits = FALSE)
        val_old <- get0("explore_across_components", ifnotfound = NULL, inherits = FALSE)
        if (!is.null(val_now)) {
          explore_across_components_flag <- isTRUE(val_now)
        } else if (!is.null(val_old)) {
          explore_across_components_flag <- isTRUE(val_old)
        }
        # nocov end

        # Split candidates into non-repeat / repeat pools so exploration can't
        # exceed the repeat quota.
        is_repeat_row <- FALSE
        if ("source" %in% names(capped_tbl_explore)) {
          is_repeat_row <- capped_tbl_explore$source == repeat_source_name
          is_repeat_row[is.na(is_repeat_row)] <- FALSE
        }
        capped_nonrepeat <- capped_tbl_explore[!is_repeat_row, , drop = FALSE]
        capped_repeat <- capped_tbl_explore[is_repeat_row, , drop = FALSE]
        deg_sum_nonrepeat <- deg_sum[!is_repeat_row]
        deg_sum_repeat <- deg_sum[is_repeat_row]

        # nocov start
        if (explore_across_components_flag) {
          comp <- graph_state$component_id
          comp1 <- comp[match(capped_tbl_explore$ID1, names(comp))]
          comp2 <- comp[match(capped_tbl_explore$ID2, names(comp))]

          # If lookup failed, avoid length-0 logical subscripts; treat as same-component.
          if (length(comp1) != nrow(capped_tbl_explore) || length(comp2) != nrow(capped_tbl_explore)) {
            comp1 <- rep.int(1L, nrow(capped_tbl_explore))
            comp2 <- rep.int(1L, nrow(capped_tbl_explore))
          }

          keep <- (comp1 != comp2)
          if (length(keep) != nrow(capped_tbl_explore)) keep <- rep.int(FALSE, nrow(capped_tbl_explore))

          keep_nonrepeat <- keep[!is_repeat_row]
          keep_repeat <- keep[is_repeat_row]
          explore_pool_nonrepeat <- capped_nonrepeat[keep_nonrepeat, , drop = FALSE]
          deg_sum_pool_nonrepeat <- deg_sum_nonrepeat[keep_nonrepeat]
          explore_pool_repeat <- capped_repeat[keep_repeat, , drop = FALSE]
          deg_sum_pool_repeat <- deg_sum_repeat[keep_repeat]
        } else {
          explore_pool_nonrepeat <- capped_nonrepeat
          deg_sum_pool_nonrepeat <- deg_sum_nonrepeat
          explore_pool_repeat <- capped_repeat
          deg_sum_pool_repeat <- deg_sum_repeat
        }
        # nocov end

        pick_low_degree <- function(pool, n_pick) {
          .ap_pick_low_degree(pool = pool, deg_sum = deg_sum, n_pick = n_pick)
        }

        # Start exploration with any explicit component-bridge pairs, then fill
        # remaining exploration quota with low-degree candidates.
        explore_selected_tbl <- bridge_selected_tbl

        # If explore_frac == 0, we bridge only (no additional exploration beyond bridging).
        if (explore_frac > 0) {
          n_nonrepeat_for_explore <- as.integer(max(0L, n_explore_target - nrow(explore_selected_tbl)))
          if (n_nonrepeat_for_explore > 0L && nrow(explore_pool_nonrepeat) > 0L) {
            explore_nonrepeat_selected <- pick_low_degree(explore_pool_nonrepeat, n_nonrepeat_for_explore)
            if (nrow(explore_nonrepeat_selected) > 0L) {
              explore_selected_tbl <- dplyr::bind_rows(explore_selected_tbl, explore_nonrepeat_selected)
            }
          }

          # If exploration quota isn't filled, allow repeats into exploration but
          # cap by remaining repeat quota.
          n_explore_remaining <- as.integer(n_explore_target - nrow(explore_selected_tbl))
          if (n_explore_remaining > 0L && explore_repeat_cap > 0L && nrow(explore_pool_repeat) > 0L) {
            n_repeat_for_explore <- as.integer(min(n_explore_remaining, explore_repeat_cap))
            explore_repeat_selected <- pick_low_degree(explore_pool_repeat, n_repeat_for_explore)
            if (nrow(explore_repeat_selected) > 0L) {
              explore_selected_tbl <- dplyr::bind_rows(explore_selected_tbl, explore_repeat_selected)
            }
          }
        }
      }

      # Decrement repeat quota by repeats already selected during exploration.
      n_repeat_explore <- 0L
      if ("source" %in% names(explore_selected_tbl)) {
        n_repeat_explore <- as.integer(sum(explore_selected_tbl$source == repeat_source_name, na.rm = TRUE))
      }

      repeat_quota_remaining <- repeat_quota_n
      if (repeat_policy == "reverse_only") {
        repeat_quota_remaining <- as.integer(max(0L, repeat_quota_n - n_repeat_explore))
      }

      n_pairs_exploit <- as.integer(n_pairs - nrow(explore_selected_tbl))

      # Apply explore_frac quota: reserve the explore-selected pairs and only
      # request the remaining quota from the normal (exploitative) selector.
      # Exclude exploration keys from the exploit pool to avoid duplicates.
      exploit_pool <- capped_tbl
      if (nrow(explore_selected_tbl) > 0L && "pair_key" %in% names(explore_selected_tbl)) {
        exploit_pool <- dplyr::filter(exploit_pool, !(.data$pair_key %in% explore_selected_tbl$pair_key))
      }

      selected_tbl_normal <- .ap_select_pairs_from_scored(
        scored_tbl = exploit_pool,
        n_pairs = n_pairs_exploit,
        embed_quota_frac = embed_quota_frac,
        embed_sources = "embed",
        repeat_quota_n = repeat_quota_remaining,
        repeat_sources = "repeat_reverse"
      )

      if (nrow(explore_selected_tbl) > 0L) {
        selected_tbl_normal <- dplyr::bind_rows(explore_selected_tbl, selected_tbl_normal)
        # Hard cap to the caller's request.
        if (nrow(selected_tbl_normal) > n_pairs) {
          selected_tbl_normal <- dplyr::slice_head(selected_tbl_normal, n = as.integer(n_pairs))
        }
      }

      # ---- PR9.2 bridge/repair fallback (embedding-based, deterministic) ----
      bridge_trigger <- NA_character_
      bridge_selected_tbl <- selected_tbl_normal[0, , drop = FALSE]

      normal_empty <- nrow(selected_tbl_normal) == 0L
      should_try_bridge <- (!is.null(embed_nbrs)) && (isTRUE(graph_unhealthy) || normal_empty)

      if (isTRUE(should_try_bridge)) {
        bridge_trigger <- if (normal_empty) "normal_empty" else "graph_unhealthy"

        n_bridge_target <- if (normal_empty) {
          as.integer(n_pairs)
        } else {
          as.integer(min(max(1L, ceiling(0.1 * n_pairs)), n_pairs))
        }

        bridge_raw <- .ap_gen_bridge_candidates(
          id_vec = id_vec,
          embed_nbrs = embed_nbrs,
          component_id = graph_state$component_id,
          max_neighbors_per_anchor = 20L
        )

        if (nrow(bridge_raw) > 0L) {
          bridge_scored <- .ap_score_candidates(
            cand_tbl = bridge_raw,
            th_vec = th_vec,
            se_vec = se_vec,
            tot_vec = tot_vec,
            min_judgments = min_judgments,
            w_embed = w_embed,
            embed_score_mode = embed_score_mode
          )

          bridge_constrained <- .ap_apply_constraints(
            cand_tbl = bridge_scored,
            id_vec = id_vec,
            existing_counts = existing_counts,
            existing_dir = existing_dir,
            existing_counts_all = existing_counts_all,
            forbid_unordered = if (is.null(forbid_repeats)) TRUE else isTRUE(forbid_repeats),
            repeat_policy = repeat_policy,
            repeat_cap = repeat_cap
          )

          bridge_capped <- bridge_constrained
          if (is.finite(per_anchor_cap)) {
            bridge_capped <- dplyr::arrange(bridge_capped, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)
            bridge_capped <- dplyr::group_by(bridge_capped, .data$anchor_idx)
            bridge_capped <- dplyr::slice_head(bridge_capped, n = per_anchor_cap)
            bridge_capped <- dplyr::ungroup(bridge_capped)
          }
          if (is.finite(candidate_pool_cap)) {
            bridge_capped <- dplyr::arrange(bridge_capped, dplyr::desc(.data$score_total), .data$i_idx, .data$j_idx)
            bridge_capped <- dplyr::slice_head(bridge_capped, n = candidate_pool_cap)
          }

          # Cap the boundary-node usage in the bridge pool to avoid repeatedly
          # selecting the same cross-component endpoints.
          bridge_node_cap <- as.integer(max(1L, min(2L, ceiling(n_pairs / 2))))
          bridge_capped <- .ap_enforce_per_node_cap(
            scored_tbl = bridge_capped,
            id_vec = id_vec,
            per_node_cap = bridge_node_cap
          )

          bridge_selected_tbl <- .ap_select_pairs_from_scored(
            scored_tbl = bridge_capped,
            n_pairs = n_bridge_target,
            embed_quota_frac = 0,
            embed_sources = "embed",
            repeat_quota_n = 0L,
            repeat_sources = "repeat_reverse"
          )
        }
      }

      # Combine: bridge pairs (if any) then fill remainder from the normal selection.
      used_bridge <- nrow(bridge_selected_tbl) > 0L
      selected_tbl <- if (used_bridge) {
        if (normal_empty) {
          bridge_selected_tbl
        } else {
          fill_pool <- dplyr::filter(selected_tbl_normal, !(.data$pair_key %in% bridge_selected_tbl$pair_key))
          need <- max(as.integer(n_pairs) - nrow(bridge_selected_tbl), 0L)
          dplyr::bind_rows(bridge_selected_tbl, dplyr::slice_head(fill_pool, n = need))
        }
      } else {
        selected_tbl_normal
      }

      # --- PR9.3: Controlled random (deterministic) ---
      # If normal selection (and optional bridge/repair) yields zero pairs, we
      # fall back to a deterministic pseudo-random ordering of a broader
      # candidate pool generated without enumerating all O(n^2) pairs.
      used_random <- FALSE
      random_trigger <- NA_character_

      if (n_pairs > 0L && nrow(selected_tbl) == 0L && length(id_vec) >= 2L) {
        random_trigger <- if (isTRUE(should_try_bridge)) "bridge_empty" else "normal_empty"

        random_raw <- .ap_gen_controlled_random_candidates(
          id_vec = id_vec,
          n_pairs = as.integer(n_pairs),
          round_key = hash_round,
          salt = hash_salt
        )

        random_constrained <- .ap_apply_constraints(
          cand_tbl = random_raw,
          id_vec = id_vec,
          existing_counts = existing_counts,
          existing_dir = existing_dir,
          existing_counts_all = existing_counts_all,
          forbid_unordered = if (is.null(forbid_repeats)) TRUE else isTRUE(forbid_repeats),
          repeat_policy = repeat_policy,
          repeat_cap = repeat_cap
        )

        if (nrow(random_constrained) > 0L) {
          mode <- getOption("pairwiseLLM.controlled_random_mode", "hash")

          if (identical(mode, "rng")) {
            ord_rng <- sample.int(nrow(random_constrained))
            random_constrained <- random_constrained[ord_rng, , drop = FALSE]
          } else {
            h <- .hash_key_u32(
              id1 = id_vec[random_constrained$i_idx],
              id2 = id_vec[random_constrained$j_idx],
              round = hash_round,
              salt = paste0(hash_salt, "|controlled_random")
            )

            ord_h <- order(h, random_constrained$pair_key, random_constrained$i_idx, random_constrained$j_idx, na.last = TRUE)
            random_constrained <- random_constrained[ord_h, , drop = FALSE]
          }

          selected_tbl <- dplyr::slice_head(random_constrained, n = as.integer(n_pairs))
          used_random <- nrow(selected_tbl) > 0L
        }
      }

      pairs_tbl <- .ap_orient_pairs(
        selected_tbl = selected_tbl,
        id_vec = id_vec,
        samples = samples,
        p1_vec = p1_vec,
        p2_vec = p2_vec,
        balance_positions = balance_positions,
        hash_round = hash_round,
        hash_salt = hash_salt
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


      # PR9.4: graph metrics before/after planning (existing vs existing + planned pairs)
      graph_after_state <- graph_state
      if (nrow(pairs_tbl) > 0L) {
        after_pairs <- dplyr::bind_rows(
          dplyr::select(ex, "ID1", "ID2"),
          dplyr::select(pairs_tbl, "ID1", "ID2")
        )
        graph_after_state <- .graph_state_from_pairs(after_pairs, ids = ids)
      }

      degree_min_before <- as.double(graph_state$metrics$degree_min[[1]])
      largest_component_frac_before <- as.double(graph_state$metrics$largest_component_frac[[1]])
      degree_min_after <- as.double(graph_after_state$metrics$degree_min[[1]])
      largest_component_frac_after <- as.double(graph_after_state$metrics$largest_component_frac[[1]])

      fallback_path <- if (used_bridge) {
        "bridge_repair"
      } else if (isTRUE(used_random)) {
        "controlled_random"
      } else if (nrow(selected_tbl) > 0L) {
        "normal"
      } else {
        "exhausted_no_pairs"
      }

      fallback_trigger <- if (fallback_path == "bridge_repair") {
        bridge_trigger
      } else if (fallback_path == "controlled_random") {
        random_trigger
      } else if (fallback_path == "exhausted_no_pairs") {
        if (isTRUE(normal_empty)) {
          "normal_empty"
        } else if (isTRUE(graph_unhealthy)) {
          "graph_unhealthy"
        } else {
          "no_pairs_found"
        }
      } else {
        NA_character_
      }

      n_pairs_source_normal <- as.integer(sum(selected_tbl$source %in% c("theta", "embed", "far")))
      n_pairs_source_bridge <- as.integer(sum(selected_tbl$source == "bridge"))
      n_pairs_source_repeat_reverse <- as.integer(sum(selected_tbl$source == "repeat_reverse"))
      n_pairs_source_random <- as.integer(sum(selected_tbl$source == "random"))

      # Controlled-random is already auditable via `fallback_path == "controlled_random"`
      # and its `fallback_trigger`. Keep diagnostics atomic (no list-cols) so
      # callers can `bind_rows()` across rounds without schema drift.

      diag_candidate_pool_cap <- if (is.finite(candidate_pool_cap)) {
        as.integer(candidate_pool_cap)
      } else {
        NA_integer_
      }
      diag_per_anchor_cap <- if (is.finite(per_anchor_cap)) {
        as.integer(per_anchor_cap)
      } else {
        NA_integer_
      }

      diagnostics <- tibble::tibble(
        n_candidates_raw = nrow(cand_tbl_raw),
        n_candidates_scored = nrow(scored_tbl),
        n_candidates_after_constraints = nrow(constrained_tbl),
        n_candidates_after_caps = nrow(capped_tbl),
        n_selected = nrow(selected_tbl),
        n_repeat_eligible = as.integer(n_repeat_eligible),
        n_repeat_planned = as.integer(nrow(planned_repeat)),
        repeat_policy = as.character(repeat_policy),
        repeat_cap = as.integer(repeat_cap),
        repeat_frac = as.double(repeat_frac),
        repeat_n = ifelse(is.null(repeat_n), NA_integer_, as.integer(repeat_n)),
        repeat_quota_n = as.integer(repeat_quota_n),
        repeat_guard_passed = as.logical(repeat_guard_passed),
        repeat_guard_min_degree = as.integer(repeat_guard_min_degree),
        repeat_guard_largest_component_frac = as.double(repeat_guard_largest_component_frac),
        degree_min_before = as.double(degree_min_before),
        largest_component_frac_before = as.double(largest_component_frac_before),
        degree_min_after = as.double(degree_min_after),
        largest_component_frac_after = as.double(largest_component_frac_after),
        graph_degree_min = as.double(graph_state$metrics$degree_min[[1]]),
        graph_largest_component_frac = as.double(graph_state$metrics$largest_component_frac[[1]]),
        fallback_path = as.character(fallback_path),
        fallback_trigger = as.character(fallback_trigger),
        n_pairs_source_normal = as.integer(n_pairs_source_normal),
        n_pairs_source_bridge = as.integer(n_pairs_source_bridge),
        n_pairs_source_repeat_reverse = as.integer(n_pairs_source_repeat_reverse),
        n_pairs_source_random = as.integer(n_pairs_source_random),
        n_selected_theta = as.integer(sum(selected_tbl$source == "theta")),
        n_selected_embed = as.integer(sum(selected_tbl$source == "embed")),
        n_selected_far = as.integer(sum(selected_tbl$source == "far")),
        embed_neighbor_hit_rate = embed_hit_rate,
        embed_quota_frac = embed_quota_frac,
        candidate_pool_cap = diag_candidate_pool_cap,
        per_anchor_cap = diag_per_anchor_cap,
        w_embed = w_embed,
        embed_score_mode = embed_score_mode
      )

      diagnostics <- .ap_pairing_diagnostics_contract(diagnostics)

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
  attr(out, "pairing_diagnostics") <- .ap_pairing_diagnostics_contract(res$diagnostics)
  if (!is.null(attr(out, "planned_repeat_pairs"))) {
    # keep
  } else if ("planned_repeat_pairs" %in% names(res$diagnostics)) {
    # This is a defensive fallback in case an internal helper drops attributes.
    # Under normal execution we set the attribute on `pairs_tbl` whenever the
    # diagnostics entry is produced.
    # nocov start
    attr(out, "planned_repeat_pairs") <- res$diagnostics$planned_repeat_pairs[[1]]
    # nocov end
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

# Internal: ensure PR9 pairing diagnostics columns exist with stable, atomic types.
#
# Defensive plumbing for PR9.1: diagnostics should never silently drop fallback
# fields, and downstream `bind_rows()` should not create list-cols.
.ap_pairing_diagnostics_contract <- function(diag) {
  diag <- tibble::as_tibble(diag)

  ensure_col <- function(.tbl, .name, .value) {
    if (!(.name %in% names(.tbl))) {
      .tbl[[.name]] <- .value
    }
    .tbl
  }

  n <- nrow(diag)

  diag <- ensure_col(diag, "fallback_path", rep(NA_character_, n))
  diag <- ensure_col(diag, "fallback_trigger", rep(NA_character_, n))
  diag <- ensure_col(diag, "n_pairs_source_normal", rep(0L, n))
  diag <- ensure_col(diag, "n_pairs_source_bridge", rep(0L, n))
  diag <- ensure_col(diag, "n_pairs_source_repeat_reverse", rep(0L, n))
  diag <- ensure_col(diag, "n_pairs_source_random", rep(0L, n))

  diag <- ensure_col(diag, "degree_min_before", rep(NA_real_, n))
  diag <- ensure_col(diag, "largest_component_frac_before", rep(NA_real_, n))
  diag <- ensure_col(diag, "degree_min_after", rep(NA_real_, n))
  diag <- ensure_col(diag, "largest_component_frac_after", rep(NA_real_, n))

  # Coerce to atomic, schema-stable types.
  diag$fallback_path <- as.character(diag$fallback_path)
  diag$fallback_trigger <- as.character(diag$fallback_trigger)
  diag$n_pairs_source_normal <- as.integer(diag$n_pairs_source_normal)
  diag$n_pairs_source_bridge <- as.integer(diag$n_pairs_source_bridge)
  diag$n_pairs_source_repeat_reverse <- as.integer(diag$n_pairs_source_repeat_reverse)
  diag$n_pairs_source_random <- as.integer(diag$n_pairs_source_random)

  diag$degree_min_before <- as.double(diag$degree_min_before)
  diag$largest_component_frac_before <- as.double(diag$largest_component_frac_before)
  diag$degree_min_after <- as.double(diag$degree_min_after)
  diag$largest_component_frac_after <- as.double(diag$largest_component_frac_after)

  # If fallback fields are missing/NA, infer a reasonable PR9.1 state from
  # existing diagnostics. This keeps tests and downstream bind_rows stable.
  if (n > 0L) {
    needs_infer <- is.na(diag$fallback_path) | !nzchar(diag$fallback_path)
    if (any(needs_infer)) {
      n_selected <- if ("n_selected" %in% names(diag)) {
        suppressWarnings(as.integer(diag$n_selected))
      } else {
        rep(NA_integer_, n)
      }

      inferred_path <- dplyr::case_when(
        diag$n_pairs_source_random > 0L ~ "controlled_random",
        diag$n_pairs_source_bridge > 0L ~ "bridge_repair",
        !is.na(n_selected) & n_selected > 0L ~ "normal",
        TRUE ~ "exhausted_no_pairs"
      )

      inferred_trigger <- dplyr::case_when(
        inferred_path %in% c("bridge_repair", "controlled_random") ~ "normal_empty",
        inferred_path == "exhausted_no_pairs" ~ "no_pairs_found",
        TRUE ~ NA_character_
      )

      diag$fallback_path[needs_infer] <- inferred_path[needs_infer]
      diag$fallback_trigger[needs_infer] <- inferred_trigger[needs_infer]
    }
  }

  diag
}
