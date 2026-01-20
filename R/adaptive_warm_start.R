# -------------------------------------------------------------------------
# Adaptive warm-start v3 helpers
# -------------------------------------------------------------------------

.warm_start_validate_config <- function(ids, config) {
  ids <- as.character(ids)
  n_items <- length(ids)
  if (n_items < 2L) {
    rlang::abort("`ids` must contain at least 2 items.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.")
  }

  config <- config %||% list()
  min_degree <- config$min_degree %||% 2L
  if (!.adaptive_v3_intish(min_degree)) {
    rlang::abort("`min_degree` must be an integer.")
  }
  min_degree <- as.integer(min_degree)
  if (min_degree < 1L) {
    rlang::abort("`min_degree` must be >= 1.")
  }
  if (n_items >= 3L && min_degree < 2L) {
    rlang::abort("`min_degree` must be >= 2 for N >= 3.")
  }
  if (min_degree > (n_items - 1L)) {
    rlang::abort("`min_degree` must be <= N - 1.")
  }

  target_mean_degree <- config$target_mean_degree %||% NULL
  if (!is.null(target_mean_degree)) {
    if (!is.numeric(target_mean_degree) || length(target_mean_degree) != 1L ||
      !is.finite(target_mean_degree)) {
      rlang::abort("`target_mean_degree` must be a finite numeric scalar.")
    }
    if (target_mean_degree <= 0) {
      rlang::abort("`target_mean_degree` must be > 0.")
    }
    if (target_mean_degree > (n_items - 1L)) {
      rlang::abort("`target_mean_degree` must be <= N - 1.")
    }
  }

  list(
    ids = ids,
    n_items = n_items,
    min_degree = min_degree,
    target_mean_degree = target_mean_degree
  )
}

#' @keywords internal
#' @noRd
warm_start <- function(ids, config, seed = NULL) {
  validated <- .warm_start_validate_config(ids, config)
  ids <- validated$ids
  n_items <- validated$n_items
  min_degree <- validated$min_degree
  target_mean_degree <- validated$target_mean_degree

  deg <- stats::setNames(rep.int(0L, n_items), ids)
  id_pos <- stats::setNames(seq_len(n_items), ids)
  pairs_i <- character()
  pairs_j <- character()
  pair_keys <- character()

  idx_next <- c(seq_len(n_items - 1L) + 1L, 1L)
  for (idx in seq_len(n_items)) {
    i_id <- ids[[idx]]
    j_id <- ids[[idx_next[[idx]]]]
    key <- make_unordered_key(i_id, j_id)
    if (key %in% pair_keys) next

    ordered <- if (id_pos[[i_id]] < id_pos[[j_id]]) {
      c(i_id, j_id)
    } else {
      c(j_id, i_id)
    }
    pairs_i <- c(pairs_i, ordered[[1L]])
    pairs_j <- c(pairs_j, ordered[[2L]])
    pair_keys <- c(pair_keys, key)
    deg[[i_id]] <- deg[[i_id]] + 1L
    deg[[j_id]] <- deg[[j_id]] + 1L
  }

  if (min_degree > 2L) {
    completed <- .pairwiseLLM_with_seed(seed, function() {
      deg_local <- deg
      pairs_i_local <- pairs_i
      pairs_j_local <- pairs_j
      pair_keys_local <- pair_keys

      while (any(deg_local < min_degree)) {
        deficit <- ids[deg_local[ids] < min_degree]
        i_id <- deficit[[1L]]

        candidates <- ids[ids != i_id]
        candidate_keys <- make_unordered_key(i_id, candidates)
        eligible <- candidates[!candidate_keys %in% pair_keys_local]

        if (length(eligible) == 0L) {
          neighbors <- unique(c(
            pairs_j_local[pairs_i_local == i_id],
            pairs_i_local[pairs_j_local == i_id]
          ))
          neighbor_count <- length(neighbors)
          rlang::abort(paste0(
            "Warm start min-degree completion stalled: no eligible partners. ",
            "N=", n_items,
            ", i=", i_id,
            ", deg[i]=", deg_local[[i_id]],
            ", neighbor_count=", neighbor_count,
            "."
          ))
        }

        j_id <- sample(eligible, size = 1L)
        key <- make_unordered_key(i_id, j_id)
        if (key %in% pair_keys_local) next

        ordered <- if (id_pos[[i_id]] < id_pos[[j_id]]) {
          c(i_id, j_id)
        } else {
          c(j_id, i_id)
        }
        pairs_i_local <- c(pairs_i_local, ordered[[1L]])
        pairs_j_local <- c(pairs_j_local, ordered[[2L]])
        pair_keys_local <- c(pair_keys_local, key)
        deg_local[[i_id]] <- deg_local[[i_id]] + 1L
        deg_local[[j_id]] <- deg_local[[j_id]] + 1L
      }

      list(
        deg = deg_local,
        pairs_i = pairs_i_local,
        pairs_j = pairs_j_local,
        pair_keys = pair_keys_local
      )
    })

    deg <- completed$deg
    pairs_i <- completed$pairs_i
    pairs_j <- completed$pairs_j
    pair_keys <- completed$pair_keys
  }

  if (any(deg < min_degree)) {
    rlang::abort("Warm start failed to reach the requested `min_degree`.")
  }

  if (!is.null(target_mean_degree)) {
    max_edges <- floor(target_mean_degree * n_items / 2)
    max_edges <- min(max_edges, choose(n_items, 2L))
    current_edges <- length(pair_keys)

    if (max_edges > current_edges) {
      combos <- utils::combn(ids, 2L)
      all_i <- combos[1L, ]
      all_j <- combos[2L, ]
      all_keys <- make_unordered_key(all_i, all_j)
      remaining <- which(!all_keys %in% pair_keys)
      needed <- as.integer(max_edges - current_edges)
      needed <- min(needed, length(remaining))

      if (needed > 0L) {
        picked <- .pairwiseLLM_with_seed(seed, function() {
          sample(remaining, size = needed, replace = FALSE)
        })
        for (idx in picked) {
          i_id <- all_i[[idx]]
          j_id <- all_j[[idx]]
          key <- all_keys[[idx]]
          if (key %in% pair_keys) next
          ordered <- if (id_pos[[i_id]] < id_pos[[j_id]]) {
            c(i_id, j_id)
          } else {
            c(j_id, i_id)
          }
          pairs_i <- c(pairs_i, ordered[[1L]])
          pairs_j <- c(pairs_j, ordered[[2L]])
          pair_keys <- c(pair_keys, key)
          current_edges <- current_edges + 1L
          if (current_edges >= max_edges) break
        }
      }
    }
  }

  tibble::tibble(
    i = as.character(pairs_i),
    j = as.character(pairs_j)
  )
}
