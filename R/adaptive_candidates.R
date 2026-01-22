# -------------------------------------------------------------------------
# Adaptive candidate construction helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
compute_ranking_from_theta_mean <- function(theta_mean, state) {
  if (!is.numeric(theta_mean) || length(theta_mean) < 1L) {
    rlang::abort("`theta_mean` must be a non-empty numeric vector.")
  }
  validate_state(state)
  ids <- names(theta_mean)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`theta_mean` must be a named numeric vector with non-empty names.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`theta_mean` names must be unique.")
  }
  if (anyNA(theta_mean)) {
    rlang::abort("`theta_mean` must not contain missing values.")
  }
  if (!setequal(ids, state$ids) || length(ids) != length(state$ids)) {
    rlang::abort("`theta_mean` names must match `state$ids`.")
  }

  ord <- order(-theta_mean, ids)
  as.character(ids[ord])
}

#' @keywords internal
#' @noRd
select_window_size <- function(N, phase = c("phase2", "phase3"), near_stop = FALSE) {
  if (!is.numeric(N) || length(N) != 1L || is.na(N) || N < 1) {
    rlang::abort("`N` must be a positive numeric value.")
  }
  phase <- match.arg(phase)
  if (!is.logical(near_stop) || length(near_stop) != 1L || is.na(near_stop)) {
    rlang::abort("`near_stop` must be TRUE or FALSE.")
  }

  if (near_stop) {
    phase <- "phase3"
  }

  if (phase == "phase2") {
    if (N <= 500) {
      return(50L)
    }
    return(100L)
  }

  window <- floor(N / 10)
  window <- max(25L, min(50L, as.integer(window)))
  as.integer(window)
}

.adaptive_v3_theta_summary <- function(theta_summary, state) {
  if (!is.data.frame(theta_summary)) {
    rlang::abort("`theta_summary` must be a data frame or tibble.")
  }
  theta_summary <- tibble::as_tibble(theta_summary)

  id_col <- NULL
  if ("item_id" %in% names(theta_summary)) {
    id_col <- "item_id"
  } else if ("ID" %in% names(theta_summary)) {
    id_col <- "ID"
  }
  if (is.null(id_col)) {
    rlang::abort("`theta_summary` must include an `item_id` column.")
  }

  required <- c(id_col, "theta_mean", "theta_sd")
  missing <- setdiff(required, names(theta_summary))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "`theta_summary` is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }

  ids <- as.character(theta_summary[[id_col]])
  if (anyNA(ids) || any(ids == "")) {
    rlang::abort("`theta_summary$item_id` must be non-empty.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`theta_summary$item_id` must be unique.")
  }
  if (!setequal(ids, state$ids) || length(ids) != length(state$ids)) {
    rlang::abort("`theta_summary$item_id` must match `state$ids`.")
  }

  theta_mean <- as.double(theta_summary$theta_mean)
  theta_sd <- as.double(theta_summary$theta_sd)
  if (length(theta_mean) != length(ids) || length(theta_sd) != length(ids)) {
    rlang::abort("`theta_summary` columns must align with `item_id` length.")
  }
  if (anyNA(theta_mean) || anyNA(theta_sd)) {
    rlang::abort("`theta_summary` must not include missing values.")
  }

  tibble::tibble(
    item_id = ids,
    theta_mean = theta_mean,
    theta_sd = theta_sd,
    deg = as.integer(state$deg[ids])
  )
}

.adaptive_theta_summary_from_fit <- function(fit, state) {
  if (!is.list(fit) || is.null(fit$theta_draws) || is.null(fit$theta_mean)) {
    rlang::abort("`fit` must include `theta_draws` and `theta_mean`.")
  }
  theta_draws <- fit$theta_draws
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix.")
  }
  ids <- colnames(theta_draws)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`fit$theta_draws` must have non-empty column names.")
  }
  if (!setequal(ids, state$ids) || length(ids) != length(state$ids)) {
    rlang::abort("`fit$theta_draws` columns must match `state$ids`.")
  }
  theta_mean <- fit$theta_mean
  if (!is.numeric(theta_mean) || length(theta_mean) != length(state$ids)) {
    rlang::abort("`fit$theta_mean` must be a numeric vector over `state$ids`.")
  }
  if (is.null(names(theta_mean))) {
    names(theta_mean) <- ids
  }
  theta_mean <- as.double(theta_mean[state$ids])
  theta_sd <- as.double(apply(theta_draws[, state$ids, drop = FALSE], 2, stats::sd))

  tibble::tibble(
    item_id = as.character(state$ids),
    theta_mean = theta_mean,
    theta_sd = theta_sd
  )
}

#' @keywords internal
#' @noRd
select_anchors <- function(theta_summary, state, config) {
  validate_state(state)
  theta_summary <- .adaptive_v3_theta_summary(theta_summary, state)

  total <- min(as.integer(config$A_anchors), nrow(theta_summary))
  if (is.na(total) || total < 1L) {
    rlang::abort("`config$A_anchors` must be a positive integer.")
  }
  if (total == 0L) {
    return(character())
  }

  top_order <- theta_summary[order(-theta_summary$theta_mean, theta_summary$item_id), , drop = FALSE]
  bottom_order <- theta_summary[order(theta_summary$theta_mean, theta_summary$item_id), , drop = FALSE]

  if (total == 1L) {
    return(as.character(top_order$item_id[1L]))
  }

  edge_count <- min(floor(total / 2L), nrow(theta_summary))
  top_ids <- utils::head(top_order$item_id, edge_count)
  bottom_ids <- utils::head(bottom_order$item_id, edge_count)
  anchors <- unique(c(top_ids, bottom_ids))

  if (length(anchors) < total) {
    remaining_ids <- setdiff(theta_summary$item_id, anchors)
    if (length(remaining_ids) > 0L) {
      remaining_tbl <- theta_summary[theta_summary$item_id %in% remaining_ids, , drop = FALSE]
      remaining_tbl <- remaining_tbl[
        order(-remaining_tbl$theta_sd, remaining_tbl$deg, remaining_tbl$item_id),
        ,
        drop = FALSE
      ]
      need <- total - length(anchors)
      anchors <- c(anchors, utils::head(remaining_tbl$item_id, need))
    }
  }

  as.character(anchors)
}

#' @keywords internal
#' @noRd
generate_candidates_streamed <- function(
    ranked_ids,
    anchors,
    W,
    cap,
    mode = c("window_stream", "global_sample"),
    seed = NULL
) {
  ranked_ids <- as.character(ranked_ids)
  if (length(ranked_ids) < 2L) {
    rlang::abort("`ranked_ids` must contain at least two ids.")
  }
  if (anyNA(ranked_ids) || any(ranked_ids == "")) {
    rlang::abort("`ranked_ids` must be non-empty and non-missing.")
  }
  if (anyDuplicated(ranked_ids)) {
    rlang::abort("`ranked_ids` must be unique.")
  }

  anchors <- unique(as.character(anchors))
  anchors <- anchors[!is.na(anchors) & anchors != ""]
  anchors <- anchors[anchors %in% ranked_ids]
  if (length(anchors) == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  W <- as.integer(W)
  if (is.na(W) || W < 1L) {
    rlang::abort("`W` must be a positive integer.")
  }
  cap <- as.integer(cap)
  if (is.na(cap) || cap < 1L) {
    rlang::abort("`cap` must be a positive integer.")
  }

  n_ids <- length(ranked_ids)
  max_pairs <- as.integer(n_ids * (n_ids - 1L) / 2)
  cap <- min(cap, max_pairs)
  if (cap < 1L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  mode <- match.arg(mode)

  i_vals <- character()
  j_vals <- character()
  n_added <- 0L

  if (mode == "window_stream") {
    rank_map <- stats::setNames(seq_len(n_ids), ranked_ids)
    seen <- new.env(parent = emptyenv())
    i_vals <- character(cap)
    j_vals <- character(cap)

    for (anchor_id in anchors) {
      anchor_rank <- rank_map[[anchor_id]]
      if (is.null(anchor_rank)) next
      lower <- max(1L, anchor_rank - W)
      upper <- min(n_ids, anchor_rank + W)
      window_ids <- ranked_ids[lower:upper]
      window_ids <- window_ids[window_ids != anchor_id]
      if (length(window_ids) == 0L) next

      for (partner_id in window_ids) {
        i_id <- pmin(anchor_id, partner_id)
        j_id <- pmax(anchor_id, partner_id)
        if (i_id == j_id) next
        unordered_key <- make_unordered_key(i_id, j_id)
        if (exists(unordered_key, envir = seen, inherits = FALSE)) next

        n_added <- n_added + 1L
        i_vals[[n_added]] <- i_id
        j_vals[[n_added]] <- j_id
        seen[[unordered_key]] <- TRUE
        if (n_added >= cap) break
      }
      if (n_added >= cap) break
    }

    if (n_added > 0L) {
      i_vals <- i_vals[seq_len(n_added)]
      j_vals <- j_vals[seq_len(n_added)]
    } else {
      i_vals <- character()
      j_vals <- character()
    }
  } else {
    counts <- seq.int(n_ids - 1L, 1L, by = -1L)
    cumulative <- cumsum(counts)
    total_pairs <- cumulative[[length(cumulative)]]
    cap <- min(cap, total_pairs)
    if (cap > 0L) {
      pair_idx <- .pairwiseLLM_with_seed(seed, function() {
        sample.int(total_pairs, size = cap, replace = FALSE)
      })
      i_idx <- findInterval(pair_idx - 1L, cumulative) + 1L
      prev <- numeric(length(i_idx))
      use_idx <- i_idx > 1L
      if (any(use_idx)) {
        prev[use_idx] <- cumulative[i_idx[use_idx] - 1L]
      }
      j_offset <- pair_idx - prev
      j_idx <- i_idx + j_offset
      i_vals <- ranked_ids[i_idx]
      j_vals <- ranked_ids[j_idx]
      n_added <- length(i_vals)
    }
  }

  if (n_added == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  out <- tibble::tibble(
    i = as.character(i_vals),
    j = as.character(j_vals),
    unordered_key = make_unordered_key(i_vals, j_vals)
  )
  out <- dplyr::arrange(out, .data$unordered_key)
  out <- dplyr::select(out, "i", "j")
  tibble::as_tibble(out)
}

#' @keywords internal
#' @noRd
enumerate_candidates <- function(anchors, theta_summary, state, config) {
  validate_state(state)
  theta_summary <- .adaptive_v3_theta_summary(theta_summary, state)

  anchors <- unique(as.character(anchors))
  anchors <- anchors[anchors %in% theta_summary$item_id]
  if (length(anchors) == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  W <- as.integer(config$W)
  if (is.na(W) || W < 1L) {
    rlang::abort("`config$W` must be a positive integer.")
  }

  cap <- as.integer(config$C_max)
  if (is.na(cap) || cap < 1L) {
    rlang::abort("`config$C_max` must be a positive integer.")
  }

  ord <- order(-theta_summary$theta_mean, theta_summary$item_id)
  ranked_ids <- theta_summary$item_id[ord]

  generate_candidates_streamed(
    ranked_ids = ranked_ids,
    anchors = anchors,
    W = W,
    cap = cap,
    mode = "window_stream",
    seed = state$seed
  )
}

#' @keywords internal
#' @noRd
generate_candidates <- function(theta_summary, state, config) {
  validate_state(state)
  theta_summary <- .adaptive_v3_theta_summary(theta_summary, state)
  anchors <- select_anchors(theta_summary, state, config)
  candidates <- enumerate_candidates(anchors, theta_summary, state, config)
  tibble::as_tibble(candidates)
}

#' @keywords internal
#' @noRd
generate_candidates_from_anchors <- function(anchors, theta_summary, state, config) {
  validate_state(state)
  theta_summary <- .adaptive_v3_theta_summary(theta_summary, state)
  anchors <- unique(as.character(anchors))
  anchors <- anchors[!is.na(anchors) & anchors != ""]
  anchors <- anchors[anchors %in% theta_summary$item_id]
  if (length(anchors) == 0L) {
    return(tibble::tibble(i = character(), j = character()))
  }

  W <- as.integer(config$W)
  if (is.na(W) || W < 1L) {
    rlang::abort("`config$W` must be a positive integer.")
  }
  cap <- as.integer(config$C_max)
  if (is.na(cap) || cap < 1L) {
    rlang::abort("`config$C_max` must be a positive integer.")
  }

  ord <- order(-theta_summary$theta_mean, theta_summary$item_id)
  ranked_ids <- theta_summary$item_id[ord]

  global_safe <- length(anchors) == length(ranked_ids) && W >= length(ranked_ids) - 1L
  mode <- if (isTRUE(global_safe)) "global_sample" else "window_stream"

  generate_candidates_streamed(
    ranked_ids = ranked_ids,
    anchors = anchors,
    W = W,
    cap = cap,
    mode = mode,
    seed = state$seed
  )
}

.adaptive_unordered_allowed <- function(state, i_id, j_id) {
  duplicate_allowed(state, i_id, j_id) || duplicate_allowed(state, j_id, i_id)
}

#' @keywords internal
#' @noRd
build_candidate_pairs <- function(
    ranking_ids,
    W,
    state,
    exploration_frac = 0.05,
    seed = NULL
) {
  validate_state(state)
  ranking_ids <- as.character(ranking_ids)
  if (length(ranking_ids) < 2L) {
    rlang::abort("`ranking_ids` must contain at least two ids.")
  }
  if (anyDuplicated(ranking_ids)) {
    rlang::abort("`ranking_ids` must not contain duplicates.")
  }
  if (!setequal(ranking_ids, state$ids) || length(ranking_ids) != length(state$ids)) {
    rlang::abort("`ranking_ids` must match `state$ids`.")
  }
  W <- as.integer(W)
  if (is.na(W) || W < 1L) {
    rlang::abort("`W` must be a positive integer.")
  }
  if (!is.numeric(exploration_frac) || is.na(exploration_frac) ||
    exploration_frac < 0 || exploration_frac > 1) {
    rlang::abort("`exploration_frac` must be between 0 and 1.")
  }

  n_ids <- length(ranking_ids)
  i_list <- vector("list", n_ids)
  j_list <- vector("list", n_ids)

  for (idx in seq_len(n_ids)) {
    lower <- max(1L, idx - W)
    upper <- min(n_ids, idx + W)
    window_ids <- ranking_ids[lower:upper]
    window_ids <- window_ids[window_ids != ranking_ids[idx]]
    if (length(window_ids) > 0L) {
      i_list[[idx]] <- rep(ranking_ids[idx], length(window_ids))
      j_list[[idx]] <- window_ids
    } else {
      i_list[[idx]] <- character()
      j_list[[idx]] <- character()
    }
  }

  i_vals <- unlist(i_list, use.names = FALSE)
  j_vals <- unlist(j_list, use.names = FALSE)
  if (length(i_vals) == 0L) {
    window_tbl <- tibble::tibble(i_id = character(), j_id = character(), unordered_key = character())
  } else {
    i_id <- pmin(i_vals, j_vals)
    j_id <- pmax(i_vals, j_vals)
    unordered_key <- make_unordered_key(i_id, j_id)
    window_tbl <- tibble::tibble(i_id = i_id, j_id = j_id, unordered_key = unordered_key)
    window_tbl <- window_tbl[window_tbl$i_id != window_tbl$j_id, , drop = FALSE]
    window_tbl <- dplyr::distinct(window_tbl, unordered_key, .keep_all = TRUE)
    allowed <- mapply(
      function(i, j) .adaptive_unordered_allowed(state, i, j),
      window_tbl$i_id,
      window_tbl$j_id,
      USE.NAMES = FALSE
    )
    window_tbl <- window_tbl[allowed, , drop = FALSE]
  }

  all_pairs <- utils::combn(state$ids, 2)
  all_i <- pmin(all_pairs[1L, ], all_pairs[2L, ])
  all_j <- pmax(all_pairs[1L, ], all_pairs[2L, ])
  all_unordered <- make_unordered_key(all_i, all_j)
  all_tbl <- tibble::tibble(i_id = all_i, j_id = all_j, unordered_key = all_unordered)
  allowed_all <- mapply(
    function(i, j) .adaptive_unordered_allowed(state, i, j),
    all_tbl$i_id,
    all_tbl$j_id,
    USE.NAMES = FALSE
  )
  all_tbl <- all_tbl[allowed_all, , drop = FALSE]

  remaining_tbl <- all_tbl
  if (nrow(window_tbl) > 0L) {
    remaining_tbl <- remaining_tbl[!remaining_tbl$unordered_key %in% window_tbl$unordered_key, , drop = FALSE]
  }

  explore_tbl <- tibble::tibble(i_id = character(), j_id = character(), unordered_key = character())
  if (exploration_frac > 0 && nrow(remaining_tbl) > 0L) {
    target <- round(exploration_frac * nrow(window_tbl))
    if (target < 1L) {
      target <- 1L
    }
    target <- min(as.integer(target), nrow(remaining_tbl))
    if (target > 0L) {
      picks <- .pairwiseLLM_with_seed(seed, function() {
        sample.int(nrow(remaining_tbl), size = target)
      })
      explore_tbl <- remaining_tbl[picks, , drop = FALSE]
    }
  }

  out <- dplyr::bind_rows(window_tbl, explore_tbl)
  out <- dplyr::distinct(out, unordered_key, .keep_all = TRUE)
  tibble::as_tibble(out)
}
