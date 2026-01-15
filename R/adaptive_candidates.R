# -------------------------------------------------------------------------
# Adaptive candidate construction helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
compute_ranking_from_theta_mean <- function(theta_mean) {
  if (!is.numeric(theta_mean) || length(theta_mean) < 1L) {
    rlang::abort("`theta_mean` must be a non-empty numeric vector.")
  }
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
