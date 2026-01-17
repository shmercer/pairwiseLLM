# -------------------------------------------------------------------------
# Adaptive constraints and key helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
make_unordered_key <- function(id1, id2) {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  paste(pmin(id1, id2), pmax(id1, id2), sep = ":")
}

#' @keywords internal
#' @noRd
make_ordered_key <- function(A_id, B_id) {
  paste(as.character(A_id), as.character(B_id), sep = ":")
}

#' @keywords internal
#' @noRd
pair_uid_from_state <- function(state, unordered_key) {
  counts <- state$unordered_count
  if (is.null(names(counts)) || length(counts) == 0L) {
    current <- 0L
  } else if (unordered_key %in% names(counts)) {
    current <- counts[[unordered_key]]
  } else {
    current <- 0L
  }
  paste0(unordered_key, "#", as.integer(current) + 1L)
}

#' @keywords internal
#' @noRd
duplicate_allowed <- function(state, A_id, B_id) {
  unordered_key <- make_unordered_key(A_id, B_id)
  ordered_key <- make_ordered_key(A_id, B_id)
  reverse_key <- make_ordered_key(B_id, A_id)

  counts <- state$unordered_count
  if (is.null(names(counts)) || length(counts) == 0L || !unordered_key %in% names(counts)) {
    return(TRUE)
  }

  count <- counts[[unordered_key]]
  if (count == 0L) return(TRUE)
  if (count >= 2L) return(FALSE)

  seen <- state$ordered_seen
  if (is.environment(seen)) {
    reverse_seen <- isTRUE(seen[[reverse_key]])
    same_seen <- isTRUE(seen[[ordered_key]])
  } else {
    reverse_seen <- isTRUE(seen[reverse_key])
    same_seen <- isTRUE(seen[ordered_key])
  }

  if (same_seen) return(FALSE)
  reverse_seen
}

#' @keywords internal
#' @noRd
record_exposure <- function(state, A_id, B_id) {
  if (!A_id %in% state$ids || !B_id %in% state$ids) {
    rlang::abort("`A_id` and `B_id` must exist in `state$ids`.")
  }

  ordered_key <- make_ordered_key(A_id, B_id)
  unordered_key <- make_unordered_key(A_id, B_id)

  state$pos1[A_id] <- state$pos1[A_id] + 1L
  state$pos2[B_id] <- state$pos2[B_id] + 1L
  if (is.null(state$pos_count)) {
    state$pos_count <- state$pos1
  } else {
    state$pos_count[A_id] <- state$pos_count[A_id] + 1L
  }
  state$deg[A_id] <- state$deg[A_id] + 1L
  state$deg[B_id] <- state$deg[B_id] + 1L
  state$imb <- state$pos1 - state$pos2

  counts <- state$unordered_count
  if (is.null(names(counts)) || length(counts) == 0L) {
    counts <- integer()
  }
  if (!unordered_key %in% names(counts)) {
    counts[unordered_key] <- 0L
  }
  counts[unordered_key] <- counts[unordered_key] + 1L
  count_names <- names(counts)
  counts <- as.integer(counts)
  names(counts) <- count_names
  state$unordered_count <- counts
  state$pair_count <- counts

  seen <- state$ordered_seen
  if (is.environment(seen)) {
    seen[[ordered_key]] <- TRUE
    state$ordered_seen <- seen
  } else {
    if (is.null(names(seen)) || length(seen) == 0L) {
      seen <- logical()
    }
    seen[ordered_key] <- TRUE
    state$ordered_seen <- seen
  }

  ordered_counts <- state$pair_ordered_count
  if (is.null(ordered_counts) || length(ordered_counts) == 0L) {
    ordered_counts <- integer()
  }
  if (is.null(names(ordered_counts)) || !ordered_key %in% names(ordered_counts)) {
    ordered_counts[ordered_key] <- 0L
  }
  ordered_counts[ordered_key] <- ordered_counts[ordered_key] + 1L
  ordered_names <- names(ordered_counts)
  ordered_counts <- as.integer(ordered_counts)
  names(ordered_counts) <- ordered_names
  state$pair_ordered_count <- ordered_counts

  state
}

.adaptive_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  eval.parent(substitute(expr))
}

#' @keywords internal
#' @noRd
choose_order_with_position_balance <- function(state, i_id, j_id, seed = NULL) {
  if (!i_id %in% state$ids || !j_id %in% state$ids) {
    rlang::abort("`i_id` and `j_id` must exist in `state$ids`.")
  }
  imb_i <- state$imb[[i_id]]
  imb_j <- state$imb[[j_id]]

  if (imb_i > imb_j) {
    return(list(A_id = j_id, B_id = i_id))
  }
  if (imb_j > imb_i) {
    return(list(A_id = i_id, B_id = j_id))
  }

  pick_first <- .adaptive_with_seed(seed, stats::runif(1) < 0.5)
  if (pick_first) {
    list(A_id = i_id, B_id = j_id)
  } else {
    list(A_id = j_id, B_id = i_id)
  }
}
