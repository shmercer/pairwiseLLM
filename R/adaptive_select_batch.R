# -------------------------------------------------------------------------
# Adaptive v3 batch selection and ordering
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.adaptive_last_order_for_pair <- function(state, unordered_key) {
  results <- state$history_results
  if (is.null(results) || nrow(results) == 0L) return(NULL)
  idx <- which(results$unordered_key == unordered_key)
  if (length(idx) == 0L) return(NULL)
  last_idx <- max(idx)
  list(
    A_id = as.character(results$A_id[[last_idx]]),
    B_id = as.character(results$B_id[[last_idx]])
  )
}

#' @keywords internal
#' @noRd
.adaptive_duplicate_allowed <- function(state, unordered_key, p_mean, utility, config,
                                        policy = c("default", "relaxed")) {
  policy <- match.arg(policy)
  counts <- state$pair_count
  count <- if (!is.null(names(counts)) && unordered_key %in% names(counts)) {
    counts[[unordered_key]]
  } else {
    0L
  }
  if (is.na(count) || count <= 0L) {
    return(TRUE)
  }
  last_order <- .adaptive_last_order_for_pair(state, unordered_key)
  if (is.null(last_order)) {
    return(FALSE)
  }
  if (count == 1L) {
    return(TRUE)
  }
  if (policy != "relaxed") {
    return(FALSE)
  }

  if (!is.numeric(p_mean) || length(p_mean) != 1L || !is.finite(p_mean)) return(FALSE)
  if (!is.numeric(utility) || length(utility) != 1L || !is.finite(utility)) return(FALSE)
  if (abs(p_mean - 0.5) > config$dup_p_margin) return(FALSE)

  threshold <- state$posterior$U_dup_threshold
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) return(FALSE)
  if (utility < threshold) return(FALSE)

  max_count <- as.integer(config$dup_max_count)
  if (policy == "relaxed") {
    max_count <- max_count + 1L
  }
  if (count >= max_count) return(FALSE)
  TRUE
}

#' @keywords internal
#' @noRd
.adaptive_filter_duplicate_candidates <- function(candidates_with_utility, state, config,
                                                  dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)
  required <- c("i_id", "j_id", "unordered_key", "utility", "p_mean")
  .adaptive_required_cols(candidates_with_utility, "candidates_with_utility", required)

  if (nrow(candidates_with_utility) == 0L) {
    return(candidates_with_utility)
  }

  i_id <- as.character(candidates_with_utility$i_id)
  j_id <- as.character(candidates_with_utility$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates_with_utility` ids must exist in `state$ids`.")
  }

  keep <- i_id != j_id
  if (!any(keep)) {
    return(candidates_with_utility[0, , drop = FALSE])
  }
  candidates_with_utility <- candidates_with_utility[keep, , drop = FALSE]

  unordered_key <- as.character(candidates_with_utility$unordered_key)
  counts <- state$pair_count
  if (is.null(names(counts)) || length(counts) == 0L) {
    counts <- integer()
  }
  count_vals <- counts[unordered_key]
  count_vals[is.na(count_vals)] <- 0L
  repeat_mask <- count_vals >= 2L

  if (!any(repeat_mask)) {
    return(candidates_with_utility)
  }

  if (dup_policy != "relaxed") {
    return(candidates_with_utility[!repeat_mask, , drop = FALSE])
  }

  threshold <- state$posterior$U_dup_threshold
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) {
    return(candidates_with_utility[!repeat_mask, , drop = FALSE])
  }

  max_count <- as.integer(config$dup_max_count)
  if (dup_policy == "relaxed") {
    max_count <- max_count + 1L
  }

  p_mean <- as.double(candidates_with_utility$p_mean)
  utility <- as.double(candidates_with_utility$utility)
  allowed_repeats <- is.finite(p_mean) & is.finite(utility) &
    abs(p_mean - 0.5) <= config$dup_p_margin &
    utility >= threshold &
    count_vals < max_count

  allowed <- !repeat_mask | allowed_repeats
  candidates_with_utility[allowed, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.adaptive_inflight_unordered_keys <- function(state) {
  pairs <- state$history_pairs
  if (is.null(pairs) || nrow(pairs) == 0L) return(character())
  results <- state$history_results
  observed <- if (is.null(results) || nrow(results) == 0L) {
    character()
  } else {
    as.character(results$pair_uid)
  }
  pending <- !(as.character(pairs$pair_uid) %in% observed)
  if (!any(pending)) return(character())
  unique(as.character(pairs$unordered_key[pending]))
}

#' @keywords internal
#' @noRd
.adaptive_filter_inflight_pairs <- function(pairs, state) {
  pairs <- tibble::as_tibble(pairs)
  inflight <- .adaptive_inflight_unordered_keys(state)
  if (length(inflight) == 0L) return(pairs)
  pairs[!as.character(pairs$unordered_key) %in% inflight, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.adaptive_candidate_after_filters <- function(candidates_with_utility, state, config,
                                              dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)
  required <- c("i_id", "j_id", "unordered_key", "utility", "p_mean")
  .adaptive_required_cols(candidates_with_utility, "candidates_with_utility", required)

  if (nrow(candidates_with_utility) == 0L) {
    return(0L)
  }

  i_id <- as.character(candidates_with_utility$i_id)
  j_id <- as.character(candidates_with_utility$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates_with_utility` ids must exist in `state$ids`.")
  }
  keep <- i_id != j_id
  if (!any(keep)) {
    return(0L)
  }
  candidates_with_utility <- candidates_with_utility[keep, , drop = FALSE]

  unordered_key <- as.character(candidates_with_utility$unordered_key)
  p_mean <- as.double(candidates_with_utility$p_mean)
  utility <- as.double(candidates_with_utility$utility)
  counts <- state$pair_count[unordered_key]
  counts[is.na(counts)] <- 0L

  allowed <- vapply(seq_len(nrow(candidates_with_utility)), function(idx) {
    count <- counts[[idx]]
    if (is.na(count) || count <= 0L) {
      return(TRUE)
    }
    .adaptive_duplicate_allowed(
      state = state,
      unordered_key = unordered_key[[idx]],
      p_mean = p_mean[[idx]],
      utility = utility[[idx]],
      config = config,
      policy = dup_policy
    )
  }, logical(1L))

  as.integer(sum(allowed))
}

#' @keywords internal
#' @noRd
sample_exploration_pairs <- function(state, candidates, n_explore, config,
                                     dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  candidates <- tibble::as_tibble(candidates)
  required <- c("i_id", "j_id", "unordered_key", "utility", "p_mean")
  .adaptive_required_cols(candidates, "candidates", required)
  if (!"utility_raw" %in% names(candidates)) {
    candidates$utility_raw <- candidates$utility
  }

  n_explore <- as.integer(n_explore)
  if (is.na(n_explore) || n_explore < 0L) {
    rlang::abort("`n_explore` must be a non-negative integer.")
  }
  if (n_explore == 0L) {
    return(candidates[0, , drop = FALSE])
  }

  i_id <- as.character(candidates$i_id)
  j_id <- as.character(candidates$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates` ids must exist in `state$ids`.")
  }
  candidates <- candidates[i_id != j_id, , drop = FALSE]

  lookup <- candidates[, c("unordered_key", "utility", "utility_raw", "p_mean"), drop = FALSE]
  lookup$unordered_key <- as.character(lookup$unordered_key)
  key_idx <- stats::setNames(seq_len(nrow(lookup)), lookup$unordered_key)

  weights <- 1 / (state$deg + 1)
  weights <- weights[state$ids]
  weights <- stats::setNames(as.double(weights), state$ids)

  picked <- vector("list", n_explore)
  picked_keys <- character()
  accepted <- 0L
  attempts <- 0L
  max_attempts <- max(20L, n_explore * 50L)

  while (accepted < n_explore && attempts < max_attempts) {
    attempts <- attempts + 1L
    i_id <- sample(state$ids, size = 1L, prob = weights)
    j_id <- sample(state$ids, size = 1L, prob = weights)
    if (identical(i_id, j_id)) next
    unordered_key <- make_unordered_key(i_id, j_id)
    if (unordered_key %in% picked_keys) next

    counts <- state$pair_count
    if (!is.integer(counts) || is.null(names(counts)) || length(counts) == 0L) {
      count <- 0L
    } else {
      count <- counts[unordered_key]
      if (length(count) == 0L || is.na(count)) {
        count <- 0L
      }
    }
    if (!is.na(count) && count >= 1L) {
      if (!unordered_key %in% names(key_idx)) next
      idx <- key_idx[[unordered_key]]
      utility <- lookup$utility[[idx]]
      p_mean <- lookup$p_mean[[idx]]
      if (!.adaptive_duplicate_allowed(
        state,
        unordered_key,
        p_mean,
        utility,
        config,
        policy = dup_policy
      )) {
        next
      }
      utility_raw <- lookup$utility_raw[[idx]]
    } else {
      utility <- NA_real_
      utility_raw <- NA_real_
      p_mean <- NA_real_
    }

    accepted <- accepted + 1L
    picked[[accepted]] <- tibble::tibble(
      i_id = as.character(i_id),
      j_id = as.character(j_id),
      unordered_key = as.character(unordered_key),
      utility = as.double(utility),
      utility_raw = as.double(utility_raw),
      p_mean = as.double(p_mean)
    )
    picked_keys <- c(picked_keys, unordered_key)
  }

  if (accepted == 0L) {
    return(candidates[0, , drop = FALSE])
  }
  dplyr::bind_rows(picked[seq_len(accepted)])
}

#' @keywords internal
#' @noRd
select_exploitation_pairs <- function(candidates_with_utility, state, n_exploit, config,
                                      dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)
  required <- c("i_id", "j_id", "unordered_key", "utility", "p_mean")
  .adaptive_required_cols(candidates_with_utility, "candidates_with_utility", required)

  n_exploit <- as.integer(n_exploit)
  if (is.na(n_exploit) || n_exploit < 0L) {
    rlang::abort("`n_exploit` must be a non-negative integer.")
  }
  if (n_exploit == 0L || nrow(candidates_with_utility) == 0L) {
    return(candidates_with_utility[0, , drop = FALSE])
  }

  i_id <- as.character(candidates_with_utility$i_id)
  j_id <- as.character(candidates_with_utility$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates_with_utility` ids must exist in `state$ids`.")
  }
  candidates_with_utility <- candidates_with_utility[i_id != j_id, , drop = FALSE]
  if (nrow(candidates_with_utility) == 0L) {
    return(candidates_with_utility[0, , drop = FALSE])
  }

  ordered <- candidates_with_utility[order(
    -candidates_with_utility$utility,
    candidates_with_utility$unordered_key,
    as.character(candidates_with_utility$i_id),
    as.character(candidates_with_utility$j_id)
  ), , drop = FALSE]

  selected <- vector("list", n_exploit)
  selected_keys <- character()
  accepted <- 0L
  for (idx in seq_len(nrow(ordered))) {
    if (accepted >= n_exploit) break
    row <- ordered[idx, , drop = FALSE]
    unordered_key <- as.character(row$unordered_key)
    if (unordered_key %in% selected_keys) next

    if (!.adaptive_duplicate_allowed(
      state,
      unordered_key,
      row$p_mean[[1L]],
      row$utility[[1L]],
      config,
      policy = dup_policy
    )) {
      next
    }

    accepted <- accepted + 1L
    selected[[accepted]] <- row
    selected_keys <- c(selected_keys, unordered_key)
  }

  if (accepted == 0L) {
    return(ordered[0, , drop = FALSE])
  }
  dplyr::bind_rows(selected[seq_len(accepted)])
}

#' @keywords internal
#' @noRd
assign_order <- function(pairs, state) {
  validate_state(state)
  if (!is.data.frame(pairs)) {
    rlang::abort("`pairs` must be a data frame or tibble.")
  }
  pairs <- tibble::as_tibble(pairs)
  required <- c("i_id", "j_id", "unordered_key")
  .adaptive_required_cols(pairs, "pairs", required)

  pairs <- .adaptive_filter_inflight_pairs(pairs, state)
  if (nrow(pairs) == 0L) {
    pairs$A_id <- character()
    pairs$B_id <- character()
    return(pairs)
  }

  seed_use <- state$seed
  balanced_margin <- 0.02
  flip <- if (is.null(seed_use)) {
    rep_len(TRUE, nrow(pairs))
  } else {
    .pairwiseLLM_with_seed(seed_use, function() {
      stats::runif(nrow(pairs)) < 0.5
    })
  }

  A_id <- character(nrow(pairs))
  B_id <- character(nrow(pairs))
  for (idx in seq_len(nrow(pairs))) {
    i_id <- as.character(pairs$i_id[[idx]])
    j_id <- as.character(pairs$j_id[[idx]])
    if (!i_id %in% state$ids || !j_id %in% state$ids) {
      rlang::abort("`pairs` ids must exist in `state$ids`.")
    }
    unordered_key <- as.character(pairs$unordered_key[[idx]])
    counts <- state$pair_count
    if (is.null(names(counts)) || length(counts) == 0L || !unordered_key %in% names(counts)) {
      count <- 0L
    } else {
      count <- counts[[unordered_key]]
      if (is.null(count) || is.na(count)) {
        count <- 0L
      }
    }
    if (!is.na(count) && count >= 1L) {
      last_order <- .adaptive_last_order_for_pair(state, unordered_key)
      if (is.null(last_order)) {
        rlang::abort("Duplicate ordering requires a prior completed presentation.")
      }
      A_id[[idx]] <- last_order$B_id
      B_id[[idx]] <- last_order$A_id
      next
    }

    deg_i <- as.integer(state$deg[[i_id]])
    deg_j <- as.integer(state$deg[[j_id]])
    pos_i <- as.integer(state$pos_count[[i_id]])
    pos_j <- as.integer(state$pos_count[[j_id]])
    p_i <- as.double(pos_i) / max(1L, deg_i)
    p_j <- as.double(pos_j) / max(1L, deg_j)
    if (abs(p_i - p_j) > balanced_margin) {
      if (p_i > p_j) {
        A_id[[idx]] <- j_id
        B_id[[idx]] <- i_id
      } else {
        A_id[[idx]] <- i_id
        B_id[[idx]] <- j_id
      }
    } else {
      if (isTRUE(flip[[idx]])) {
        A_id[[idx]] <- i_id
        B_id[[idx]] <- j_id
      } else {
        A_id[[idx]] <- j_id
        B_id[[idx]] <- i_id
      }
    }
  }

  pairs$A_id <- A_id
  pairs$B_id <- B_id
  pairs
}

#' @keywords internal
#' @noRd
select_batch <- function(state, candidates_with_utility, config, seed = NULL,
                         exploration_only = FALSE,
                         dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)
  required <- c("i_id", "j_id", "unordered_key", "utility", "utility_raw", "p_mean")
  .adaptive_required_cols(candidates_with_utility, "candidates_with_utility", required)

  batch_size <- as.integer(config$batch_size)
  if (is.na(batch_size) || batch_size < 0L) {
    rlang::abort("`config$batch_size` must be a non-negative integer.")
  }
  if (batch_size == 0L) {
    return(candidates_with_utility[0, , drop = FALSE])
  }

  if (isTRUE(exploration_only)) {
    n_explore <- batch_size
    n_exploit <- 0L
  } else {
    n_explore <- as.integer(round(config$explore_rate * batch_size))
    n_exploit <- as.integer(batch_size - n_explore)
  }

  explore <- .pairwiseLLM_with_seed(seed, function() {
    sample_exploration_pairs(
      state = state,
      candidates = candidates_with_utility,
      n_explore = n_explore,
      config = config,
      dup_policy = dup_policy
    )
  })
  if (nrow(explore) > 0L) {
    explore$is_explore <- TRUE
  }
  selected_keys <- as.character(explore$unordered_key)
  if (length(selected_keys) > 0L) {
    candidates_with_utility <- candidates_with_utility[
      !candidates_with_utility$unordered_key %in% selected_keys,
      ,
      drop = FALSE
    ]
  }

  if (nrow(explore) < n_explore) {
    n_exploit <- as.integer(batch_size - nrow(explore))
  }

  exploit <- select_exploitation_pairs(
    candidates_with_utility,
    state = state,
    n_exploit = n_exploit,
    config = config,
    dup_policy = dup_policy
  )
  if (nrow(exploit) > 0L) {
    exploit$is_explore <- FALSE
  }

  combined <- dplyr::bind_rows(explore, exploit)
  if (nrow(combined) == 0L) {
    combined$is_explore <- logical()
  }
  state_seed <- seed %||% state$seed
  state_tmp <- state
  state_tmp$seed <- state_seed
  assigned <- assign_order(combined, state_tmp)
  assigned
}
