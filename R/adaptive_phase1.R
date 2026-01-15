# -------------------------------------------------------------------------
# Adaptive Phase 1 warm-start pair generation
# -------------------------------------------------------------------------

.phase1_word_count <- function(texts) {
  texts <- as.character(texts)
  vapply(
    texts,
    function(txt) {
      txt <- trimws(txt)
      if (nchar(txt) == 0L) return(0L)
      length(strsplit(txt, "\\s+")[[1L]])
    },
    integer(1L)
  )
}

.phase1_compute_bins <- function(wc, bins) {
  compute <- function(b) {
    edges <- stats::quantile(
      wc,
      probs = seq(0, 1, length.out = b + 1L),
      names = FALSE,
      type = 7
    )
    if (length(unique(edges)) < length(edges)) {
      return(list(bin = NULL, bin_edges = edges, bins = b, ok = FALSE))
    }
    bin <- cut(wc, breaks = edges, include.lowest = TRUE, labels = FALSE, right = TRUE)
    if (anyNA(bin) || any(table(bin) == 0L)) {
      return(list(bin = NULL, bin_edges = edges, bins = b, ok = FALSE))
    }
    list(bin = as.integer(bin), bin_edges = edges, bins = b, ok = TRUE)
  }

  out <- compute(bins)
  if (!out$ok && bins != 6L) {
    out <- compute(6L)
  }
  if (!out$ok) {
    fallback <- rep.int(1L, length(wc))
    out <- list(bin = fallback, bin_edges = range(wc, na.rm = TRUE), bins = 1L, ok = TRUE)
  }
  out
}

.phase1_prepare_bins <- function(state, bins) {
  if (!is.null(state$bin) && !is.null(state$bin_edges) && !is.null(state$wc)) {
    if (is.null(names(state$bin)) || !identical(names(state$bin), state$ids)) {
      rlang::abort("`state$bin` must be a named integer vector over `ids`.")
    }
    return(state)
  }

  wc <- .phase1_word_count(state$texts)
  names(wc) <- state$ids
  bin_out <- .phase1_compute_bins(wc, bins)

  state$wc <- stats::setNames(as.integer(wc), state$ids)
  state$bin <- stats::setNames(as.integer(bin_out$bin), state$ids)
  state$bin_edges <- as.numeric(bin_out$bin_edges)
  state
}

.phase1_pick_focus_id <- function(state) {
  weights <- 1 / (state$deg + 1)
  base::sample(state$ids, size = 1L, prob = weights)
}

.phase1_choose_adjacent_bin <- function(bin_i, max_bin) {
  if (max_bin <= 1L) return(1L)
  if (bin_i <= 1L) return(2L)
  if (bin_i >= max_bin) return(max_bin - 1L)
  if (stats::runif(1) < 0.5) bin_i - 1L else bin_i + 1L
}

.phase1_pick_opponent <- function(ids, candidates, i_id) {
  pool <- setdiff(candidates, i_id)
  if (length(pool) == 0L) {
    return(NULL)
  }
  base::sample(pool, size = 1L)
}

.phase1_attempt_pair <- function(state, strategy) {
  i_id <- .phase1_pick_focus_id(state)
  bins <- state$bin
  max_bin <- max(bins, na.rm = TRUE)

  if (strategy == "within") {
    target_bin <- bins[[i_id]]
    candidates <- names(bins)[bins == target_bin]
    j_id <- .phase1_pick_opponent(state$ids, candidates, i_id)
    if (is.null(j_id)) {
      return(list(success = FALSE, reason = "no_opponent", i_id = i_id))
    }
  } else if (strategy == "adjacent") {
    target_bin <- .phase1_choose_adjacent_bin(bins[[i_id]], max_bin)
    candidates <- names(bins)[bins == target_bin]
    j_id <- .phase1_pick_opponent(state$ids, candidates, i_id)
    if (is.null(j_id)) {
      return(list(success = FALSE, reason = "no_opponent", i_id = i_id))
    }
  } else {
    j_id <- .phase1_pick_opponent(state$ids, state$ids, i_id)
    if (is.null(j_id)) {
      return(list(success = FALSE, reason = "no_opponent", i_id = i_id))
    }
  }

  order <- choose_order_with_position_balance(state, i_id, j_id)
  if (!duplicate_allowed(state, order$A_id, order$B_id)) {
    if (duplicate_allowed(state, order$B_id, order$A_id)) {
      order <- list(A_id = order$B_id, B_id = order$A_id)
    } else {
      return(list(success = FALSE, reason = "duplicate_blocked", i_id = i_id))
    }
  }

  list(success = TRUE, A_id = order$A_id, B_id = order$B_id, i_id = i_id)
}

.phase1_build_pair_row <- function(state, A_id, B_id, created_at, strategy) {
  unordered_key <- make_unordered_key(A_id, B_id)
  ordered_key <- make_ordered_key(A_id, B_id)
  pair_uid <- pair_uid_from_state(state, unordered_key)

  tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = "phase1",
    iter = 0L,
    created_at = created_at,
    strategy = as.character(strategy),
    wc_A = as.integer(state$wc[[A_id]]),
    wc_B = as.integer(state$wc[[B_id]]),
    bin_A = as.integer(state$bin[[A_id]]),
    bin_B = as.integer(state$bin[[B_id]]),
    deg_A = as.integer(state$deg[[A_id]]),
    deg_B = as.integer(state$deg[[B_id]]),
    imb_A = as.integer(state$imb[[A_id]]),
    imb_B = as.integer(state$imb[[B_id]])
  )
}

#' @keywords internal
#' @noRd
phase1_generate_pairs <- function(
    state,
    n_pairs = NULL,
    mix_struct = 0.70,
    within_adj_split = 0.50,
    bins = 8,
    max_attempts_per_pair = 50,
    seed = NULL
) {
  validate_state(state)

  if (is.null(n_pairs)) {
    n_pairs <- state$M1_target - state$comparisons_scheduled
  }
  n_pairs <- as.integer(n_pairs)
  if (n_pairs <= 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  if (!is.numeric(mix_struct) || mix_struct <= 0 || mix_struct >= 1) {
    rlang::abort("`mix_struct` must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(within_adj_split) || within_adj_split <= 0 || within_adj_split >= 1) {
    rlang::abort("`within_adj_split` must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(max_attempts_per_pair) || max_attempts_per_pair < 1) {
    rlang::abort("`max_attempts_per_pair` must be a positive numeric value.")
  }

  state <- .phase1_prepare_bins(state, bins)
  created_at <- Sys.time()
  pairs <- vector("list", n_pairs)
  generated <- 0L
  reason_counts <- list()

  result <- .pairwiseLLM_with_seed(seed, function() {
    for (idx in seq_len(n_pairs)) {
      is_structured <- stats::runif(1) < mix_struct
      primary_within <- stats::runif(1) < within_adj_split

      if (is_structured) {
        strategies <- if (primary_within) {
          c("within", "adjacent", "random")
        } else {
          c("adjacent", "within", "random")
        }
      } else {
        strategies <- c("random", "within", "adjacent")
      }

      attempts_remaining <- as.integer(max_attempts_per_pair)
      success <- FALSE

      for (stage in seq_along(strategies)) {
        if (success || attempts_remaining <= 0L) break
        stages_left <- length(strategies) - stage + 1L
        stage_attempts <- ceiling(attempts_remaining / stages_left)
        attempts_remaining <- attempts_remaining - stage_attempts

        for (attempt_idx in seq_len(stage_attempts)) {
          attempt <- .phase1_attempt_pair(state, strategies[stage])
          if (attempt$success) {
            row <- .phase1_build_pair_row(
              state,
              attempt$A_id,
              attempt$B_id,
              created_at,
              strategies[stage]
            )
            state <- record_exposure(state, attempt$A_id, attempt$B_id)
            state$history_pairs <- dplyr::bind_rows(state$history_pairs, row)
            state$comparisons_scheduled <- as.integer(state$comparisons_scheduled + 1L)
            generated <- generated + 1L
            pairs[[generated]] <- row
            success <- TRUE
            break
          }
          reason_counts[[attempt$reason]] <- (reason_counts[[attempt$reason]] %||% 0L) + 1L
        }
      }

      if (!success) {
        break
      }
    }

    list(state = state, generated = generated, pairs = pairs, reasons = reason_counts)
  })

  state <- result$state
  generated <- result$generated
  pairs <- result$pairs
  reason_counts <- result$reasons

  if (generated < n_pairs) {
    reason_text <- ""
    if (length(reason_counts) > 0L) {
      reason_parts <- paste0(names(reason_counts), "=", unlist(reason_counts))
      reason_text <- paste0(" Reasons: ", paste(reason_parts, collapse = ", "), ".")
    }
    rlang::warn(paste0(
      "Generated ", generated, " of ", n_pairs, " requested Phase 1 pairs.",
      reason_text
    ))
  }

  pairs_tbl <- if (generated == 0L) {
    .adaptive_empty_pairs_tbl()
  } else {
    dplyr::bind_rows(pairs[seq_len(generated)])
  }

  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  validate_pairs_tbl(pairs_tbl)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs_tbl <- pairs_tbl[, c(required, setdiff(names(pairs_tbl), required)), drop = FALSE]
  list(state = state, pairs = pairs_tbl)
}
