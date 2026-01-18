# -------------------------------------------------------------------------
# Adaptive greedy selection for Phase 2/3 batches
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
select_pairs_from_candidates <- function(
    state,
    utilities_tbl,
    batch_size,
    per_item_cap = NULL,
    phase = c("phase2", "phase3"),
    iter,
    seed = NULL
) {
  validate_state(state)
  if (!is.data.frame(utilities_tbl)) {
    rlang::abort("`utilities_tbl` must be a data frame or tibble.")
  }
  utilities_tbl <- tibble::as_tibble(utilities_tbl)
  required <- c("i_id", "j_id", "unordered_key", "utility", "utility_raw")
  .adaptive_required_cols(utilities_tbl, "utilities_tbl", required)

  batch_size <- as.integer(batch_size)
  if (is.na(batch_size) || batch_size < 0L) {
    rlang::abort("`batch_size` must be a non-negative integer.")
  }
  if (batch_size == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  if (!is.integer(iter) || length(iter) != 1L) {
    rlang::abort("`iter` must be a length-1 integer.")
  }
  phase <- match.arg(phase)

  if (is.null(per_item_cap)) {
    per_item_cap <- max(1L, ceiling(2 * batch_size / state$N))
  }
  per_item_cap <- as.integer(per_item_cap)
  if (is.na(per_item_cap) || per_item_cap < 1L) {
    rlang::abort("`per_item_cap` must be a positive integer.")
  }

  i_id <- as.character(utilities_tbl$i_id)
  j_id <- as.character(utilities_tbl$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`utilities_tbl` ids must exist in `state$ids`.")
  }

  ordered <- utilities_tbl[order(
    -utilities_tbl$utility,
    utilities_tbl$unordered_key,
    i_id,
    j_id
  ), , drop = FALSE]

  batch_count <- stats::setNames(rep.int(0L, state$N), state$ids)
  created_at <- Sys.time()

  result <- .pairwiseLLM_with_seed(seed, function() {
    state_local <- state
    accepted_local <- 0L
    rows_local <- vector("list", batch_size)
    for (idx in seq_len(nrow(ordered))) {
      if (accepted_local >= batch_size) break
      row <- ordered[idx, , drop = FALSE]
      i <- as.character(row$i_id)
      j <- as.character(row$j_id)

      if (batch_count[[i]] >= per_item_cap || batch_count[[j]] >= per_item_cap) {
        next
      }

      if (!.adaptive_unordered_allowed(state_local, i, j)) {
        next
      }

      order <- choose_order_with_position_balance(state_local, i, j)
      if (!duplicate_allowed(state_local, order$A_id, order$B_id)) {
        if (duplicate_allowed(state_local, order$B_id, order$A_id)) {
          order <- list(A_id = order$B_id, B_id = order$A_id)
        } else {
          next
        }
      }

      unordered_key <- make_unordered_key(order$A_id, order$B_id)
      ordered_key <- make_ordered_key(order$A_id, order$B_id)
      pair_uid <- pair_uid_from_state(state_local, unordered_key)

      pair_row <- tibble::tibble(
        pair_uid = pair_uid,
        unordered_key = unordered_key,
        ordered_key = ordered_key,
        A_id = order$A_id,
        B_id = order$B_id,
        A_text = state_local$texts[[order$A_id]],
        B_text = state_local$texts[[order$B_id]],
        phase = phase,
        iter = iter,
        created_at = created_at,
        utility_raw = as.double(row$utility_raw),
        utility = as.double(row$utility),
        deg_A = as.integer(state_local$deg[[order$A_id]]),
        deg_B = as.integer(state_local$deg[[order$B_id]]),
        imb_A = as.integer(state_local$imb[[order$A_id]]),
        imb_B = as.integer(state_local$imb[[order$B_id]])
      )

      state_local <- record_presentation(state_local, order$A_id, order$B_id)
      state_local$history_pairs <- dplyr::bind_rows(state_local$history_pairs, pair_row)
      state_local$comparisons_scheduled <- as.integer(state_local$comparisons_scheduled + 1L)

      batch_count[[order$A_id]] <- batch_count[[order$A_id]] + 1L
      batch_count[[order$B_id]] <- batch_count[[order$B_id]] + 1L

      accepted_local <- accepted_local + 1L
      rows_local[[accepted_local]] <- pair_row
    }
    list(state = state_local, accepted = accepted_local, rows = rows_local)
  })

  pairs_tbl <- if (result$accepted == 0L) {
    .adaptive_empty_pairs_tbl()
  } else {
    dplyr::bind_rows(result$rows[seq_len(result$accepted)])
  }

  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  validate_pairs_tbl(pairs_tbl)
  required_cols <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs_tbl <- pairs_tbl[, c(required_cols, setdiff(names(pairs_tbl), required_cols)), drop = FALSE]
  list(state = result$state, pairs = pairs_tbl)
}
