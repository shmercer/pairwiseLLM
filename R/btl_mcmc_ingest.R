# -------------------------------------------------------------------------
# Non-adaptive BTL MCMC ingestion helpers.
# -------------------------------------------------------------------------

btl_mcmc_results_seen_names <- function(state) {
  if (is.environment(state$results_seen)) {
    return(ls(state$results_seen, all.names = TRUE))
  }
  if (is.null(state$results_seen) || length(state$results_seen) == 0L) {
    return(character())
  }
  names(state$results_seen)
}

btl_mcmc_results_seen_set <- function(state, keys) {
  keys <- as.character(keys)
  keys <- keys[!is.na(keys) & keys != ""]
  if (length(keys) == 0L) {
    return(state)
  }
  if (is.environment(state$results_seen)) {
    for (key in keys) {
      state$results_seen[[key]] <- TRUE
    }
    return(state)
  }
  seen <- state$results_seen %||% logical()
  seen[keys] <- TRUE
  state$results_seen <- seen
  state
}

btl_mcmc_state_sync_results_seen <- function(state) {
  if (!is.null(state$results_seen) && length(btl_mcmc_results_seen_names(state)) > 0L) {
    return(state)
  }
  if (nrow(state$history_results) == 0L) {
    state$results_seen <- logical()
    return(state)
  }
  keys <- as.character(state$history_results$pair_uid)
  keys <- keys[!is.na(keys) & keys != ""]
  if (length(keys) == 0L) {
    state$results_seen <- logical()
    return(state)
  }
  state$results_seen <- stats::setNames(rep(TRUE, length(keys)), keys)
  state
}

btl_mcmc_ingest_results_incremental <- function(state, results_tbl) {
  if (is.null(results_tbl) || nrow(results_tbl) == 0L) {
    return(list(state = state, new_results = .adaptive_empty_results_tbl()))
  }

  results_tbl <- tibble::as_tibble(results_tbl)
  validate_results_tbl(results_tbl)
  state <- btl_mcmc_state_sync_results_seen(state)

  pair_uid <- as.character(results_tbl$pair_uid)
  missing_uid <- is.na(pair_uid) | pair_uid == ""
  if (any(missing_uid)) {
    rlang::warn("Dropping results with missing `pair_uid`.")
  }
  keep_mask <- !missing_uid
  results_tbl <- results_tbl[keep_mask, , drop = FALSE]
  pair_uid <- pair_uid[keep_mask]

  seen <- btl_mcmc_results_seen_names(state)
  new_mask <- !(pair_uid %in% seen)
  new_results <- results_tbl[new_mask, , drop = FALSE]

  if (nrow(new_results) == 0L) {
    return(list(state = state, new_results = .adaptive_empty_results_tbl()))
  }

  new_results <- new_results[!duplicated(new_results$pair_uid), , drop = FALSE]
  for (idx in seq_len(nrow(new_results))) {
    state <- btl_mcmc_record_judgment_exposure(
      state,
      as.character(new_results$A_id[[idx]]),
      as.character(new_results$B_id[[idx]])
    )
  }
  state$history_results <- dplyr::bind_rows(state$history_results, new_results)
  state$comparisons_observed <- as.integer(nrow(state$history_results))
  state$new_since_refit <- as.integer((state$new_since_refit %||% 0L) + nrow(new_results))
  state <- btl_mcmc_results_seen_set(state, new_results$pair_uid)

  list(state = state, new_results = new_results)
}

btl_mcmc_fill_terminal_stop_metrics <- function(state, config, metrics = NULL) {
  metrics <- .adaptive_stop_metrics_align(metrics %||% state$posterior$stop_metrics %||% list())
  metrics$scheduled_pairs <- as.integer(state$comparisons_scheduled %||% NA_integer_)
  metrics$completed_pairs <- as.integer(state$comparisons_observed %||% NA_integer_)
  metrics$n_unique_pairs_seen <- if (!is.null(state$pair_count) && length(state$pair_count) > 0L) {
    as.integer(sum(state$pair_count >= 1L))
  } else {
    NA_integer_
  }

  hard_cap_frac <- as.double(config$hard_cap_frac %||% NA_real_)
  total_pairs <- as.integer(state$N * (state$N - 1L) / 2)
  if (is.finite(hard_cap_frac) && hard_cap_frac > 0 && hard_cap_frac <= 1) {
    hard_cap_threshold <- as.integer(ceiling(hard_cap_frac * total_pairs))
  } else {
    hard_cap_threshold <- NA_integer_
  }
  metrics$hard_cap_threshold <- as.integer(hard_cap_threshold)
  if (!is.na(metrics$n_unique_pairs_seen) && !is.na(metrics$hard_cap_threshold)) {
    metrics$hard_cap_reached <- metrics$n_unique_pairs_seen >= metrics$hard_cap_threshold
  }
  metrics
}
