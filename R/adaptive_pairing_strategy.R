# Post-bootstrap direct pairing policies. Hybrid selection remains in adaptive_select.R.

.adaptive_pairing_strategy <- function(state_or_controller) {
  controller <- if (inherits(state_or_controller, "adaptive_state")) {
    state_or_controller$controller
  } else {
    state_or_controller
  }
  strategy <- controller$pairing_strategy
  if (is.null(strategy)) strategy <- "hybrid"
  choices <- c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")
  if (!is.character(strategy) || length(strategy) != 1L || !is.null(dim(strategy)) ||
    is.na(strategy) || !strategy %in% choices) {
    rlang::abort(paste0("`adaptive_config$pairing_strategy` must be one of: ",
      paste(choices, collapse = ", "), "."))
  }
  strategy <- unname(strategy)
  if (strategy != "hybrid" &&
    !identical(controller$run_mode %||% "within_set", "within_set")) {
    rlang::abort("Non-hybrid `pairing_strategy` currently requires `run_mode = \"within_set\"`.")
  }
  strategy
}

.adaptive_pairing_target_distance <- function(p, strategy) {
  switch(strategy,
    trueskill_p50 = abs(p - 0.5),
    trueskill_pollitt = pmin(abs(p - 1 / 3), abs(p - 2 / 3)),
    rep_len(NA_real_, length(p))
  )
}

.adaptive_select_direct <- function(state, strategy, history_state, counts, defaults) {
  ids <- sort(as.character(state$item_ids))
  seed_base <- as.integer(state$meta$seed %||% 1L)
  # Committed count, not attempted step ID: invalid judgments retry the same policy draw.
  decision_id <- as.integer(history_state$n_pairs + 1L)
  focal_ids <- ids[counts$deg[ids] == min(counts$deg[ids])]
  focal_seed <- .adaptive_stage_seed(seed_base, decision_id, 1L, offset = 301L)
  focal <- focal_ids[[withr::with_seed(focal_seed, sample.int(length(focal_ids), 1L))]]
  partners <- setdiff(ids, focal)
  candidates <- tibble::tibble(i = rep(focal, length(partners)), j = partners)
  keys <- make_unordered_key(candidates$i, candidates$j)
  has_order <- .adaptive_repeat_pair_has_order(keys, counts$pair_count[keys], counts$pair_last_order)
  candidates <- candidates[has_order, , drop = FALSE]
  n_hard <- nrow(candidates)
  candidates <- .adaptive_duplicate_filter(candidates, counts$pair_count, defaults$dup_max_obs)
  n_legal <- nrow(candidates)

  # Typed NAs keep inapplicable hybrid diagnostics explicit without inventing stage quotas.
  out <- lapply(schema_step_log, .adaptive_schema_typed_na)
  out$i <- out$j <- out$A <- out$B <- NA_integer_
  out$round_id <- as.integer(state$round$round_id %||% 1L)
  out$round_stage <- out$pair_type <- "direct_pairing"
  out$run_mode <- "within_set"
  out$pairing_strategy <- strategy
  out$is_explore_step <- FALSE
  out$explore_rate_used <- 0.0
  out$star_override_used <- FALSE
  out$star_cap_rejects <- out$star_cap_reject_items <- 0L
  out$fallback_used <- "base"
  out$fallback_path <- paste("direct_pairing", strategy, sep = ">")
  out$candidate_starved <- n_legal == 0L
  out$n_candidates_generated <- as.integer(length(partners))
  out$n_candidates_after_hard_filters <- as.integer(n_hard)
  out$n_candidates_after_duplicates <- as.integer(n_legal)
  out$n_candidates_scored <- if (strategy == "random") 0L else as.integer(n_legal)
  if (n_legal == 0L) {
    out$starvation_reason <- if (n_hard == 0L) "filtered_by_hard_filters" else "filtered_by_duplicates"
    out$hard_filter_collapse_stage <- out$starvation_reason
    return(out)
  }

  if (strategy == "random") {
    partner_seed <- .adaptive_stage_seed(seed_base, decision_id, 1L, offset = 302L)
    picked <- withr::with_seed(partner_seed, sample.int(n_legal, 1L))
  } else {
    p <- vapply(candidates$j, function(partner) {
      trueskill_win_probability(focal, partner, state$trueskill_state)
    }, numeric(1L))
    distance <- .adaptive_pairing_target_distance(p, strategy)
    picked <- order(distance, candidates$j)[[1L]]
  }
  pair <- candidates[picked, , drop = FALSE]
  # Canonical unordered input makes first-presentation ties independent of focal orientation.
  pair <- tibble::tibble(i = pmin(pair$i, pair$j), j = pmax(pair$i, pair$j))
  presentation <- .adaptive_assign_order(pair, counts$posA, counts$posB,
    counts$pair_last_order, seed_base = seed_base)
  partner <- candidates$j[[picked]]
  out$i <- as.integer(state$item_index[[focal]])
  out$j <- as.integer(state$item_index[[partner]])
  out$A <- as.integer(state$item_index[[presentation[["A_id"]]]])
  out$B <- as.integer(state$item_index[[presentation[["B_id"]]]])
  ts <- state$trueskill_state$items
  ts_idx <- match(c(focal, partner), ts$item_id)
  out$mu_i <- as.double(ts$mu[[ts_idx[[1L]]]])
  out$mu_j <- as.double(ts$mu[[ts_idx[[2L]]]])
  out$sigma_i <- as.double(ts$sigma[[ts_idx[[1L]]]])
  out$sigma_j <- as.double(ts$sigma[[ts_idx[[2L]]]])
  out$deg_i <- as.integer(counts$deg[[focal]])
  out$deg_j <- as.integer(counts$deg[[partner]])
  recent <- .adaptive_history_state_recent_deg(history_state, ids, defaults$W_cap)
  out$recent_deg_i <- as.integer(recent[[focal]])
  out$recent_deg_j <- as.integer(recent[[partner]])
  out$p_ij <- as.double(trueskill_win_probability(
    presentation[["A_id"]], presentation[["B_id"]], state$trueskill_state))
  out$U0_ij <- out$p_ij * (1 - out$p_ij)
  out$target_distance <- .adaptive_pairing_target_distance(out$p_ij, strategy)
  out
}
