# -------------------------------------------------------------------------
# Deterministic simulation harnesses for adaptive runs.
# -------------------------------------------------------------------------

.adaptive_simulation_validate_seed <- function(seed, arg) {
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    rlang::abort(paste0("`", arg, "` must be a single non-missing integer."))
  }
  seed
}

.adaptive_simulation_default_items <- function(n_items = 12L) {
  n_items <- as.integer(n_items)
  if (length(n_items) != 1L || is.na(n_items) || n_items < 2L) {
    rlang::abort("`n_items` must be an integer >= 2.")
  }

  score_grid <- seq(1, 0, length.out = n_items)
  score_grid <- round(score_grid, digits = 3)
  tibble::tibble(
    item_id = as.character(seq_len(n_items)),
    text = paste("item", seq_len(n_items)),
    quality_score = as.double(score_grid)
  )
}

.adaptive_simulation_unit_value <- function(judge_seed, step_id, A_id, B_id) {
  key <- paste0(as.character(A_id), "|", as.character(B_id), "|", as.integer(step_id))
  chars <- utf8ToInt(key)
  mix <- as.integer(sum(chars * seq_along(chars)))
  draw_seed <- as.integer(judge_seed + mix)
  withr::with_seed(draw_seed, stats::runif(1L))
}

.adaptive_simulation_judge <- function(items, judge_seed, invalid_steps = integer()) {
  items <- tibble::as_tibble(items)
  if (!all(c("item_id", "quality_score") %in% names(items))) {
    rlang::abort("`items` must contain `item_id` and `quality_score` columns.")
  }
  invalid_steps <- as.integer(invalid_steps)
  invalid_steps <- invalid_steps[!is.na(invalid_steps)]

  score <- as.double(items$quality_score)
  names(score) <- as.character(items$item_id)
  judge_seed <- .adaptive_simulation_validate_seed(judge_seed, "judge_seed")

  function(A, B, state, ...) {
    step_id <- as.integer(nrow(state$step_log) + 1L)
    if (step_id %in% invalid_steps) {
      return(list(is_valid = FALSE, Y = NA_integer_, invalid_reason = "invalid_fixture_step"))
    }

    A_id <- as.character(A$item_id[[1L]])
    B_id <- as.character(B$item_id[[1L]])
    A_score <- as.double(score[[A_id]])
    B_score <- as.double(score[[B_id]])
    if (is.na(A_score) || is.na(B_score)) {
      return(list(is_valid = FALSE, Y = NA_integer_, invalid_reason = "missing_fixture_score"))
    }

    diff <- A_score - B_score
    p_A <- stats::plogis(2 * diff)
    u <- .adaptive_simulation_unit_value(judge_seed, step_id, A_id, B_id)
    list(
      is_valid = TRUE,
      Y = as.integer(u < p_A),
      invalid_reason = NA_character_
    )
  }
}

.adaptive_simulation_apply_scenario <- function(state, scenario) {
  out <- state
  if (identical(scenario, "quota_shortfall")) {
    out$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
    out$warm_start_idx <- 1L
    out$warm_start_done <- TRUE
    out$round$staged_active <- TRUE
    out$history_pairs <- tibble::tibble(
      A_id = rep(out$item_ids[[1L]], 3L),
      B_id = rep(out$item_ids[[2L]], 3L)
    )
  } else if (identical(scenario, "local_repeat_pressure")) {
    out$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
    out$warm_start_idx <- 1L
    out$warm_start_done <- TRUE
    out$round$staged_active <- TRUE
    out$round$stage_index <- 4L
    out$round$repeat_in_round_budget <- 2L
    out$round$repeat_in_round_used <- 0L
    out$round$per_round_item_uses[["1"]] <- 1L
    out$history_pairs <- tibble::tibble(
      A_id = c("2", "3", "2", "4", "3", "4"),
      B_id = c("3", "2", "4", "2", "4", "3")
    )
  } else if (identical(scenario, "local_repeat_blocked")) {
    out$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
    out$warm_start_idx <- 1L
    out$warm_start_done <- TRUE
    out$round$staged_active <- TRUE
    out$round$stage_index <- 4L
    out$round$repeat_in_round_budget <- 2L
    out$round$repeat_in_round_used <- 0L
    out$round$per_round_item_uses[] <- 1L
    out$history_pairs <- tibble::tibble(
      A_id = c("2", "2", "2", "3", "3", "4"),
      B_id = c("3", "4", "5", "4", "5", "5")
    )
  } else if (identical(scenario, "starvation_fallback")) {
    out$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
    out$warm_start_idx <- 1L
    out$warm_start_done <- TRUE
    out$round$staged_active <- TRUE
    out$history_pairs <- tibble::tibble(
      A_id = c("1", "1", "1"),
      B_id = c("2", "2", "2")
    )
  }
  out
}

.adaptive_simulation_run <- function(scenario = c(
                                       "baseline",
                                       "quota_shortfall",
                                       "warm_start_connectivity",
                                       "anchor_link_mix",
                                       "local_repeat_pressure",
                                       "local_repeat_blocked",
                                       "starvation_fallback"
                                     ),
                                     run_seed = 1L,
                                     judge_seed = 1L,
                                     n_steps = NULL,
                                     n_items = NULL,
                                     btl_config = NULL,
                                     fit_fn = NULL,
                                     items = NULL) {
  scenario <- match.arg(scenario)
  run_seed <- .adaptive_simulation_validate_seed(run_seed, "run_seed")
  judge_seed <- .adaptive_simulation_validate_seed(judge_seed, "judge_seed")

  if (is.null(items)) {
    default_n <- switch(scenario,
      quota_shortfall = 2L,
      starvation_fallback = 2L,
      local_repeat_pressure = 6L,
      local_repeat_blocked = 6L,
      anchor_link_mix = 30L,
      warm_start_connectivity = 10L,
      baseline = 10L
    )
    n_items <- as.integer(n_items %||% default_n)
    items <- .adaptive_simulation_default_items(n_items)
  } else {
    items <- tibble::as_tibble(items)
    if (!all(c("item_id", "quality_score") %in% names(items))) {
      rlang::abort("`items` must contain `item_id` and `quality_score` columns.")
    }
    items$item_id <- as.character(items$item_id)
    items$quality_score <- as.double(items$quality_score)
    n_items <- nrow(items)
  }

  default_steps <- switch(scenario,
    quota_shortfall = 8L,
    starvation_fallback = 8L,
    local_repeat_pressure = 2L,
    local_repeat_blocked = 2L,
    anchor_link_mix = 80L,
    warm_start_connectivity = max(2L, n_items - 1L),
    baseline = max(4L, n_items)
  )
  n_steps <- as.integer(n_steps %||% default_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 1L) {
    rlang::abort("`n_steps` must be an integer >= 1.")
  }

  sim <- withr::with_seed(run_seed, {
    state <- adaptive_rank_start(items = items, seed = run_seed)
    state <- .adaptive_simulation_apply_scenario(state, scenario = scenario)
    judge <- .adaptive_simulation_judge(items = items, judge_seed = judge_seed)
    if (is.null(btl_config)) {
      btl_config <- list(refit_pairs_target = as.integer(max(5000L, n_steps + 1L)))
    }

    anchor_trace <- tibble::tibble(
      step_id = integer(),
      round_id = integer(),
      anchor_signature = character(),
      anchor_refresh_source = character(),
      anchor_refit_round_id = integer()
    )

    for (idx in seq_len(n_steps)) {
      state <- adaptive_rank_run_live(
        state = state,
        judge = judge,
        n_steps = 1L,
        fit_fn = fit_fn,
        btl_config = btl_config,
        progress = "none"
      )
      round <- state$round %||% list()
      step_id <- as.integer(nrow(state$step_log))
      signature <- paste(sort(as.character(round$anchor_ids %||% character())), collapse = "|")
      row <- tibble::tibble(
        step_id = step_id,
        round_id = as.integer(round$round_id %||% NA_integer_),
        anchor_signature = as.character(signature),
        anchor_refresh_source = as.character(round$anchor_refresh_source %||% NA_character_),
        anchor_refit_round_id = as.integer(round$anchor_refit_round_id %||% NA_integer_)
      )
      anchor_trace <- dplyr::bind_rows(anchor_trace, row)
      if (isTRUE(state$meta$stop_decision %||% FALSE)) {
        break
      }
    }

    list(state = state, anchor_trace = anchor_trace)
  })

  list(
    scenario = scenario,
    run_seed = run_seed,
    judge_seed = judge_seed,
    state = sim$state,
    step_log = adaptive_step_log(sim$state),
    round_log = adaptive_round_log(sim$state),
    item_log = adaptive_item_log(sim$state, stack = TRUE),
    anchor_trace = sim$anchor_trace
  )
}

.adaptive_simulation_signature <- function(run) {
  step_log <- tibble::as_tibble(run$step_log)
  round_log <- tibble::as_tibble(run$round_log)
  anchor_trace <- tibble::as_tibble(run$anchor_trace)

  step_cols <- intersect(
    c(
      "step_id", "pair_id", "status", "round_id", "round_stage", "pair_type",
      "i", "j", "A", "B", "Y", "fallback_used", "fallback_path",
      "starvation_reason", "dist_stratum"
    ),
    names(step_log)
  )
  round_cols <- intersect(
    c("refit_id", "round_id_at_refit", "step_id_at_refit", "stop_decision", "stop_reason"),
    names(round_log)
  )

  list(
    scenario = run$scenario,
    run_seed = run$run_seed,
    judge_seed = run$judge_seed,
    step = step_log[, step_cols, drop = FALSE],
    round = round_log[, round_cols, drop = FALSE],
    anchor_trace = anchor_trace
  )
}

.adaptive_stage_quota_summary <- function(step_log) {
  log <- tibble::as_tibble(step_log)
  if (nrow(log) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      quota = integer(),
      committed = integer(),
      shortfall = integer()
    ))
  }

  staged <- log[log$round_stage %in% .adaptive_stage_order(), , drop = FALSE]
  if (nrow(staged) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      quota = integer(),
      committed = integer(),
      shortfall = integer()
    ))
  }

  keys <- paste(staged$round_id, staged$round_stage, sep = "::")
  groups <- split(staged, keys)
  out <- lapply(groups, function(tbl) {
    quotas <- tbl$stage_quota[!is.na(tbl$stage_quota)]
    quota <- if (length(quotas) == 0L) NA_integer_ else as.integer(quotas[[1L]])
    committed <- sum(!is.na(tbl$pair_id))
    shortfall <- if (is.na(quota)) NA_integer_ else as.integer(max(0L, quota - committed))
    tibble::tibble(
      round_id = as.integer(tbl$round_id[[1L]]),
      round_stage = as.character(tbl$round_stage[[1L]]),
      quota = quota,
      committed = as.integer(committed),
      shortfall = shortfall
    )
  })
  dplyr::bind_rows(out)
}

.adaptive_warm_start_connectivity <- function(step_log, item_ids) {
  log <- tibble::as_tibble(step_log)
  ids <- as.character(item_ids)
  warm <- log[log$round_stage == "warm_start" & !is.na(log$pair_id), , drop = FALSE]
  if (length(ids) <= 1L) {
    return(TRUE)
  }
  if (nrow(warm) == 0L) {
    return(FALSE)
  }

  idx <- stats::setNames(seq_along(ids), ids)
  adjacency <- vector("list", length(ids))
  for (k in seq_len(nrow(warm))) {
    i <- as.integer(warm$i[[k]])
    j <- as.integer(warm$j[[k]])
    if (is.na(i) || is.na(j) || i < 1L || j < 1L || i > length(ids) || j > length(ids)) {
      next
    }
    adjacency[[i]] <- unique(c(adjacency[[i]], j))
    adjacency[[j]] <- unique(c(adjacency[[j]], i))
  }

  seen <- rep(FALSE, length(ids))
  queue <- c(1L)
  seen[[1L]] <- TRUE
  while (length(queue) > 0L) {
    node <- queue[[1L]]
    queue <- queue[-1L]
    nbr <- as.integer(adjacency[[node]] %||% integer())
    nbr <- nbr[!is.na(nbr) & nbr >= 1L & nbr <= length(ids)]
    new_nodes <- nbr[!seen[nbr]]
    if (length(new_nodes) > 0L) {
      seen[new_nodes] <- TRUE
      queue <- c(queue, new_nodes)
    }
  }
  all(seen)
}

.adaptive_committed_degree_trajectory <- function(step_log, item_ids) {
  log <- tibble::as_tibble(step_log)
  ids <- as.character(item_ids)
  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  idx_to_id <- ids

  out <- vector("list", nrow(log))
  for (k in seq_len(nrow(log))) {
    if (!is.na(log$pair_id[[k]])) {
      i <- as.integer(log$i[[k]])
      j <- as.integer(log$j[[k]])
      if (!is.na(i) && i >= 1L && i <= length(ids)) {
        deg[[idx_to_id[[i]]]] <- deg[[idx_to_id[[i]]]] + 1L
      }
      if (!is.na(j) && j >= 1L && j <= length(ids)) {
        deg[[idx_to_id[[j]]]] <- deg[[idx_to_id[[j]]]] + 1L
      }
    }
    out[[k]] <- tibble::tibble(
      step_id = as.integer(log$step_id[[k]]),
      min_degree = as.integer(min(deg)),
      max_degree = as.integer(max(deg)),
      covered_items = as.integer(sum(deg > 0L))
    )
  }
  dplyr::bind_rows(out)
}
