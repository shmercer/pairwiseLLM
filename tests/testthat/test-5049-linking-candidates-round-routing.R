mark_link_phase_b_ready <- function(state, source = "import", probe_edges_min_for_stop = 0L) {
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  if (is.null(state$linking$phase_a)) {
    state$linking$phase_a <- list()
  }
  artifacts <- lapply(set_ids, function(set_id) {
    rows <- state$items[state$items$set_id == as.integer(set_id), , drop = FALSE]
    tib <- tibble::as_tibble(rows)
    tib$theta_raw_mean <- seq(from = nrow(tib), to = 1, by = -1)
    tib$theta_raw_sd <- rep(0.5, nrow(tib))
    tib$rank_mu_raw <- seq_len(nrow(tib))
    list(items = tib)
  })
  names(artifacts) <- as.character(set_ids)
  for (set_id in names(artifacts)) {
    artifacts[[set_id]] <- add_test_phase_a_evidence(
      artifacts[[set_id]],
      state = state,
      set_id = as.integer(set_id)
    )
  }
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = as.integer(set_ids),
    source = rep(as.character(source), length(set_ids)),
    status = rep("ready", length(set_ids)),
    validation_message = rep("ready", length(set_ids)),
    artifact_path = rep(NA_character_, length(set_ids))
  )
  state$linking$phase_a$artifacts <- artifacts
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$phase <- "phase_b"
  state$controller$probe_edges_min_for_stop <- as.integer(probe_edges_min_for_stop)
  state
}

adaptive_rank_start <- function(items, seed, adaptive_config = NULL, ...) {
  pairwiseLLM::adaptive_rank_start(
    items = items,
    seed = seed,
    adaptive_config = adaptive_config,
    ...
  )
}

reference_phase_b_stage_candidates <- function(state,
                                               stage_name,
                                               fallback_name,
                                               local_inputs,
                                               rank_index,
                                               stratum_map,
                                               spoke_id,
                                               C_max,
                                               seed,
                                               reserved_keys = character()) {
  fallback <- function(x, default) {
    if (is.null(x)) default else x
  }

  defaults <- pairwiseLLM:::adaptive_defaults(length(state$item_ids))
  bounds <- pairwiseLLM:::.adaptive_stage_distance_bounds(stage_name, fallback_name, defaults)
  hub_ids <- as.character(local_inputs$hub_ids)
  spoke_ids <- as.character(local_inputs$spoke_ids)
  active_items <- fallback(local_inputs$active_items, list())
  active_hub_ids <- as.character(fallback(active_items$active_hub, character()))
  routing_hub_ids <- if (identical(stage_name, "anchor_link")) hub_ids else active_hub_ids
  active_ids <- unique(c(routing_hub_ids, spoke_ids))
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  coverage <- fallback(local_inputs$coverage, list(
    bin_map = stats::setNames(integer(), character()),
    bins_used = NA_integer_,
    bins_undercovered = integer(),
    source = NA_character_
  ))
  hub_anchor_ids <- as.character(fallback(local_inputs$hub_anchor_ids, character()))
  ids <- names(sort(rank_index[active_ids]))

  n_after_route_filters <- 0L
  n_after_active_domain <- 0L
  i_vals <- character()
  j_vals <- character()
  dist_vals <- integer()
  coverage_priority <- integer()
  coverage_bin <- integer()

  if (length(ids) >= 2L) {
    for (a in seq_len(length(ids) - 1L)) {
      i_id <- ids[[a]]
      for (b in (a + 1L):length(ids)) {
        j_id <- ids[[b]]
        i_set <- as.integer(fallback(set_map[[i_id]], NA_integer_))
        j_set <- as.integer(fallback(set_map[[j_id]], NA_integer_))
        if (is.na(i_set) || is.na(j_set) || i_set == j_set) {
          next
        }
        i_hub <- i_id %in% hub_ids
        j_hub <- j_id %in% hub_ids
        if (!isTRUE(xor(i_hub, j_hub))) {
          next
        }
        n_after_route_filters <- as.integer(n_after_route_filters + 1L)

        keep <- FALSE
        dist <- abs(as.integer(stratum_map[[i_id]]) - as.integer(stratum_map[[j_id]]))
        if (identical(stage_name, "anchor_link")) {
          n_after_active_domain <- as.integer(n_after_active_domain + 1L)
          keep <- xor(i_id %in% hub_anchor_ids, j_id %in% hub_anchor_ids)
        } else {
          hub_item_id <- if (isTRUE(i_hub)) i_id else j_id
          if (!hub_item_id %in% active_hub_ids) {
            next
          }
          n_after_active_domain <- as.integer(n_after_active_domain + 1L)
          keep <- dist >= bounds$min && dist <= bounds$max
        }

        if (isTRUE(keep)) {
          spoke_item_id <- if (identical(i_set, as.integer(spoke_id))) i_id else j_id
          spoke_bin <- as.integer(fallback(coverage$bin_map[[spoke_item_id]], NA_integer_))
          i_vals <- c(i_vals, i_id)
          j_vals <- c(j_vals, j_id)
          dist_vals <- c(dist_vals, as.integer(dist))
          coverage_bin <- c(coverage_bin, spoke_bin)
          coverage_priority <- c(
            coverage_priority,
            as.integer(!is.na(spoke_bin) && spoke_bin %in% as.integer(coverage$bins_undercovered))
          )
        }
      }
    }
  }

  pair_cols <- list(
    i = character(),
    j = character(),
    dist_stratum_global = integer(),
    coverage_priority = integer(),
    coverage_bin_spoke = integer(),
    link_spoke_id = integer(),
    coverage_bins_used = integer(),
    coverage_source = character()
  )

  if (length(i_vals) > 0L) {
    cand <- tibble::tibble(
      i = as.character(i_vals),
      j = as.character(j_vals),
      dist_stratum_global = as.integer(dist_vals),
      coverage_priority = as.integer(coverage_priority),
      coverage_bin_spoke = as.integer(coverage_bin),
      link_spoke_id = as.integer(spoke_id),
      coverage_bins_used = as.integer(coverage$bins_used),
      coverage_source = as.character(coverage$source)
    )
    if (length(reserved_keys) > 0L) {
      pair_keys <- vapply(seq_len(nrow(cand)), function(idx) {
        pairwiseLLM:::make_unordered_key(cand$i[[idx]], cand$j[[idx]])
      }, character(1L))
      cand <- cand[!pair_keys %in% reserved_keys, , drop = FALSE]
    }
  } else {
    cand <- tibble::as_tibble(pair_cols)
  }

  if (nrow(cand) > 0L) {
    cand <- pairwiseLLM:::.adaptive_uniform_subsample_pairs(
      cand,
      C_max = as.integer(C_max),
      seed = as.integer(seed)
    )
  }

  counts <- list(
    n_candidates_after_route_filters = as.integer(n_after_route_filters),
    n_candidates_after_active_domain = as.integer(n_after_active_domain),
    n_candidates_after_stage_filters = as.integer(nrow(cand))
  )

  list(
    candidates = pairwiseLLM:::.adaptive_set_candidate_filter_counts(cand, counts),
    counts = counts
  )
}

test_that("linking candidates are hub-spoke only by default", {
  items <- tibble::tibble(
    item_id = as.character(1:9),
    set_id = c(rep(1L, 3L), rep(2L, 3L), rep(3L, 3L)),
    global_item_id = paste0("g", 1:9)
  )
  state <- adaptive_rank_start(
    items,
    seed = 123L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state <- mark_link_phase_b_ready(state)

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 1L
  )
  set_map <- stats::setNames(items$set_id, items$item_id)
  set_i <- as.integer(set_map[cand$i])
  set_j <- as.integer(set_map[cand$j])

  expect_true(nrow(cand) > 0L)
  expect_true(all((set_i == 1L & set_j == 2L) | (set_i == 2L & set_j == 1L)))
})

test_that("spoke-spoke Phase B routing remains hard-gated on the current path", {
  items <- tibble::tibble(
    item_id = as.character(1:9),
    set_id = c(rep(1L, 3L), rep(2L, 3L), rep(3L, 3L)),
    global_item_id = paste0("g", 1:9)
  )

  state <- adaptive_rank_start(
    items,
    seed = 124L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state$controller$allow_spoke_spoke_cross_set <- TRUE
  state <- mark_link_phase_b_ready(state)

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
      state,
      stage_name = "long_link",
      fallback_name = "base",
      C_max = 10000L,
      seed = 2L
  )
  set_i <- as.integer(state$items$set_id[match(cand$i, state$items$item_id)])
  set_j <- as.integer(state$items$set_id[match(cand$j, state$items$item_id)])
  expect_true(all(set_i == 1L | set_j == 1L))
})

test_that("phase B non-anchor routing activates only after a committed active-link edge", {
  items <- tibble::tibble(
    item_id = as.character(1:9),
    set_id = c(rep(1L, 3L), rep(2L, 3L), rep(3L, 3L)),
    global_item_id = paste0("g", 1:9)
  )
  state <- adaptive_rank_start(
    items,
    seed = 1234L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state <- mark_link_phase_b_ready(state)

  set_map <- stats::setNames(items$set_id, items$item_id)
  anchor_cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 4L
  )
  expect_true(nrow(anchor_cand) > 0L)
  set_i <- as.integer(set_map[anchor_cand$i])
  set_j <- as.integer(set_map[anchor_cand$j])
  expect_true(all((set_i == 1L & set_j == 2L) | (set_i == 2L & set_j == 1L)))

  for (stage in c("long_link", "mid_link", "local_link")) {
    cand <- pairwiseLLM:::generate_stage_candidates_from_state(
      state,
      stage_name = stage,
      fallback_name = "base",
      C_max = 10000L,
      seed = 4L
    )
    expect_equal(nrow(cand), 0L)
  }

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 4L,
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:00:02", tz = "UTC"),
      pair_id = 2L,
      i = 2L,
      j = 5L,
      A = 2L,
      B = 5L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )

  for (stage in c("long_link", "mid_link", "local_link")) {
    cand <- testthat::with_mocked_bindings(
      .adaptive_assign_strata = function(scores, defaults) {
        ids <- names(scores)
        ranks <- stats::setNames(seq_along(ids), ids)
        list(
          rank_index = ranks,
          stratum_id = as.integer(ranks[ids]),
          stratum_map = ranks,
          top_band_ids = character()
        )
      },
      pairwiseLLM:::generate_stage_candidates_from_state(
        state,
        stage_name = stage,
        fallback_name = "base",
        C_max = 10000L,
        seed = 5L
      ),
      .package = "pairwiseLLM"
    )
    if (nrow(cand) > 0L) {
      set_i <- as.integer(set_map[cand$i])
      set_j <- as.integer(set_map[cand$j])
      expect_true(all((set_i == 1L & set_j == 2L) | (set_i == 2L & set_j == 1L)))
      hub_item <- ifelse(set_i == 1L, cand$i, cand$j)
      expect_true(all(hub_item %in% c("1", "2")))
    }
  }
})

test_that("linking long-link taper applies only to the active spoke and respects floor", {
  q_base <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(
      run_mode = "link_one_spoke",
      current_link_spoke_id = 2L,
      linking_identified_by_spoke = list(`2` = FALSE)
    )
  )
  q_taper <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(
      run_mode = "link_one_spoke",
      current_link_spoke_id = 2L,
      linking_identified_by_spoke = list(`2` = TRUE)
    )
  )
  q_other_spoke <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(
      run_mode = "link_one_spoke",
      current_link_spoke_id = 3L,
      linking_identified_by_spoke = list(`2` = TRUE)
    )
  )

  expect_identical(unname(q_base[c("anchor_link", "long_link", "mid_link", "local_link")]), c(13L, 18L, 12L, 7L))
  expect_identical(unname(q_taper[c("anchor_link", "mid_link", "local_link")]), c(17L, 15L, 9L))
  expect_true(q_taper[["long_link"]] == 9L)
  expect_true(q_taper[["long_link"]] >= 2L)
  expect_identical(q_other_spoke[["long_link"]], 18L)
})

test_that("phase B hub-anchor candidates are derived from hub-only scores", {
  items <- tibble::tibble(
    item_id = c(
      "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8",
      "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"
    ),
    set_id = c(rep(1L, 8L), rep(2L, 8L)),
    global_item_id = paste0("g", seq_len(16L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 88L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)

  ids <- as.character(state$trueskill_state$items$item_id)
  mu <- rep(0, length(ids))
  names(mu) <- ids
  mu[paste0("h", 1:8)] <- c(20, 18, 16, 14, 12, 10, 8, 6)
  mu[paste0("s", 1:8)] <- c(80, 79, 78, 77, -20, -21, -22, -23)
  state$trueskill_state$items$mu <- as.double(mu[ids])
  cand_a <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 1L
  )

  mu[paste0("s", 1:8)] <- c(-80, -79, -78, -77, 40, 39, 38, 37)
  state$trueskill_state$items$mu <- as.double(mu[ids])
  cand_b <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 1L
  )

  set_map <- stats::setNames(items$set_id, items$item_id)
  hub_anchor_a <- sort(unique(c(
    cand_a$i[set_map[cand_a$i] == 1L],
    cand_a$j[set_map[cand_a$j] == 1L]
  )))
  hub_anchor_b <- sort(unique(c(
    cand_b$i[set_map[cand_b$i] == 1L],
    cand_b$j[set_map[cand_b$j] == 1L]
  )))

  expect_true(length(hub_anchor_a) > 0L)
  expect_identical(hub_anchor_a, hub_anchor_b)
})

test_that("multi-spoke long-link taper remains isolated to identified spoke", {
  q_spoke_2 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(
      run_mode = "link_multi_spoke",
      current_link_spoke_id = 2L,
      linking_identified_by_spoke = list(`2` = TRUE, `3` = FALSE)
    )
  )
  q_spoke_3 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(
      run_mode = "link_multi_spoke",
      current_link_spoke_id = 3L,
      linking_identified_by_spoke = list(`2` = TRUE, `3` = FALSE)
    )
  )
  meta_2 <- attr(q_spoke_2, "quota_meta")
  meta_3 <- attr(q_spoke_3, "quota_meta")

  expect_identical(q_spoke_2[["long_link"]], 9L)
  expect_identical(q_spoke_3[["long_link"]], 18L)
  expect_true(isTRUE(meta_2$taper_applied))
  expect_false(isTRUE(meta_3$taper_applied))
})

test_that("phase A linking scheduling uses within-set round defaults", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 99L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  q_within <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = nrow(items),
    controller = list(run_mode = "within_set")
  )
  expect_identical(state$linking$phase_a$phase, "phase_a")
  expect_equal(sum(state$round$stage_quotas), sum(q_within))
  expect_equal(as.integer(state$round$stage_quotas[["anchor_link"]]), as.integer(q_within[["anchor_link"]]))
})

test_that("phase A linking quotas use active set size, not global multi-set size", {
  items <- tibble::tibble(
    item_id = as.character(1:100),
    set_id = c(rep(1L, 50L), rep(2L, 50L)),
    global_item_id = paste0("g", 1:100)
  )
  state <- adaptive_rank_start(
    items,
    seed = 109L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  q_set <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 50L,
    controller = list(run_mode = "within_set")
  )
  q_global <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(run_mode = "within_set")
  )

  expect_identical(state$linking$phase_a$phase, "phase_a")
  expect_equal(as.integer(sum(state$round$stage_quotas)), as.integer(sum(q_set)))
  expect_false(identical(unname(as.integer(state$round$stage_quotas)), unname(as.integer(q_global))))
})

test_that("link stage rows carry per-spoke per-refit quota totals and committed counts", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32", "gs33")
  )
  state <- adaptive_rank_start(
    items,
    seed = 19L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$controller$current_link_spoke_id <- 2L
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 4L,
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:00:02", tz = "UTC"),
      pair_id = 2L,
      i = 2L,
      j = 6L,
      A = 2L,
      B = 6L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 3L,
      timestamp = as.POSIXct("2026-01-01 00:00:03", tz = "UTC"),
      pair_id = 3L,
      i = 3L,
      j = 5L,
      A = 3L,
      B = 5L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      link_stage = "long_link",
      round_stage = "long_link"
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log, list(refit_id = 1L, diagnostics_pass = TRUE))
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(), `3` = list())
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row2 <- rows[rows$spoke_id == 2L, , drop = FALSE]
  row3 <- rows[rows$spoke_id == 3L, , drop = FALSE]
  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )
  expected2 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = as.integer(state$round$round_id),
    n_items = as.integer(state$n_items),
    controller = utils::modifyList(
      state$controller,
      list(
        current_link_spoke_id = 2L,
        B_spoke_refit_budget = budget_map[["2"]]$B_spoke_refit_budget,
        B_spoke_refit_budget_source = budget_map[["2"]]$B_spoke_refit_budget_source
      )
    )
  )
  expected2 <- pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
    state = state,
    controller = utils::modifyList(
      state$controller,
      list(
        current_link_spoke_id = 2L,
        B_spoke_refit_budget = budget_map[["2"]]$B_spoke_refit_budget,
        B_spoke_refit_budget_source = budget_map[["2"]]$B_spoke_refit_budget_source
      )
    ),
    spoke_id = 2L,
    stage_quotas = expected2,
    stage_order = pairwiseLLM:::.adaptive_stage_order(),
    refit_id = 1L
  )
  meta2 <- attr(expected2, "quota_meta")
  if (is.null(meta2)) meta2 <- list()
  expect_true(nrow(row2) == 1L)
  expect_true(nrow(row3) == 1L)
  expect_identical(row2$quota_anchor_link[[1L]], expected2[["anchor_link"]])
  expect_identical(row2$quota_long_link[[1L]], expected2[["long_link"]])
  expect_true(row2$committed_anchor_link[[1L]] + row2$committed_long_link[[1L]] +
    row2$committed_mid_link[[1L]] + row2$committed_local_link[[1L]] >= 1L)
  expect_identical(row3$B_spoke_refit_budget[[1L]], 2L)
  expect_identical(as.character(row3$B_spoke_refit_budget_source[[1L]]), "concurrent_allocator")
  expect_identical(row3$committed_anchor_link[[1L]] + row3$committed_long_link[[1L]] +
    row3$committed_mid_link[[1L]] + row3$committed_local_link[[1L]], 0L)
  expect_identical(row2$quota_long_link_raw[[1L]], meta2$long_quota_raw)
  expect_identical(row2$quota_long_link_effective[[1L]], meta2$long_quota_effective)
  expect_identical(row2$quota_long_link_removed[[1L]], meta2$long_quota_removed)
  expect_false(isTRUE(row2$quota_taper_applied[[1L]]))
  expect_identical(row2$quota_taper_spoke_id[[1L]], 2L)
  expect_identical(row3$quota_anchor_link[[1L]], 2L)
  expect_identical(row3$quota_long_link[[1L]], 0L)
  expect_identical(row3$quota_mid_link[[1L]], 0L)
  expect_identical(row3$quota_local_link[[1L]], 0L)
})

test_that("linking spoke quantile bins dynamically fall back for small spokes", {
  items <- tibble::tibble(
    item_id = as.character(1:10),
    set_id = c(rep(1L, 3L), rep(2L, 7L)),
    global_item_id = paste0("g", 1:10)
  )
  state <- adaptive_rank_start(
    items,
    seed = 44L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, spoke_quantile_coverage_bins = 3L)
  )
  proxy <- pairwiseLLM:::.adaptive_rank_proxy(state)
  spoke_ids <- as.character(items$item_id[items$set_id == 2L])

  cov <- pairwiseLLM:::.adaptive_link_spoke_coverage(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    spoke_ids = spoke_ids,
    routing_scores = proxy$scores,
    score_source = "linking_global_score"
  )
  expect_identical(cov$bins_used, 2L)
})

test_that("phase B coverage bins use linking-global score source", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 7L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  ids <- as.character(state$item_ids)
  for (k in seq_len(10L)) {
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(k),
        pair_id = as.integer(k),
        is_cross_set = TRUE,
        link_spoke_id = 2L,
        set_i = 1L,
        set_j = 2L,
        i = as.integer(match("1", ids)),
        j = as.integer(match("5", ids))
      )
    )
  }
  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )
  expect_true(nrow(cand) > 0L)
  expect_true(all(cand$coverage_source == "linking_global_score"))
})

test_that("coverage source switches from Phase A rank to linking-global after early cross-set sparsity", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 70L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state <- mark_link_phase_b_ready(state)
  proxy <- pairwiseLLM:::.adaptive_rank_proxy(state)
  spoke_ids <- as.character(items$item_id[items$set_id == 2L])

  cov_early <- pairwiseLLM:::.adaptive_link_spoke_coverage(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    spoke_ids = spoke_ids,
    routing_scores = proxy$scores,
    score_source = "linking_global_score"
  )
  expect_identical(cov_early$source, "phase_a_rank_mu_raw")

  ids <- as.character(state$item_ids)
  for (k in seq_len(10L)) {
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(k),
        pair_id = as.integer(k),
        is_cross_set = TRUE,
        link_spoke_id = 2L,
        set_i = 1L,
        set_j = 2L,
        i = as.integer(match("1", ids)),
        j = as.integer(match("5", ids))
      )
    )
  }

  cov_late <- pairwiseLLM:::.adaptive_link_spoke_coverage(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    spoke_ids = spoke_ids,
    routing_scores = proxy$scores,
    score_source = "linking_global_score"
  )
  expect_identical(cov_late$source, "linking_global_score")
})

test_that("coverage source propagates through selection and linking stage rows", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 71L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$config$btl_config <- test_link_btl_config(state$config$btl_config %||% list())
  draws <- matrix(
    seq_along(state$item_ids),
    nrow = 4L,
    ncol = length(state$item_ids),
    byrow = TRUE
  )
  colnames(draws) <- as.character(state$item_ids)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")

  sel <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_identical(sel$coverage_source, "phase_a_rank_mu_raw")

  external_candidates <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )
  external_candidates$coverage_source <- NULL
  external_candidates$coverage_bins_used <- NULL
  external_candidates$link_spoke_id <- NULL
  sel_external <- pairwiseLLM:::select_next_pair(
    state,
    step_id = 1L,
    candidates = external_candidates
  )
  expect_identical(sel_external$coverage_source, "phase_a_rank_mu_raw")
  expect_identical(sel_external$coverage_bins_used, 1L)

  state$controller$link_stage_coverage_source <- list(`2` = sel$coverage_source)
  state$controller$link_stage_coverage_bins_used <- list(`2` = as.integer(sel$coverage_bins_used))
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 5L,
      A = 1L,
      B = 5L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log, list(refit_id = 1L, diagnostics_pass = TRUE))
  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(
    state = state,
    refit_context = list(last_refit_step = 0L)
  )
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(row) == 1L)
  expect_identical(row$coverage_source[[1L]], "phase_a_rank_mu_raw")
})

test_that("refit-local routing memo matches direct helper outputs and reuses the current state", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = paste0("g", seq_len(6L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 321L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state <- mark_link_phase_b_ready(state)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 4L,
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )

  controller <- state$controller
  defaults <- adaptive_defaults(length(state$item_ids))
  hub_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == 1L])
  spoke_ids <- as.character(state$items$item_id[as.integer(state$items$set_id) == 2L])
  direct_active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  direct_scores <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = controller,
    active_ids = unique(c(hub_ids, spoke_ids)),
    hub_id = 1L
  )
  direct_anchors <- pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
    state = state,
    hub_ids = hub_ids,
    hub_scores = direct_scores,
    defaults = defaults
  )
  direct_coverage <- pairwiseLLM:::.adaptive_link_spoke_coverage(
    state = state,
    controller = controller,
    spoke_id = 2L,
    spoke_ids = spoke_ids,
    routing_scores = direct_scores,
    score_source = "linking_global_score"
  )

  orig_routing <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores
  orig_coverage <- pairwiseLLM:::.adaptive_link_spoke_coverage
  calls <- new.env(parent = emptyenv())
  calls$routing <- 0L
  calls$coverage <- 0L

  memo <- testthat::with_mocked_bindings(
    .adaptive_link_phase_b_routing_scores = function(...) {
      calls$routing <- as.integer(calls$routing) + 1L
      orig_routing(...)
    },
    .adaptive_link_spoke_coverage = function(...) {
      calls$coverage <- as.integer(calls$coverage) + 1L
      orig_coverage(...)
    },
    {
      first <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state,
        controller = controller,
        spoke_id = 2L,
        defaults = defaults
      )
      second <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state,
        controller = controller,
        spoke_id = 2L,
        defaults = defaults
      )
      list(first = first, second = second)
    },
    .package = "pairwiseLLM"
  )

  expect_identical(as.integer(calls$routing), 1L)
  expect_identical(as.integer(calls$coverage), 1L)
  expect_identical(memo$first$active_items, memo$second$active_items)
  expect_identical(memo$first$routing_scores, memo$second$routing_scores)
  expect_identical(memo$first$hub_anchor_ids, memo$second$hub_anchor_ids)
  expect_identical(memo$first$coverage, memo$second$coverage)
  expect_identical(memo$first$active_items, direct_active)
  expect_identical(memo$first$routing_scores, direct_scores)
  expect_identical(memo$first$hub_anchor_ids, direct_anchors)
  expect_identical(memo$first$coverage, direct_coverage)
})

test_that("linking refit-local inputs invalidate on step, refit, epoch, spoke, and probe-panel boundaries", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", seq_len(7L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 402L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L, `3` = 1L)
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_a",
      link_epoch_id = 1L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    ),
    `3` = tibble::tibble(
      probe_panel_id = "panel_z",
      link_epoch_id = 1L,
      spoke_id = 3L,
      hub_item_id = "h2",
      spoke_item_id = "s31",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pairwiseLLM:::make_unordered_key("h2", "s31"),
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )

  defaults <- adaptive_defaults(length(state$item_ids))
  controller <- state$controller
  orig_build <- pairwiseLLM:::.adaptive_link_refit_local_inputs_build
  calls <- new.env(parent = emptyenv())
  calls$build <- 0L

  memo <- testthat::with_mocked_bindings(
    .adaptive_link_refit_local_inputs_build = function(...) {
      calls$build <- as.integer(calls$build) + 1L
      orig_build(...)
    },
    {
      first <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state,
        controller = controller,
        spoke_id = 2L,
        defaults = defaults
      )
      second <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state,
        controller = controller,
        spoke_id = 2L,
        defaults = defaults
      )

      state_step <- state
      state_step$step_log <- tibble::add_row(state_step$step_log, step_id = 1L)
      third <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state_step,
        controller = state_step$controller,
        spoke_id = 2L,
        defaults = defaults
      )

      fourth <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state_step,
        controller = state_step$controller,
        spoke_id = 3L,
        defaults = defaults
      )

      state_panel <- state_step
      state_panel$linking$probe$panels_by_spoke$`2`$probe_panel_id <- "panel_b"
      fifth <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state_panel,
        controller = state_panel$controller,
        spoke_id = 2L,
        defaults = defaults
      )

      sixth <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state_panel,
        controller = state_panel$controller,
        spoke_id = 2L,
        defaults = defaults,
        refit_id = 2L
      )

      state_epoch <- state_panel
      state_epoch$controller$link_epoch_id_by_spoke$`2` <- 2L
      state_epoch$linking$probe$panels_by_spoke$`2`$link_epoch_id <- 2L
      state_epoch$linking$probe$panels_by_spoke$`2`$probe_panel_id <- "panel_c"
      seventh <- pairwiseLLM:::.adaptive_link_refit_local_inputs(
        state = state_epoch,
        controller = state_epoch$controller,
        spoke_id = 2L,
        defaults = defaults
      )

      list(
        first = first,
        second = second,
        third = third,
        fourth = fourth,
        fifth = fifth,
        sixth = sixth,
        seventh = seventh,
        state_step = state_step,
        state_panel = state_panel,
        state_epoch = state_epoch
      )
    },
    .package = "pairwiseLLM"
  )

  expect_identical(as.integer(calls$build), 6L)
  expect_identical(memo$first, memo$second)
  expect_identical(
    memo$first,
    orig_build(state = state, controller = controller, spoke_id = 2L, defaults = defaults)
  )
  expect_identical(
    memo$third,
    orig_build(
      state = memo$state_step,
      controller = memo$state_step$controller,
      spoke_id = 2L,
      defaults = defaults
    )
  )
  expect_identical(
    memo$fourth,
    orig_build(
      state = memo$state_step,
      controller = memo$state_step$controller,
      spoke_id = 3L,
      defaults = defaults
    )
  )
  expect_identical(
    memo$fifth,
    orig_build(
      state = memo$state_panel,
      controller = memo$state_panel$controller,
      spoke_id = 2L,
      defaults = defaults
    )
  )
  expect_identical(memo$fifth, memo$sixth)
  expect_identical(
    memo$seventh,
    orig_build(
      state = memo$state_epoch,
      controller = memo$state_epoch$controller,
      spoke_id = 2L,
      defaults = defaults
    )
  )
})

test_that("linking deterministic ordering prioritizes coverage before utility", {
  cand <- tibble::tibble(
    i = c("a", "b"),
    j = c("c", "d"),
    u0 = c(0.24, 0.25),
    link_d_opt_gain = c(0.2, 0.9),
    coverage_priority = c(1L, 0L)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord[[1L]], 1L)
})

test_that("linking deterministic ordering ranks by D-opt gain with stable ties", {
  cand <- tibble::tibble(
    i = c("a", "b", "c"),
    j = c("d", "e", "f"),
    link_d_opt_gain = c(0.10, 0.30, 0.30)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord, c(2L, 3L, 1L))
})

test_that("model D order-averaged probability is symmetric across order swap", {
  pbar_1 <- pairwiseLLM:::.adaptive_link_model_d_pbar(
    theta_h = 1.2,
    theta_x = -0.4,
    beta = 0.1,
    epsilon = 0.05
  )
  pbar_2 <- pairwiseLLM:::.adaptive_link_model_d_pbar(
    theta_h = -0.4,
    theta_x = 1.2,
    beta = 0.1,
    epsilon = 0.05
  )
  expect_equal(pbar_1, pbar_2, tolerance = 1e-12)
  expect_true(is.finite(pbar_1))
  expect_true(pbar_1 >= 0 && pbar_1 <= 1)
})

test_that("linking information gradient follows transform-mode formulas", {
  g_shift <- pairwiseLLM:::.adaptive_link_info_gradient(
    transform_mode = "shift_only",
    alpha = 1,
    theta_raw_x = 2
  )
  g_scale <- pairwiseLLM:::.adaptive_link_info_gradient(
    transform_mode = "shift_scale",
    alpha = 1.5,
    theta_raw_x = -2
  )
  expect_identical(dim(g_shift), c(1L, 1L))
  expect_equal(as.numeric(g_shift), 1)
  expect_identical(dim(g_scale), c(2L, 1L))
  expect_equal(as.numeric(g_scale), c(1, -3), tolerance = 1e-12)
})

test_that("linking deterministic ordering prioritizes D-opt gain before fallback utility", {
  cand <- tibble::tibble(
    i = c("h1", "h2"),
    j = c("s1", "s2"),
    u0 = c(0.26, 0.25),
    link_u = c(0.20, 0.40),
    link_d_opt_gain = c(0.80, 0.10)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord[[1L]], 1L)
})

test_that("D-opt helper guards cover non-finite and malformed inputs", {
  expect_true(is.na(pairwiseLLM:::.adaptive_link_model_d_prob(NA_real_, 0, 0, 0.1)))
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_model_d_prob(0, 0, NA_real_, NA_real_)))

  expect_true(is.na(pairwiseLLM:::.adaptive_link_model_d_pbar(NA_real_, 0, 0, 0)))

  g_bad <- pairwiseLLM:::.adaptive_link_info_gradient(
    transform_mode = "shift_scale",
    alpha = NA_real_,
    theta_raw_x = NA_real_
  )
  expect_equal(as.numeric(g_bad), c(1, 0), tolerance = 1e-12)

  expect_true(is.na(pairwiseLLM:::.adaptive_link_logdet_spd(matrix(c(1, 2, 3), nrow = 1L))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_d_opt_gain_logdet(diag(1), diag(2))))

  bad_ctl <- list(
    link_d_opt_it_by_spoke = list(
      `1::2` = list(it = matrix(1, nrow = 1L, ncol = 2L), it_n_pairs_accumulated = -1L)
    )
  )
  st <- pairwiseLLM:::.adaptive_link_d_opt_state_get(
    controller = bad_ctl,
    refit_id = 1L,
    spoke_id = 2L,
    transform_mode = "shift_only"
  )
  expect_true(is.matrix(st$it))
  expect_identical(dim(st$it), c(1L, 1L))
  expect_identical(st$it_n_pairs_accumulated, 0L)
  expect_true(is.finite(st$it_logdet_start))
})

test_that("vectorized predictive helpers match scalar helper outputs exactly", {
  theta_a <- c(0.8, -0.3, NA_real_, 0.1)
  theta_b <- c(-0.4, 0.2, 0.5, NA_real_)

  prob_vec <- pairwiseLLM:::.adaptive_link_model_d_prob_vec(
    theta_a = theta_a,
    theta_b = theta_b,
    beta = 0.2,
    epsilon = 0.1
  )
  prob_ref <- vapply(seq_along(theta_a), function(idx) {
    pairwiseLLM:::.adaptive_link_model_d_prob(
      theta_a = theta_a[[idx]],
      theta_b = theta_b[[idx]],
      beta = 0.2,
      epsilon = 0.1
    )
  }, numeric(1L))
  expect_equal(prob_vec, prob_ref, tolerance = 0)

  pbar_vec <- pairwiseLLM:::.adaptive_link_model_d_pbar_vec(
    theta_h = theta_a,
    theta_x = theta_b,
    beta = 0.2,
    epsilon = 0.1
  )
  pbar_ref <- vapply(seq_along(theta_a), function(idx) {
    pairwiseLLM:::.adaptive_link_model_d_pbar(
      theta_h = theta_a[[idx]],
      theta_x = theta_b[[idx]],
      beta = 0.2,
      epsilon = 0.1
    )
  }, numeric(1L))
  expect_equal(pbar_vec, pbar_ref, tolerance = 0)
})

test_that("cached-start D-opt helper matches the uncached helper exactly", {
  it <- matrix(c(1.3, 0.2, 0.2, 0.9), nrow = 2L)
  ipair <- matrix(c(0.4, 0.1, 0.1, 0.3), nrow = 2L)
  logdet_start <- pairwiseLLM:::.adaptive_link_logdet_spd(it, ridge = 1e-6)

  expect_equal(
    pairwiseLLM:::.adaptive_link_d_opt_gain_logdet_from_start(
      it = it,
      ipair = ipair,
      logdet_start = logdet_start,
      ridge = 1e-6
    ),
    pairwiseLLM:::.adaptive_link_d_opt_gain_logdet(it = it, ipair = ipair, ridge = 1e-6),
    tolerance = 0
  )
})

test_that("rank-one D-opt helpers stay numerically aligned with the legacy logdet path", {
  legacy_gain <- function(it, ipair, ridge = 1e-6) {
    logdet_ref <- function(mat) {
      x <- as.matrix(mat)
      x <- (x + t(x)) / 2
      x <- x + diag(ridge, nrow(x))
      sum(log(eigen(x, symmetric = TRUE, only.values = TRUE)$values))
    }
    logdet_ref(it + ipair) - logdet_ref(it)
  }

  prepared_transform <- pairwiseLLM:::.adaptive_link_d_opt_rank1_prepare(
    matrix(c(1.4, 0.25, 0.25, 0.9), nrow = 2L),
    ridge = 1e-6
  )
  transform_gain <- pairwiseLLM:::.adaptive_link_d_opt_rank1_gain_transform(
    prepared = prepared_transform,
    info_scale = 0.21,
    transform_mode = "shift_scale",
    alpha = 1.3,
    theta_raw_x = -2
  )
  transform_g <- pairwiseLLM:::.adaptive_link_info_gradient(
    transform_mode = "shift_scale",
    alpha = 1.3,
    theta_raw_x = -2
  )
  transform_legacy <- legacy_gain(
    it = matrix(c(1.4, 0.25, 0.25, 0.9), nrow = 2L),
    ipair = as.matrix(0.21 * (transform_g %*% t(transform_g))),
    ridge = 1e-6
  )
  expect_equal(transform_gain, transform_legacy, tolerance = 1e-12)

  prepared_anchored <- pairwiseLLM:::.adaptive_link_d_opt_rank1_prepare(
    matrix(c(1.2, 0.15, 0.15, 1.1), nrow = 2L),
    ridge = 1e-6
  )
  anchored_gain <- pairwiseLLM:::.adaptive_link_d_opt_rank1_gain_diag(
    prepared = prepared_anchored,
    info_scale = 0.24,
    diag_index = 1L
  )
  anchored_legacy <- legacy_gain(
    it = matrix(c(1.2, 0.15, 0.15, 1.1), nrow = 2L),
    ipair = matrix(c(0.24, 0, 0, 0), nrow = 2L),
    ridge = 1e-6
  )
  expect_equal(anchored_gain, anchored_legacy, tolerance = 1e-12)
})

test_that("anchored-joint diagonal D-opt helpers avoid dense state while matching logdet", {
  it_diag <- c(1.2, 1.1)
  info_scale <- c(0.24, 0.18)
  diag_index <- c(1L, 2L)
  diag_gain <- pairwiseLLM:::.adaptive_link_d_opt_gain_diag_state(
    it_diag = it_diag,
    info_scale = info_scale,
    diag_index = diag_index,
    ridge = 1e-6
  )
  legacy_gain <- vapply(seq_along(info_scale), function(idx) {
    ipair <- matrix(0, nrow = 2L, ncol = 2L)
    ipair[diag_index[[idx]], diag_index[[idx]]] <- info_scale[[idx]]
    pairwiseLLM:::.adaptive_link_d_opt_gain_logdet(
      it = diag(it_diag, nrow = 2L),
      ipair = ipair,
      ridge = 1e-6
    )
  }, numeric(1L))

  expect_equal(diag_gain, legacy_gain, tolerance = 1e-12)
  prepared <- pairwiseLLM:::.adaptive_link_d_opt_diag_prepare(it_diag, ridge = 1e-6)
  expect_true(isTRUE(prepared$ok))
  expect_equal(prepared$trace, sum(it_diag), tolerance = 0)
})

test_that("selection utility helpers cover additional fallback branches", {
  expect_identical(
    pairwiseLLM:::.adaptive_selection_utility_mode(
      run_mode = "within_set",
      is_cross_set = FALSE
    ),
    "pairing_trueskill_u0"
  )
  expect_true(is.na(pairwiseLLM:::.adaptive_resolve_selection_column("pairing_trueskill_u")))
  expect_true(is.na(pairwiseLLM:::.adaptive_resolve_selection_column("unknown")))
})

test_that("theta/global and predictive utility helpers handle empty and sparse domains", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 201L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)

  expect_identical(
    pairwiseLLM:::.adaptive_link_theta_global_map_for_items(
      state = state,
      controller = state$controller,
      item_ids = character()
    ),
    stats::setNames(numeric(), character())
  )

  sparse_items <- tibble::tibble(
    item_id = c("h1", "s1"),
    set_id = c(1L, 2L),
    global_item_id = c("gh1", "gs1")
  )
  sparse_state <- adaptive_rank_start(
    sparse_items,
    seed = 202L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  sparse_state$warm_start_done <- TRUE
  sparse_state <- mark_link_phase_b_ready(sparse_state)

  sparse <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = tibble::tibble(i = "h1", j = "missing"),
    state = sparse_state,
    controller = sparse_state$controller,
    spoke_id = 2L
  )
  expect_true(is.na(sparse$link_p[[1L]]))
  expect_true(is.na(sparse$link_u[[1L]]))

  expect_error(
    pairwiseLLM:::.adaptive_link_predictive_prob_oriented(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      A_id = "missing",
      B_id = "h1"
    )
  )
})

test_that("predictive utility scoring receives full linking controller fields", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 101L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state <- mark_link_phase_b_ready(state)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L

  seen_judge_mode <- new.env(parent = emptyenv())
  seen_judge_mode$value <- NA_character_
  cand <- tibble::tibble(i = c("h1", "h2"), j = c("s1", "s2"), link_spoke_id = c(2L, 2L))
  testthat::with_mocked_bindings(
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      seen_judge_mode$value <- as.character(controller$judge_param_mode %||% NA_character_)
      candidates$link_p <- as.double(candidates$p %||% rep(0.5, nrow(candidates)))
      candidates$link_u <- as.double(candidates$link_p * (1 - candidates$link_p))
      candidates$link_d_opt_gain <- rep(1, nrow(candidates))
      candidates
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )
  expect_identical(seen_judge_mode$value, "global_shared")
})

test_that("active spoke routing handles no-spoke and single-spoke modes deterministically", {
  items_single <- tibble::tibble(
    item_id = c("h1", "h2"),
    set_id = c(1L, 1L),
    global_item_id = c("gh1", "gh2")
  )
  state_single <- adaptive_rank_start(items_single, seed = 1L)
  controller_single <- state_single$controller
  controller_single$run_mode <- "link_one_spoke"
  controller_single$hub_id <- 1L
  expect_true(is.na(pairwiseLLM:::.adaptive_link_active_spoke(state_single, controller_single)))

  items_multi <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state_multi <- adaptive_rank_start(
    items_multi,
    seed = 2L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  expect_identical(pairwiseLLM:::.adaptive_link_active_spoke(state_multi, state_multi$controller), 2L)

  state_multi$controller$current_link_spoke_id <- 2L
  expect_identical(pairwiseLLM:::.adaptive_link_active_spoke(state_multi, state_multi$controller), 2L)
})

test_that("concurrent active spoke routing falls back deterministically when deficits are exhausted", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 3L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )
  # No history: chooses smallest spoke id by deterministic tie-break.
  expect_identical(pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller), 2L)

  # Equal counts: deterministic tie handling still yields a stable spoke choice.
  ids <- as.character(state$item_ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L, pair_id = 1L, is_cross_set = TRUE, link_spoke_id = 2L,
      A = match("h1", ids), B = match("s21", ids)
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L, pair_id = 2L, is_cross_set = TRUE, link_spoke_id = 3L,
      A = match("h2", ids), B = match("s31", ids)
    )
  )
  expect_true(is.na(pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)))
})

test_that("concurrent selector falls back to next eligible spoke in same step when primary is infeasible", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 77L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$controller$global_identified <- TRUE
  state$controller$explore_taper_mult <- 0
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )
  phase_ctx <- pairwiseLLM:::.adaptive_link_phase_context(state, controller = state$controller)
  expect_identical(phase_ctx$phase, "phase_b")
  expect_identical(sort(phase_ctx$ready_spokes), c(2L, 3L))

  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                    link_spoke_id = NA_integer_) {
      if (is.na(link_spoke_id) || as.integer(link_spoke_id) == 2L) {
        return(tibble::tibble(i = character(), j = character()))
      }
      tibble::tibble(i = "h1", j = "s31")
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_false(isTRUE(out$candidate_starved))
  expect_identical(out$link_spoke_id_selected, 3L)
  set_i <- as.integer(state$items$set_id[[out$i]])
  set_j <- as.integer(state$items$set_id[[out$j]])
  expect_true(xor(set_i == 1L, set_j == 1L))
})

test_that("concurrent fallback memoizes per-spoke stage context within a selector call", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 80L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )
  # Force spoke 2 to advance past anchor stage while spoke 3 remains at anchor.
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::2` = list(anchor_link = TRUE)
  )
  phase_ctx <- pairwiseLLM:::.adaptive_link_phase_context(state, controller = state$controller)
  expect_identical(phase_ctx$phase, "phase_b")
  expect_identical(sort(phase_ctx$ready_spokes), c(2L, 3L))
  expect_identical(sort(phase_ctx$active_spokes), c(2L, 3L))

  refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  quota_controller2 <- state$controller
  quota_controller2$current_link_spoke_id <- 2L
  stage_quotas2 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = as.integer(state$round$round_id),
    n_items = as.integer(state$n_items),
    controller = quota_controller2
  )
  quota_controller3 <- state$controller
  quota_controller3$current_link_spoke_id <- 3L
  stage_quotas3 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = as.integer(state$round$round_id),
    n_items = as.integer(state$n_items),
    controller = quota_controller3
  )
  progress2 <- pairwiseLLM:::.adaptive_link_stage_progress(
    state = state,
    spoke_id = 2L,
    stage_quotas = stage_quotas2,
    stage_order = state$round$stage_order,
    refit_id = refit_id
  )
  progress3 <- pairwiseLLM:::.adaptive_link_stage_progress(
    state = state,
    spoke_id = 3L,
    stage_quotas = stage_quotas3,
    stage_order = state$round$stage_order,
    refit_id = refit_id
  )
  expect_true(progress2$active_stage %in% c(names(stage_quotas2), "pooled_backfill"))
  expect_true(progress3$active_stage %in% c(names(stage_quotas3), "pooled_backfill"))
  expect_gte(
    as.integer(progress2$stage_committed[["anchor_link"]]),
    as.integer(progress3$stage_committed[["anchor_link"]])
  )

  orig_compute_quotas <- pairwiseLLM:::.adaptive_round_compute_quotas
  orig_stage_progress <- pairwiseLLM:::.adaptive_link_stage_progress
  calls <- new.env(parent = emptyenv())
  calls$quota <- stats::setNames(integer(), character())
  calls$progress <- stats::setNames(integer(), character())

  out <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      key <- as.character(as.integer(controller$current_link_spoke_id %||% NA_integer_))
      current <- as.integer(calls$quota[key])
      current[is.na(current)] <- 0L
      calls$quota[key] <- as.integer(current[[1L]] %||% 0L) + 1L
      orig_compute_quotas(round_id = round_id, n_items = n_items, controller = controller)
    },
    .adaptive_link_stage_progress = function(state, spoke_id, stage_quotas, stage_order, refit_id = NULL,
                                             adjust_for_feasibility = TRUE) {
      key <- as.character(as.integer(spoke_id))
      current <- as.integer(calls$progress[key])
      current[is.na(current)] <- 0L
      calls$progress[key] <- as.integer(current[[1L]] %||% 0L) + 1L
      orig_stage_progress(
        state = state,
        spoke_id = spoke_id,
        stage_quotas = stage_quotas,
        stage_order = stage_order,
        refit_id = refit_id,
        adjust_for_feasibility = adjust_for_feasibility
      )
    },
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                    link_spoke_id = NA_integer_) {
      tibble::tibble(i = character(), j = character())
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(as.integer(calls$quota[["2"]]), 1L)
  expect_identical(as.integer(calls$quota[["3"]]), 1L)
  expect_identical(as.integer(calls$progress[["2"]]), 1L)
  expect_identical(as.integer(calls$progress[["3"]]), 1L)
})

test_that("concurrent selector starves only after all eligible spokes are infeasible", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 78L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$controller$global_identified <- TRUE
  state$controller$explore_taper_mult <- 0
  state$refit_meta$last_refit_step <- 0L
  phase_ctx <- pairwiseLLM:::.adaptive_link_phase_context(state, controller = state$controller)
  expect_identical(phase_ctx$phase, "phase_b")
  expect_identical(sort(phase_ctx$ready_spokes), c(2L, 3L))

  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                    link_spoke_id = NA_integer_) {
      tibble::tibble(i = character(), j = character())
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(out$starvation_reason, "all_eligible_spokes_infeasible")
  expect_true(as.integer(out$link_spoke_id_selected) %in% c(2L, 3L))
})

test_that("selector reports hard-filter starvation when raw Phase B candidates collapse", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 84L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$controller$global_identified <- TRUE
  state$refit_meta$last_refit_step <- 0L

  out <- testthat::with_mocked_bindings(
    .adaptive_select_stage = function(...) {
      list(
        selected = tibble::tibble(),
        counts = list(
          n_candidates_generated = 11L,
          n_candidates_after_hard_filters = 0L,
          n_candidates_after_duplicates = 0L,
          n_candidates_after_star_caps = 0L,
          n_candidates_scored = 0L
        ),
        star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_,
        recent_deg = NULL
      )
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(out$starvation_reason, "filtered_by_hard_filters")
  expect_gt(as.integer(out$n_candidates_generated), 0L)
  expect_identical(as.integer(out$n_candidates_after_hard_filters), 0L)
})

test_that("selector reports exposure-filter starvation when exposure is the final hard gate", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 85L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$controller$global_identified <- TRUE
  state$refit_meta$last_refit_step <- 0L

  out <- testthat::with_mocked_bindings(
    .adaptive_select_stage = function(...) {
      list(
        selected = tibble::tibble(),
        counts = list(
          n_candidates_generated = 11L,
          n_candidates_after_route_filters = 11L,
          n_candidates_after_active_domain = 11L,
          n_candidates_after_stage_filters = 11L,
          n_candidates_after_exposure_filters = 0L,
          n_candidates_after_hard_filters = 0L,
          n_candidates_after_duplicates = 0L,
          n_candidates_after_star_caps = 0L,
          n_candidates_scored = 0L
        ),
        star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_,
        recent_deg = NULL
      )
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(out$starvation_reason, "filtered_by_exposure_filters")
  expect_identical(as.integer(out$n_candidates_after_route_filters), 11L)
  expect_identical(as.integer(out$n_candidates_after_active_domain), 11L)
  expect_identical(as.integer(out$n_candidates_after_stage_filters), 11L)
  expect_identical(as.integer(out$n_candidates_after_exposure_filters), 0L)
  expect_identical(out$hard_filter_collapse_stage, "filtered_by_exposure_filters")
})

test_that("concurrent fallback ordering is deterministic under fixed state and seed", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 79L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$controller$global_identified <- TRUE
  state$controller$explore_taper_mult <- 0
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )

  draw_once <- function() {
    testthat::with_mocked_bindings(
      generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                      link_spoke_id = NA_integer_) {
        if (is.na(link_spoke_id) || as.integer(link_spoke_id) == 2L) {
          return(tibble::tibble(i = character(), j = character()))
        }
        tibble::tibble(i = "h1", j = "s31")
      },
      pairwiseLLM:::select_next_pair(state, step_id = 5L),
      .package = "pairwiseLLM"
    )
  }

  out1 <- draw_once()
  out2 <- draw_once()
  expect_identical(out1$link_spoke_id_selected, out2$link_spoke_id_selected)
  expect_identical(out1$i, out2$i)
  expect_identical(out1$j, out2$j)
})

test_that("cross_set_utility_pre logs linking utility before commit in linking mode", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 11L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state <- mark_link_phase_b_ready(state)
  judge <- make_deterministic_judge("i_wins")
  out <- pairwiseLLM:::run_one_step(state, judge)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_equal(row$utility_mode[[1L]], "linking_d_optimal_anchored_joint")
  expect_true(is.finite(row$cross_set_utility_pre[[1L]]))
  expect_gte(row$cross_set_utility_pre[[1L]], 0)
})

test_that("cross-set ordering aborts when canonical D-opt utility is missing", {
  cand <- tibble::tibble(
    i = c("h1", "h2"),
    j = c("s1", "s2"),
    u0 = c(0.26, 0.24),
    link_u = c(0.20, 0.28)
  )
  expect_error(
    pairwiseLLM:::.adaptive_linking_selection_order(
      cand,
      stage_name = "mid_link",
      spoke_id = 2L
    ),
    paste0(
      "adaptive_linking_selection_order invariant failed: canonical D-opt ordering ",
      "could not proceed for stage=mid_link, spoke_id=2 because `link_d_opt_gain` is unavailable"
    )
  )
})

test_that("pairing ordering ignores linking utility fields", {
  cand <- tibble::tibble(
    i = c("a", "b"),
    j = c("c", "d"),
    u0 = c(0.24, 0.26),
    link_u = c(0.90, 0.10)
  )
  utility_mode <- pairwiseLLM:::.adaptive_selection_utility_mode(
    run_mode = "within_set",
    is_cross_set = FALSE
  )
  utility_col <- pairwiseLLM:::.adaptive_resolve_selection_column(utility_mode)
  ord <- order(-as.double(cand[[utility_col]]), cand$i, cand$j)
  expect_identical(ord[[1L]], 2L)
})

test_that("linking deterministic ordering aborts when D-opt utility is fully non-finite", {
  cand <- tibble::tibble(
    i = c("a", "b", "c"),
    j = c("d", "e", "f"),
    u0 = c(0.20, 0.30, 0.30),
    link_u = c(0.10, 0.40, 0.30),
    link_d_opt_gain = c(NA_real_, NaN, Inf)
  )
  expect_error(
    pairwiseLLM:::.adaptive_linking_selection_order(
      cand,
      utility_mode = "linking_d_optimal_transform",
      stage_name = "local_link",
      spoke_id = 2L
    ),
    paste0(
      "adaptive_linking_selection_order invariant failed: canonical D-opt ordering ",
      "could not proceed for stage=local_link, spoke_id=2 because all `link_d_opt_gain` values were non-finite"
    )
  )
})

test_that("pooled backfill ordering shifts toward blocker-weighted stages but stays deterministic when neutral", {
  cand <- tibble::tibble(
    i = c("h1", "h1", "h1"),
    j = c("s21", "s22", "s23"),
    link_stage = c("anchor_link", "mid_link", "local_link"),
    link_d_opt_gain = c(0.5, 0.5, 0.5)
  )
  set_map <- c(h1 = 1L, s21 = 2L, s22 = 2L, s23 = 2L)

  ord_neutral <- pairwiseLLM:::.adaptive_link_backfill_order(
    cand,
    hub_id = 1L,
    set_map = set_map,
    blocker_stage_weights = c(anchor_link = 1, long_link = 1, mid_link = 1, local_link = 1)
  )
  expect_identical(ord_neutral, c(1L, 2L, 3L))

  ord_theta <- pairwiseLLM:::.adaptive_link_backfill_order(
    cand,
    hub_id = 1L,
    set_map = set_map,
    blocker_stage_weights = c(anchor_link = 1, long_link = 1, mid_link = 2, local_link = 3)
  )
  expect_identical(ord_theta, c(3L, 2L, 1L))
})

test_that("pooled backfill ordering aborts when D-opt utility is fully non-finite", {
  cand <- tibble::tibble(
    i = c("h1", "h1"),
    j = c("s21", "s22"),
    link_stage = c("anchor_link", "mid_link"),
    link_d_opt_gain = c(NA_real_, NaN)
  )
  set_map <- c(h1 = 1L, s21 = 2L, s22 = 2L)

  expect_error(
    pairwiseLLM:::.adaptive_link_backfill_order(
      cand,
      hub_id = 1L,
      set_map = set_map,
      spoke_id = 2L
    ),
    paste0(
      "adaptive_link_backfill_order invariant failed: canonical D-opt ordering ",
      "could not proceed for stage=pooled_backfill, spoke_id=2 because all `link_d_opt_gain` ",
      "values were non-finite"
    )
  )
})

test_that("concurrent spoke ranking breaks matched deficits toward stronger canonical blockers", {
  state <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
      set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
      global_item_id = paste0("g", seq_len(7L))
    ),
    seed = 404L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(probe_panel_shortfall = 0L),
    `3` = list(probe_panel_shortfall = 30L, probe_edges_min_for_stop_used = 30L)
  )

  ranked <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(...) {
      list(
        `2` = list(
          B_spoke_refit_budget = 2L,
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        ),
        `3` = list(
          B_spoke_refit_budget = 2L,
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        )
      )
    },
    pairwiseLLM:::.adaptive_link_ranked_spokes(
      state = state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(ranked, c(3L, 2L))
})

test_that("ranked spokes retire concurrent targets once a spoke budget is fully spent", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 182L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$refit_meta$last_refit_step <- 0L
  add_cross_row <- function(state, step_id, pair_id, hub_item, spoke_item, spoke_id) {
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(step_id),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
        pair_id = as.integer(pair_id),
        i = match(hub_item, state$item_ids),
        j = match(spoke_item, state$item_ids),
        A = match(hub_item, state$item_ids),
        B = match(spoke_item, state$item_ids),
        Y = 1L,
        set_i = 1L,
        set_j = as.integer(spoke_id),
        is_cross_set = TRUE,
        is_probe_step = FALSE,
        is_holdout_probe_step = FALSE,
        is_drift_probe_step = FALSE,
        link_spoke_id = as.integer(spoke_id),
        run_mode = "link_multi_spoke",
        link_stage = "anchor_link",
        round_stage = "anchor_link"
      )
    )
    state
  }
  state <- add_cross_row(state, 1L, 1L, "h1", "s21", 2L)
  state <- add_cross_row(state, 2L, 2L, "h1", "s31", 3L)
  state <- add_cross_row(state, 3L, 3L, "h2", "s32", 3L)

  ranked <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(...) {
      list(
        `2` = list(
          B_spoke_refit_budget = 2L,
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        ),
        `3` = list(
          B_spoke_refit_budget = 2L,
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        )
      )
    },
    pairwiseLLM:::.adaptive_link_ranked_spokes(
      state = state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(as.integer(ranked), 2L)
})

test_that("selector does not fall through to a target-met concurrent spoke", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 183L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$refit_meta$last_refit_step <- 0L
  add_cross_row <- function(state, step_id, pair_id, hub_item, spoke_item, spoke_id) {
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(step_id),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
        pair_id = as.integer(pair_id),
        i = match(hub_item, state$item_ids),
        j = match(spoke_item, state$item_ids),
        A = match(hub_item, state$item_ids),
        B = match(spoke_item, state$item_ids),
        Y = 1L,
        set_i = 1L,
        set_j = as.integer(spoke_id),
        is_cross_set = TRUE,
        is_probe_step = FALSE,
        is_holdout_probe_step = FALSE,
        is_drift_probe_step = FALSE,
        link_spoke_id = as.integer(spoke_id),
        run_mode = "link_multi_spoke",
        link_stage = "anchor_link",
        round_stage = "anchor_link"
      )
    )
    state
  }
  state <- add_cross_row(state, 1L, 1L, "h1", "s21", 2L)
  state <- add_cross_row(state, 2L, 2L, "h1", "s31", 3L)
  state <- add_cross_row(state, 3L, 3L, "h2", "s32", 3L)

  out <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(...) {
      list(
        `2` = list(
          B_spoke_refit_budget = 2L,
          B_spoke_refit_budget_source = "concurrent_allocator",
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        ),
        `3` = list(
          B_spoke_refit_budget = 2L,
          B_spoke_refit_budget_source = "concurrent_allocator",
          concurrent_utility_mass = 1,
          concurrent_floor_pairs = 1L
        )
      )
    },
    generate_stage_candidates_from_state = function(
      state, stage_name, fallback_name, C_max, seed, link_spoke_id = NA_integer_
    ) {
      if (identical(as.integer(link_spoke_id), 2L)) {
        return(tibble::tibble())
      }
      tibble::tibble(
        i = "h1",
        j = "s31",
        p = 0.5,
        u0 = 0.5,
        link_spoke_id = 3L
      )
    },
    .adaptive_select_stage = function(
      stage,
      state,
      config,
      controller,
      generation_stage,
      round,
      history_state,
      counts,
      step_id,
      seed_base,
      candidates
    ) {
      cand <- tibble::as_tibble(candidates)
      n_cand <- as.integer(nrow(cand))
      list(
        selected = cand,
        counts = list(
          n_candidates_generated = n_cand,
          n_candidates_after_route_filters = n_cand,
          n_candidates_after_active_domain = n_cand,
          n_candidates_after_stage_filters = n_cand,
          n_candidates_after_exposure_filters = n_cand,
          n_candidates_after_hard_filters = n_cand,
          n_candidates_after_duplicates = n_cand,
          n_candidates_after_star_caps = n_cand,
          n_candidates_scored = n_cand
        ),
        star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
        recent_deg = integer(),
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_
      )
    },
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      tibble::as_tibble(candidates)
    },
    pairwiseLLM:::select_next_pair(state, step_id = 4L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(as.integer(out$link_spoke_id_selected), 2L)
})

test_that("frozen spokes are retired from ranked routing immediately", {
  items <- tibble::tibble(
    item_id = as.character(1:9),
    set_id = c(rep(1L, 3L), rep(2L, 3L), rep(3L, 3L)),
    global_item_id = paste0("g", 1:9)
  )
  state <- adaptive_rank_start(
    items,
    seed = 101L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
  state$controller$probe_pairs_per_refit_per_spoke <- 2L
  ranked <- pairwiseLLM:::.adaptive_link_ranked_spokes(
    state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )

  expect_identical(ranked, 3L)
})

test_that("selector keeps frozen concurrent spokes retired after controller reduction", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = paste0("g", seq_len(9L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 214L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$controller$link_state_frozen_by_spoke <- list(`3` = TRUE)
  state$controller$link_transform_frozen_by_spoke <- list(`3` = FALSE)
  state$controller$link_stopped_by_spoke <- list(`2` = FALSE, `3` = TRUE)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(delta_spoke_mean = 0, log_alpha_spoke_mean = 0, link_identified = FALSE),
    `3` = list(delta_spoke_mean = 0, log_alpha_spoke_mean = 0, link_identified = TRUE)
  )

  reduced <- pairwiseLLM:::.adaptive_resolve_controller(state, adaptive_defaults(nrow(items)))
  expect_true(isTRUE(reduced$link_state_frozen_by_spoke[["3"]]))
  expect_true(isTRUE(reduced$link_stopped_by_spoke[["3"]]))
  expect_identical(
    pairwiseLLM:::.adaptive_link_ranked_spokes(
      state,
      controller = reduced,
      eligible_spoke_ids = c(2L, 3L)
    ),
    2L
  )

  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(
      state, stage_name, fallback_name, C_max, seed, link_spoke_id = NA_integer_
    ) {
      tibble::tibble(
        i = "h1",
        j = if (identical(as.integer(link_spoke_id), 2L)) "s21" else "s31",
        p = 0.5,
        u0 = 0.5,
        link_spoke_id = as.integer(link_spoke_id)
      )
    },
    .adaptive_select_stage = function(
      stage,
      state,
      config,
      controller,
      generation_stage,
      round,
      history_state,
      counts,
      step_id,
      seed_base,
      candidates
    ) {
      cand <- tibble::as_tibble(candidates)
      spoke_id <- unique(as.integer(cand$link_spoke_id))
      if (identical(spoke_id, 2L)) {
        cand <- cand[0, , drop = FALSE]
      }
      n_cand <- as.integer(nrow(cand))
      list(
        selected = cand,
        counts = list(
          n_candidates_generated = n_cand,
          n_candidates_after_route_filters = n_cand,
          n_candidates_after_active_domain = n_cand,
          n_candidates_after_stage_filters = n_cand,
          n_candidates_after_exposure_filters = n_cand,
          n_candidates_after_hard_filters = n_cand,
          n_candidates_after_duplicates = n_cand,
          n_candidates_after_star_caps = n_cand,
          n_candidates_scored = n_cand
        ),
        star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
        recent_deg = integer(),
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_
      )
    },
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      cand <- tibble::as_tibble(candidates)
      cand$link_d_opt_gain <- 1
      cand$link_u <- 1
      cand
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(as.integer(out$link_spoke_id_selected), 2L)
})

test_that("concurrent spoke stage progress is computed per spoke", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = paste0("g", seq_len(9L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 303L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  judge <- make_deterministic_judge("i_wins")

  for (idx in seq_len(4L)) {
    state <- pairwiseLLM:::run_one_step(state, judge)
    state <- pairwiseLLM:::.adaptive_round_commit(state, state$step_log[nrow(state$step_log), , drop = FALSE])
  }

  quotas_2 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = nrow(items),
    controller = utils::modifyList(state$controller, list(current_link_spoke_id = 2L))
  )
  quotas_3 <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = nrow(items),
    controller = utils::modifyList(state$controller, list(current_link_spoke_id = 3L))
  )
  p3_before <- pairwiseLLM:::.adaptive_link_stage_progress(state, 3L, quotas_3, pairwiseLLM:::.adaptive_stage_order())
  state2 <- state
  state2$step_log <- dplyr::bind_rows(
    state2$step_log,
    tibble::tibble(
      pair_id = 999L,
      step_id = as.integer(max(as.integer(state2$step_log$step_id), na.rm = TRUE) + 1L),
      i = match("h1", state2$item_ids),
      j = match("s21", state2$item_ids),
      A = match("h1", state2$item_ids),
      B = match("s21", state2$item_ids),
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )
  p3_after <- pairwiseLLM:::.adaptive_link_stage_progress(state2, 3L, quotas_3, pairwiseLLM:::.adaptive_stage_order())
  p2_after <- pairwiseLLM:::.adaptive_link_stage_progress(state2, 2L, quotas_2, pairwiseLLM:::.adaptive_stage_order())

  expect_identical(p3_before$stage_committed, p3_after$stage_committed)
  expect_true(any(p2_after$stage_committed >= 0L))
})

test_that("link stop rows update per-spoke stop state in controller metadata", {
  items <- tibble::tibble(
    item_id = as.character(1:9),
    set_id = c(rep(1L, 3L), rep(2L, 3L), rep(3L, 3L)),
    global_item_id = paste0("g", 1:9)
  )
  state <- adaptive_rank_start(
    items,
    seed = 202L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state <- mark_link_phase_b_ready(state)
  rows <- tibble::tibble(
    refit_id = c(1L, 1L),
    spoke_id = c(2L, 3L),
    link_stop_pass = c(TRUE, FALSE)
  )
  out <- pairwiseLLM:::.adaptive_link_apply_stop_state(state, rows)
  phase_ctx <- pairwiseLLM:::.adaptive_link_phase_context(out, controller = out$controller)

  expect_true(isTRUE(out$controller$link_stopped_by_spoke[["2"]]))
  expect_true(isFALSE(out$controller$link_stopped_by_spoke[["3"]]))
  expect_identical(out$controller$link_stop_refit_id_by_spoke[["2"]], 1L)
  expect_true(isTRUE(out$controller$link_transform_frozen_by_spoke[["2"]]))
  expect_identical(out$controller$link_transform_frozen_refit_id_by_spoke[["2"]], 1L)
  expect_true(all(sort(phase_ctx$active_spokes) == c(2L, 3L)))
})

test_that("frozen spokes do not emit post-freeze probe or active steps", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = paste0("g", seq_len(6L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 213L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
  state$controller$link_transform_frozen_delta_by_spoke <- list(`2` = 0)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(
    link_transform_state = "shift_only",
    delta_spoke_mean = 0,
    delta_spoke_sd = 0.1
  ))

  n_before <- nrow(state$step_log)
  out <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  expect_identical(nrow(out$step_log), n_before)
})

test_that("planned holdout probe edges are excluded from active linking candidates", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = paste0("g", seq_len(6L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 77L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state <- pairwiseLLM:::.adaptive_link_probe_ensure_panels(state, controller = state$controller, spoke_ids = 2L)
  panel <- state$linking$probe$panels_by_spoke[["2"]]
  expect_true(nrow(panel) >= 1L)

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state = state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 100L,
    seed = 1L,
    link_spoke_id = 2L
  )
  reserved <- unique(as.character(panel$pair_key))
  cand_keys <- vapply(seq_len(nrow(cand)), function(idx) {
    pairwiseLLM:::make_unordered_key(cand$i[[idx]], cand$j[[idx]])
  }, character(1L))
  expect_false(any(cand_keys %in% reserved))
})

test_that("direct Phase B builders match reference stage domains and pooled backfill pools", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24"),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", seq_len(8L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 170L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)

  local_inputs <- list(
    hub_ids = c("h1", "h2", "h3", "h4"),
    spoke_ids = c("s21", "s22", "s23", "s24"),
    active_items = list(active_hub = c("h1", "h2", "h3")),
    routing_scores = stats::setNames(
      c(9, 6, 4, 1, 8, 5, 2, -1),
      c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24")
    ),
    hub_anchor_ids = c("h1", "h4"),
    coverage = list(
      bin_map = stats::setNames(c(1L, 2L, 3L, 3L), c("s21", "s22", "s23", "s24")),
      bins_used = 3L,
      bins_undercovered = 2L,
      source = "linking_global_score"
    )
  )
  strata_template <- stats::setNames(
    c(1L, 4L, 6L, 8L, 2L, 3L, 5L, 7L),
    c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24")
  )
  custom_strata <- function(scores, defaults) {
    ids <- as.character(names(scores))
    rank_index <- stats::setNames(as.integer(strata_template[ids]), ids)
    list(
      rank_index = rank_index,
      stratum_id = as.integer(rank_index[ids]),
      stratum_map = rank_index,
      top_band_ids = character()
    )
  }
  reserved_keys <- c(
    pairwiseLLM:::make_unordered_key("h1", "s23"),
    pairwiseLLM:::make_unordered_key("h2", "s21"),
    pairwiseLLM:::make_unordered_key("h4", "s24")
  )
  stage_order <- pairwiseLLM:::.adaptive_stage_order()
  reference_stage <- function(stage_name, seed_offset = 0L) {
    reference_phase_b_stage_candidates(
      state = state,
      stage_name = stage_name,
      fallback_name = "base",
      local_inputs = local_inputs,
      rank_index = strata_template,
      stratum_map = strata_template,
      spoke_id = 2L,
      C_max = 10000L,
      seed = 40L + as.integer(seed_offset),
      reserved_keys = reserved_keys
    )
  }

  actual_by_stage <- testthat::with_mocked_bindings(
    .adaptive_link_refit_local_inputs = function(state, controller, spoke_id, defaults = NULL, refit_id = NULL) {
      local_inputs
    },
    .adaptive_assign_strata = custom_strata,
    .adaptive_link_probe_reserved_keys = function(state, spoke_id, epoch_id = NULL) {
      reserved_keys
    },
    lapply(seq_along(stage_order), function(idx) {
      stage_name <- stage_order[[idx]]
      pairwiseLLM:::generate_stage_candidates_from_state(
        state = state,
        stage_name = stage_name,
        fallback_name = "base",
        C_max = 10000L,
        seed = 40L + idx,
        link_spoke_id = 2L
      )
    }),
    .package = "pairwiseLLM"
  )
  names(actual_by_stage) <- stage_order

  reference_by_stage <- lapply(seq_along(stage_order), function(idx) {
    reference_stage(stage_order[[idx]], seed_offset = idx)
  })
  names(reference_by_stage) <- stage_order

  for (stage_name in stage_order) {
    actual <- actual_by_stage[[stage_name]]
    reference <- reference_by_stage[[stage_name]]$candidates
    expect_true(nrow(reference) > 0L)
    expect_identical(attr(actual, "candidate_filter_counts"), attr(reference, "candidate_filter_counts"))
    expect_identical(actual, reference)
  }

  actual_pool <- testthat::with_mocked_bindings(
    .adaptive_link_refit_local_inputs = function(state, controller, spoke_id, defaults = NULL, refit_id = NULL) {
      local_inputs
    },
    .adaptive_assign_strata = custom_strata,
    .adaptive_link_probe_reserved_keys = function(state, spoke_id, epoch_id = NULL) {
      reserved_keys
    },
    pairwiseLLM:::.adaptive_link_candidate_pool(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      include_utility = FALSE,
      C_max = 10000L,
      seed = 60L
    ),
    .package = "pairwiseLLM"
  )
  reference_pool <- dplyr::bind_rows(lapply(seq_along(stage_order), function(idx) {
    cand <- reference_phase_b_stage_candidates(
      state = state,
      stage_name = stage_order[[idx]],
      fallback_name = "base",
      local_inputs = local_inputs,
      rank_index = strata_template,
      stratum_map = strata_template,
      spoke_id = 2L,
      C_max = 10000L,
      seed = 60L + idx,
      reserved_keys = reserved_keys
    )$candidates
    if (nrow(cand) < 1L) {
      return(NULL)
    }
    cand$link_stage <- stage_order[[idx]]
    cand
  }))

  expect_identical(actual_pool, reference_pool)
  actual_pool_keys <- vapply(seq_len(nrow(actual_pool)), function(idx) {
    pairwiseLLM:::make_unordered_key(actual_pool$i[[idx]], actual_pool$j[[idx]])
  }, character(1L))
  expect_false(any(actual_pool_keys %in% reserved_keys))
})

test_that("bounded Phase B direct cross-pair construction limits large stage domains", {
  hub_ids <- paste0("h", seq_len(60L))
  spoke_ids <- paste0("s", seq_len(60L))
  ids <- c(hub_ids, spoke_ids)
  rank_index <- stats::setNames(seq_along(ids), ids)
  stratum_map <- stats::setNames(rep(seq_len(12L), length.out = length(ids)), ids)
  reserved <- pairwiseLLM:::make_unordered_key(hub_ids[[1L]], spoke_ids[[1L]])

  bounded <- pairwiseLLM:::.adaptive_link_direct_cross_pairs_bounded(
    hub_item_ids = hub_ids,
    spoke_ids = spoke_ids,
    rank_index = rank_index,
    stratum_map = stratum_map,
    stage_name = "local_link",
    bounds = list(min = 0L, max = .Machine$integer.max),
    active_hub_ids = hub_ids,
    reserved_keys = reserved,
    C_max = 25L,
    seed = 99L
  )

  expect_true(isTRUE(bounded$bounded_used))
  expect_identical(nrow(bounded$candidates), 25L)
  expect_identical(bounded$n_after_route_filters, 3600L)
  expect_identical(bounded$n_after_active_domain, 3600L)
  expect_identical(bounded$total_legal, 3599L)
  expect_false(any(as.character(bounded$candidates$pair_key) %in% reserved))
})

test_that("concurrent selector uses the direct Phase B candidate domain for the active spoke", {
  withr::local_seed(171L)

  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24", "s31", "s32", "s33", "s34"),
    set_id = c(rep(1L, 4L), rep(2L, 4L), rep(3L, 4L)),
    global_item_id = paste0("g", seq_len(12L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 171L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE

  strata_template <- stats::setNames(
    c(1L, 4L, 6L, 8L, 2L, 3L, 5L, 7L, 2L, 5L, 7L, 9L),
    c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24", "s31", "s32", "s33", "s34")
  )
  custom_strata <- function(scores, defaults) {
    ids <- as.character(names(scores))
    rank_index <- stats::setNames(as.integer(strata_template[ids]), ids)
    list(
      rank_index = rank_index,
      stratum_id = as.integer(rank_index[ids]),
      stratum_map = rank_index,
      top_band_ids = character()
    )
  }
  local_inputs_by_spoke <- list(
    `2` = list(
      hub_ids = c("h1", "h2", "h3", "h4"),
      spoke_ids = c("s21", "s22", "s23", "s24"),
      active_items = list(active_hub = c("h1", "h2", "h3")),
      routing_scores = stats::setNames(
        c(9, 6, 4, 1, 8, 5, 2, -1),
        c("h1", "h2", "h3", "h4", "s21", "s22", "s23", "s24")
      ),
      hub_anchor_ids = c("h1"),
      coverage = list(
        bin_map = stats::setNames(c(1L, 2L, 3L, 3L), c("s21", "s22", "s23", "s24")),
        bins_used = 3L,
        bins_undercovered = integer(),
        source = "linking_global_score"
      )
    ),
    `3` = list(
      hub_ids = c("h1", "h2", "h3", "h4"),
      spoke_ids = c("s31", "s32", "s33", "s34"),
      active_items = list(active_hub = c("h2", "h4")),
      routing_scores = stats::setNames(
        c(9, 6, 4, 1, 7, 3, 0, -2),
        c("h1", "h2", "h3", "h4", "s31", "s32", "s33", "s34")
      ),
      hub_anchor_ids = c("h4"),
      coverage = list(
        bin_map = stats::setNames(c(1L, 2L, 3L, 3L), c("s31", "s32", "s33", "s34")),
        bins_used = 3L,
        bins_undercovered = integer(),
        source = "linking_global_score"
      )
    )
  )
  ref_stage_2 <- reference_phase_b_stage_candidates(
    state = state,
    stage_name = "mid_link",
    fallback_name = "base",
    local_inputs = local_inputs_by_spoke[["2"]],
    rank_index = strata_template,
    stratum_map = strata_template,
    spoke_id = 2L,
    C_max = 10000L,
    seed = 211L,
    reserved_keys = character()
  )$candidates
  ref_keys_2 <- vapply(seq_len(nrow(ref_stage_2)), function(idx) {
    pairwiseLLM:::make_unordered_key(ref_stage_2$i[[idx]], ref_stage_2$j[[idx]])
  }, character(1L))
  ref_stage_3 <- reference_phase_b_stage_candidates(
    state = state,
    stage_name = "mid_link",
    fallback_name = "base",
    local_inputs = local_inputs_by_spoke[["3"]],
    rank_index = strata_template,
    stratum_map = strata_template,
    spoke_id = 3L,
    C_max = 10000L,
    seed = 211L,
    reserved_keys = character()
  )$candidates
  ref_keys_3 <- vapply(seq_len(nrow(ref_stage_3)), function(idx) {
    pairwiseLLM:::make_unordered_key(ref_stage_3$i[[idx]], ref_stage_3$j[[idx]])
  }, character(1L))

  out <- testthat::with_mocked_bindings(
    .adaptive_link_ranked_spokes = function(state, controller, eligible_spoke_ids = NULL) c(2L, 3L),
    .adaptive_link_budget_map_for_refit = function(state, controller, eligible_spoke_ids = NULL) {
      list(
        `2` = list(B_spoke_refit_budget = 2L, B_spoke_refit_budget_source = "concurrent_allocator"),
        `3` = list(B_spoke_refit_budget = 1L, B_spoke_refit_budget_source = "concurrent_allocator")
      )
    },
    .adaptive_link_stage_progress = function(state, spoke_id, stage_quotas, stage_order, refit_id) {
      list(
        active_stage = "mid_link",
        backfill_active = FALSE,
        stage_quotas = as.list(stats::setNames(c(1L, 0L, 2L, 0L), pairwiseLLM:::.adaptive_stage_order())),
        stage_committed = as.list(stats::setNames(rep.int(0L, 4L), pairwiseLLM:::.adaptive_stage_order())),
        stage_realized = as.list(stats::setNames(rep.int(0L, 4L), pairwiseLLM:::.adaptive_stage_order())),
        budget_remaining_actual = 2L
      )
    },
    .adaptive_link_refit_local_inputs = function(state, controller, spoke_id, defaults = NULL, refit_id = NULL) {
      local_inputs_by_spoke[[as.character(spoke_id)]]
    },
    .adaptive_assign_strata = custom_strata,
    .adaptive_link_probe_reserved_keys = function(state, spoke_id, epoch_id = NULL) character(),
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      candidates <- tibble::as_tibble(candidates)
      candidates$link_d_opt_gain <- rev(seq_len(nrow(candidates)))
      candidates$link_p <- rep(0.6, nrow(candidates))
      candidates$link_u <- rep(0.24, nrow(candidates))
      candidates
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  selected_key <- pairwiseLLM:::make_unordered_key(
    as.character(state$item_ids[[out$i]]),
    as.character(state$item_ids[[out$j]])
  )
  selected_ids <- c(as.character(state$item_ids[[out$i]]), as.character(state$item_ids[[out$j]]))
  selected_sets <- as.integer(stats::setNames(state$items$set_id, state$items$item_id)[selected_ids])

  selected_spoke <- as.integer(out$link_spoke_id_selected)
  expected_keys <- list(`2` = ref_keys_2, `3` = ref_keys_3)[[as.character(selected_spoke)]]

  expect_true(selected_spoke %in% c(2L, 3L))
  expect_true(selected_key %in% expected_keys)
  expect_true(all(sort(unique(selected_sets)) == c(1L, selected_spoke)))
  expect_true(any(selected_ids %in% local_inputs_by_spoke[[as.character(selected_spoke)]]$active_items$active_hub))
})

test_that("linking predictive utility applies signed position bias by (A,B) orientation", {
  items <- tibble::tibble(
    item_id = c("h1", "s1"),
    set_id = c(1L, 2L),
    global_item_id = c("gh1", "gs1")
  )
  state <- adaptive_rank_start(
    items,
    seed = 17L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state <- mark_link_phase_b_ready(state)
  cand <- tibble::tibble(
    i = c("h1", "s1"),
    j = c("s1", "h1")
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_link_transform_state_for_spoke = function(controller, spoke_id) "shift_only",
    .adaptive_link_safe_theta_map = function(state, set_id, prefer_current = FALSE) {
      if (identical(as.integer(set_id), 1L)) {
        stats::setNames(0.4, "h1")
      } else {
        stats::setNames(-0.2, "s1")
      }
    },
    .adaptive_link_phase_b_startup_gap_for_spoke = function(state, spoke_id) FALSE,
    .adaptive_link_judge_params = function(state, controller, scope, allow_cold_start_fallback, expected_link_params) {
      list(beta = 0.3, epsilon = 0.1, scope = "link")
    },
    pairwiseLLM:::.adaptive_link_attach_predictive_utility(
      candidates = cand,
      state = state,
      controller = state$controller,
      spoke_id = 2L
    ),
    .package = "pairwiseLLM"
  )

  expect_equal(out$link_p[[1L]], out$link_p[[2L]], tolerance = 1e-12)
  expect_equal(out$link_u[[1L]], out$link_p[[1L]] * (1 - out$link_p[[1L]]), tolerance = 1e-12)
  expect_equal(out$link_u[[2L]], out$link_p[[2L]] * (1 - out$link_p[[2L]]), tolerance = 1e-12)
})

test_that("cross-set logged predictive probability uses final A/B orientation", {
  items <- tibble::tibble(
    item_id = c("h1", "s1"),
    set_id = c(1L, 2L),
    global_item_id = c("gh1", "gs1")
  )
  state <- adaptive_rank_start(
    items,
    seed = 121L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  state$round$stage_quotas <- as.list(stats::setNames(rep.int(2L, 4L), state$round$stage_order))
  state$round$stage_committed <- as.list(stats::setNames(rep.int(0L, 4L), state$round$stage_order))

  cand <- tibble::tibble(i = "h1", j = "s1", link_spoke_id = 2L)
  out <- testthat::with_mocked_bindings(
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      candidates$link_p <- 0.9
      candidates$link_u <- 0.09
      candidates$link_d_opt_gain <- 0.4
      candidates
    },
    .adaptive_assign_order = function(pair, posA, posB, pair_last_order, seed_base = 1L) {
      c(A_id = "s1", B_id = "h1")
    },
    .adaptive_link_predictive_prob_oriented = function(state, controller, spoke_id, A_id, B_id) {
      if (identical(A_id, "s1") && identical(B_id, "h1")) 0.2 else 0.8
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )

  if (!isTRUE(out$candidate_starved)) {
    expect_equal(out$A, 2L)
    expect_equal(out$B, 1L)
    expect_equal(out$p_ij, 0.2, tolerance = 1e-12)
    expect_equal(out$U0_ij, 0.16, tolerance = 1e-12)
  }
})

test_that("active linking hub domain excludes anchor-only hub items before any committed cross-set edge", {
  items <- tibble::tibble(
    item_id = c(
      "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8",
      "s1", "s2", "s3", "s4"
    ),
    set_id = c(rep(1L, 8L), rep(2L, 4L)),
    global_item_id = paste0("g", seq_len(12L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 13L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)
  defaults <- adaptive_defaults(length(state$item_ids))
  hub_ids <- as.character(state$items$item_id[state$items$set_id == 1L])
  routing_scores <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = controller,
    active_ids = c(hub_ids, as.character(state$items$item_id[state$items$set_id == 2L])),
    hub_id = 1L
  )
  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  got_anchor <- sort(intersect(active$active_hub, hub_ids))
  expect_identical(got_anchor, character())
})

test_that("held-out probes do not activate hub items in the active linking domain", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22"),
    set_id = c(1L, 1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 31L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 4L,
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = TRUE,
      is_holdout_probe_step = TRUE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  expect_identical(active$active_hub, character())
  expect_identical(sort(active$active_spoke), c("s21", "s22"))
})

test_that("non-anchor active-domain count guard aborts on impossible candidate counts", {
  expect_error(
    pairwiseLLM:::.adaptive_link_assert_active_domain_count(
      stage_name = "long_link",
      n_candidates_after_active_domain = 5L,
      active_hub_ids = c("h1", "h2"),
      spoke_ids = c("s21", "s22"),
      spoke_id = 2L
    ),
    "n_candidates_after_active_domain=5 exceeds the maximum possible active-domain cross-set pairs=4"
  )

  expect_invisible(
    pairwiseLLM:::.adaptive_link_assert_active_domain_count(
      stage_name = "anchor_link",
      n_candidates_after_active_domain = 99L,
      active_hub_ids = character(),
      spoke_ids = c("s21", "s22"),
      spoke_id = 2L
    )
  )
})

test_that("non-anchor routing guard rejects inactive hub endpoints and reserved probe pairs", {
  set_map <- c(h1 = 1L, h2 = 1L, s21 = 2L, s22 = 2L)

  expect_error(
    pairwiseLLM:::.adaptive_link_assert_non_anchor_candidate_domain(
      candidates = tibble::tibble(i = "h2", j = "s21"),
      stage_name = "mid_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      reserved_keys = character(),
      set_map = set_map
    ),
    "generated candidates fell outside active_link_items\\(s\\)"
  )

  expect_error(
    pairwiseLLM:::.adaptive_link_assert_non_anchor_candidate_domain(
      candidates = tibble::tibble(i = "h1", j = "s21"),
      stage_name = "local_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      reserved_keys = pairwiseLLM:::make_unordered_key("h1", "s21"),
      set_map = set_map
    ),
    "reserved held-out probe pairs entered linking-active candidates"
  )

  expect_invisible(
    pairwiseLLM:::.adaptive_link_assert_non_anchor_candidate_domain(
      candidates = tibble::tibble(i = "h1", j = "s21"),
      stage_name = "long_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      reserved_keys = character(),
      set_map = set_map
    )
  )
})

test_that("phase-B routing helpers enforce finite inputs and anchor fallback rules", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 901L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state <- mark_link_phase_b_ready(state)
  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)

  empty_scores <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = controller,
    active_ids = "missing_item_id",
    hub_id = 1L
  )
  expect_identical(empty_scores, stats::setNames(numeric(), character()))

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        if (as.integer(set_id) == 1L) {
          c(h1 = NA_real_, h2 = 0.2)
        } else {
          c(s1 = -0.5, s2 = -0.7)
        }
      },
      pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
        state = state,
        controller = controller,
        active_ids = c("h1", "s1"),
        hub_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "set_id=1"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) c(h1 = NA_real_),
      pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
        state = state,
        controller = controller,
        active_ids = "s1",
        hub_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "Phase A theta_raw_mean missing/non-finite"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        rlang::abort("broken artifact")
      },
      pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
        state = state,
        controller = controller,
        active_ids = "s1",
        hub_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "Phase A theta_raw_mean unavailable"
  )

  controller_scale <- utils::modifyList(
    controller,
    list(
      link_transform_state_by_spoke = list(`2` = "shift_scale"),
      link_refit_stats_by_spoke = list(`2` = list(delta_spoke_mean = NA_real_, log_alpha_spoke_mean = NA_real_))
    )
  )
  scale_scores <- testthat::with_mocked_bindings(
    .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
      if (set_id == 1L) c(h1 = 0.1, h2 = 0.2) else c(s1 = 1.5, s2 = -1.5)
    },
    pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
      state = state,
      controller = controller_scale,
      active_ids = c("h1", "s1"),
      hub_id = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_equal(scale_scores[["s1"]], 1.5, tolerance = 1e-12)

  defaults <- adaptive_defaults(length(state$item_ids))
  state$round$per_round_item_uses <- c(h1 = 0L, h2 = 1L, h3 = 1L)
  anchor_fill <- testthat::with_mocked_bindings(
    .adaptive_select_rolling_anchors = function(scores, defaults) c("h1", "h2"),
    .adaptive_rank_index_from_scores = function(scores) c(h1 = 1L, h2 = 2L, h3 = 3L),
    pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
      state = state,
      hub_ids = c("h1", "h2", "h3"),
      hub_scores = c(h1 = 3, h2 = 2, h3 = 1),
      defaults = defaults
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(anchor_fill, c("h1", "h2"))

  anchor_rank_fallback <- testthat::with_mocked_bindings(
    .adaptive_select_rolling_anchors = function(scores, defaults) c("h1"),
    .adaptive_rank_index_from_scores = function(scores) integer(),
    pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
      state = state,
      hub_ids = c("h1", "h2"),
      hub_scores = c(h1 = 2, h2 = 1),
      defaults = defaults
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(anchor_rank_fallback, "h1")
})

test_that("probe panel construction hard-gates missing Phase A theta surfaces", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 903L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        if (as.integer(set_id) == 1L) {
          c(h1 = 0.1, h2 = 0.2)
        } else {
          c(s1 = NA_real_, s2 = -0.4)
        }
      },
      pairwiseLLM:::.adaptive_link_probe_construct_panel(
        state = state,
        controller = controller,
        spoke_id = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "Probe panel construction invariant failed: Phase A theta_raw_mean missing/non-finite for set_id=2"
  )
})

test_that("phase-B routing score source switches between Phase A and current theta by refit mode", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 902L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state <- mark_link_phase_b_ready(state)
  active_ids <- c("h1", "h2", "s1", "s2")
  controller_shift <- utils::modifyList(
    pairwiseLLM:::.adaptive_controller_resolve(state),
    list(
      link_refit_stats_by_spoke = list(`2` = list(delta_spoke_mean = 0, log_alpha_spoke_mean = 0))
    )
  )
  controller_joint <- utils::modifyList(
    controller_shift,
    list(link_refit_mode = "joint_refit")
  )

  out_shift <- testthat::with_mocked_bindings(
    .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
      if (as.integer(set_id) == 1L) c(h1 = 10, h2 = 9) else c(s1 = -2, s2 = -3)
    },
    .adaptive_link_theta_mean_map = function(state, set_id) {
      if (as.integer(set_id) == 1L) c(h1 = 1, h2 = 0.5) else c(s1 = 3, s2 = 2.5)
    },
    pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
      state = state,
      controller = controller_shift,
      active_ids = active_ids,
      hub_id = 1L
    ),
    .package = "pairwiseLLM"
  )
  out_joint <- testthat::with_mocked_bindings(
    .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
      if (as.integer(set_id) == 1L) c(h1 = 10, h2 = 9) else c(s1 = -2, s2 = -3)
    },
    .adaptive_link_theta_mean_map = function(state, set_id) {
      if (as.integer(set_id) == 1L) c(h1 = 1, h2 = 0.5) else c(s1 = 3, s2 = 2.5)
    },
    pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
      state = state,
      controller = controller_joint,
      active_ids = active_ids,
      hub_id = 1L
    ),
    .package = "pairwiseLLM"
  )

  expect_equal(out_shift[["s1"]], -2, tolerance = 1e-12)
  expect_equal(out_joint[["s1"]], -2, tolerance = 1e-12)
  expect_true(isTRUE(all.equal(out_shift[["h1"]], out_joint[["h1"]], tolerance = 1e-12)))
})

test_that("linking candidates and step log carry global distance strata", {
  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    set_id = c(rep(1L, 10L), rep(2L, 6L)),
    global_item_id = c(paste0("gh", seq_len(10L)), paste0("gs2", seq_len(6L)))
  )
  trueskill_state <- make_test_trueskill_state(items, mu = seq(nrow(items), 1))
  state <- make_test_state(items, trueskill_state)
  state <- pairwiseLLM:::.adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state <- mark_link_phase_b_ready(state)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 11L,
      A = 1L,
      B = 11L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE,
      is_drift_probe_step = FALSE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      link_stage = "anchor_link",
      round_stage = "anchor_link"
    )
  )

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "long_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 5L
  )
  expect_true(nrow(cand) > 0L)
  expect_true("dist_stratum_global" %in% names(cand))
  expect_true(all(!is.na(cand$dist_stratum_global)))

  judge <- make_deterministic_judge("i_wins")
  out <- pairwiseLLM:::run_one_step(state, judge)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_false(is.na(row$dist_stratum_global[[1L]]))
})

test_that("link stage log is appended per refit and spoke in linking mode", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 2L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state <- mark_link_phase_b_ready(state)
  judge <- make_deterministic_judge("i_wins")
  state <- pairwiseLLM:::run_one_step(state, judge)
  refit_context <- list(last_refit_step = 0L)
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = refit_context
  )

  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(state$link_stage_log, rows)
  expect_true(nrow(state$link_stage_log) >= 1L)
  expect_true(all(c("refit_id", "spoke_id", "coverage_bins_used") %in% names(state$link_stage_log)))
})

test_that("link stage log uses NA hub_lock_kappa when lock mode is not soft_lock", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 4L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state <- mark_link_phase_b_ready(state)
  judge <- make_deterministic_judge("i_wins")
  state <- pairwiseLLM:::run_one_step(state, judge)
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )

  expect_true(nrow(rows) >= 1L)
  expect_equal(rows$hub_lock_mode[[1L]], "hard_lock")
  expect_true(is.na(rows$hub_lock_kappa[[1L]]))
})

test_that("per-spoke link stage rows do not inherit global identified fallback", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 25L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log, list(refit_id = 1L, diagnostics_pass = TRUE))
  state$controller$linking_identified <- TRUE
  state$controller$linking_identified_by_spoke <- list()
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(), `3` = list())

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )

  expect_true(nrow(rows) == 2L)
  expect_true(all(rows$linking_identified %in% FALSE))
})

test_that("round candidate helper branches are exercised for anchor/phase-a paths", {
  scores <- stats::setNames(c(5, 4, 3, 2, 1), as.character(1:5))
  defaults <- adaptive_defaults(5)
  anchors <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores, defaults)
  expect_true(length(anchors) >= 1L)

  state <- adaptive_rank_start(
    tibble::tibble(
      item_id = as.character(1:6),
      set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
      global_item_id = paste0("g", 1:6)
    ),
    seed = 4L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  state$round$anchor_ids <- as.character(state$item_ids[1:2])
  state$round$anchor_round_id <- 1L
  state$round$round_id <- 2L
  expect_true(pairwiseLLM:::.adaptive_round_anchor_needs_refresh(
    state,
    utils::modifyList(adaptive_defaults(6), list(anchor_refresh_on_round = TRUE))
  ))

  # In Phase A (pending run sets), generation falls back to within-set candidates.
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("pending", "pending"),
      validation_message = c("x", "y"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    ready_spokes = integer(),
    active_phase_a_set = 1L
  )
  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "local_link",
    fallback_name = "expand_locality",
    C_max = 200L,
    seed = 11L
  )
  expect_true(nrow(cand) > 0L)
  set_map <- stats::setNames(state$items$set_id, state$items$item_id)
  expect_true(all(set_map[cand$i] == 1L & set_map[cand$j] == 1L))
})

test_that("cross-set candidate generation aborts when requested spoke is not phase-b eligible", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 66L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  # Only spoke 2 is eligible; spoke 3 must fail loudly.
  status <- state$linking$phase_a$set_status
  status$status[status$set_id == 3L] <- "pending_finalization"
  state$linking$phase_a$set_status <- status

  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = state,
      stage_name = "anchor_link",
      fallback_name = "base",
      C_max = 1000L,
      seed = 1L,
      link_spoke_id = 3L
    ),
    "requested spoke_id=3 is not eligible"
  )
})

test_that("phase B starved selection preserves the attempted spoke id", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 77L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 3L
  state <- mark_link_phase_b_ready(state)

  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(...) tibble::tibble(),
    .adaptive_link_candidate_pool = function(...) tibble::tibble(),
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::select_next_pair(state, step_id = 1L)
    }
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(as.integer(out$link_spoke_id_selected), 3L)
  expect_identical(as.character(out$round_stage), "local_link")
})

test_that("phase B starvation marks the attempted spoke exhausted and advances stage", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 78L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 10L
  state$controller$current_link_spoke_id <- 3L
  state <- mark_link_phase_b_ready(state)
  state$refit_meta$last_refit_step <- 0L
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::2` = list(long_link = TRUE)
  )
  state$refit_meta$link_stage_shortfalls_by_refit_spoke <- list(
    `1::2` = list(long_link = 1L)
  )

  step_row <- tibble::tibble(
    round_stage = "long_link",
    link_spoke_id = 3L,
    starvation_reason = "few_candidates_generated"
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(0L, 1L, 1L, 0L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_round_starvation(state, step_row)$state
    }
  )

  exhausted_map <- out$refit_meta$link_stage_exhausted_by_refit_spoke
  expect_true(isTRUE(exhausted_map[["1::3"]]$long_link))
  expect_identical(out$controller$current_link_spoke_id, 3L)
  next_stage <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(0L, 1L, 1L, 0L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_round_active_stage(out)
    }
  )
  expect_identical(next_stage, "anchor_link")
})

test_that("phase B global-safe starvation exhausts only the attempted stage for the last active spoke", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 780L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 10L
  state$controller$current_link_spoke_id <- 3L
  state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
  state <- mark_link_phase_b_ready(state)
  state$refit_meta$last_refit_step <- 0L

  step_row <- tibble::tibble(
    round_stage = "local_link",
    link_spoke_id = 3L,
    fallback_used = "global_safe",
    starvation_reason = "few_candidates_generated"
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(1L, 1L, 1L, 1L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_round_starvation(state, step_row)$state
    }
  )

  exhausted_map <- out$refit_meta$link_stage_exhausted_by_refit_spoke
  expect_identical(exhausted_map[["1::3"]]$local_link, TRUE)
  expect_false(isTRUE(exhausted_map[["1::3"]]$anchor_link))
  expect_false(isTRUE(exhausted_map[["1::3"]]$long_link))
  expect_false(isTRUE(exhausted_map[["1::3"]]$mid_link))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_all_spokes_exhausted(out, refit_id = 1L)))
})

test_that("phase B selector short-circuits when no eligible spoke budget remains", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 781L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$controller$current_link_spoke_id <- 2L

  out <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(...) {
      list(`2` = list(
        B_spoke_refit_budget = 1L,
        B_spoke_refit_budget_source = "single_spoke_controller"
      ))
    },
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(1L, 0L, 0L, 0L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .adaptive_link_stage_progress = function(...) {
      list(
        active_stage = "anchor_link",
        backfill_active = FALSE,
        stage_realized = stats::setNames(c(1L, 0L, 0L, 0L), pairwiseLLM:::.adaptive_stage_order()),
        stage_committed = stats::setNames(c(1L, 0L, 0L, 0L), pairwiseLLM:::.adaptive_stage_order()),
        stage_quotas = stats::setNames(c(1L, 0L, 0L, 0L), pairwiseLLM:::.adaptive_stage_order()),
        budget_remaining_actual = 0L
      )
    },
    generate_stage_candidates_from_state = function(...) {
      rlang::abort("candidate generation should be skipped when budget is depleted")
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$candidate_starved))
  expect_identical(as.character(out$starvation_reason), "all_eligible_spokes_infeasible")
  expect_identical(as.character(out$fallback_used), "global_safe")
})

test_that("phase B pooled backfill starvation exhausts only the attempted spoke", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 79L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 10L
  state$controller$current_link_spoke_id <- 3L
  state <- mark_link_phase_b_ready(state)
  state$refit_meta$last_refit_step <- 0L
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::2` = list(anchor_link = TRUE)
  )

  step_row <- tibble::tibble(
    round_stage = "pooled_backfill",
    link_spoke_id = 3L,
    starvation_reason = "few_candidates_generated"
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(1L, 1L, 1L, 1L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_round_starvation(state, step_row)
    }
  )

  expect_false(isTRUE(out$exhausted))
  exhausted_map <- out$state$refit_meta$link_stage_exhausted_by_refit_spoke
  expect_true(all(vapply(
    pairwiseLLM:::.adaptive_stage_order(),
    function(stage_name) isTRUE(exhausted_map[["1::3"]][[stage_name]]),
    logical(1L)
  )))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_all_spokes_exhausted(out$state, refit_id = 1L)))
})

test_that("ranked spokes exclude fully exhausted spokes in the current refit", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", 1:6)
  )
  state <- adaptive_rank_start(
    items,
    seed = 82L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$controller$current_link_spoke_id <- 3L
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::3` = list(anchor_link = TRUE, long_link = TRUE, mid_link = TRUE, local_link = TRUE)
  )

  ranked <- pairwiseLLM:::.adaptive_link_ranked_spokes(
    state = state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )

  expect_identical(as.integer(ranked), 2L)
})

test_that("pooled backfill enforces duplicate caps and preserves candidate counts", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 81L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$round_id <- 1L
  state <- mark_link_phase_b_ready(state)
  state$controller$current_link_spoke_id <- 2L
  state$history_pairs <- tibble::tibble(
    A_id = rep("h1", 10L),
    B_id = rep("s21", 10L)
  )
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::2` = list(anchor_link = TRUE, long_link = TRUE, mid_link = TRUE, local_link = TRUE)
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(1L, 1L, 1L, 1L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .adaptive_link_candidate_pool = function(
      state, controller, spoke_id, include_utility = TRUE, C_max = NULL, seed = 1L
    ) {
      tibble::tibble(
        i = c("h1", "h2"),
        j = c("s21", "s22"),
        p = c(0.5, 0.5),
        u0 = c(0.25, 0.25),
        link_d_opt_gain = c(10, 5),
        link_u = c(10, 5),
        link_stage = c("long_link", "long_link"),
        link_spoke_id = c(2L, 2L),
        coverage_bins_used = c(3L, 3L),
        coverage_source = c("linking_global_score", "linking_global_score")
      )
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::select_next_pair(state, step_id = 1L)
    }
  )

  expect_false(isTRUE(out$candidate_starved))
  expect_false(identical(c(as.integer(out$i), as.integer(out$j)), c(1L, 3L)))
  expect_gt(as.integer(out$n_candidates_generated), 0L)
  expect_gt(as.integer(out$n_candidates_after_duplicates), 0L)
  expect_gt(as.integer(out$n_candidates_scored), 0L)
})
