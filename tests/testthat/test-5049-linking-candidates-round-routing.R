mark_link_phase_b_ready <- function(state, source = "import") {
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  if (is.null(state$linking$phase_a)) {
    state$linking$phase_a <- list()
  }
  artifacts <- lapply(set_ids, function(set_id) {
    rows <- state$items[state$items$set_id == as.integer(set_id), , drop = FALSE]
    tib <- tibble::as_tibble(rows)
    tib$theta_raw_mean <- seq(from = nrow(tib), to = 1, by = -1)
    tib$theta_raw_sd <- rep(0.5, nrow(tib))
    list(items = tib)
  })
  names(artifacts) <- as.character(set_ids)
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
  state
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
    stage_name = "long_link",
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

test_that("linking candidates allow selected-spoke to other-spoke edges when enabled", {
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
      hub_id = 1L,
      allow_spoke_spoke_cross_set = TRUE
    )
  )
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state <- mark_link_phase_b_ready(state)

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "long_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 2L
  )
  set_map <- stats::setNames(items$set_id, items$item_id)
  set_i <- as.integer(set_map[cand$i])
  set_j <- as.integer(set_map[cand$j])
  spoke_spoke <- (set_i == 2L & set_j == 3L) | (set_i == 3L & set_j == 2L)

  expect_true(nrow(cand) > 0L)
  expect_true(all(set_i != set_j))
  expect_true(all(set_i == 2L | set_j == 2L))
  expect_true(any(spoke_spoke))
})

test_that("phase B routing invariants hold across anchor/long/mid/local stages", {
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
  stages <- c("anchor_link", "long_link", "mid_link", "local_link")
  for (stage in stages) {
    cand <- pairwiseLLM:::generate_stage_candidates_from_state(
      state,
      stage_name = stage,
      fallback_name = "base",
      C_max = 10000L,
      seed = 4L
    )
    expect_true(nrow(cand) > 0L)
    set_i <- as.integer(set_map[cand$i])
    set_j <- as.integer(set_map[cand$j])
    expect_true(all((set_i == 1L & set_j == 2L) | (set_i == 2L & set_j == 1L)))
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

  expect_identical(unname(q_base[c("anchor_link", "long_link", "mid_link", "local_link")]), c(6L, 8L, 6L, 6L))
  expect_identical(unname(q_taper[c("anchor_link", "mid_link", "local_link")]), c(6L, 6L, 6L))
  expect_true(q_taper[["long_link"]] == 4L)
  expect_true(q_taper[["long_link"]] >= 2L)
  expect_identical(q_other_spoke[["long_link"]], 8L)
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

  expect_identical(q_spoke_2[["long_link"]], 4L)
  expect_identical(q_spoke_3[["long_link"]], 8L)
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

test_that("link stage rows carry per-spoke per-refit quota totals and committed counts", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32", "gs33")
  )
  state <- adaptive_rank_start(
    items,
    seed = 19L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L, multi_spoke_mode = "independent")
  )
  state$warm_start_done <- TRUE
  state <- mark_link_phase_b_ready(state)
  judge <- make_deterministic_judge("i_wins")
  state <- pairwiseLLM:::run_one_step(state, judge)
  state <- pairwiseLLM:::.adaptive_round_commit(state, state$step_log[nrow(state$step_log), , drop = FALSE])
  state <- pairwiseLLM:::run_one_step(state, judge)
  state <- pairwiseLLM:::.adaptive_round_commit(state, state$step_log[nrow(state$step_log), , drop = FALSE])
  state <- pairwiseLLM:::run_one_step(state, judge)
  state <- pairwiseLLM:::.adaptive_round_commit(state, state$step_log[nrow(state$step_log), , drop = FALSE])
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log, list(refit_id = 1L, diagnostics_pass = TRUE))
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(), `3` = list())
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row2 <- rows[rows$spoke_id == 2L, , drop = FALSE]
  row3 <- rows[rows$spoke_id == 3L, , drop = FALSE]
  expect_true(nrow(row2) == 1L)
  expect_true(nrow(row3) == 1L)
  expect_identical(row2$quota_anchor_link[[1L]], 6L)
  expect_identical(row2$quota_long_link[[1L]], 8L)
  expect_true(row2$committed_anchor_link[[1L]] + row2$committed_long_link[[1L]] +
    row2$committed_mid_link[[1L]] + row2$committed_local_link[[1L]] >= 1L)
  expect_true(row3$committed_anchor_link[[1L]] + row3$committed_long_link[[1L]] +
    row3$committed_mid_link[[1L]] + row3$committed_local_link[[1L]] >= 1L)
  expect_identical(row2$quota_long_link_raw[[1L]], 8L)
  expect_identical(row2$quota_long_link_effective[[1L]], 8L)
  expect_identical(row2$quota_long_link_removed[[1L]], 0L)
  expect_false(isTRUE(row2$quota_taper_applied[[1L]]))
  expect_identical(row2$quota_taper_spoke_id[[1L]], 2L)
  expect_identical(row3$quota_long_link_raw[[1L]], 8L)
  expect_identical(row3$quota_long_link_effective[[1L]], 8L)
  expect_identical(row3$quota_long_link_removed[[1L]], 0L)
  expect_false(isTRUE(row3$quota_taper_applied[[1L]]))
  expect_identical(row3$quota_taper_spoke_id[[1L]], 3L)
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

test_that("linking deterministic ordering prioritizes coverage before utility", {
  cand <- tibble::tibble(
    i = c("a", "b"),
    j = c("c", "d"),
    u0 = c(0.24, 0.25),
    coverage_priority = c(1L, 0L)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord[[1L]], 1L)
})

test_that("linking deterministic ordering ranks by linking utility with stable ties", {
  cand <- tibble::tibble(
    i = c("a", "b", "c"),
    j = c("d", "e", "f"),
    u0 = c(0.49, 0.48, 0.47),
    link_u = c(0.10, 0.30, 0.30)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord, c(2L, 3L, 1L))
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
      hub_id = 1L,
      judge_param_mode = "phase_specific"
    )
  )
  state <- mark_link_phase_b_ready(state)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L

  seen_judge_mode <- NA_character_
  cand <- tibble::tibble(i = c("h1", "h2"), j = c("s1", "s2"), link_spoke_id = c(2L, 2L))
  testthat::with_mocked_bindings(
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      seen_judge_mode <<- as.character(controller$judge_param_mode %||% NA_character_)
      candidates$link_p <- as.double(candidates$p %||% rep(0.5, nrow(candidates)))
      candidates$link_u <- as.double(candidates$link_p * (1 - candidates$link_p))
      candidates
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )
  expect_identical(seen_judge_mode, "phase_specific")
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
      multi_spoke_mode = "concurrent",
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
  expect_true(pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller) %in% c(2L, 3L))
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
      multi_spoke_mode = "concurrent",
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

test_that("concurrent fallback recomputes stage context per spoke attempt", {
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
      multi_spoke_mode = "concurrent",
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

  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                    link_spoke_id = NA_integer_) {
      if (as.integer(link_spoke_id) == 3L && identical(stage_name, "anchor_link")) {
        return(tibble::tibble(i = "h1", j = "s31"))
      }
      tibble::tibble(i = character(), j = character())
    },
    pairwiseLLM:::select_next_pair(state, step_id = 2L),
    .package = "pairwiseLLM"
  )

  expect_false(isTRUE(out$candidate_starved))
  expect_identical(out$link_spoke_id_selected, 3L)
  expect_identical(out$round_stage, "anchor_link")
  quota_controller <- state$controller
  quota_controller$current_link_spoke_id <- 3L
  stage_quotas <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = as.integer(state$round$round_id),
    n_items = as.integer(state$n_items),
    controller = quota_controller
  )
  expect_identical(out$stage_quota, as.integer(stage_quotas[["anchor_link"]]))
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
      multi_spoke_mode = "concurrent",
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
  expect_true(is.na(out$link_spoke_id_selected))
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
      multi_spoke_mode = "concurrent",
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

test_that("cross_set_utility_pre logs p*(1-p) before commit in linking mode", {
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
  judge <- make_deterministic_judge("i_wins")
  out <- pairwiseLLM:::run_one_step(state, judge)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expected <- row$posterior_win_prob_pre[[1L]] * (1 - row$posterior_win_prob_pre[[1L]])

  expect_equal(row$utility_mode[[1L]], "linking_cross_set_p_times_1_minus_p")
  expect_equal(row$cross_set_utility_pre[[1L]], expected, tolerance = 1e-12)
})

test_that("cross-set ordering uses linking utility when predictive utility differs", {
  cand <- tibble::tibble(
    i = c("h1", "h2"),
    j = c("s1", "s2"),
    u0 = c(0.26, 0.24),
    link_u = c(0.20, 0.28)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(cand)
  expect_identical(ord[[1L]], 2L)
})

test_that("pairing ordering ignores linking utility fields", {
  cand <- tibble::tibble(
    i = c("a", "b"),
    j = c("c", "d"),
    u0 = c(0.24, 0.26),
    u = c(0.24, 0.26),
    link_u = c(0.90, 0.10)
  )
  utility_mode <- pairwiseLLM:::.adaptive_selection_utility_mode(
    run_mode = "within_set",
    has_regularization = FALSE,
    is_cross_set = FALSE
  )
  utility_col <- pairwiseLLM:::.adaptive_resolve_selection_column(utility_mode)
  ord <- order(-as.double(cand[[utility_col]]), cand$i, cand$j)
  expect_identical(ord[[1L]], 2L)
})

test_that("linking deterministic ordering falls back when linking utility is fully non-finite", {
  cand <- tibble::tibble(
    i = c("a", "b", "c"),
    j = c("d", "e", "f"),
    u0 = c(0.20, 0.30, 0.30),
    link_u = c(NA_real_, NaN, Inf)
  )
  ord <- pairwiseLLM:::.adaptive_linking_selection_order(
    cand,
    utility_mode = "linking_cross_set_p_times_1_minus_p"
  )
  expect_identical(ord, c(2L, 3L, 1L))
})

test_that("stopped spokes are excluded from phase B routing/candidate generation", {
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
  state$controller$link_stopped_by_spoke <- list(`2` = TRUE)

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "long_link",
    fallback_name = "base",
    C_max = 10000L,
    seed = 1L
  )
  set_map <- stats::setNames(items$set_id, items$item_id)
  set_i <- as.integer(set_map[cand$i])
  set_j <- as.integer(set_map[cand$j])
  spoke_set <- ifelse(set_i == 1L, set_j, set_i)

  expect_true(nrow(cand) > 0L)
  expect_true(all(spoke_set == 3L))
})

test_that("independent spoke stage progress is computed per spoke without shared coupling", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = paste0("g", seq_len(9L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 303L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L, multi_spoke_mode = "independent")
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
      is_cross_set = TRUE,
      link_spoke_id = 2L,
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
  expect_true(all(sort(phase_ctx$active_spokes) == 3L))
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
  cand <- tibble::tibble(
    i = c("h1", "s1"),
    j = c("s1", "h1")
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_link_transform_mode_for_spoke = function(controller, spoke_id) "shift_only",
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

  p_hs <- (1 - 0.1) * stats::plogis(0.4 - (-0.2) + 0.3) + 0.1 * 0.5
  p_sh <- (1 - 0.1) * stats::plogis(-0.2 - 0.4 + 0.3) + 0.1 * 0.5
  expect_equal(out$link_p[[1L]], p_hs, tolerance = 1e-12)
  expect_equal(out$link_p[[2L]], p_sh, tolerance = 1e-12)
  expect_equal(out$link_u[[1L]], p_hs * (1 - p_hs), tolerance = 1e-12)
  expect_equal(out$link_u[[2L]], p_sh * (1 - p_sh), tolerance = 1e-12)
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
      candidates
    },
    .adaptive_assign_order = function(pair, posA, posB, pair_last_order) {
      c(A_id = "s1", B_id = "h1")
    },
    .adaptive_link_predictive_prob_oriented = function(state, controller, spoke_id, A_id, B_id) {
      if (identical(A_id, "s1") && identical(B_id, "h1")) 0.2 else 0.8
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )

  expect_equal(out$A, 2L)
  expect_equal(out$B, 1L)
  expect_equal(out$p_ij, 0.2, tolerance = 1e-12)
  expect_equal(out$U0_ij, 0.16, tolerance = 1e-12)
})

test_that("active linking hub domain uses the same hub-only anchors as phase-B routing", {
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
  expected_anchor <- sort(pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
    state = state,
    hub_ids = hub_ids,
    hub_scores = routing_scores,
    defaults = defaults
  ))

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  got_anchor <- sort(intersect(active$active_hub, hub_ids))
  expect_identical(got_anchor, expected_anchor)
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
      link_transform_mode_by_spoke = list(`2` = "shift_scale"),
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

test_that("linking candidates and step log carry global distance strata", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  trueskill_state <- make_test_trueskill_state(items, mu = seq(8, 1))
  state <- make_test_state(items, trueskill_state)
  state <- pairwiseLLM:::.adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state <- mark_link_phase_b_ready(state)

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
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
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
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, hub_lock_mode = "free")
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
  expect_equal(rows$hub_lock_mode[[1L]], "free")
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
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
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
