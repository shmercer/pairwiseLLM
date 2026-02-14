mark_link_phase_b_ready <- function(state, source = "import") {
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  if (is.null(state$linking$phase_a)) {
    state$linking$phase_a <- list()
  }
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = as.integer(set_ids),
    source = rep(as.character(source), length(set_ids)),
    status = rep("ready", length(set_ids)),
    validation_message = rep("ready", length(set_ids)),
    artifact_path = rep(NA_character_, length(set_ids))
  )
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
    proxy_scores = proxy$scores
  )
  expect_identical(cov$bins_used, 2L)
})

test_that("early linking sparsity falls back to Phase A rank summaries for bins", {
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
  state$linking$phase_a$artifacts <- list(
    `2` = list(
      set_id = 2L,
      items = tibble::tibble(
        global_item_id = paste0("g", 5:8),
        rank_mu_raw = c(4, 3, 2, 1)
      )
    )
  )

  cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )
  expect_true(nrow(cand) > 0L)
  expect_true(all(cand$coverage_source == "phase_a_rank_mu_raw"))
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

  expect_equal(row$utility_mode[[1L]], "p_times_1_minus_p")
  expect_equal(row$cross_set_utility_pre[[1L]], expected, tolerance = 1e-12)
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
