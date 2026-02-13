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

test_that("linking round quotas use fixed defaults and long taper floor rule", {
  q_base <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(run_mode = "link_one_spoke", linking_identified = FALSE)
  )
  q_taper <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 100L,
    controller = list(run_mode = "link_one_spoke", linking_identified = TRUE)
  )

  expect_identical(unname(q_base[c("anchor_link", "long_link", "mid_link", "local_link")]), c(6L, 8L, 6L, 6L))
  expect_identical(unname(q_taper[c("anchor_link", "mid_link", "local_link")]), c(6L, 6L, 6L))
  expect_true(q_taper[["long_link"]] == 4L)
  expect_true(q_taper[["long_link"]] >= 2L)
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
