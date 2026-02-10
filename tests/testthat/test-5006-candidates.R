test_that("generate_candidate_pairs uses rank-local windowed generation", {
  items <- make_test_items(4)
  rank_mu <- sort(items$item_id)

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 3,
    C_max = 20000L,
    seed = 1L
  )

  candidates <- dplyr::arrange(candidates, .data$i, .data$j)
  expected <- tibble::tibble(i = c(1, 2, 3), j = c(2, 3, 4))

  expect_equal(nrow(candidates), 3L)
  expect_equal(candidates, expected)
  expect_true(all(candidates$i < candidates$j))
  expect_equal(anyDuplicated(pairwiseLLM:::make_unordered_key(candidates$i, candidates$j)), 0L)
})

test_that("generate_candidate_pairs caps and is deterministic by seed", {
  items <- make_test_items(60)
  rank_mu <- items$item_id

  cand1 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 123L
  )
  cand2 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 123L
  )
  cand3 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 124L
  )

  expect_equal(nrow(cand1), 100L)
  expect_equal(cand1, cand2)
  expect_false(identical(cand1, cand3))

  expect_true(all(cand1$i < cand1$j))
  expect_equal(anyDuplicated(pairwiseLLM:::make_unordered_key(cand1$i, cand1$j)), 0L)
})

test_that("generate_candidate_pairs accepts even W_used values", {
  items <- make_test_items(5)
  rank_mu <- items$item_id

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 4L,
    C_max = 20000L,
    seed = 1L
  )

  expect_true(nrow(candidates) > 0L)
})

test_that("generate_candidate_pairs allows N=2 with W_used=1", {
  items <- make_test_items(2)
  rank_mu <- items$item_id

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 1L,
    C_max = 20000L,
    seed = 1L
  )

  expect_equal(nrow(candidates), 1L)
  expect_equal(candidates$i, 1L)
  expect_equal(candidates$j, 2L)
})

test_that("generate_candidate_pairs validates inputs", {
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = 1L,
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "at least two"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c(1L, 2L, 3L),
      W_used = 1L,
      C_max = 20000L,
      seed = 1L
    ),
    ">= 2"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c(1L, 1L, 2L),
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "unique"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = integer(),
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "non-empty"
  )
})

test_that("rolling anchors refresh from trueskill and refit sources deterministically", {
  items <- make_test_items(10)
  trueskill_state <- make_test_trueskill_state(
    items,
    mu = seq(10, 1)
  )
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE

  state_1 <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)
  anchors_1 <- state_1$round$anchor_ids
  expect_true(length(anchors_1) >= 1L)
  expect_equal(state_1$round$anchor_refresh_source, "trueskill_mu")

  state_1$btl_fit <- list(
    theta_mean = stats::setNames(seq(1, 10), as.character(items$item_id))
  )
  state_1$refit_meta$last_refit_round_id <- 1L
  state_2 <- pairwiseLLM:::.adaptive_refresh_round_anchors(state_1)

  expect_equal(state_2$round$anchor_refresh_source, "refit_theta_mean")
  expect_false(identical(state_2$round$anchor_ids, anchors_1))
  expect_equal(state_2$round$anchor_refit_round_id, 1L)
})

test_that("rolling anchor count follows clamped default", {
  scores_small <- stats::setNames(seq(9, 1), as.character(1:9))
  scores_mid <- stats::setNames(seq(30, 1), as.character(1:30))
  scores_large <- stats::setNames(seq(1000, 1), as.character(1:1000))
  defaults <- pairwiseLLM:::adaptive_defaults(1000L)

  anchors_small <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores_small, defaults)
  anchors_mid <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores_mid, defaults)
  anchors_large <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores_large, defaults)

  expect_equal(length(anchors_small), 1L)
  expect_equal(length(anchors_mid), 10L)
  expect_equal(length(anchors_large), 40L)
})

test_that("rolling anchor composition follows 30/40/30 buckets", {
  scores <- stats::setNames(seq(200, 1), as.character(1:200))
  defaults <- pairwiseLLM:::adaptive_defaults(200L)
  anchors <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores, defaults)

  rank_index <- pairwiseLLM:::.adaptive_rank_index_from_scores(scores)
  ids_sorted <- names(sort(rank_index))
  n <- length(ids_sorted)
  n_anchor <- 20L
  n_top <- 6L
  n_mid <- 8L
  n_bottom <- 6L

  top_ids <- ids_sorted[seq_len(n_top)]
  bottom_ids <- rev(ids_sorted)[seq_len(n_bottom)]
  center <- floor((n + 1L) / 2L)
  mid_radius <- floor((n_mid - 1L) / 2L)
  mid_start <- max(1L, center - mid_radius)
  mid_end <- min(n, mid_start + n_mid - 1L)
  mid_ids <- ids_sorted[mid_start:mid_end]

  expect_equal(length(anchors), n_anchor)
  expect_equal(sum(anchors %in% top_ids), n_top)
  expect_equal(sum(anchors %in% mid_ids), n_mid)
  expect_equal(sum(anchors %in% bottom_ids), n_bottom)
})

test_that("strata assignment uses deterministic ranking and top-band refinement", {
  scores <- stats::setNames(c(9, 9, 8, 7, 6, 5, 4, 3), as.character(1:8))
  defaults <- pairwiseLLM:::adaptive_defaults(8L)
  strata <- pairwiseLLM:::.adaptive_assign_strata(scores, defaults)
  rank_index <- strata$rank_index

  expect_equal(rank_index[["1"]], 1L)
  expect_equal(rank_index[["2"]], 2L)
  expect_true(all(diff(unname(rank_index[names(sort(rank_index))])) >= 0L))
  expect_true(length(strata$top_band_ids) >= 1L)
  expect_true(all(strata$top_band_ids %in% names(scores)))
})

test_that("stage candidate generators enforce stage admissibility", {
  items <- make_test_items(12)
  trueskill_state <- make_test_trueskill_state(items, mu = seq(12, 1))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)
  anchors <- state$round$anchor_ids

  anchor_cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "anchor_link", "base", C_max = 5000L, seed = 1L
  )
  expect_true(nrow(anchor_cand) > 0L)
  expect_true(all(xor(anchor_cand$i %in% anchors, anchor_cand$j %in% anchors)))

  long_cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "long_link", "base", C_max = 5000L, seed = 1L
  )
  mid_cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "mid_link", "base", C_max = 5000L, seed = 1L
  )
  local_cand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "base", C_max = 5000L, seed = 1L
  )

  expect_true(all(!long_cand$i %in% anchors & !long_cand$j %in% anchors))
  expect_true(all(!mid_cand$i %in% anchors & !mid_cand$j %in% anchors))
  expect_true(nrow(local_cand) > 0L)

  defaults <- pairwiseLLM:::adaptive_defaults(nrow(items))
  proxy <- pairwiseLLM:::.adaptive_rank_proxy(state)
  strata <- pairwiseLLM:::.adaptive_assign_strata(proxy$scores, defaults)
  dist_local <- abs(
    as.integer(strata$stratum_map[local_cand$i]) - as.integer(strata$stratum_map[local_cand$j])
  )
  expect_true(any(local_cand$i %in% anchors | local_cand$j %in% anchors))
  expect_true(any(dist_local == 0L))
})

test_that("long-link fallback candidate generators preserve long distance bounds", {
  items <- make_test_items(30)
  trueskill_state <- make_test_trueskill_state(items, mu = seq(30, 1))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)

  long_expand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "long_link", "expand_locality", C_max = 5000L, seed = 1L
  )
  long_global <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "long_link", "global_safe", C_max = 5000L, seed = 1L
  )

  defaults <- pairwiseLLM:::adaptive_defaults(nrow(items))
  proxy <- pairwiseLLM:::.adaptive_rank_proxy(state)
  strata <- pairwiseLLM:::.adaptive_assign_strata(proxy$scores, defaults)
  dist_expand <- abs(
    as.integer(strata$stratum_map[long_expand$i]) - as.integer(strata$stratum_map[long_expand$j])
  )
  dist_global <- abs(
    as.integer(strata$stratum_map[long_global$i]) - as.integer(strata$stratum_map[long_global$j])
  )

  expect_true(nrow(long_expand) > 0L)
  expect_true(nrow(long_global) > 0L)
  expect_true(all(dist_expand >= defaults$long_min_dist))
  expect_true(all(dist_global >= defaults$long_min_dist))
})

test_that("defaults formulas are deterministic across representative N", {
  d_small <- pairwiseLLM:::adaptive_defaults(30L)
  d_med <- pairwiseLLM:::adaptive_defaults(120L)
  d_large <- pairwiseLLM:::adaptive_defaults(1000L)

  expect_true(d_small$W <= d_med$W)
  expect_true(d_med$W <= d_large$W)
  expect_true(d_small$long_min_dist <= d_small$k_base)
  expect_true(d_med$mid_min_dist <= d_med$mid_max_dist)
  expect_true(d_large$C_max >= 1L)
  expect_true(d_large$explore_rate >= 0 && d_large$explore_rate <= 1)
  expect_equal(d_small$top_band_pct, 0.10)
  expect_equal(d_small$top_band_bins, 5L)
  expect_equal(d_med$top_band_pct, 0.10)
  expect_equal(d_med$top_band_bins, 5L)
  expect_equal(d_small$exposure_underrep_q, 0.25)
  expect_equal(d_med$exposure_underrep_q, 0.25)
})

test_that("top-band size uses ceiling rule", {
  scores <- stats::setNames(seq(15, 1), as.character(1:15))
  defaults <- pairwiseLLM:::adaptive_defaults(15L)
  strata <- pairwiseLLM:::.adaptive_assign_strata(scores, defaults)

  # ceil(0.10 * 15) = 2
  expect_equal(length(strata$top_band_ids), 2L)
})

test_that("stage candidate subsampling is deterministic by seed", {
  items <- make_test_items(25)
  trueskill_state <- make_test_trueskill_state(items, mu = seq(25, 1))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE

  c1 <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "global_safe", C_max = 20L, seed = 21L
  )
  c2 <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "global_safe", C_max = 20L, seed = 21L
  )
  c3 <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "global_safe", C_max = 20L, seed = 22L
  )

  expect_equal(nrow(c1), 20L)
  expect_equal(c1, c2)
  expect_false(identical(c1, c3))
})

test_that("round candidates helper edge branches are exercised", {
  scores <- stats::setNames(seq(6, 1), as.character(1:6))
  defaults <- pairwiseLLM:::adaptive_defaults(6L)
  anchors <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores, defaults)
  expect_true(length(anchors) >= 1L)

  state <- adaptive_rank_start(make_test_items(6), seed = 2L)
  state$round$anchor_ids <- character()
  state$round$anchor_round_id <- 0L
  state$round$round_id <- 1L
  state$refit_meta$last_refit_round_id <- 0L
  d <- pairwiseLLM:::adaptive_defaults(6L)
  d$anchor_refresh_on_round <- TRUE
  expect_true(pairwiseLLM:::.adaptive_round_anchor_needs_refresh(state, d))

  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(list(), "anchor_link", "base", C_max = 10L, seed = 1L),
    "adaptive_state object"
  )
  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(state, "bad_stage", "base", C_max = 10L, seed = 1L),
    "must be one of the stage labels"
  )

  no_recent <- pairwiseLLM:::.adaptive_round_exposure_filter(
    candidates = tibble::tibble(i = c("1", "2"), j = c("3", "4")),
    round = list(per_round_item_uses = stats::setNames(integer(), character()), repeat_in_round_budget = 1L, repeat_in_round_used = 0L),
    recent_deg = numeric(),
    defaults = pairwiseLLM:::adaptive_defaults(8L),
    allow_repeat_pressure = TRUE
  )
  expect_true(nrow(no_recent) >= 0L)
})
