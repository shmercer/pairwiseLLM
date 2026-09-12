test_that("ordinary group candidates preserve rank orientation and stratum distance", {
  ids <- letters[1:6]
  ranks <- stats::setNames(c(4L, 1L, 6L, 2L, 5L, 3L), ids)
  strata <- stats::setNames(c(2L, 1L, 3L, 1L, 3L, 2L), ids)
  same <- pairwiseLLM:::.adaptive_within_set_same_group_pairs(rev(ids[1:3]), ranks, 2L)
  expect_identical(same$i, c("b", "b", "a"))
  expect_identical(same$j, c("a", "c", "c"))
  expect_identical(same$dist_stratum_global, rep(2L, 3L))
  empty <- pairwiseLLM:::.adaptive_within_set_same_group_pairs("a", ranks)
  expect_identical(empty, tibble::tibble(i = character(), j = character(),
    dist_stratum_global = integer()))
  cross <- pairwiseLLM:::.adaptive_within_set_cross_group_pairs(
    c("c", "a", "c"), c("d", "b"), ranks, strata)
  expect_identical(cross$i, c("b", "b", "d", "d"))
  expect_identical(cross$j, c("a", "c", "a", "c"))
  expect_identical(cross$dist_stratum_global, c(1L, 2L, 1L, 2L))
  expect_identical(pairwiseLLM:::.adaptive_within_set_cross_group_pairs(
    character(), ids, ranks, strata), empty)
  expect_error(pairwiseLLM:::.adaptive_within_set_stage_sorted_inputs(
    c("a", "missing"), "a", ranks, strata, "local_link"), "finite ranks")
  expect_identical(pairwiseLLM:::.adaptive_within_set_stage_right_mask(
    c(1L, 3L), c(TRUE, FALSE), 1L, TRUE, "anchor_link", list()), c(FALSE, TRUE))
})

test_that("bounded anchor candidates equal a small exhaustive legal domain", {
  withr::local_seed(706L)
  rng <- .Random.seed
  ids <- letters[1:8]
  strata <- rep(1:4, each = 2L)
  for (anchors in list(c(1L, 2L), c(2L, 5L), c(7L, 8L))) {
    anchor <- seq_along(ids) %in% anchors
    all_pairs <- utils::combn(seq_along(ids), 2L)
    legal <- all_pairs[, xor(anchor[all_pairs[1, ]], anchor[all_pairs[2, ]]), drop = FALSE]
    expected <- tibble::tibble(i = ids[legal[1, ]], j = ids[legal[2, ]],
      dist_stratum_global = as.integer(abs(strata[legal[1, ]] - strata[legal[2, ]])))
    full <- pairwiseLLM:::.adaptive_within_set_anchor_pairs_bounded(ids, anchor, strata)
    expect_identical(full$candidates, expected)
    expect_identical(full$total_legal, nrow(expected))
    expect_false(full$bounded_used)
    for (seed in c(3L, 19L)) {
      small <- pairwiseLLM:::.adaptive_within_set_anchor_pairs_bounded(
        ids, anchor, strata, C_max = 2L, seed = seed)
      sampled <- withr::with_seed(seed, sample.int(nrow(expected), 2L))
      expect_identical(small$candidates, expected[sampled, ])
      expect_true(small$bounded_used)
      expect_identical(small$total_legal, nrow(expected))
    }
  }
  for (n in c(0L, 1L, 8L)) {
    for (is_anchor in c(TRUE, FALSE)) {
      out <- pairwiseLLM:::.adaptive_within_set_anchor_pairs_bounded(
        head(ids, n), rep(is_anchor, n), head(strata, n))
      expect_identical(out$total_legal, 0L)
      expect_identical(nrow(out$candidates), 0L)
      expect_false(out$bounded_used)
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("item and TrueSkill guards reject malformed identities before use", {
  items <- tibble::tibble(item_id = letters[1:3], set_id = 1L, global_item_id = letters[1:3])
  for (value in list(c(1, 1.5, 2), c(1L, NA_integer_, 2L), c(0L, 1L, 2L))) {
    bad <- items
    bad$set_id <- value
    expect_error(adaptive_rank_start(bad), "set_id")
  }
  for (value in list(c("a", NA, "c"), c("a", "", "c"), c("a", "a", "c"))) {
    bad <- items
    bad$global_item_id <- value
    expect_error(adaptive_rank_start(bad), "global_item_id")
  }
  expect_error(pairwiseLLM:::validate_trueskill_state(structure(1L, class = "trueskill_state")),
    "must be a list")
  ts <- adaptive_rank_start(items)$trueskill_state
  bad <- ts
  bad$items$item_id[1] <- NA_character_
  expect_error(pairwiseLLM:::validate_trueskill_state(bad), "non-missing")
  bad <- ts
  bad$items$sigma <- as.character(bad$items$sigma)
  expect_error(pairwiseLLM:::validate_trueskill_state(bad), "finite numeric")
  expect_error(pairwiseLLM:::trueskill_win_probability(c("a", "b"), "c", ts), "scalar")
})

test_that("judge metadata normalization preserves valid scalar values and invalid transactions", {
  expect_identical(pairwiseLLM:::.adaptive_judge_scalar_character(list("replay")), "replay")
  expect_identical(pairwiseLLM:::.adaptive_judge_scalar_character(c("a", "b")), NA_character_)
  expect_identical(pairwiseLLM:::.adaptive_judge_scalar_integer(list(2L)), 2L)
  expect_identical(pairwiseLLM:::.adaptive_judge_scalar_double(list(0.25)), 0.25)
  expect_identical(pairwiseLLM:::.adaptive_judge_scalar_double("bad"), NA_real_)
  for (value in list(NA_character_, "", list(NULL), new.env(parent = emptyenv()))) {
    expect_identical(pairwiseLLM:::.adaptive_serialize_raw_response(value), NA_character_)
  }
  expect_identical(pairwiseLLM:::.adaptive_serialize_raw_response("raw text"), "raw text")
  expect_identical(pairwiseLLM:::.adaptive_serialize_raw_response(list(list(Y = 1L))), '{"Y":1}')
  for (strategy in c("hybrid", "trueskill_p50")) {
    state <- task07_start(task07_fixture(), strategy = strategy)
    before <- state
    state <- adaptive_rank_run_live(state, function(...) {
      list(is_valid = FALSE, invalid_reason = "fixture", raw_response = "unparseable")
    }, n_steps = 1L, progress = "none")
    for (field in c("warm_start_idx", "history_pairs", "trueskill_state", "round")) {
      expect_identical(state[[field]], before[[field]])
    }
    expect_identical(state$step_log$pair_id, NA_integer_)
  }
})

test_that("committed evidence caches exclude invalid rows and preserve empty scopes", {
  f <- task07_fixture(n = 4L)
  state <- task07_run(f, strategy = "random", n_steps = 3L)$state
  state$step_log$is_cross_set <- NULL
  cache <- pairwiseLLM:::.adaptive_committed_results_rebuild(state)
  expect_identical(cache$is_cross_set, rep(FALSE, 3L))
  expect_identical(cache$Y, state$step_log$Y)
  update <- pairwiseLLM:::.adaptive_committed_results_update
  expect_identical(update(cache, state$step_log[0, ], "a", "b", 1L), cache)
  invalid <- state$step_log[1, ]
  invalid$pair_id <- NA_integer_
  expect_identical(update(cache, invalid, "a", "b", 1L), cache)
  expect_identical(update(NULL, invalid, "a", "b", 1L),
    pairwiseLLM:::.adaptive_committed_results_empty())
  expect_identical(pairwiseLLM:::.adaptive_results_from_step_log(state, "missing"), tibble::tibble())
  expect_error(pairwiseLLM:::maybe_refit_btl(list(), list()), "adaptive_state")
  fresh <- task07_start(f)
  not_due <- pairwiseLLM:::maybe_refit_btl(fresh, list())
  expect_identical(not_due$state, fresh)
  expect_false(not_due$refit_performed)
  expect_identical(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(
    fresh, stats::setNames(1:3, letters[1:3])), NA_real_)
})

test_that("history caches roll their recent window while preserving total committed counts", {
  ids <- letters[1:4]
  cache <- pairwiseLLM:::.adaptive_history_state_empty(ids)
  # Small explicit recent window exercises eviction without a large adaptive run.
  cache$recent_window_n <- 2L
  pairs <- tibble::tibble(A_id = c("a", "b", "c", "d"), B_id = c("b", "c", "d", "a"))
  for (k in 1:4) {
    cache <- pairwiseLLM:::.adaptive_history_state_update(cache, pairs$A_id[k], pairs$B_id[k])
    expected <- pairs[seq_len(k), ]
    expect_identical(cache$deg, pairwiseLLM:::.adaptive_pair_counts(expected, ids)$deg)
    expect_identical(cache$recent_deg, pairwiseLLM:::.adaptive_recent_deg(expected, ids, 2L))
  }
  expect_identical(cache$n_pairs, 4L)
  expect_identical(pairwiseLLM:::.adaptive_history_state_live_recent_window(character()), 0L)
  cache$recent_window_n <- 0L
  cache <- pairwiseLLM:::.adaptive_history_state_update(cache, "a", "c")
  expect_identical(unname(cache$recent_deg), rep(0L, 4L))
})

test_that("round recovery and canonical direct diagnostics retain their types", {
  state <- task07_start(task07_fixture(), strategy = "trueskill_pollitt")
  state$round <- NULL
  restored <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
  expect_identical(restored$round$round_id, 1L)
  expect_false(restored$round$staged_active)
  expect_length(restored$round$stage_order, 0L)
  expect_error(pairwiseLLM:::append_canonical_row(pairwiseLLM:::new_step_log(), list(),
    unname(pairwiseLLM:::schema_step_log)), "named list")
  expect_error(pairwiseLLM:::append_canonical_row(
    tibble::tibble(extra = 1L), list(), pairwiseLLM:::schema_step_log), "non-canonical")
  row <- list(step_id = 1L, pairing_strategy = "trueskill_pollitt", target_distance = 0.1)
  log <- pairwiseLLM:::append_step_log(pairwiseLLM:::new_step_log(), row)
  expect_identical(log$pairing_strategy, "trueskill_pollitt")
  expect_identical(log$target_distance, 0.1)
  expect_identical(names(log), names(pairwiseLLM:::schema_step_log))
})

test_that("report helpers describe direct rounds without manufacturing hybrid quotas", {
  f <- task07_fixture(n = 4L)
  state <- task07_run(f, strategy = "trueskill_p50", n_steps = 5L)$state
  expect_identical(nrow(pairwiseLLM:::.adaptive_stage_quota_summary(state$step_log)), 0L)
  profile <- pairwiseLLM:::.adaptive_efficiency_profile(state,
    config = list(refit_pairs_target = 5000L))
  expect_identical(profile$predicted_selection_round_stage, "direct_pairing")
  expect_false(profile$warm_start_active)
  expect_identical(profile$refit_profile_state_source, "current_state")
  expect_identical(nrow(pairwiseLLM:::.adaptive_efficiency_timing_rows(list())), 0L)
  expect_error(pairwiseLLM:::.adaptive_efficiency_profile(list()), "adaptive_state")
  public <- pairwiseLLM::adaptive_step_log(state)
  expect_identical(as.character(public$pairing_strategy), rep("trueskill_p50", 5L))
  expect_identical(public$target_distance, state$step_log$target_distance)
  legacy <- state$step_log
  legacy$link_estimation_mode <- legacy$is_probe_step <- NULL
  normalized <- pairwiseLLM:::.adaptive_normalize_public_step_log(legacy)
  expect_identical(normalized$pairing_strategy, legacy$pairing_strategy)
  expect_identical(normalized$target_distance, legacy$target_distance)
})
