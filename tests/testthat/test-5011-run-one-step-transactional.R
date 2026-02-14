test_that("run_one_step commits valid results transactionally", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- make_deterministic_judge("i_wins")

  before_mu <- state$trueskill_state$items$mu
  before_sigma <- state$trueskill_state$items$sigma
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "ok")
  expect_equal(out$step_log$utility_mode[[1L]], "pairing_trueskill_u0")
  expect_false(is.na(out$step_log$pair_id[[1L]]))
  expect_equal(out$step_log$Y[[1L]], 1L)
  expect_false(isTRUE(all.equal(before_mu, out$trueskill_state$items$mu)))
  expect_false(isTRUE(all.equal(before_sigma, out$trueskill_state$items$sigma)))

  expect_equal(nrow(out$history_pairs), 1L)
  expect_equal(nrow(out$item_step_log), out$n_items)
})

test_that("run_one_step logs invalid results without mutating state", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- make_deterministic_judge("invalid")

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("run_one_step enforces canonical judge contract", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- function(A, B, state, ...) list(Y = 1L)

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("run_one_step consumes warm-start pairs only on valid results", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items, seed = 42L)
  judge_ok <- make_deterministic_judge("i_wins")
  judge_bad <- make_deterministic_judge("invalid")

  first_pair <- state$warm_start_pairs[1, , drop = FALSE]
  out_bad <- pairwiseLLM:::run_one_step(state, judge_bad)
  expect_equal(out_bad$warm_start_idx, 1L)
  expect_false(out_bad$warm_start_done)

  out_ok <- pairwiseLLM:::run_one_step(out_bad, judge_ok)
  unordered <- pairwiseLLM:::make_unordered_key(
    out_ok$history_pairs$A_id[[1L]],
    out_ok$history_pairs$B_id[[1L]]
  )
  expected <- pairwiseLLM:::make_unordered_key(first_pair$i_id[[1L]], first_pair$j_id[[1L]])
  expect_equal(unordered, expected)
  expect_equal(out_ok$warm_start_idx, 2L)
})

test_that("run_one_step populates linking scaffold columns for cross-set rows", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 7L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_mode = "auto",
      hub_lock_mode = "soft_lock",
      hub_lock_kappa = 0.75
    )
  )
  judge_ok <- make_deterministic_judge("i_wins")

  out <- pairwiseLLM:::run_one_step(state, judge_ok)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_equal(sort(c(row$set_i[[1L]], row$set_j[[1L]])), c(1L, 2L))
  expect_true(isTRUE(row$is_cross_set[[1L]]))
  expect_equal(row$link_spoke_id[[1L]], 2L)
  expect_equal(row$run_mode[[1L]], "link_one_spoke")
  expect_equal(row$link_transform_mode[[1L]], "shift_only")
  expect_equal(row$utility_mode[[1L]], "linking_cross_set_p_times_1_minus_p")
  expect_equal(row$hub_lock_mode[[1L]], "soft_lock")
  expect_equal(row$hub_lock_kappa[[1L]], 0.75)
  expect_false(is.na(row$posterior_win_prob_pre[[1L]]))
  expect_false(is.na(row$cross_set_utility_pre[[1L]]))
})

test_that("run_one_step logs hub_lock_kappa as NA unless hub_lock_mode is soft_lock", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  judge_ok <- make_deterministic_judge("i_wins")

  state_free <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      hub_lock_mode = "free",
      hub_lock_kappa = 0.75
    )
  )
  out_free <- pairwiseLLM:::run_one_step(state_free, judge_ok)
  row_free <- out_free$step_log[nrow(out_free$step_log), , drop = FALSE]
  expect_equal(row_free$hub_lock_mode[[1L]], "free")
  expect_true(is.na(row_free$hub_lock_kappa[[1L]]))
})

test_that("run_one_step logs linking pre-step transform estimates when available", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 17L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_mode = "auto"
    )
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.12,
      delta_spoke_sd = 0.03,
      log_alpha_spoke_mean = 0.04,
      log_alpha_spoke_sd = 0.02
    )
  )
  judge_ok <- make_deterministic_judge("i_wins")

  out <- pairwiseLLM:::run_one_step(state, judge_ok)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_equal(row$link_transform_mode[[1L]], "shift_scale")
  expect_equal(row$delta_spoke_estimate_pre[[1L]], 0.12, tolerance = 1e-12)
  expect_equal(row$delta_spoke_sd_pre[[1L]], 0.03, tolerance = 1e-12)
  expect_equal(row$log_alpha_spoke_estimate_pre[[1L]], 0.04, tolerance = 1e-12)
  expect_equal(row$log_alpha_spoke_sd_pre[[1L]], 0.02, tolerance = 1e-12)
})

test_that("invalid linking step does not mutate controller link routing state", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$controller$current_link_spoke_id <- 99L
  state$controller$link_stage_coverage_bins_used <- list(`99` = 3L)
  state$controller$link_stage_coverage_source <- list(`99` = "seed")
  judge_bad <- make_deterministic_judge("invalid")

  out <- pairwiseLLM:::run_one_step(state, judge_bad)

  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_identical(out$controller$current_link_spoke_id, 99L)
  expect_identical(out$controller$link_stage_coverage_bins_used, list(`99` = 3L))
  expect_identical(out$controller$link_stage_coverage_source, list(`99` = "seed"))
})

test_that("run_one_step handles starved selections with NA linking endpoints", {
  items <- make_test_items(2)
  state <- adaptive_rank_start(items, seed = 2L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  judge_ok <- make_deterministic_judge("i_wins")

  out <- state
  for (idx in seq_len(6L)) {
    out <- pairwiseLLM:::run_one_step(out, judge_ok)
    if (identical(utils::tail(out$step_log$status, 1L), "starved")) {
      break
    }
  }

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_equal(row$status[[1L]], "starved")
  expect_true(is.na(row$set_i[[1L]]))
  expect_true(is.na(row$set_j[[1L]]))
  expect_true(is.na(row$is_cross_set[[1L]]))
  expect_true(is.na(row$link_spoke_id[[1L]]))
})
