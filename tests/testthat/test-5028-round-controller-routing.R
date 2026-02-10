test_that("round state initializes with deterministic stage quotas", {
  state <- pairwiseLLM:::new_adaptive_state(make_test_items(10))
  round <- state$round

  expect_equal(round$round_id, 1L)
  expect_equal(round$stage_order, c("anchor_link", "long_link", "mid_link", "local_link"))
  expect_equal(sum(round$stage_quotas), round$round_pairs_target)
  expect_true(all(round$stage_committed == 0L))
})

test_that("round quota schedule switches deterministically after early rounds", {
  q1 <- pairwiseLLM:::.adaptive_round_compute_quotas(round_id = 1L, n_items = 100L)
  q5 <- pairwiseLLM:::.adaptive_round_compute_quotas(round_id = 5L, n_items = 100L)

  expect_equal(sum(q1), pairwiseLLM:::adaptive_defaults(100L)$round_pairs_target)
  expect_equal(sum(q5), pairwiseLLM:::adaptive_defaults(100L)$round_pairs_target)
  expect_true(q1[["anchor_link"]] >= q5[["anchor_link"]])
  expect_true(q1[["long_link"]] >= q5[["long_link"]])
})

test_that("invalid steps do not advance stage committed counters", {
  items <- make_test_items(6)
  state <- adaptive_rank_start(items, seed = 11)
  judge <- make_deterministic_judge("invalid")

  withr::local_seed(1)
  out <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  expect_true(all(out$round$stage_committed == 0L))
  expect_equal(out$step_log$status[[1L]], "invalid")
})

test_that("candidate starvation advances across stages before run-level stop", {
  items <- make_test_items(2)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$round$staged_active <- TRUE
  state$history_pairs <- tibble::tibble(
    A_id = c("1", "1", "1"),
    B_id = c("2", "2", "2")
  )
  judge <- make_deterministic_judge("i_wins")

  out <- adaptive_rank_run_live(state, judge, n_steps = 10L, progress = "none")

  expect_true(isTRUE(out$meta$stop_decision))
  expect_equal(out$meta$stop_reason, "candidate_starvation")
  expect_true(all(out$step_log$status == "starved"))
  expect_equal(unique(out$step_log$round_stage), c("anchor_link", "long_link", "mid_link", "local_link"))
})

test_that("committed stage steps advance stage counters and round totals", {
  items <- make_test_items(6)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$round$staged_active <- TRUE
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  out <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  expect_equal(out$step_log$status[[1L]], "ok")
  expect_equal(out$step_log$round_stage[[1L]], "anchor_link")
  expect_equal(out$round$stage_committed[["anchor_link"]], 1L)
  expect_equal(out$round$round_committed, 1L)
})

test_that("round repeat budget tracks repeated item-uses per endpoint", {
  state <- pairwiseLLM:::new_adaptive_state(make_test_items(4))
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$per_round_item_uses[["2"]] <- 1L

  step_row <- tibble::tibble(
    round_stage = "anchor_link",
    A = 1L,
    B = 2L
  )

  out <- pairwiseLLM:::.adaptive_round_commit(state, step_row)

  expect_equal(out$round$repeat_in_round_used, 2L)
})

test_that("warm-start committed pairs count toward round committed totals", {
  items <- make_test_items(5)
  state <- adaptive_rank_start(items, seed = 7)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  out <- adaptive_rank_run_live(state, judge, n_steps = 4L, progress = "none")

  warm_steps <- sum(out$step_log$round_stage == "warm_start", na.rm = TRUE)
  expect_true(warm_steps >= 1L)
  expect_true(out$round$committed_total >= warm_steps)
  expect_true(out$round$round_committed >= warm_steps)
})
