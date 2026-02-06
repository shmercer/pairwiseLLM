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
