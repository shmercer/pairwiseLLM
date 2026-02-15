test_that("repeated pairs reverse order and invalid steps do not affect last order", {
  items <- make_test_items(2)
  judge_valid <- make_deterministic_judge("i_wins")
  judge_invalid <- make_deterministic_judge("invalid")

  withr::local_seed(1)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  first <- tail(state$step_log, 1L)

  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  second <- tail(state$step_log, 1L)

  expect_equal(second$A, first$B)
  expect_equal(second$B, first$A)
  expect_true(!is.na(first$pair_id))
  expect_true(!is.na(second$pair_id))

  state <- pairwiseLLM:::new_adaptive_state(items)
  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  first <- tail(state$step_log, 1L)

  state <- pairwiseLLM:::run_one_step(state, judge_invalid)
  invalid <- tail(state$step_log, 1L)
  expect_true(is.na(invalid$pair_id))

  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  third <- tail(state$step_log, 1L)
  expect_equal(third$A, first$B)
  expect_equal(third$B, first$A)
})

test_that("adaptive_assign_order keeps reversal precedence across seed values", {
  pos <- stats::setNames(c(0L, 0L), c("1", "2"))
  pair_last_order <- list(`1:2` = c("1", "2"))

  out_a <- pairwiseLLM:::.adaptive_assign_order(
    pair = tibble::tibble(i = "1", j = "2"),
    posA = pos,
    posB = pos,
    pair_last_order = pair_last_order,
    seed_base = 1L
  )
  out_b <- pairwiseLLM:::.adaptive_assign_order(
    pair = tibble::tibble(i = "1", j = "2"),
    posA = pos,
    posB = pos,
    pair_last_order = pair_last_order,
    seed_base = 999L
  )

  expect_identical(out_a[["A_id"]], "2")
  expect_identical(out_a[["B_id"]], "1")
  expect_identical(out_b, out_a)
})

test_that("warm-start selection passes canonical seed_base into adaptive_assign_order", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$meta$seed <- 91L
  state$warm_start_done <- FALSE
  state$warm_start_idx <- 1L
  state$warm_start_pairs <- tibble::tibble(i_id = "1", j_id = "2")

  captured_seed <- NA_integer_
  out <- testthat::with_mocked_bindings(
    .adaptive_assign_order = function(pair, posA, posB, pair_last_order, seed_base = 1L) {
      captured_seed <<- as.integer(seed_base)
      c(A_id = "1", B_id = "2")
    },
    pairwiseLLM:::.adaptive_warm_start_selection(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_identical(captured_seed, 91L)
  expect_identical(out$A, 1L)
  expect_identical(out$B, 2L)
})
