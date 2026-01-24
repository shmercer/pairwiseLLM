test_that("round log includes position balance and backlog fields", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  state$deg <- c(A = 2L, B = 2L, C = 0L)
  state$pos1 <- c(A = 1L, B = 0L, C = 0L)

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = NULL,
    metrics = list(scheduled_pairs = 10L, completed_pairs = 7L),
    config = list(batch_size = 2L, W = 2L, explore_rate = 0.2, U_abs = 0.1)
  )

  expect_true("pos_balance_mean" %in% names(row))
  expect_true("backlog_unjudged" %in% names(row))
  expect_equal(row$pos_balance_mean[[1L]], -0.25)
  expect_equal(row$pos_balance_sd[[1L]], sqrt(0.125), tolerance = 1e-8)
  expect_equal(row$backlog_unjudged[[1L]], 3L)
})
