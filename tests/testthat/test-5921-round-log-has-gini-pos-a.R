test_that("round log includes gini_pos_A and it is non-negative", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  state <- pairwiseLLM:::record_exposure(state, "A", "B")
  state <- pairwiseLLM:::record_exposure(state, "A", "C")

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = NULL,
    config = list(batch_size = 2L, W = 2L, explore_rate = 0.2, U_abs = 0.1)
  )

  expect_true("gini_degree" %in% names(row))
  expect_true("gini_pos_A" %in% names(row))
  expect_true(is.finite(row$gini_pos_A[[1L]]))
  expect_gte(row$gini_pos_A[[1L]], 0)
})
