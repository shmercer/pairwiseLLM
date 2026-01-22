testthat::test_that("generate_candidates_streamed validates inputs", {
  expect_error(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = "A",
      anchors = "A",
      W = 1L,
      cap = 1L
    ),
    "at least two ids"
  )
  expect_error(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", NA),
      anchors = "A",
      W = 1L,
      cap = 1L
    ),
    "non-empty"
  )
  expect_error(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", "A"),
      anchors = "A",
      W = 1L,
      cap = 1L
    ),
    "unique"
  )
  expect_error(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", "B"),
      anchors = "A",
      W = 0L,
      cap = 1L
    ),
    "`W`"
  )
  expect_error(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", "B"),
      anchors = "A",
      W = 1L,
      cap = 0L
    ),
    "`cap`"
  )
})

testthat::test_that("generate_candidates_streamed covers streaming branches", {
  out_empty <- pairwiseLLM:::generate_candidates_streamed(
    ranked_ids = c("A", "B"),
    anchors = "Z",
    W = 1L,
    cap = 2L
  )
  expect_equal(nrow(out_empty), 0L)

  out_window <- pairwiseLLM:::generate_candidates_streamed(
    ranked_ids = c("A", "B", "C"),
    anchors = "A",
    W = 1L,
    cap = 2L
  )
  expect_true(nrow(out_window) >= 1L)
})

testthat::test_that("generate_candidates_streamed handles edge window cases", {
  out_cap_zero <- testthat::with_mocked_bindings(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = "A",
      anchors = "A",
      W = 1L,
      cap = 1L
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(out_cap_zero), 0L)

  out_null_rank <- testthat::with_mocked_bindings(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", "B"),
      anchors = "A",
      W = 1L,
      cap = 2L
    ),
    setNames = function(x, nm) list(),
    .package = "stats"
  )
  expect_equal(nrow(out_null_rank), 0L)

  out_empty_window <- testthat::with_mocked_bindings(
    pairwiseLLM:::generate_candidates_streamed(
      ranked_ids = c("A", "B"),
      anchors = "A",
      W = 0L,
      cap = 2L
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(out_empty_window), 0L)
})

testthat::test_that("candidate helpers cover invalid config branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(1, 0, -1),
    theta_sd = c(0.2, 0.3, 0.4)
  )

  empty_cap <- testthat::with_mocked_bindings(
    pairwiseLLM:::enumerate_candidates(
      anchors = "A",
      theta_summary = theta_summary,
      state = state,
      config = list(W = 1L, C_max = 0L)
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(empty_cap), 0L)

  empty_anchors <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors = "Z",
    theta_summary = theta_summary,
    state = state,
    config = pairwiseLLM:::adaptive_v3_config(state$N)
  )
  expect_equal(nrow(empty_anchors), 0L)

  empty_W <- testthat::with_mocked_bindings(
    pairwiseLLM:::generate_candidates_from_anchors(
      anchors = "A",
      theta_summary = theta_summary,
      state = state,
      config = list(W = 0L, C_max = 1L)
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(empty_W), 0L)

  empty_C <- testthat::with_mocked_bindings(
    pairwiseLLM:::generate_candidates_from_anchors(
      anchors = "A",
      theta_summary = theta_summary,
      state = state,
      config = list(W = 1L, C_max = 0L)
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(empty_C), 0L)
})
