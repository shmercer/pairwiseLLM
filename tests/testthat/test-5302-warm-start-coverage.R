testthat::test_that("warm_start validates inputs and edge cases", {
  withr::local_seed(1001)

  expect_error(
    pairwiseLLM:::warm_start("A", config = list()),
    "at least 2 items"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "A"), config = list()),
    "unique"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "B"), config = list(min_degree = 1.2)),
    "integer"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "B"), config = list(min_degree = 0L)),
    ">= 1"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "B", "C"), config = list(min_degree = 1L)),
    ">= 2"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "B"), config = list(min_degree = 2L)),
    "<= N - 1"
  )
  expect_error(
    pairwiseLLM:::warm_start(
      c("A", "B"),
      config = list(min_degree = 1L, target_mean_degree = "bad")
    ),
    "finite numeric"
  )
  expect_error(
    pairwiseLLM:::warm_start(
      c("A", "B"),
      config = list(min_degree = 1L, target_mean_degree = 0)
    ),
    "> 0"
  )
  expect_error(
    pairwiseLLM:::warm_start(c("A", "B", "C"), config = list(target_mean_degree = 3)),
    "<= N - 1"
  )
})

testthat::test_that("warm_start handles ring duplicates for N = 2", {
  withr::local_seed(1002)
  ids <- c("B", "A")
  pairs <- pairwiseLLM:::warm_start(ids, config = list(min_degree = 1L))

  expect_equal(nrow(pairs), 1L)
  expect_true(all(pairs$i != pairs$j))
  keys <- paste(pairs$i, pairs$j, sep = ":")
  expect_equal(length(unique(keys)), 1L)
})

testthat::test_that("warm_start covers min-degree fallback with mocked combn", {
  withr::local_seed(1003)
  ids <- c("A", "B", "C", "D")

  mock_combn <- function(x, m) {
    matrix(c("B", "A", "C", "A"), nrow = 2L)
  }

  testthat::with_mocked_bindings(
    expect_error(
      pairwiseLLM:::warm_start(ids, config = list(min_degree = 3L)),
      "Warm start failed"
    ),
    combn = mock_combn,
    .package = "utils"
  )
})

testthat::test_that("warm_start mean-degree fill adds random remaining edges", {
  withr::local_seed(1004)
  ids <- c("A", "B", "C", "D")

  pairs <- pairwiseLLM:::warm_start(ids, config = list(target_mean_degree = 2.5))

  ring_pairs <- pairwiseLLM:::warm_start(ids, config = list(target_mean_degree = NULL))
  ring_keys <- pairwiseLLM:::make_unordered_key(ring_pairs$i, ring_pairs$j)
  keys <- pairwiseLLM:::make_unordered_key(pairs$i, pairs$j)
  extra_keys <- setdiff(keys, ring_keys)

  expect_equal(length(extra_keys), 1L)
  expect_true(extra_keys %in% c("A:C", "B:D"))
})

testthat::test_that("warm-start scheduling covers even index and empty pairs", {
  withr::local_seed(1005)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, budget_max = 4L),
    seed = 1
  )
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)

  order_even <- pairwiseLLM:::.adaptive_warm_start_order(state, "A", "B", pair_index = 2L)
  expect_equal(order_even$A_id, "B")
  expect_equal(order_even$B_id, "A")

  empty_out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_warm_start(state, config = state$config$v3),
    warm_start = function(ids, config, seed = NULL) {
      tibble::tibble(i = character(), j = character())
    },
    .package = "pairwiseLLM"
  )
  expect_equal(nrow(empty_out$pairs), 0L)
  expect_equal(empty_out$state$comparisons_scheduled, 0L)
})

testthat::test_that("warm-start scheduling respects budget limits", {
  withr::local_seed(1006)
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, budget_max = 1L),
    seed = 2
  )

  expect_error(
    pairwiseLLM:::.adaptive_schedule_warm_start(state, config = list(min_degree = 2L)),
    "budget_max"
  )
})

testthat::test_that("replacement loop breaks when target is zero", {
  withr::local_seed(1007)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L),
    seed = 3
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_replacements_live(
      state = state,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      prompt_template = "template",
      backend = "openai",
      adaptive = list(max_refill_rounds = 1L),
      submission = list(),
      missing = 1L,
      seed = 1,
      replacement_phase = "phase1",
      base_batch_size = 1L
    ),
    .adaptive_replacement_target = function(...) 0L,
    .adaptive_schedule_replacement_pairs = function(...) testthat::fail("should not schedule"),
    .package = "pairwiseLLM"
  )

  expect_equal(length(out$submissions), 0L)
})
