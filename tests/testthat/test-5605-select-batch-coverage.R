testthat::test_that("batch helpers handle empty history and duplicate gating", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_null(pairwiseLLM:::.adaptive_last_order_for_pair(state, "A:B"))

  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L
  expect_null(pairwiseLLM:::.adaptive_last_order_for_pair(state, "A:C"))

  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$pair_count <- stats::setNames(integer(), character())
  expect_true(pairwiseLLM:::.adaptive_duplicate_allowed(state, "A:B", 0.5, 0.5, config))

  state$pair_count <- stats::setNames(1L, "A:B")
  state$posterior$U_dup_threshold <- 0.4
  expect_false(pairwiseLLM:::.adaptive_duplicate_allowed(state, "A:B", NA_real_, 0.5, config))
  state$posterior$U_dup_threshold <- NA_real_
  expect_false(pairwiseLLM:::.adaptive_duplicate_allowed(state, "A:B", 0.5, 0.5, config))
})

testthat::test_that("exploration sampling validates inputs and defaults utility_raw", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  candidates <- tibble::tibble(
    i_id = c("A", "A"),
    j_id = c("B", "C"),
    unordered_key = c("A:B", "A:C"),
    utility = c(0.4, 0.6),
    p_mean = c(0.5, 0.5)
  )

  expect_error(
    pairwiseLLM:::sample_exploration_pairs(state, "bad", 1L, config),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::sample_exploration_pairs(state, candidates, -1L, config),
    "non-negative"
  )
  candidates_bad <- candidates
  candidates_bad$i_id[[1L]] <- "Z"
  expect_error(
    pairwiseLLM:::sample_exploration_pairs(state, candidates_bad, 1L, config),
    "state\\$ids"
  )

  empty_out <- pairwiseLLM:::sample_exploration_pairs(
    state = state,
    candidates = candidates,
    n_explore = 0L,
    config = config
  )
  expect_equal(nrow(empty_out), 0L)
  expect_true("utility_raw" %in% names(empty_out))

})

testthat::test_that("exploration sampling covers missing lookup and duplicate reuse", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  state$pair_count <- stats::setNames(1L, "A:B")
  state$posterior$U_dup_threshold <- 0.1
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L
  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.9,
    utility_raw = 0.9,
    p_mean = 0.5
  )
  withr::local_seed(2)
  dup_out <- pairwiseLLM:::sample_exploration_pairs(
    state = state,
    candidates = candidates,
    n_explore = 1L,
    config = config
  )
  expect_equal(nrow(dup_out), 1L)
  expect_equal(dup_out$utility_raw, 0.9)
})

testthat::test_that("exploitation and ordering helpers enforce validation", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  expect_error(
    pairwiseLLM:::select_exploitation_pairs("bad", state, 1L, config),
    "data frame"
  )
  empty_candidates <- tibble::tibble(
    i_id = character(),
    j_id = character(),
    unordered_key = character(),
    utility = double(),
    p_mean = double()
  )
  expect_error(
    pairwiseLLM:::select_exploitation_pairs(empty_candidates, state, -1L, config),
    "non-negative"
  )
  bad_ids <- tibble::tibble(
    i_id = "A",
    j_id = "Z",
    unordered_key = "A:Z",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )
  expect_error(
    pairwiseLLM:::select_exploitation_pairs(bad_ids, state, 1L, config),
    "state\\$ids"
  )
  self_pairs <- tibble::tibble(
    i_id = "A",
    j_id = "A",
    unordered_key = "A:A",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )
  out_self <- pairwiseLLM:::select_exploitation_pairs(self_pairs, state, 1L, config)
  expect_equal(nrow(out_self), 0L)

  dup_candidates <- tibble::tibble(
    i_id = c("A", "A"),
    j_id = c("B", "B"),
    unordered_key = c("A:B", "A:B"),
    utility = c(0.9, 0.1),
    utility_raw = c(0.9, 0.1),
    p_mean = c(0.5, 0.5)
  )
  dup_out <- pairwiseLLM:::select_exploitation_pairs(dup_candidates, state, 2L, config)
  expect_equal(nrow(dup_out), 1L)

  expect_error(pairwiseLLM:::assign_order("bad", state), "data frame")
  missing <- tibble::tibble(
    i_id = "A",
    j_id = "Z",
    unordered_key = "A:Z"
  )
  expect_error(pairwiseLLM:::assign_order(missing, state), "state\\$ids")
  state$pair_count <- stats::setNames(1L, "A:B")
  dup_pair <- tibble::tibble(i_id = "A", j_id = "B", unordered_key = "A:B")
  expect_error(pairwiseLLM:::assign_order(dup_pair, state), "prior presentation")
})

testthat::test_that("select_batch validates inputs and handles zero batch size", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  expect_error(
    pairwiseLLM:::select_batch(state, "bad", config),
    "data frame"
  )

  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.2,
    utility_raw = 0.2,
    p_mean = 0.5
  )
  config_bad <- config
  config_bad$batch_size <- -1L
  expect_error(
    pairwiseLLM:::select_batch(state, candidates, config_bad),
    "batch_size"
  )

  config_zero <- config
  config_zero$batch_size <- 0L
  out <- pairwiseLLM:::select_batch(state, candidates, config_zero)
  expect_equal(nrow(out), 0L)
})
