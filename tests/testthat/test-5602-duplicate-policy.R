testthat::test_that("duplicate policy enforces all locked conditions", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$posterior$U_dup_threshold <- 0.5

  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  state$pair_count[[unordered_key]] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = unordered_key,
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  allowed <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = unordered_key,
    utility = 0.8,
    utility_raw = 0.8,
    p_mean = 0.51
  )
  out_ok <- pairwiseLLM:::select_exploitation_pairs_v3(
    candidates_with_utility = allowed,
    state = state,
    n_exploit = 1L,
    config = config
  )
  expect_equal(nrow(out_ok), 1L)

  blocked_count <- allowed
  state$pair_count[[unordered_key]] <- config$dup_max_count
  out_count <- pairwiseLLM:::select_exploitation_pairs_v3(
    candidates_with_utility = blocked_count,
    state = state,
    n_exploit = 1L,
    config = config
  )
  expect_equal(nrow(out_count), 0L)

  blocked_util <- allowed
  blocked_util$utility <- 0.1
  blocked_util$utility_raw <- 0.1
  state$pair_count[[unordered_key]] <- 1L
  out_util <- pairwiseLLM:::select_exploitation_pairs_v3(
    candidates_with_utility = blocked_util,
    state = state,
    n_exploit = 1L,
    config = config
  )
  expect_equal(nrow(out_util), 0L)
})
