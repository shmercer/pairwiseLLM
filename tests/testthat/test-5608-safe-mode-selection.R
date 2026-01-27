make_safe_candidates <- function(ids) {
  ids <- as.character(ids)
  combos <- utils::combn(ids, 2)
  n_pairs <- ncol(combos)
  base_util <- seq_len(n_pairs) / max(1L, n_pairs)
  tibble::tibble(
    i_id = combos[1L, ],
    j_id = combos[2L, ],
    unordered_key = pairwiseLLM:::make_unordered_key(combos[1L, ], combos[2L, ]),
    utility = as.double(base_util),
    utility_raw = as.double(base_util),
    p_mean = rep(0.5, n_pairs)
  )
}

testthat::test_that("safe mode selects explore fraction and random remainder", {
  samples <- tibble::tibble(
    ID = LETTERS[1:7],
    text = paste0("text-", seq_len(7))
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N, batch_size = 10L, explore_rate = 0.3)
  candidates <- make_safe_candidates(state$ids)

  withr::local_seed(101)
  selection <- pairwiseLLM:::.adaptive_select_safe_no_utility(
    state = state,
    candidates_with_utility = candidates,
    config = config,
    seed = 101
  )

  testthat::expect_equal(nrow(selection), 10L)
  testthat::expect_true("is_explore" %in% names(selection))
  testthat::expect_equal(sum(selection$is_explore %in% TRUE), 3L)

  explored_keys <- as.character(selection$unordered_key[selection$is_explore %in% TRUE])
  remainder_keys <- as.character(selection$unordered_key[!(selection$is_explore %in% TRUE)])
  testthat::expect_true(all(remainder_keys %in% candidates$unordered_key))
  testthat::expect_equal(length(intersect(explored_keys, remainder_keys)), 0L)
})

testthat::test_that("safe mode selection does not depend on utilities", {
  samples <- tibble::tibble(
    ID = LETTERS[1:7],
    text = paste0("text-", seq_len(7))
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N, batch_size = 8L, explore_rate = 0.25)

  candidates_a <- make_safe_candidates(state$ids)
  candidates_b <- candidates_a
  candidates_b$utility <- rev(candidates_a$utility)
  candidates_b$utility_raw <- rev(candidates_a$utility_raw)

  withr::local_seed(202)
  selection_a <- pairwiseLLM:::.adaptive_select_safe_no_utility(
    state = state,
    candidates_with_utility = candidates_a,
    config = config,
    seed = 202
  )
  selection_b <- pairwiseLLM:::.adaptive_select_safe_no_utility(
    state = state,
    candidates_with_utility = candidates_b,
    config = config,
    seed = 202
  )

  testthat::expect_identical(selection_a$unordered_key, selection_b$unordered_key)
})

testthat::test_that("safe mode honors duplicate policy and inflight filters", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("text-", seq_len(6))
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N, batch_size = 6L, explore_rate = 0.34)
  candidates <- make_safe_candidates(state$ids)

  inflight_key <- pairwiseLLM:::make_unordered_key("A", "B")
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  inflight_row <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = inflight_key,
    ordered_key = pairwiseLLM:::make_ordered_key("A", "B"),
    A_id = "A",
    B_id = "B",
    A_text = state$texts[["A"]],
    B_text = state$texts[["B"]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, inflight_row)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))

  dup_key <- pairwiseLLM:::make_unordered_key("C", "D")
  state$pair_count[dup_key] <- 2L

  withr::local_seed(303)
  selection <- pairwiseLLM:::.adaptive_select_safe_no_utility(
    state = state,
    candidates_with_utility = candidates,
    config = config,
    seed = 303
  )

  testthat::expect_false(inflight_key %in% selection$unordered_key)
  testthat::expect_false(dup_key %in% selection$unordered_key)
  testthat::expect_true(nrow(selection) > 0L)
})
