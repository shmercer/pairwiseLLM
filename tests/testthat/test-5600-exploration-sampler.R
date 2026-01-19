testthat::test_that("exploration sampler favors low-degree items and avoids duplicates", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  state$deg[["A"]] <- 10L
  state$deg[["B"]] <- 8L
  state$deg[["C"]] <- 1L
  state$deg[["D"]] <- 1L
  state$pos1 <- state$deg
  state$pos2 <- stats::setNames(rep.int(0L, length(state$ids)), state$ids)
  state$imb <- state$pos1 - state$pos2
  state$pos_count <- state$pos1

  combos <- utils::combn(state$ids, 2)
  candidates <- tibble::tibble(
    i_id = combos[1L, ],
    j_id = combos[2L, ]
  )
  candidates$unordered_key <- pairwiseLLM:::make_unordered_key(candidates$i_id, candidates$j_id)
  candidates$utility <- 1
  candidates$utility_raw <- 1
  candidates$p_mean <- 0.5

  withr::local_seed(1)
  counts <- stats::setNames(rep.int(0L, length(state$ids)), state$ids)
  for (idx in seq_len(200L)) {
    picks <- pairwiseLLM:::sample_exploration_pairs(
      state = state,
      candidates = candidates,
      n_explore = 2L,
      config = config
    )
    used <- c(picks$i_id, picks$j_id)
    counts[used] <- counts[used] + 1L
  }

  expect_gt(mean(counts[c("C", "D")]), mean(counts[c("A", "B")]))

  withr::local_seed(2)
  batch <- pairwiseLLM:::sample_exploration_pairs(
    state = state,
    candidates = candidates,
    n_explore = 3L,
    config = config
  )
  expect_true(all(batch$i_id != batch$j_id))
  expect_equal(anyDuplicated(batch$unordered_key), 0L)
})
