testthat::test_that("repair mode uses exploration-only selection", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$explore_rate <- 0
  config$batch_size <- 2L

  state$deg[["A"]] <- 10L
  state$deg[["B"]] <- 10L
  state$deg[["C"]] <- 0L
  state$pos1[["A"]] <- 10L
  state$pos2[["A"]] <- 0L
  state$pos1[["B"]] <- 10L
  state$pos2[["B"]] <- 0L
  state$pos1[["C"]] <- 0L
  state$pos2[["C"]] <- 0L
  state$imb <- state$pos1 - state$pos2

  candidates <- tibble::tibble(
    i_id = c("A", "A", "B"),
    j_id = c("B", "C", "C"),
    unordered_key = c("A:B", "A:C", "B:C"),
    utility = c(0.9, 0.4, 0.3),
    utility_raw = c(0.9, 0.4, 0.3),
    p_mean = c(0.5, 0.5, 0.5)
  )

  expected <- pairwiseLLM:::.pairwiseLLM_with_seed(2, function() {
    pairwiseLLM:::sample_exploration_pairs(
      state = state,
      candidates = candidates,
      n_explore = config$batch_size,
      config = config
    )
  })

  out <- pairwiseLLM:::select_batch(
    state = state,
    candidates_with_utility = candidates,
    config = config,
    seed = 2,
    exploration_only = TRUE
  )

  expect_equal(nrow(out), config$batch_size)
  expect_equal(sort(out$unordered_key), sort(expected$unordered_key))
})
