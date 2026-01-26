testthat::test_that("build_candidate_pairs filters duplicates and window pairs", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "beta", "gamma", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$pair_count[["B:C"]] <- 2L

  ranking_ids <- state$ids
  out <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0
  )
  expect_equal(out$unordered_key, c("A:B", "C:D"))
})

testthat::test_that("build_candidate_pairs exploration is deterministic", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  ranking_ids <- state$ids

  out1 <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0.5,
    seed = 11
  )
  out2 <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0.5,
    seed = 11
  )

  expect_equal(out1, out2)
  expect_equal(length(unique(out1$unordered_key)), nrow(out1))
})

testthat::test_that("build_candidate_pairs handles full windows and validation", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "beta", "gamma", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  ranking_ids <- state$ids

  out <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 10,
    state = state,
    exploration_frac = 0.5,
    seed = 2
  )
  expect_equal(nrow(out), choose(length(state$ids), 2))

  expect_error(
    pairwiseLLM:::build_candidate_pairs("A", W = 1, state = state),
    "at least two ids"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "A", "B"), W = 1, state = state),
    "duplicates"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "B", "D"), W = 1, state = state),
    "match `state\\$ids`"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "B", "C", "D"), W = 0, state = state),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(
      ranking_ids,
      W = 1,
      state = state,
      exploration_frac = 2
    ),
    "between 0 and 1"
  )
})
