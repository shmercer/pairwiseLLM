test_that("generate_candidate_pairs uses rank-local windowed generation", {
  items <- make_test_items(4)
  rank_mu <- sort(items$item_id)

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 3,
    C_max = 20000L,
    seed = 1L
  )

  candidates <- dplyr::arrange(candidates, .data$i, .data$j)
  expected <- tibble::tibble(i = c(1, 2, 3), j = c(2, 3, 4))

  expect_equal(nrow(candidates), 3L)
  expect_equal(candidates, expected)
  expect_true(all(candidates$i < candidates$j))
  expect_equal(anyDuplicated(pairwiseLLM:::make_unordered_key(candidates$i, candidates$j)), 0L)
})

test_that("generate_candidate_pairs caps and is deterministic by seed", {
  items <- make_test_items(60)
  rank_mu <- items$item_id

  cand1 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 123L
  )
  cand2 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 123L
  )
  cand3 <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 9999,
    C_max = 100L,
    seed = 124L
  )

  expect_equal(nrow(cand1), 100L)
  expect_equal(cand1, cand2)
  expect_false(identical(cand1, cand3))

  expect_true(all(cand1$i < cand1$j))
  expect_equal(anyDuplicated(pairwiseLLM:::make_unordered_key(cand1$i, cand1$j)), 0L)
})

test_that("generate_candidate_pairs accepts even W_used values", {
  items <- make_test_items(5)
  rank_mu <- items$item_id

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 4L,
    C_max = 20000L,
    seed = 1L
  )

  expect_true(nrow(candidates) > 0L)
})

test_that("generate_candidate_pairs allows N=2 with W_used=1", {
  items <- make_test_items(2)
  rank_mu <- items$item_id

  candidates <- pairwiseLLM:::generate_candidate_pairs(
    rank_mu = rank_mu,
    W_used = 1L,
    C_max = 20000L,
    seed = 1L
  )

  expect_equal(nrow(candidates), 1L)
  expect_equal(candidates$i, 1L)
  expect_equal(candidates$j, 2L)
})

test_that("generate_candidate_pairs validates inputs", {
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = 1L,
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "at least two"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c(1L, 2L, 3L),
      W_used = 1L,
      C_max = 20000L,
      seed = 1L
    ),
    ">= 2"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c(1L, 1L, 2L),
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "unique"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = integer(),
      W_used = 3L,
      C_max = 20000L,
      seed = 1L
    ),
    "non-empty"
  )
})
