testthat::test_that("warm_start min-degree completion uses random eligible partners", {
  ids <- LETTERS[1:10]
  config <- list(min_degree = 4L, target_mean_degree = NULL)

  ring_keys <- pairwiseLLM:::make_unordered_key(ids, c(ids[-1L], ids[[1L]]))

  pairs <- withr::with_seed(101, pairwiseLLM:::warm_start(ids, config = config))
  keys <- pairwiseLLM:::make_unordered_key(pairs$i, pairs$j)

  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  for (idx in seq_len(nrow(pairs))) {
    deg[[pairs$i[[idx]]]] <- deg[[pairs$i[[idx]]]] + 1L
    deg[[pairs$j[[idx]]]] <- deg[[pairs$j[[idx]]]] + 1L
  }

  expect_true(min(deg) >= config$min_degree)
  expect_true(all(pairs$i != pairs$j))
  expect_equal(length(keys), length(unique(keys)))

  completion_keys <- setdiff(keys, ring_keys)
  expect_true(length(completion_keys) > 0L)
})

testthat::test_that("warm_start min-degree completion is deterministic under a fixed seed", {
  ids <- LETTERS[1:10]
  config <- list(min_degree = 4L, target_mean_degree = NULL)
  ring_keys <- pairwiseLLM:::make_unordered_key(ids, c(ids[-1L], ids[[1L]]))

  pairs_a <- withr::with_seed(202, pairwiseLLM:::warm_start(ids, config = config))
  pairs_b <- withr::with_seed(202, pairwiseLLM:::warm_start(ids, config = config))

  completion_a <- sort(setdiff(
    pairwiseLLM:::make_unordered_key(pairs_a$i, pairs_a$j),
    ring_keys
  ))
  completion_b <- sort(setdiff(
    pairwiseLLM:::make_unordered_key(pairs_b$i, pairs_b$j),
    ring_keys
  ))

  expect_identical(completion_a, completion_b)
})

testthat::test_that("warm_start min-degree completion changes with different seeds", {
  ids <- LETTERS[1:10]
  config <- list(min_degree = 4L, target_mean_degree = NULL)
  ring_keys <- pairwiseLLM:::make_unordered_key(ids, c(ids[-1L], ids[[1L]]))

  completion_signature <- function(seed) {
    pairs <- withr::with_seed(seed, pairwiseLLM:::warm_start(ids, config = config))
    completion <- sort(setdiff(
      pairwiseLLM:::make_unordered_key(pairs$i, pairs$j),
      ring_keys
    ))
    paste(completion, collapse = "|")
  }

  signatures <- vapply(c(301, 302, 303), completion_signature, character(1L))
  expect_true(length(unique(signatures)) >= 2L)
})

testthat::test_that("warm_start returns ring edges exactly when min_degree == 2", {
  ids <- LETTERS[1:10]
  config <- list(min_degree = 2L, target_mean_degree = NULL)

  expected_ring <- sort(pairwiseLLM:::make_unordered_key(ids, c(ids[-1L], ids[[1L]])))
  pairs <- withr::with_seed(404, pairwiseLLM:::warm_start(ids, config = config))
  keys <- sort(pairwiseLLM:::make_unordered_key(pairs$i, pairs$j))

  expect_identical(keys, expected_ring)
})
