testthat::test_that("warm_start respects target mean degree", {
  ids <- as.character(seq_len(8L))
  config <- list(min_degree = 2L, target_mean_degree = 3.5)

  pairs_a <- pairwiseLLM:::warm_start(ids, config = config, seed = 303)
  pairs_b <- pairwiseLLM:::warm_start(ids, config = config, seed = 303)
  expect_identical(pairs_a, pairs_b)

  pairs <- pairs_a
  mean_degree <- 2 * nrow(pairs) / length(ids)
  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  for (idx in seq_len(nrow(pairs))) {
    deg[[pairs$i[[idx]]]] <- deg[[pairs$i[[idx]]]] + 1L
    deg[[pairs$j[[idx]]]] <- deg[[pairs$j[[idx]]]] + 1L
  }

  expect_true(mean_degree <= config$target_mean_degree)
  expect_true(min(deg) >= config$min_degree)
  expect_true(all(pairs$i != pairs$j))
  keys <- paste(pairs$i, pairs$j, sep = ":")
  expect_equal(length(keys), length(unique(keys)))
})
