testthat::test_that("warm_start produces a connected ring backbone", {
  withr::local_seed(101)
  is_connected <- function(ids, pairs) {
    ids <- as.character(ids)
    neighbors <- stats::setNames(vector("list", length(ids)), ids)
    for (idx in seq_len(nrow(pairs))) {
      i_id <- pairs$i[[idx]]
      j_id <- pairs$j[[idx]]
      neighbors[[i_id]] <- c(neighbors[[i_id]], j_id)
      neighbors[[j_id]] <- c(neighbors[[j_id]], i_id)
    }
    seen <- stats::setNames(rep(FALSE, length(ids)), ids)
    queue <- ids[[1L]]
    seen[[queue]] <- TRUE
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      next_nodes <- neighbors[[current]]
      next_nodes <- next_nodes[!seen[next_nodes]]
      if (length(next_nodes) > 0L) {
        seen[next_nodes] <- TRUE
        queue <- c(queue, next_nodes)
      }
    }
    all(seen)
  }

  for (n_items in c(6L, 10L, 12L)) {
    ids <- as.character(seq_len(n_items))
    pairs <- pairwiseLLM:::warm_start(ids, config = list())
    expect_true(is_connected(ids, pairs))
  }
})

testthat::test_that("warm_start enforces minimum degree, uniqueness, and determinism", {
  withr::local_seed(202)
  ids <- as.character(seq_len(10L))
  pairs <- pairwiseLLM:::warm_start(ids, config = list())
  deg <- stats::setNames(rep.int(0L, length(ids)), ids)

  for (idx in seq_len(nrow(pairs))) {
    deg[[pairs$i[[idx]]]] <- deg[[pairs$i[[idx]]]] + 1L
    deg[[pairs$j[[idx]]]] <- deg[[pairs$j[[idx]]]] + 1L
  }

  expect_true(min(deg) >= 2L)
  expect_true(all(pairs$i != pairs$j))
  keys <- paste(pairs$i, pairs$j, sep = ":")
  expect_equal(length(keys), length(unique(keys)))

  pairs_min3 <- pairwiseLLM:::warm_start(ids, config = list(min_degree = 3L))
  deg_min3 <- stats::setNames(rep.int(0L, length(ids)), ids)
  for (idx in seq_len(nrow(pairs_min3))) {
    deg_min3[[pairs_min3$i[[idx]]]] <- deg_min3[[pairs_min3$i[[idx]]]] + 1L
    deg_min3[[pairs_min3$j[[idx]]]] <- deg_min3[[pairs_min3$j[[idx]]]] + 1L
  }
  expect_true(min(deg_min3) >= 3L)

  pairs_a <- pairwiseLLM:::warm_start(ids, config = list())
  pairs_b <- pairwiseLLM:::warm_start(ids, config = list())
  expect_identical(pairs_a, pairs_b)
})

testthat::test_that("warm_start completes higher min_degree and target mean", {
  ids <- c("A", "B", "C", "D")
  config <- list(min_degree = 3L, target_mean_degree = 3)

  pairs <- pairwiseLLM:::warm_start(ids, config = config, seed = 42)
  keys <- paste(pairs$i, pairs$j, sep = ":")

  expect_true(all(pairs$i != pairs$j))
  expect_equal(length(keys), length(unique(keys)))
  expect_true(nrow(pairs) >= 4L)
})
