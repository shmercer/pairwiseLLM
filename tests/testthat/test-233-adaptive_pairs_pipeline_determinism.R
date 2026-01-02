test_that("select_adaptive_pairs is deterministic given seed (pipeline refactor)", {
  samples <- tibble::tibble(
    ID = sprintf("S%02d", 1:20),
    text = paste("text", sprintf("S%02d", 1:20))
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = seq(-1, 1, length.out = nrow(samples)),
    se = rep(0.2, nrow(samples))
  )

  # Same seed => identical output (required invariant)
  out1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 101
  )

  out2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 101
  )

  expect_identical(out1, out2)

  # Different seed: output should still be valid. We do NOT require it to differ
  # because the algorithm may be fully deterministic for some configurations.
  out3 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 202
  )

  expect_true(is.data.frame(out3))
  expect_named(out3, c("ID1", "text1", "ID2", "text2"))
  expect_true(all(out3$ID1 %in% samples$ID))
  expect_true(all(out3$ID2 %in% samples$ID))
  expect_true(all(out3$ID1 != out3$ID2))

  # RNG state restoration: calling with seed should not permanently change RNG
  set.seed(999)
  a <- stats::runif(5)
  b <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 303
  )
  c <- stats::runif(5)

  set.seed(999)
  a_ref <- stats::runif(5)
  c_ref <- stats::runif(5)

  expect_identical(a, a_ref)
  expect_identical(c, c_ref)
})

test_that("select_adaptive_pairs typically differs across seeds when scores are fully tied (optional)", {
  samples <- tibble::tibble(
    ID = sprintf("S%02d", 1:20),
    text = paste("text", sprintf("S%02d", 1:20))
  )

  # Force full score ties:
  # - all theta equal => p = 0.5, info constant for all candidates
  # - all se equal => (se_i + se_j) constant
  # - min_judgments = 0 => need constant (0)
  theta_tie <- tibble::tibble(
    ID = samples$ID,
    theta = rep(0, nrow(samples)),
    se = rep(0.2, nrow(samples))
  )

  out1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta_tie,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 101
  )

  out2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta_tie,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 12,
    k_neighbors = 5,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    embed_far_k = 2,
    seed = 202
  )

  # Under full ties, the far-candidate sampling (embed_far_k > 0) and/or
  # orientation tie-breaking should vary with seed. This should *typically*
  # produce different outputs; if it doesn't, it suggests seed is not being used
  # (or the algorithm has become fully deterministic in an unexpected way).
  expect_false(identical(out1, out2))
})
