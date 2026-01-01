test_that("select_adaptive_pairs uses embedding neighbor candidates when provided", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("Sample", c("A", "B", "C", "D"))
  )

  # Nearly tied items, but make A and D high-uncertainty so their pair should be prioritized
  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0.00, 0.01, 0.02, 0.03),
    se = c(1.0, 0.01, 0.01, 1.0)
  )

  # Without embedding neighbors, only adjacent-in-theta pairs are candidates
  out_theta <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    k_neighbors = 1,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = FALSE,
    seed = 1
  )
  expect_true(nrow(out_theta) == 1)
  expect_true(paste(out_theta$ID1, out_theta$ID2) %in% c("A B", "C D"))

  # With embedding neighbors, allow non-adjacent (A,D) which should be top-scoring
  emb_nbrs <- list(A = c("D"), B = c("C"), C = c("B"), D = c("A"))
  out_emb <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    k_neighbors = 1,
    min_judgments = 0,
    forbid_repeats = TRUE,
    balance_positions = FALSE,
    embedding_neighbors = emb_nbrs,
    embed_far_k = 0,
    seed = 1
  )
  expect_true(nrow(out_emb) == 1)
  expect_equal(paste(out_emb$ID1, out_emb$ID2), "A D")
})
