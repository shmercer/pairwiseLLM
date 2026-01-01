testthat::test_that("select_adaptive_pairs validates embedding_neighbors and embed_far_k", {
  samples <- tibble::tibble(ID = LETTERS[1:4], text = letters[1:4])
  theta <- tibble::tibble(ID = LETTERS[1:4], theta = seq_len(4), se = rep(1, 4))

  # embed_far_k validation
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embed_far_k = NA),
    "embed_far_k"
  )
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embed_far_k = -1),
    "embed_far_k"
  )
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embed_far_k = "2"),
    "embed_far_k"
  )

  # embedding_neighbors validation: wrong type
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embedding_neighbors = 1),
    "embedding_neighbors"
  )

  # embedding_neighbors validation: named list missing IDs
  bad_nbrs <- list(A = c("B", "C"), B = c("A", "C"))
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embedding_neighbors = bad_nbrs),
    "missing IDs"
  )

  # embedding_neighbors as matrix: requires rownames
  m <- matrix(c("B", "C", "A", "C", "A", "B", "A", "B"), nrow = 4, byrow = TRUE)
  testthat::expect_error(
    pairwiseLLM::select_adaptive_pairs(samples, theta, n_pairs = 2, embedding_neighbors = m),
    "rownames"
  )

  # Valid embedding_neighbors matrix with rownames should run
  rownames(m) <- samples$ID
  out <- pairwiseLLM::select_adaptive_pairs(
    samples, theta,
    n_pairs = 2,
    embedding_neighbors = m,
    embed_far_k = 0,
    seed = 123
  )
  testthat::expect_true(is.data.frame(out))
  testthat::expect_true(all(c("ID1", "text1", "ID2", "text2") %in% names(out)))
  testthat::expect_equal(nrow(out), 2)
})
