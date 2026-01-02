testthat::test_that("select_adaptive_pairs supports embedding_neighbors as a matrix and validates embedding inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(0, 0.1, 0.2, 10),
    se = c(10, 1, 10, 1)
  )

  # Theta-neighborhood candidates with k_neighbors = 1 would be only adjacent
  # pairs. We add an embedding-based neighbor A<->C so A-C becomes a candidate
  # even though it is not adjacent when k_neighbors is small.
  emb_mat <- matrix(
    c(
      "C", NA,
      "A", NA,
      NA, NA,
      NA, NA
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(samples$ID, c("nbr1", "nbr2"))
  )

  out <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    embedding_neighbors = emb_mat,
    n_pairs = 1,
    k_neighbors = 1,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 1
  )

  key <- paste0(pmin(out$ID1, out$ID2), "-", pmax(out$ID1, out$ID2))
  testthat::expect_identical(key, "A-C")

  emb_no_rownames <- matrix("A", nrow = 1)
  testthat::expect_error(
    select_adaptive_pairs(samples, theta, embedding_neighbors = emb_no_rownames, n_pairs = 1),
    "rownames"
  )

  emb_list_missing_ids <- list(A = c("B"), B = c("A"))
  testthat::expect_error(
    select_adaptive_pairs(samples, theta, embedding_neighbors = emb_list_missing_ids, n_pairs = 1),
    "missing IDs"
  )
})


testthat::test_that(".compute_embedding_neighbors handles edge cases (k=0, normalize=FALSE)", {
  ids <- c("A", "B", "C")
  emb <- matrix(
    c(
      1, 0,
      0, 1,
      1, 1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(ids, c("x", "y"))
  )

  out0 <- pairwiseLLM:::.compute_embedding_neighbors(embeddings = emb, ids = ids, k = 0)
  testthat::expect_true(is.list(out0))
  testthat::expect_identical(names(out0), ids)
  testthat::expect_true(all(vapply(out0, length, integer(1)) == 0L))

  out1 <- pairwiseLLM:::.compute_embedding_neighbors(embeddings = emb, ids = ids, k = 1, normalize = FALSE)
  testthat::expect_true(all(vapply(out1, length, integer(1)) == 1L))
  # Should never include self as neighbor
  testthat::expect_true(all(mapply(function(id, nbr) !(id %in% nbr), ids, out1)))
})
