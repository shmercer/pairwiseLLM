test_that("controlled random uses stable hash ordering and ignores RNG", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", 1:6)
  )

  theta <- tibble::tibble(
    ID = LETTERS[1:6],
    theta = seq_along(ID),
    se = rep(1, 6)
  )

  # Forbid all theta-neighborhood pairs with k_neighbors=1 so that "normal"
  # candidate generation yields 0 pairs, while other pairs remain feasible.
  existing_pairs <- tibble::tibble(
    ID1 = LETTERS[1:5],
    ID2 = LETTERS[2:6]
  )

  set.seed(1)
  res1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    embedding_neighbors = NULL,
    n_pairs = 2,
    k_neighbors = 1,
    embed_far_k = 0,
    return_internal = TRUE,
    seed = NULL
  )

  set.seed(999)
  res2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    embedding_neighbors = NULL,
    n_pairs = 2,
    k_neighbors = 1,
    embed_far_k = 0,
    return_internal = TRUE,
    seed = NULL
  )

  expect_identical(
    dplyr::select(res1$pairs, ID1, ID2),
    dplyr::select(res2$pairs, ID1, ID2)
  )

  expect_identical(res1$diagnostics$fallback_path, "controlled_random")
  expect_true(nzchar(res1$diagnostics$fallback_trigger))
  expect_identical(res1$diagnostics$n_pairs_source_random, 2L)
})
