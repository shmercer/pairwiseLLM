test_that("bridge improves connectivity when possible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0, 0.01, 3, 3.01),
    se = rep(1, 4)
  )

  # Two disconnected components: (A-B) and (C-D)
  existing <- tibble::tibble(
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )

  embedding_neighbors <- list(
    A = c("C", "D"),
    B = c("C", "D"),
    C = c("A", "B"),
    D = c("A", "B")
  )

  before_state <- pairwiseLLM:::.graph_state_from_pairs(existing, ids = samples$ID)
  before_frac <- before_state$metrics$largest_component_frac[[1]]

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 2,
    k_neighbors = 2,
    embedding_neighbors = embedding_neighbors,
    forbid_repeats = TRUE,
    seed = 1,
    balance_positions = FALSE,
    return_internal = TRUE
  )

  after_pairs <- dplyr::bind_rows(
    existing,
    dplyr::select(res$pairs, "ID1", "ID2")
  )
  after_state <- pairwiseLLM:::.graph_state_from_pairs(after_pairs, ids = samples$ID)
  after_frac <- after_state$metrics$largest_component_frac[[1]]

  expect_true(any(res$candidates$selected$source == "bridge"))
  expect_true(after_frac > before_frac)
})
