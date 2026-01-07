test_that("bridge respects constraints (directed/repeat caps) when graph is unhealthy", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E"),
    text = paste0("t", c("A", "B", "C", "D", "E"))
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E"),
    theta = c(0, 0.1, 1, 1.1, 4),
    se = rep(1, 5)
  )

  # Completed/observed edges: (A-B), (A-C), (C-D). E is isolated.
  # This yields an unhealthy graph (min degree 0), which should trigger bridge/repair.
  existing <- tibble::tibble(
    ID1 = c("A", "A", "C"),
    ID2 = c("B", "C", "D"),
    better_id = c("A", "A", "C")
  )

  # Embedding neighbors propose a repeated edge (A-C) first; bridge must still
  # respect repeat forbids and choose a non-repeat cross-component edge.
  embedding_neighbors <- list(
    A = c("C", "E", "B", "D"),
    B = c("E", "A", "C", "D"),
    C = c("A", "E", "D", "B"),
    D = c("E", "C", "A", "B"),
    E = c("A", "C", "B", "D")
  )

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 3,
    k_neighbors = 2,
    embedding_neighbors = embedding_neighbors,
    repeat_policy = "reverse_only",
    forbid_repeats = TRUE,
    seed = 1,
    balance_positions = FALSE,
    return_internal = TRUE
  )

  expect_true(any(res$candidates$selected$source == "bridge"))
  expect_equal(res$diagnostics$fallback_path[[1]], "bridge_repair")

  # Ensure no selected unordered pair is a repeat from existing_pairs.
  selected_keys <- paste0(
    pmin(res$pairs$ID1, res$pairs$ID2),
    "-",
    pmax(res$pairs$ID1, res$pairs$ID2)
  )
  existing_keys <- paste0(
    pmin(existing$ID1, existing$ID2),
    "-",
    pmax(existing$ID1, existing$ID2)
  )

  expect_true(all(!selected_keys %in% existing_keys))
})
