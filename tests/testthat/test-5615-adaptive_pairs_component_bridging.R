test_that("disconnected graphs allocate explicit component-bridge pairs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("txt_", ID)
  )

  theta_tbl <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(-2, -1, 1, 2),
    se = rep(0.5, 4)
  )

  # Two disconnected components: {A,B} and {C,D}.
  existing_pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )

  res <- pairwiseLLM:::select_adaptive_pairs(
    samples = samples,
    theta = theta_tbl,
    existing_pairs = existing_pairs,
    n_pairs = 2L,
    explore_frac = 0.1,
    repeat_policy = "none",
    k_neighbors = 1L,
    return_internal = TRUE
  )

  selected <- res$candidates$selected
  bridges <- dplyr::filter(selected, .data$pair_type == "component_bridge")
  expect_equal(nrow(bridges), 1L)

  # Deterministic best bridge is A-C given theta ordering.
  expect_equal(bridges$ID1, "A")
  expect_equal(bridges$ID2, "C")

  # Bridge endpoints are in different components.
  gs <- pairwiseLLM:::.graph_state_from_pairs(existing_pairs, ids = unique(samples$ID))
  comp <- gs$component_id
  expect_false(comp[[bridges$ID1]] == comp[[bridges$ID2]])
})
