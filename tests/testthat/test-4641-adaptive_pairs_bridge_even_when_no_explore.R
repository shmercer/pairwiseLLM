test_that("select_adaptive_pairs bridges components even when explore_frac = 0", {
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

  gs <- pairwiseLLM:::.graph_state_from_pairs(existing_pairs, ids = samples$ID)

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta_tbl,
    existing_pairs = existing_pairs,
    graph_state = gs,
    n_pairs = 2L,
    explore_frac = 0,
    repeat_policy = "none",
    k_neighbors = 1L,
    return_internal = TRUE,
    seed = 123
  )

  selected <- res$candidates$selected
  bridges <- dplyr::filter(selected, .data$pair_type == "component_bridge")
  expect_equal(nrow(bridges), 1L)

  # Deterministic best bridge is A-C given theta ordering.
  expect_equal(bridges$ID1, "A")
  expect_equal(bridges$ID2, "C")

  # Bridge endpoints are in different components.
  expect_false(bridges$component_id_1 == bridges$component_id_2)
})
