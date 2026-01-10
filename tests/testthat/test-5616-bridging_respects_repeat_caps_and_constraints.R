test_that("component bridging respects forbidden pairs and falls back deterministically", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("txt_", ID)
  )

  theta_tbl <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(-2, -1, 1, 2),
    se = rep(0.5, 4)
  )

  # Base disconnected components in the graph state.
  base_pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )
  graph_state <- pairwiseLLM:::.graph_state_from_pairs(base_pairs, ids = unique(samples$ID))

  # Provide an additional (forbidden) cross-component pair in existing_pairs,
  # but keep graph_state disconnected via the explicit graph_state override.
  existing_pairs <- dplyr::bind_rows(
    base_pairs,
    tibble::tibble(ID1 = "A", ID2 = "C")
  )

  res <- pairwiseLLM:::select_adaptive_pairs(
    samples = samples,
    theta_tbl = theta_tbl,
    existing_pairs = existing_pairs,
    graph_state = graph_state,
    n_pairs = 1L,
    explore_frac = 0.1,
    repeat_policy = "none",
    k_neighbors = 1L,
    return_internal = TRUE
  )

  selected <- res$candidates$selected
  bridges <- dplyr::filter(selected, .data$pair_type == "component_bridge")
  expect_equal(nrow(bridges), 1L)

  # Should not repeat any existing unordered edge.
  forbidden_keys <- unique(pairwiseLLM:::.unordered_pair_key(existing_pairs$ID1, existing_pairs$ID2))
  expect_false(bridges$pair_key %in% forbidden_keys)

  # Still connects components under the (disconnected) graph_state used.
  comp <- graph_state$component_id
  expect_false(comp[[bridges$ID1]] == comp[[bridges$ID2]])
})
