test_that("graph_state_from_pairs drops invalid edges", {
  ids <- c("A", "B")

  # Self-pairs + NA endpoints => no valid edges
  pairs <- tibble::tibble(ID1 = c("A", NA_character_), ID2 = c("A", "B"))
  gs <- pairwiseLLM:::.graph_state_from_pairs(pairs, ids = ids)

  expect_true(is.list(gs))
  expect_equal(gs$metrics$n_edges[[1]], 0L)
  expect_equal(unname(as.integer(gs$degree)), c(0L, 0L))
  expect_equal(gs$metrics$n_components[[1]], 2L)
})

test_that("component bridge helper returns stable schema", {
  ids <- c("A", "B", "C", "D")
  pairs <- tibble::tibble(ID1 = c("A", "C"), ID2 = c("B", "D"))
  gs <- pairwiseLLM:::.graph_state_from_pairs(pairs, ids = ids)

  b0 <- pairwiseLLM:::.ap_make_component_bridge_pairs(graph_state = gs, id_vec_theta = ids, n_bridge = 0L)
  expect_equal(nrow(b0), 0L)
  expect_true(all(c("ID1", "ID2", "pair_type") %in% names(b0)))

  b1 <- pairwiseLLM:::.ap_make_component_bridge_pairs(graph_state = gs, id_vec_theta = ids, n_bridge = 2L)
  expect_true(nrow(b1) >= 1L)
  expect_true(all(c("ID1", "ID2", "pair_type", "target_component_id") %in% names(b1)))
})
