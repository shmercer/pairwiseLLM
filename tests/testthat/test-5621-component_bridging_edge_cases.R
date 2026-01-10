# Workstream E: component bridging edge cases

# These tests focus on schema-stable early exits and basic correctness in the
# internal component-bridging candidate generator.

testthat::test_that(".ap_make_component_bridge_pairs returns schema-stable empties", {
  graph_state <- list(
    component_id = stats::setNames(c(1L, 1L, 2L), c("A", "B", "C")),
    degree = stats::setNames(c(1L, 1L, 0L), c("A", "B", "C")),
    ids = c("A", "B", "C")
  )

  # Too few IDs -> empty.
  out_1 <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = graph_state,
    id_vec_theta = "A",
    n_bridge = 5L
  )

  testthat::expect_s3_class(out_1, "tbl_df")
  testthat::expect_identical(
    names(out_1),
    c(
      "i_idx",
      "j_idx",
      "anchor_idx",
      "pair_key",
      "source",
      "embed_hit",
      "embed_rank",
      "directed",
      "score_info",
      "score_need",
      "score_embed",
      "score_total",
      "ID1",
      "ID2",
      "pair_type",
      "reason",
      "component_id_1",
      "component_id_2",
      "target_component_id",
      "target_component_size"
    )
  )
  testthat::expect_equal(nrow(out_1), 0L)

  # n_bridge <= 0 -> empty.
  out_2 <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = graph_state,
    id_vec_theta = c("A", "B", "C"),
    n_bridge = 0L
  )
  testthat::expect_equal(nrow(out_2), 0L)

  # Missing graph state -> empty.
  out_3 <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = NULL,
    id_vec_theta = c("A", "B", "C"),
    n_bridge = 5L
  )
  testthat::expect_equal(nrow(out_3), 0L)
})

testthat::test_that(".ap_make_component_bridge_pairs proposes LCC-to-other bridges when disconnected", {
  withr::local_seed(123)

  graph_state <- list(
    component_id = stats::setNames(c(1L, 1L, 2L), c("A", "B", "C")),
    degree = stats::setNames(c(1L, 1L, 0L), c("A", "B", "C")),
    ids = c("A", "B", "C")
  )

  out <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = graph_state,
    id_vec_theta = c("A", "B", "C"),
    n_bridge = 2L
  )

  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(out$pair_type == "component_bridge"))
  testthat::expect_true(all(out$reason == "graph_disconnected"))
  testthat::expect_true(all(out$component_id_1 == 1L))
  testthat::expect_true(all(out$component_id_2 == 2L))
  testthat::expect_true(all(out$target_component_id == 2L))

  # All proposed bridges must be unordered cross-component pairs.
  testthat::expect_true(all(out$pair_key == pairwiseLLM:::.unordered_pair_key(out$ID1, out$ID2)))
  testthat::expect_true(all(out$ID1 %in% c("A", "B")))
  testthat::expect_true(all(graph_state$component_id[out$ID2] == 2L))
  testthat::expect_true(all(out$ID1 != out$ID2))
})
