testthat::test_that(".bridge_edges_from_pairs is schema-stable and handles empty graphs", {
  ids <- c("A", "B", "C")

  edges_null <- pairwiseLLM:::.bridge_edges_from_pairs(pairs = NULL, ids = ids)
  testthat::expect_s3_class(edges_null, "tbl_df")
  testthat::expect_identical(names(edges_null), c("ID1", "ID2", "is_bridge"))
  testthat::expect_identical(nrow(edges_null), 0L)
  testthat::expect_true(is.character(edges_null$ID1))
  testthat::expect_true(is.character(edges_null$ID2))
  testthat::expect_true(is.logical(edges_null$is_bridge))

  m_null <- pairwiseLLM:::.graph_bottleneck_metrics_from_pairs(pairs = NULL, ids = ids)
  testthat::expect_s3_class(m_null, "tbl_df")
  testthat::expect_identical(names(m_null), c("bridge_edge_count", "bridge_edge_frac"))
  testthat::expect_equal(m_null$bridge_edge_count, 0L)
  testthat::expect_equal(m_null$bridge_edge_frac, 0)

  # Empty data.frame should behave the same.
  edges_empty <- pairwiseLLM:::.bridge_edges_from_pairs(
    pairs = tibble::tibble(ID1 = character(0), ID2 = character(0)),
    ids = ids
  )
  testthat::expect_identical(nrow(edges_empty), 0L)
  m_empty <- pairwiseLLM:::.graph_bottleneck_metrics_from_pairs(
    pairs = tibble::tibble(ID1 = character(0), ID2 = character(0)),
    ids = ids
  )
  testthat::expect_equal(m_empty$bridge_edge_count, 0L)
  testthat::expect_equal(m_empty$bridge_edge_frac, 0)
})

testthat::test_that("bridge-edge metrics match known small graphs", {
  # Path graph: all edges are bridges
  ids <- c("A", "B", "C", "D")
  pairs_path <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("B", "C", "D")
  )

  edges_path <- pairwiseLLM:::.bridge_edges_from_pairs(pairs_path, ids = ids)
  testthat::expect_equal(nrow(edges_path), 3L)
  testthat::expect_true(all(edges_path$is_bridge))
  m_path <- pairwiseLLM:::.graph_bottleneck_metrics_from_pairs(pairs_path, ids = ids)
  testthat::expect_equal(m_path$bridge_edge_count, 3L)
  testthat::expect_equal(m_path$bridge_edge_frac, 1)

  # Cycle graph: no bridges
  pairs_cycle <- tibble::tibble(
    ID1 = c("A", "B", "C", "D"),
    ID2 = c("B", "C", "D", "A")
  )
  edges_cycle <- pairwiseLLM:::.bridge_edges_from_pairs(pairs_cycle, ids = ids)
  testthat::expect_equal(nrow(edges_cycle), 4L)
  testthat::expect_false(any(edges_cycle$is_bridge))
  m_cycle <- pairwiseLLM:::.graph_bottleneck_metrics_from_pairs(pairs_cycle, ids = ids)
  testthat::expect_equal(m_cycle$bridge_edge_count, 0L)
  testthat::expect_equal(m_cycle$bridge_edge_frac, 0)

  # Barbell-like: two triangles connected by a single bridge
  ids2 <- c("A", "B", "C", "D", "E", "F")
  pairs_barbell <- tibble::tibble(
    ID1 = c(
      "A", "A", "B", # triangle 1
      "D", "D", "E", # triangle 2
      "C"              # bridge
    ),
    ID2 = c(
      "B", "C", "C",
      "E", "F", "F",
      "D"
    )
  )

  edges_barbell <- pairwiseLLM:::.bridge_edges_from_pairs(pairs_barbell, ids = ids2)
  testthat::expect_equal(nrow(edges_barbell), 7L)
  testthat::expect_equal(sum(edges_barbell$is_bridge), 1L)
  testthat::expect_true(any(edges_barbell$is_bridge))
  m_barbell <- pairwiseLLM:::.graph_bottleneck_metrics_from_pairs(pairs_barbell, ids = ids2)
  testthat::expect_equal(m_barbell$bridge_edge_count, 1L)
  testthat::expect_equal(m_barbell$bridge_edge_frac, 1 / 7)

  # Integration: .graph_state_from_pairs includes the new columns
  gs <- pairwiseLLM:::.graph_state_from_pairs(pairs_barbell, ids = ids2)
  testthat::expect_true(all(c("bridge_edge_count", "bridge_edge_frac") %in% names(gs$metrics)))
  testthat::expect_equal(gs$metrics$bridge_edge_count, 1L)
  testthat::expect_equal(gs$metrics$bridge_edge_frac, 1 / 7)
})
