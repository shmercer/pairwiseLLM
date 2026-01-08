testthat::test_that(".graph_state_from_pairs validates inputs", {
  ids <- c("A", "B")

  testthat::expect_error(
    pairwiseLLM:::.graph_state_from_pairs(ids = character(), pairs = NULL),
    "must contain at least one non-missing ID"
  )

  pairs_missing_cols <- tibble::tibble(ID1 = "A")
  testthat::expect_error(
    pairwiseLLM:::.graph_state_from_pairs(ids = ids, pairs = pairs_missing_cols),
    "must contain columns"
  )
})

testthat::test_that(".graph_state_from_pairs handles empty / invalid pairs", {
  ids <- c("A", "B", "C")

  out_null <- pairwiseLLM:::.graph_state_from_pairs(ids = ids, pairs = NULL)
  testthat::expect_s3_class(out_null$metrics, "tbl_df")
  testthat::expect_equal(out_null$metrics$n_edges, 0L)
  testthat::expect_equal(out_null$metrics$n_components, 3L)
  testthat::expect_equal(out_null$metrics$largest_component_frac, 1 / 3)
  testthat::expect_equal(out_null$metrics$degree_max, 0)
  testthat::expect_equal(out_null$metrics$pct_nodes_with_degree_gt0, 0)

  # degree/component_id are named vectors with one entry per id
  testthat::expect_named(out_null$degree, ids)
  testthat::expect_named(out_null$component_id, ids)

  pairs_invalid <- tibble::tibble(
    ID1 = c("A", NA, "", "A", "A"),
    ID2 = c("A", "B", "B", "", "D")
  )

  out_invalid <- pairwiseLLM:::.graph_state_from_pairs(ids = ids, pairs = pairs_invalid)
  testthat::expect_equal(out_invalid$metrics$n_edges, 0L)
  testthat::expect_equal(out_invalid$metrics$n_components, 3L)
  testthat::expect_equal(out_invalid$metrics$degree_min, 0)
  testthat::expect_equal(out_invalid$metrics$degree_median, 0)
  testthat::expect_equal(out_invalid$metrics$pct_nodes_with_degree_gt0, 0)
})

testthat::test_that(".graph_state_from_pairs deduplicates edges and computes components", {
  ids <- c("A", "B", "C", "D")
  pairs <- tibble::tibble(
    ID1 = c("A", "B", "B", "C", "A", "A", "C", "A"),
    ID2 = c("B", "A", "C", "B", "B", "D", "C", "X")
  )

  out <- pairwiseLLM:::.graph_state_from_pairs(ids = ids, pairs = pairs)

  testthat::expect_equal(out$metrics$n_edges, 3L)
  testthat::expect_equal(out$metrics$n_components, 1L)
  testthat::expect_equal(out$metrics$largest_component_frac, 1)

  testthat::expect_equal(out$metrics$degree_min, 1)
  testthat::expect_equal(out$metrics$degree_median, 1.5)
  testthat::expect_equal(out$metrics$degree_max, 2)
  testthat::expect_equal(out$metrics$pct_nodes_with_degree_gt0, 1)

  # degree + component_id are complete and named
  testthat::expect_named(out$degree, ids)
  testthat::expect_named(out$component_id, ids)

  # All nodes in the same component
  testthat::expect_equal(length(unique(unname(out$component_id))), 1L)
})
