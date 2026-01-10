testthat::test_that(".bt_round_state includes connectivity diagnostics when ids are provided", {
  ids <- c("A", "B", "C")

  out_empty <- pairwiseLLM:::.bt_round_state(
    results = fixture_empty_results(),
    ids = ids
  )

  testthat::expect_true(all(c(
    "n_components",
    "largest_component_frac",
    "degree_min",
    "degree_median",
    "pct_nodes_with_degree_gt0"
  ) %in% names(out_empty)))

  testthat::expect_equal(out_empty$n_components[[1]], 3L)
  testthat::expect_equal(out_empty$largest_component_frac[[1]], 1 / 3)
  testthat::expect_equal(out_empty$degree_min[[1]], 0)
  testthat::expect_equal(out_empty$degree_median[[1]], 0)
  testthat::expect_equal(out_empty$pct_nodes_with_degree_gt0[[1]], 0)

  one_edge <- fixture_results_from_edges(
    tibble::tibble(ID1 = "A", ID2 = "B")
  )

  out_one <- pairwiseLLM:::.bt_round_state(results = one_edge, ids = ids)

  testthat::expect_equal(out_one$n_components[[1]], 2L)
  testthat::expect_equal(out_one$largest_component_frac[[1]], 2 / 3)
  testthat::expect_equal(out_one$degree_min[[1]], 0)
  testthat::expect_equal(out_one$degree_median[[1]], 1)
  testthat::expect_equal(out_one$pct_nodes_with_degree_gt0[[1]], 2 / 3)
})

testthat::test_that(".bt_round_state returns NA connectivity diagnostics when ids are NULL", {
  res <- fixture_results_from_edges(
    tibble::tibble(ID1 = c("A", "B"), ID2 = c("B", "C"))
  )

  out <- pairwiseLLM:::.bt_round_state(results = res, ids = NULL)

  testthat::expect_true(all(c(
    "n_components",
    "largest_component_frac",
    "degree_min",
    "degree_median",
    "pct_nodes_with_degree_gt0"
  ) %in% names(out)))

  testthat::expect_true(is.na(out$n_components[[1]]))
  testthat::expect_true(is.na(out$largest_component_frac[[1]]))
  testthat::expect_true(is.na(out$degree_min[[1]]))
  testthat::expect_true(is.na(out$degree_median[[1]]))
  testthat::expect_true(is.na(out$pct_nodes_with_degree_gt0[[1]]))
})
