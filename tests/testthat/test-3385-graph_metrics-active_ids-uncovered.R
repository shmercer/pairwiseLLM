testthat::test_that(".active_ids_for_graph is robust to NULL/NA inputs", {
  out1 <- pairwiseLLM:::.active_ids_for_graph(
    core_ids = c("a", NA, ""),
    batches = list(c("b", "c"), c(NA, "")),
    batch_i = 1,
    results_so_far = tibble::tibble(ID1 = "c", ID2 = "d")
  )
  testthat::expect_equal(out1, c("a", "b", "c", "d"))

  out2 <- pairwiseLLM:::.active_ids_for_graph(
    core_ids = NULL,
    batches = NULL,
    batch_i = 99,
    results_so_far = NULL
  )
  testthat::expect_identical(out2, character(0))
})

testthat::test_that(".graph_state_from_pairs returns expected schema for empty pairs", {
  gs <- pairwiseLLM:::.graph_state_from_pairs(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    ids = c("a", "b", "c")
  )
  testthat::expect_true(is.list(gs))
  testthat::expect_equal(gs$metrics$n_edges, 0L)
  testthat::expect_equal(gs$metrics$n_components, 3L)
  testthat::expect_named(gs$degree, c("a", "b", "c"))
  testthat::expect_named(gs$component_id, c("a", "b", "c"))
})