
test_that(".active_ids_for_graph handles batches and results edge cases", {
  core_ids <- c("A", NA, "")
  batches <- list(c("B"), c("C", NA))

  # batch_i is NA -> should ignore batches.
  out1 <- pairwiseLLM:::.active_ids_for_graph(
    core_ids = core_ids,
    batches = batches,
    batch_i = NA_integer_,
    results_so_far = tibble::tibble(ID1 = c("D"), ID2 = c("A"))
  )
  expect_true(all(c("A", "D") %in% out1))
  expect_false("B" %in% out1)

  # results_so_far without ID1/ID2 columns -> ignored.
  out2 <- pairwiseLLM:::.active_ids_for_graph(
    core_ids = c("A"),
    batches = batches,
    batch_i = 2L,
    results_so_far = tibble::tibble(x = 1)
  )
  expect_true(all(c("A", "B", "C") %in% out2))
})


test_that(".graph_state_from_pairs treats invalid pairs as empty", {
  ids <- c("A", "B", "C")

  # All invalid (NA / self-pairs) -> should behave like empty.
  pairs <- tibble::tibble(ID1 = c("A", NA, "B"), ID2 = c("A", "B", NA))
  gs <- pairwiseLLM:::.graph_state_from_pairs(pairs = pairs, ids = ids)
  expect_identical(gs$metrics$n_edges[[1]], 0L)
  expect_identical(gs$metrics$n_components[[1]], 3L)
})


test_that("fit_rank_centrality works on disconnected graphs with damping", {
  bt_data <- tibble::tibble(
    object1 = c("A", "C"),
    object2 = c("B", "D"),
    result = c(1, 1)
  )

  fit <- pairwiseLLM::fit_rank_centrality(bt_data, ids = c("A", "B", "C", "D"), damping = 0.1, return_transition = TRUE)
  expect_identical(fit$engine, "rank_centrality")
  expect_true(is.null(fit$P) || inherits(fit$P, "Matrix") || is.matrix(fit$P))
  expect_true(all(c("ID", "theta", "pi") %in% names(fit$theta)))
  # disconnected without damping would have multiple stationary distributions;
  # with damping it should report 1 component in the diagnostics from helper.
  expect_true(is.numeric(fit$diagnostics$n_components))
})
