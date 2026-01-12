.test_graph_state_7203 <- function() {
  ids <- c("A", "B", "C", "D")
  list(
    component_id = stats::setNames(c(1L, 1L, 2L, 2L), ids),
    degree = stats::setNames(c(1, 1, 0, 0), ids)
  )
}


test_that(".ap_make_component_bridge_pairs hits empty-grid and empty-bind_rows guards", {
  gs <- .test_graph_state_7203()
  id_vec_theta <- c("A", "B", "C", "D")

  # Force an empty crossing grid -> exercises the early `next` guard.
  testthat::local_mocked_bindings(
    crossing = function(...) {
      tibble::tibble(lcc_id = character(), other_id = character())
    },
    .package = "tidyr"
  )

  out <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = gs,
    id_vec_theta = id_vec_theta,
    n_bridge = 2L,
    max_lcc = 2L,
    max_other = 2L
  )
  expect_identical(nrow(out), 0L)

  # Now allow crossing, but force bind_rows to return an empty tibble so the
  # nrow(out_tbl)==0 guard is exercised.
  testthat::local_mocked_bindings(
    bind_rows = function(...) {
      tibble::tibble(i_idx = integer(), j_idx = integer())
    },
    .package = "dplyr"
  )

  out2 <- pairwiseLLM:::.ap_make_component_bridge_pairs(
    graph_state = gs,
    id_vec_theta = id_vec_theta,
    n_bridge = 2L,
    max_lcc = 2L,
    max_other = 2L
  )
  expect_identical(nrow(out2), 0L)
})
