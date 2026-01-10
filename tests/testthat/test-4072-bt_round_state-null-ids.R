test_that(".bt_round_state drops empty/NA ids to NULL for graph", {
  res <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())

  # ids are provided but all are empty/NA, so ids_for_graph becomes NULL
  out <- pairwiseLLM:::.bt_round_state(res, ids = c("", NA_character_), prefix = "")

  expect_true(is.data.frame(out))
  expect_identical(out$n_results, 0L)

  # Since ids_for_graph becomes NULL, graph metrics remain NA
  expect_true(is.na(out$n_components))
  expect_true(is.na(out$largest_component_frac))
  expect_true(is.na(out$degree_min))
})
