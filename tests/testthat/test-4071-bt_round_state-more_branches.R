test_that(".bt_round_state handles degenerate ids inputs and empty id-restricted keys", {
  # Non-empty results, but ids supplied are all NA/blank => graph metrics stay NA
  # because ids_for_graph is normalized to NULL.
  results <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    better_id = c("A", "A")
  )

  s1 <- pairwiseLLM:::.bt_round_state(results, ids = c(NA_character_, ""))
  expect_equal(s1$n_results, 2L)
  expect_true(is.na(s1$n_components))
  expect_true(is.na(s1$largest_component_frac))

  # ids provided but none of the endpoints are in ids => n_unique_unordered_pairs_in_ids should be 0
  s2 <- pairwiseLLM:::.bt_round_state(results, ids = c("X", "Y"))
  expect_equal(s2$n_unique_unordered_pairs_in_ids, 0L)
})
