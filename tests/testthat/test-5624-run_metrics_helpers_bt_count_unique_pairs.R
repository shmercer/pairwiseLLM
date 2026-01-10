# Workstream E: metrics helper edge cases

testthat::test_that("bt_count_unique_pairs handles empty and filtered ID sets", {
  results <- tibble::tibble(
    ID1 = c("A", "A", "B", "A"),
    ID2 = c("B", "C", "C", "B")
  )

  # When `ids` cleans down to NULL, we treat it as "all IDs".
  out_all <- pairwiseLLM::bt_count_unique_pairs(results, ids = c("", NA_character_))
  testthat::expect_equal(out_all$n_pairs_total, 3L)
  testthat::expect_true(is.na(out_all$n_pairs_new))

  # Filtering to a subset of IDs.
  out_subset <- pairwiseLLM::bt_count_unique_pairs(results, ids = c("A", "B"))
  testthat::expect_equal(out_subset$n_pairs_total, 1L)
  testthat::expect_true(is.na(out_subset$n_pairs_new))

  # `new_ids` cleans to empty -> n_pairs_new is 0.
  out_none_new <- pairwiseLLM::bt_count_unique_pairs(
    results,
    ids = c("A", "B", "C"),
    new_ids = c("", NA_character_)
  )
  testthat::expect_equal(out_none_new$n_pairs_total, 3L)
  testthat::expect_equal(out_none_new$n_pairs_new, 0L)
})
