test_that("bt_count_unique_pairs validates required columns", {
  bad <- tibble::tibble(foo = 1)
  expect_error(
    pairwiseLLM::bt_count_unique_pairs(bad),
    "must have columns ID1 and ID2",
    fixed = FALSE
  )
})

test_that("bt_count_unique_pairs drops invalid rows and respects ids/new_ids", {
  results <- tibble::tibble(
    ID1 = c("A", "A", NA, "", "B"),
    ID2 = c("B", NA, "B", "C", "C")
  )

  out_all <- pairwiseLLM::bt_count_unique_pairs(results)
  expect_equal(out_all$n_pairs_total, 2L) # A-B and B-C
  expect_true(is.na(out_all$n_pairs_new))

  out_ids <- pairwiseLLM::bt_count_unique_pairs(results, ids = c("A", "B"))
  expect_equal(out_ids$n_pairs_total, 1L) # only A-B survives
  expect_true(is.na(out_ids$n_pairs_new))

  out_new <- pairwiseLLM::bt_count_unique_pairs(results, new_ids = c("C"))
  expect_equal(out_new$n_pairs_total, 2L)
  expect_equal(out_new$n_pairs_new, 1L) # B-C touches C

  out_new_empty <- pairwiseLLM::bt_count_unique_pairs(results, new_ids = c(NA_character_, ""))
  expect_equal(out_new_empty$n_pairs_new, 0L)

  # ids provided but empty/invalid -> treated as NULL (no restriction)
  out_ids_empty <- pairwiseLLM::bt_count_unique_pairs(results, ids = c(NA_character_, ""))
  expect_equal(out_ids_empty$n_pairs_total, 2L)
})
