testthat::test_that(".ap_map_idx_to_ids maps indices to IDs and errors on corruption", {
  cand <- tibble::tibble(i_idx = c(1, 2), j_idx = c(2, 3))
  ids <- c("a", "b", "c")

  out <- pairwiseLLM:::.ap_map_idx_to_ids(cand, ids, err_ctx = "t")
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(out$ID1, c("a", "b"))
  testthat::expect_equal(out$ID2, c("b", "c"))

  testthat::expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(cand, character(0), err_ctx = "t"),
    "id_vec.*NULL/empty"
  )

  testthat::expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(tibble::tibble(i_idx = 1), ids, err_ctx = "t"),
    "must include i_idx and j_idx"
  )

  bad_idx <- tibble::tibble(i_idx = 1, j_idx = 99)
  testthat::expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(bad_idx, ids, err_ctx = "t"),
    "exceed length\\(id_vec\\)"
  )

  ids_na <- c("a", NA_character_, "c")
  testthat::expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(cand, ids_na, err_ctx = "t"),
    "NA after mapping"
  )
})
