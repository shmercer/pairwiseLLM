# =====================================================================
#   test-custom-id.R
#   Tests for .pairwiseLLM_make_custom_id()
# =====================================================================

testthat::test_that(".pairwiseLLM_make_custom_id falls back on missing pair_uid values", {
  out <- .pairwiseLLM_make_custom_id(
    ID1 = c("A", "B"),
    ID2 = c("C", "D"),
    pair_uid = c("pair-001", "")
  )

  testthat::expect_equal(out[1], "pair-001")
  testthat::expect_equal(out[2], "LIVE_B_vs_D")
})

testthat::test_that(".pairwiseLLM_make_custom_id uses legacy ids when pair_uid is NULL", {
  out <- .pairwiseLLM_make_custom_id(ID1 = "X", ID2 = "Y", pair_uid = NULL)
  testthat::expect_equal(out, "LIVE_X_vs_Y")
})
