test_that(".bt_resolve_stop_reason normalizes no_new_results to no_results", {
  r1 <- pairwiseLLM:::.bt_resolve_stop_reason(no_new_results = TRUE)
  r2 <- pairwiseLLM:::.bt_resolve_stop_reason(no_results = TRUE)

  expect_identical(r1, "no_results")
  expect_identical(r2, "no_results")
})
