test_that("Anthropic httr2 wrapper helpers are callable", {
  resp <- structure(list(), class = "httr2_response")
  status <- pairwiseLLM:::.anthropic_resp_status(resp)

  # With a stubbed/empty httr2 response, httr2 may return NULL or an empty
  # vector. We just want the wrapper to be callable without throwing.
  expect_true(is.null(status) || is.numeric(status) || is.integer(status))
})
