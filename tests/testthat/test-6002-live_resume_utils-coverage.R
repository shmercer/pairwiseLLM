test_that(".coerce_live_submit_types handles NULL input and missing readr", {
  # NULL / empty data should return NULL early (covers line 6)
  expect_null(pairwiseLLM:::.coerce_live_submit_types(NULL))

  # The helper should error with a clear message if readr isn't available
  # (covers line 31).
  testthat::local_mocked_bindings(
    .require_pkg = function(pkg) {
      if (identical(pkg, "readr")) return(FALSE)
      TRUE
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_error(
    pairwiseLLM:::.read_existing_live_results('dummy.csv'),
    "readr"
  )
})
