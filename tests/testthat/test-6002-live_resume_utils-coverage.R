test_that(".coerce_live_submit_types early return and .read_existing_live_results roundtrip", {
  # NULL / empty input should return NULL early.
  expect_null(pairwiseLLM:::.coerce_live_submit_types(NULL))

  testthat::skip_if_not_installed("readr")

  withr::local_tempdir(.local_envir = environment())
  td <- tempfile(fileext = ".csv")

  # Write a minimal realistic live-results CSV and ensure we can read it back
  # with stable types.
  existing <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01",
    ID2 = "S02",
    model = "m",
    object_type = NA_character_,
    status_code = 200L,
    error_message = NA_character_
  )
  readr::write_csv(existing, td)

  out <- pairwiseLLM:::.read_existing_live_results(td)
  expect_true(is.data.frame(out))
  expect_true(is.character(out$ID1))
  expect_true(is.integer(out$status_code) || all(is.na(out$status_code)))
})
