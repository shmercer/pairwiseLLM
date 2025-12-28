test_that(".apply_backend_validation_to_submit_output attaches validation_report when validate=TRUE", {
  out <- list(
    results = tibble::tibble(
      ID1 = c("A", "A", "B"),
      ID2 = c("B", "C", "C"),
      better_id = c("A", NA_character_, "winner:2"),
      status_code = c(200L, 200L, 500L),
      error_message = c(NA_character_, NA_character_, "oops")
    ),
    failed_pairs = tibble::tibble()
  )

  # validate=FALSE returns unchanged (no report)
  out0 <- pairwiseLLM:::.apply_backend_validation_to_submit_output(out, backend = "test", validate = FALSE)
  expect_true(is.null(out0$validation_report))

  # validate=TRUE attaches report; strict=FALSE does not error on invalid winner
  out1 <- pairwiseLLM:::.apply_backend_validation_to_submit_output(out, backend = "test", validate = TRUE, validate_strict = FALSE)
  expect_true(is.list(out1$validation_report))
  expect_equal(out1$validation_report$backend, "test")
  expect_equal(out1$validation_report$n_rows, 3L)
  expect_equal(out1$validation_report$n_missing_winner, 1L)
  expect_equal(out1$validation_report$n_invalid_winner, 1L)
  expect_equal(out1$validation_report$n_status_ge_400, 1L)
  expect_equal(out1$validation_report$n_error_message, 1L)

  # strict mode errors on invalid winners
  expect_error(
    pairwiseLLM:::.apply_backend_validation_to_submit_output(out, backend = "test", validate = TRUE, validate_strict = TRUE),
    "must match `ID1` or `ID2`"
  )
})

test_that(".apply_backend_validation_to_submit_output errors on malformed output when validate=TRUE", {
  expect_error(
    pairwiseLLM:::.apply_backend_validation_to_submit_output(list(foo = 1), backend = "test", validate = TRUE),
    "must be a list with a `results` element"
  )
})
