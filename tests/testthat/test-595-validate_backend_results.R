test_that("validate_backend_results reports missing/invalid winners in non-strict mode", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B", "C"),
    ID2 = c("B", "C", "C", "A"),
    better_id = c("SAMPLE_1", "2", "winner:2", NA_character_),
    status_code = c(200L, 200L, 200L, 500L),
    error_message = c(NA_character_, NA_character_, "bad parse", "server")
  )

  out <- validate_backend_results(
    res,
    backend = "demo",
    normalize_winner = TRUE,
    strict = FALSE,
    return_report = TRUE
  )

  expect_true(is.list(out))
  expect_true(all(c("data", "report") %in% names(out)))
  expect_equal(out$report$backend, "demo")
  expect_equal(out$report$n_rows, 4L)

  # SAMPLE_1 and "2" normalize to valid; "winner:2" invalid; NA missing
  expect_equal(out$report$n_invalid_winner, 1L)
  expect_equal(out$report$n_missing_winner, 1L)

  # Backend indicators
  expect_equal(out$report$n_status_ge_400, 1L)
  expect_equal(out$report$n_error_message, 2L) # "bad parse" + "server"
})

test_that("validate_backend_results strict mode enforces validity", {
  res <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    better_id = c("A", "winner:2")
  )
  expect_error(
    validate_backend_results(res, strict = TRUE),
    "must match `ID1` or `ID2`"
  )

  res2 <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    better_id = c("SAMPLE_1", "2")
  )
  out2 <- validate_backend_results(res2, strict = TRUE, normalize_winner = TRUE)
  expect_true(is.data.frame(out2))
  expect_equal(out2$better_id, c("A", "C"))
})
