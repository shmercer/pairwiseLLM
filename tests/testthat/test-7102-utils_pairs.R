test_that(".ensure_custom_id validates required ID columns", {
  bad <- tibble::tibble(x = 1)
  expect_error(
    pairwiseLLM:::.ensure_custom_id(bad),
    "must contain columns: ID1, ID2",
    fixed = FALSE
  )
})

test_that(".ensure_custom_id fills missing ids and enforces uniqueness", {
  pairs <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    custom_id = c(NA_character_, "")
  )

  out <- pairwiseLLM:::.ensure_custom_id(pairs, prefix = "X")
  expect_true(all(nzchar(out$custom_id)))
  expect_match(out$custom_id[[1]], "^X_A_vs_B$")
  expect_match(out$custom_id[[2]], "^X_A_vs_C$")

  dup <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "C"), custom_id = c("d", "d"))
  expect_error(
    pairwiseLLM:::.ensure_custom_id(dup),
    "must be unique",
    fixed = FALSE
  )
})
