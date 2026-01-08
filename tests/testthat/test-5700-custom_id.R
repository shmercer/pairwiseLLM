test_that("5700-01 .ensure_custom_id preserves user-supplied custom_id", {
  pairs <- tibble::tibble(
    custom_id = c("X1", "X2"),
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )
  out <- pairwiseLLM:::.ensure_custom_id(pairs, prefix = "LIVE")
  expect_identical(out$custom_id, c("X1", "X2"))
})

test_that("5700-02 .ensure_custom_id synthesizes missing custom_id", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )
  out <- pairwiseLLM:::.ensure_custom_id(pairs, prefix = "LIVE")
  expect_identical(out$custom_id, c("LIVE_A_vs_B", "LIVE_C_vs_D"))
})

test_that("5700-03 .ensure_custom_id errors on duplicate custom_id", {
  pairs <- tibble::tibble(
    custom_id = c("DUP", "DUP"),
    ID1 = c("A", "C"),
    ID2 = c("B", "D")
  )
  expect_error(
    pairwiseLLM:::.ensure_custom_id(pairs, prefix = "LIVE"),
    "custom_id` must be unique"
  )
})
