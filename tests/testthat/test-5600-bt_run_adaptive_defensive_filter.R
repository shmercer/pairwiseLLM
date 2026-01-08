test_that("5600-01 adaptive defensive filter drops echoed historical rows", {
  req <- tibble::tibble(custom_id = c("a", "b"), ID1 = c("1", "1"), ID2 = c("2", "3"))
  res <- tibble::tibble(
    custom_id = c("a", "b", "old"),
    ID1 = c("1", "1", "9"),
    ID2 = c("2", "3", "10"),
    better_id = c("1", "3", "9")
  )

  expect_warning(
    out <- pairwiseLLM:::.filter_judge_results_to_request(res, req),
    "dropping extras",
    fixed = FALSE
  )
  expect_equal(out$custom_id, c("a", "b"))
})

test_that("5600-02 defensive filter deduplicates within-round duplicates", {
  req <- tibble::tibble(custom_id = c("a", "b"), ID1 = c("1", "1"), ID2 = c("2", "3"))
  res <- tibble::tibble(
    custom_id = c("a", "a", "b"),
    ID1 = c("1", "1", "1"),
    ID2 = c("2", "2", "3"),
    better_id = c("1", "2", "3")
  )

  expect_warning(
    out <- pairwiseLLM:::.filter_judge_results_to_request(res, req),
    "duplicate",
    fixed = FALSE
  )
  expect_equal(out$custom_id, c("a", "b"))
  expect_equal(nrow(out), 2L)
})

test_that("5600-03 defensive filter works without custom_id via (ID1,ID2) key", {
  req <- tibble::tibble(ID1 = c("1", "1"), ID2 = c("2", "3"))
  res <- tibble::tibble(
    ID1 = c("1", "1", "9"),
    ID2 = c("2", "3", "10"),
    better_id = c("1", "3", "9")
  )

  expect_warning(
    out <- pairwiseLLM:::.filter_judge_results_to_request(res, req),
    "dropping extras",
    fixed = FALSE
  )
  expect_equal(out$ID1, c("1", "1"))
  expect_equal(out$ID2, c("2", "3"))
})
