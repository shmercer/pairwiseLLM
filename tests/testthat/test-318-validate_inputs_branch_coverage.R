test_that("validate_embeddings returns early when ids is NULL", {
  emb <- matrix(runif(6), nrow = 2)
  out <- validate_embeddings(emb, ids = NULL)
  expect_identical(out, emb)
})

test_that("validate_embeddings errors on invalid ids vector", {
  emb <- matrix(runif(6), nrow = 2)
  expect_error(
    validate_embeddings(emb, ids = c("a", NA_character_)),
    "`ids` must be a non-missing character vector"
  )
})

test_that("validate_pairwise_results validates column-name arguments", {
  res <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  expect_error(
    validate_pairwise_results(res, id1_col = NA_character_),
    "`id1_col` must be a non-empty character scalar"
  )

  expect_error(
    validate_pairwise_results(res, id2_col = ""),
    "`id2_col` must be a non-empty character scalar"
  )

  expect_error(
    validate_pairwise_results(res, winner_col = NA_character_),
    "`winner_col` must be a non-empty character scalar"
  )

  expect_error(
    validate_pairwise_results(res, judge_col = ""),
    "`judge_col` must be a non-empty character scalar when provided"
  )
})

test_that("validate_pairwise_results validates ids and missing winner handling", {
  res_missing <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = NA_character_)

  expect_error(
    validate_pairwise_results(res_missing, ids = c("A", NA_character_)),
    "`ids` must be a character vector of non-missing, non-empty IDs"
  )

  expect_error(
    validate_pairwise_results(res_missing, allow_missing_winner = FALSE),
    "missing values in winner column"
  )
})
