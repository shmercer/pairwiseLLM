test_that("5512 hash_shuffle_pairs validates inputs and handles empty tibble", {
  empty_pairs <- tibble::tibble(ID1 = character(), ID2 = character())

  out_empty <- pairwiseLLM:::.hash_shuffle_pairs(pairs = empty_pairs, round = 1L, salt = "salt")
  expect_true(tibble::is_tibble(out_empty))
  expect_identical(out_empty, empty_pairs)

  bad_pairs <- tibble::tibble(ID1 = c("A", "B"))
  expect_error(
    pairwiseLLM:::.hash_shuffle_pairs(pairs = bad_pairs, round = 1L, salt = "salt"),
    "must contain columns"
  )
})

test_that("5512 hash_shuffle_pairs produces deterministic ordering without RNG", {
  pairs <- tibble::tibble(
    ID1 = c("A", "A", "B", "C"),
    ID2 = c("B", "C", "C", "D")
  )

  out1 <- pairwiseLLM:::.hash_shuffle_pairs(pairs = pairs, round = 7L, salt = "")
  out2 <- pairwiseLLM:::.hash_shuffle_pairs(pairs = pairs, round = 7L, salt = "")

  expect_identical(out1, out2)
  expect_true(tibble::is_tibble(out1))
  expect_identical(sort(names(out1)), sort(names(pairs)))
})
