testthat::test_that("adaptive_filter_candidate_pool handles validation branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  testthat::expect_error(
    pairwiseLLM:::.adaptive_filter_candidate_pool("bad", state),
    "data frame"
  )

  empty_tbl <- tibble::tibble(i = character(), j = character())
  empty_out <- pairwiseLLM:::.adaptive_filter_candidate_pool(empty_tbl, state)
  testthat::expect_equal(nrow(empty_out), 0L)

  testthat::expect_error(
    pairwiseLLM:::.adaptive_filter_candidate_pool(tibble::tibble(x = "A"), state),
    "include `i`/`j`"
  )

  invalid_ids <- tibble::tibble(
    i_id = c("A", "A", NA_character_),
    j_id = c("A", "Z", "B")
  )
  filtered <- pairwiseLLM:::.adaptive_filter_candidate_pool(invalid_ids, state)
  testthat::expect_equal(nrow(filtered), 0L)
})
