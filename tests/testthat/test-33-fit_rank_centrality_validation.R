testthat::test_that("fit_rank_centrality covers validation + error branches", {
  bt_data <- tibble::tibble(
    object1 = c("A", "A"),
    object2 = c("B", "B"),
    result = c(1, 0)
  )

  # ids must have length >=2
  testthat::expect_error(
    pairwiseLLM::fit_rank_centrality(bt_data, ids = "A"),
    "at least 2"
  )

  # no valid comparisons after filtering => error
  bt_bad <- tibble::tibble(object1 = "A", object2 = "B", result = NA_real_)
  testthat::expect_error(
    pairwiseLLM::fit_rank_centrality(bt_bad, ids = c("A", "B")),
    "No valid comparisons"
  )

  # verbose path prints progress
  testthat::expect_message(
    pairwiseLLM::fit_rank_centrality(bt_data, ids = c("A", "B"), verbose = TRUE, max_iter = 2),
    "Rank Centrality"
  )

  # force non-finite pi_next to cover defensive stop
  mock_xprod <- function(...) rep(NaN, 2)
  mockery::stub(pairwiseLLM::fit_rank_centrality, "Matrix::crossprod", mock_xprod)
  testthat::expect_error(
    pairwiseLLM::fit_rank_centrality(bt_data, ids = c("A", "B"), max_iter = 2),
    "non-finite"
  )
})
