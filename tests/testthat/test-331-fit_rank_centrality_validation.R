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
  
# mockery::stub mutates the function object in-place. Ensure we restore the
# original to avoid leakage into later tests.
ns <- asNamespace("pairwiseLLM")
fit_rank_centrality_orig <- get("fit_rank_centrality", envir = ns)

on.exit({
  # The namespace binding is locked; temporarily unlock to restore.
  unlockBinding("fit_rank_centrality", ns)
  assign("fit_rank_centrality", fit_rank_centrality_orig, envir = ns)
  lockBinding("fit_rank_centrality", ns)
}, add = TRUE)

mockery::stub(pairwiseLLM::fit_rank_centrality, "Matrix::crossprod", mock_xprod)
  testthat::expect_error(
    pairwiseLLM::fit_rank_centrality(bt_data, ids = c("A", "B"), max_iter = 2),
    "non-finite"
  )
})


test_that("fit_rank_centrality errors on missing required columns and invalid scalars", {
  bt_bad <- tibble::tibble(object1 = "A", object2 = "B") # missing result
  expect_error(
    fit_rank_centrality(bt_bad),
    "`bt_data` must contain columns: object1, object2, result"
  )

  bt_ok <- tibble::tibble(object1 = c("A", "A"), object2 = c("B", "B"), result = c(1, 0))

  expect_error(fit_rank_centrality(bt_ok, ids = 1:2), "`ids` must be a character vector")
  expect_error(fit_rank_centrality(bt_ok, smoothing = -0.1), "`smoothing` must be a non-negative numeric scalar")
  expect_error(fit_rank_centrality(bt_ok, damping = 1), "`damping` must be a numeric scalar in \\[0, 1\\)")
  expect_error(fit_rank_centrality(bt_ok, max_iter = 0), "`max_iter` must be a positive integer-like scalar")
  expect_error(fit_rank_centrality(bt_ok, tol = 0), "`tol` must be a positive numeric scalar")
})

test_that("fit_rank_centrality errors when fewer than 2 unique ids or no valid comparisons", {
  bt_one <- tibble::tibble(object1 = "A", object2 = "A", result = 1)
  expect_error(
    fit_rank_centrality(bt_one),
    "Need at least 2 unique IDs"
  )

  bt_self <- tibble::tibble(object1 = c("A", "B"), object2 = c("A", "B"), result = c(1, 0))
  expect_error(
    fit_rank_centrality(bt_self, ids = c("A", "B")),
    "No valid comparisons found after (aggregation|filtering)"
  )
})
