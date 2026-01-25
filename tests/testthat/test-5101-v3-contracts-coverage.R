testthat::test_that("reorder_theta_draws validates inputs", {
  ids <- c("a", "b")
  draws <- matrix(1:4, nrow = 2, ncol = 2)

  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(1, ids),
    "numeric matrix"
  )
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, c("a", "a")),
    "unique"
  )

  colnames(draws) <- c(NA_character_, "b")
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, ids),
    "column names"
  )

  colnames(draws) <- ids
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, c("a", "c")),
    "match"
  )

  colnames(draws) <- rev(ids)
  reordered <- pairwiseLLM:::reorder_theta_draws(draws, ids)
  testthat::expect_identical(colnames(reordered), ids)
})

testthat::test_that("validate_v3_fit_contract catches schema violations", {
  ids <- c("a", "b")
  theta_draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids
  base_fit <- make_v3_fit_contract(ids, theta_draws = theta_draws)

  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract("bad", ids),
    "list"
  )
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(base_fit, character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(base_fit, c("a", "a")),
    "unique"
  )

  bad_draws <- base_fit
  bad_draws$theta_draws <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_draws, ids),
    "theta_draws"
  )

  short_draws <- base_fit
  short_draws$theta_draws <- theta_draws[1, , drop = FALSE]
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(short_draws, ids),
    "at least two draws"
  )

  wrong_cols <- base_fit
  wrong_cols$theta_draws <- matrix(1:6, nrow = 3, ncol = 2)
  colnames(wrong_cols$theta_draws) <- ids
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(wrong_cols, c("a", "b", "c")),
    "one column"
  )

  bad_mean <- base_fit
  bad_mean$theta_mean <- "nope"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_mean, ids),
    "theta_mean"
  )

  short_mean <- base_fit
  short_mean$theta_mean <- stats::setNames(0.1, "a")
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(short_mean, ids),
    "length"
  )

  unnamed_mean <- base_fit
  names(unnamed_mean$theta_mean) <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(unnamed_mean, ids),
    "named"
  )

  wrong_names <- base_fit
  names(wrong_names$theta_mean) <- rev(ids)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(wrong_names, ids),
    "names"
  )
})
