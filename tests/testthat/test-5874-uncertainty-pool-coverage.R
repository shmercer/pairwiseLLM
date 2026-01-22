testthat::test_that("select_uncertainty_anchors validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(0.1, -0.1),
    theta_sd = c(0.2, 0.3)
  )

  testthat::expect_error(
    pairwiseLLM:::select_uncertainty_anchors(state, config = 1, theta_summary = theta_summary),
    "`config` must be a list"
  )
  testthat::expect_error(
    pairwiseLLM:::select_uncertainty_anchors(state, config = list()),
    "theta_summary"
  )
})

testthat::test_that("select_uncertainty_anchors errors when state N is invalid", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$N <- NA_integer_
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(0.1, -0.1),
    theta_sd = c(0.2, 0.3)
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      pairwiseLLM:::select_uncertainty_anchors(state, config = list(), theta_summary = theta_summary),
      validate_state = function(...) TRUE,
      .env = asNamespace("pairwiseLLM")
    ),
    "state\\$N"
  )
})

testthat::test_that("select_uncertainty_anchors returns empty when summary is empty", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::select_uncertainty_anchors(
      state,
      config = list(),
      theta_summary = tibble::tibble(item_id = character(), theta_mean = double(), theta_sd = double())
    ),
    .adaptive_v3_theta_summary = function(...) {
      tibble::tibble(item_id = character(), theta_mean = double(), theta_sd = double(), deg = integer())
    },
    .env = asNamespace("pairwiseLLM")
  )

  testthat::expect_identical(out, character())
})
