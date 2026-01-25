testthat::test_that("candidate helper summaries validate and normalize inputs", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  theta_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(1, 0, -1),
    theta_sd = c(0.2, 0.3, 0.4)
  )
  out <- pairwiseLLM:::.adaptive_v3_theta_summary(theta_summary, state)
  expect_equal(out$item_id, state$ids)
  expect_true(all(out$deg == 0L))

  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(tibble::tibble(x = 1), state),
    "item_id"
  )
  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(
      tibble::tibble(item_id = state$ids, theta_mean = 1),
      state
    ),
    "missing required columns"
  )
  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(
      tibble::tibble(item_id = c("A", "B", "D"), theta_mean = 1:3, theta_sd = 1),
      state
    ),
    "match `state\\$ids`"
  )
})

testthat::test_that("theta summaries from fit validate required fields", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  theta_draws <- matrix(c(1, 0, 1, 0), nrow = 2, byrow = TRUE)
  colnames(theta_draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = theta_draws)
  out <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  expect_equal(out$item_id, state$ids)

  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(list(theta_mean = c(1, 0)), state),
    "theta_mean"
  )
})

testthat::test_that("select_anchors handles single-anchor requests", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(A_anchors = 1L))
  state$config$v3 <- config

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(2, 0, -1),
    theta_sd = c(0.2, 0.3, 0.4)
  )
  anchors <- pairwiseLLM:::select_anchors(theta_summary, state, config)
  expect_equal(anchors, "A")
})

testthat::test_that("compute_ranking_from_theta_mean validates inputs and orders deterministically", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = "bad", state = state),
    "non-empty numeric vector"
  )
  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = numeric(), state = state),
    "non-empty numeric vector"
  )

  theta_mean <- stats::setNames(c(1, 0, -1), state$ids)
  names(theta_mean)[[1]] <- ""
  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = theta_mean, state = state),
    "non-empty names"
  )

  theta_mean <- stats::setNames(c(1, 0, -1), c("A", "A", "C"))
  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = theta_mean, state = state),
    "names must be unique"
  )

  theta_mean <- stats::setNames(c(1, NA_real_, -1), state$ids)
  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = theta_mean, state = state),
    "must not contain missing values"
  )

  theta_mean <- stats::setNames(c(1, 0, -1), c("A", "B", "D"))
  testthat::expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = theta_mean, state = state),
    "names must match"
  )

  theta_mean <- stats::setNames(c(1, 1, -1), c("B", "A", "C"))
  ranking <- pairwiseLLM:::compute_ranking_from_theta_mean(theta_mean = theta_mean, state = state)
  testthat::expect_equal(ranking, c("A", "B", "C"))
})

testthat::test_that("select_window_size validates inputs and follows phase rules", {
  testthat::expect_error(
    pairwiseLLM:::select_window_size(N = NA_real_),
    "positive numeric value"
  )
  testthat::expect_error(
    pairwiseLLM:::select_window_size(N = 0),
    "positive numeric value"
  )
  testthat::expect_error(
    pairwiseLLM:::select_window_size(N = 10, near_stop = NA),
    "TRUE or FALSE"
  )

  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 500, phase = "phase2"), 50L)
  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 501, phase = "phase2"), 100L)

  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 100, phase = "phase3"), 25L)
  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 500, phase = "phase3"), 50L)
  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 1000, phase = "phase3"), 50L)

  testthat::expect_equal(pairwiseLLM:::select_window_size(N = 200, phase = "phase2", near_stop = TRUE), 25L)
})
