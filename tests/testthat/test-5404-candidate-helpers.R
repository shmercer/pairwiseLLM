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
  fit <- list(theta_draws = theta_draws, theta_mean = c(1, 0))
  out <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  expect_equal(out$item_id, state$ids)

  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(list(theta_mean = c(1, 0)), state),
    "theta_draws"
  )
})

testthat::test_that("select_anchors_v3 handles single-anchor requests", {
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
  anchors <- pairwiseLLM:::select_anchors_v3(theta_summary, state, config)
  expect_equal(anchors, "A")
})
