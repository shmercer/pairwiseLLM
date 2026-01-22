testthat::test_that("select_uncertainty_anchors respects clamp and tie-breaking", {
  samples <- tibble::tibble(
    ID = sprintf("item%03d", 1:100),
    text = sprintf("text-%03d", 1:100)
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = 0,
    theta_sd = rep(1, state$N)
  )

  anchors <- pairwiseLLM:::select_uncertainty_anchors(state, config, theta_summary = theta_summary)
  M <- max(50L, min(400L, as.integer(round(6 * sqrt(state$N)))))
  M <- min(M, state$N)

  testthat::expect_equal(length(anchors), M)
  testthat::expect_identical(anchors, utils::head(state$ids, M))
})

testthat::test_that("select_uncertainty_anchors returns all items when N < M", {
  samples <- tibble::tibble(
    ID = sprintf("item%02d", 1:10),
    text = sprintf("text-%02d", 1:10)
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_sd <- c(0.3, 0.9, 0.9, 0.2, 0.1, 0.7, 0.7, 0.5, 0.4, 0.6)
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = 0,
    theta_sd = theta_sd
  )

  anchors <- pairwiseLLM:::select_uncertainty_anchors(state, config, theta_summary = theta_summary)
  expected <- state$ids[order(-theta_sd, state$ids)]

  testthat::expect_equal(length(anchors), state$N)
  testthat::expect_identical(anchors, expected)
})
