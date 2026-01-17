testthat::test_that("select_anchors_v3 prioritizes extremes and uncertainty", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste("text", LETTERS[1:8])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(A_anchors = 5L))
  state$config$v3 <- config

  deg <- c(3L, 2L, 2L, 2L, 4L, 1L, 2L, 2L)
  state$pos1 <- stats::setNames(as.integer(deg), state$ids)
  state$pos2 <- stats::setNames(as.integer(rep(0L, length(deg))), state$ids)
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(3, 2, 1, 0, -1, -2, -3, -4),
    theta_sd = c(0.1, 0.2, 0.5, 0.4, 0.6, 0.6, 0.3, 0.2)
  )

  anchors <- pairwiseLLM:::select_anchors_v3(theta_summary, state, config)
  expect_equal(anchors, c("A", "B", "H", "G", "F"))
  expect_true(length(anchors) <= config$A_anchors)

  anchors2 <- pairwiseLLM:::select_anchors_v3(theta_summary, state, config)
  expect_equal(anchors, anchors2)
})
