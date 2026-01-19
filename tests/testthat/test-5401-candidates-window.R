testthat::test_that("enumerate_candidates respects rank windows and uniqueness", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(W = 1L, A_anchors = 3L))
  state$config$v3 <- config

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(3, 2, 1, 0, -1, -2),
    theta_sd = rep(0.2, 6)
  )
  anchors <- c("C", "E")
  candidates <- pairwiseLLM:::enumerate_candidates(anchors, theta_summary, state, config)

  expect_true(all(candidates$i != candidates$j))
  expect_equal(nrow(candidates), nrow(dplyr::distinct(candidates)))

  ord <- order(-theta_summary$theta_mean, theta_summary$item_id)
  ranks <- stats::setNames(seq_along(ord), theta_summary$item_id[ord])
  rank_diff <- abs(ranks[candidates$i] - ranks[candidates$j])
  expect_true(all(rank_diff <= config$W))
})
