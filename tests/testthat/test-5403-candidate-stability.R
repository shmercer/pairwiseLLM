testthat::test_that("candidate generation is stable to irrelevant rank changes", {
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
    theta_mean = c(10, 8, 6, 4, 2, 0),
    theta_sd = rep(0.3, state$N)
  )
  anchors <- c("C")
  base <- pairwiseLLM:::enumerate_candidates_v3(anchors, theta_summary, state, config)

  perturbed <- theta_summary
  perturbed$theta_mean[6] <- perturbed$theta_mean[6] + 0.01
  out2 <- pairwiseLLM:::enumerate_candidates_v3(anchors, perturbed, state, config)

  expect_equal(base, out2)
  expect_equal(base, pairwiseLLM:::enumerate_candidates_v3(anchors, theta_summary, state, config))
})
