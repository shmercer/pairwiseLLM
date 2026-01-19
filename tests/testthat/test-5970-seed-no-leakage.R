test_that("seeded adaptive helpers are deterministic and do not leak RNG state", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  out1 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 123)
  out2 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 123)
  expect_identical(out1, out2)

  withr::local_seed(999)
  baseline_pre <- stats::runif(3)
  baseline_post <- stats::runif(3)

  withr::local_seed(999)
  pre <- stats::runif(3)
  pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 456)
  post <- stats::runif(3)

  expect_equal(pre, baseline_pre)
  expect_equal(post, baseline_post)
})
