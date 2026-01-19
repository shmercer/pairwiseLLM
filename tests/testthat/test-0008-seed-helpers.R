testthat::test_that(".pairwiseLLM_with_seed validates seed input", {
  testthat::expect_error(
    .pairwiseLLM_with_seed(NA_real_, function() 1),
    "seed"
  )
})

testthat::test_that(".pairwiseLLM_with_seed preserves RNG state", {
  withr::local_seed(202)
  baseline_pre <- stats::runif(3)
  baseline_post <- stats::runif(3)

  withr::local_seed(202)
  pre <- stats::runif(3)
  out <- .pairwiseLLM_with_seed(123, function() stats::runif(1))
  post <- stats::runif(3)

  testthat::expect_equal(pre, baseline_pre)
  testthat::expect_equal(post, baseline_post)
  testthat::expect_true(is.numeric(out))
})
