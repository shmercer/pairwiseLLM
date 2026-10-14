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

test_that("nested local seeds restore the exact caller state and RNG kinds", {
  # Establish a seed before changing kinds so cleanup can restore kinds even
  # when the test runner entered with no .Random.seed.
  withr::local_seed(203)
  withr::local_seed(204, .rng_kind = "L'Ecuyer-CMRG", .rng_normal_kind = "Ahrens-Dieter")
  before <- .Random.seed
  kind <- RNGkind()
  scoped <- function() {
    withr::local_seed(205)
    .pairwiseLLM_with_seed(206, function() stats::runif(3))
    stats::runif(2)
  }
  expect_identical(scoped(), scoped())
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  expect_error(.pairwiseLLM_with_seed(207, function() {
    stats::runif(4)
    stop("seeded failure")
  }), "seeded failure")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
})

test_that("seed scopes preserve an absent caller seed on success and failure", {
  withr::local_seed(208)
  kind <- RNGkind()
  rm(".Random.seed", envir = .GlobalEnv)
  scoped <- function(fail = FALSE) {
    withr::local_seed(209)
    .pairwiseLLM_with_seed(210, function() {
      stats::runif(3)
      if (fail) stop("scope failure")
      42L
    })
  }
  expect_identical(scoped(), 42L)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), kind)
  expect_error(scoped(TRUE), "scope failure")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), kind)
})
