testthat::test_that("adaptive_v3_config covers NULL override normalization", {
  testthat::expect_silent(
    testthat::with_mocked_bindings(
      pairwiseLLM:::adaptive_v3_config(6),
      is.null = function(x) TRUE,
      .package = "base"
    )
  )
})

testthat::test_that("adaptive_v3_config rejects non-list overrides after normalization", {
  fn <- pairwiseLLM:::adaptive_v3_config
  test_env <- new.env(parent = environment(fn))
  assign("list", function(...) character(), envir = test_env)
  environment(fn) <- test_env

  testthat::expect_error(
    fn(6),
    "overrides"
  )
})

testthat::test_that("round log row and item summary require adaptive_state", {
  testthat::expect_error(
    pairwiseLLM:::build_round_log_row(state = list()),
    "adaptive_state"
  )

  testthat::expect_error(
    pairwiseLLM:::build_item_summary(state = list()),
    "adaptive_state"
  )
})
