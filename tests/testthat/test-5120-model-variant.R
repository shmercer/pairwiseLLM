testthat::test_that("adaptive_v3_config defaults to btl_e_b", {
  cfg <- pairwiseLLM:::adaptive_v3_config(6L, list())
  testthat::expect_equal(cfg$model_variant, "btl_e_b")
})

testthat::test_that("invalid model_variant aborts with allowed values", {
  testthat::expect_error(
    pairwiseLLM:::adaptive_v3_config(6L, list(model_variant = "nope")),
    "btl, btl_e, btl_b, btl_e_b"
  )
})

testthat::test_that("model variant helpers report epsilon and beta flags", {
  variants <- c("btl", "btl_e", "btl_b", "btl_e_b")
  has_e <- vapply(variants, pairwiseLLM:::model_has_e, logical(1L))
  has_b <- vapply(variants, pairwiseLLM:::model_has_b, logical(1L))

  testthat::expect_identical(has_e, c(FALSE, TRUE, FALSE, TRUE))
  testthat::expect_identical(has_b, c(FALSE, FALSE, TRUE, TRUE))
})
