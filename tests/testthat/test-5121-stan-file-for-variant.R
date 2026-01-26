testthat::test_that("stan_file_for_variant resolves stan paths", {
  variants <- c("btl", "btl_e", "btl_b", "btl_e_b")
  paths <- vapply(variants, pairwiseLLM:::stan_file_for_variant, character(1L))

  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_match(paths[[1L]], "stan[\\\\/]btl\\.stan$")
  testthat::expect_match(paths[[2L]], "stan[\\\\/]btl_e\\.stan$")
  testthat::expect_match(paths[[3L]], "stan[\\\\/]btl_b\\.stan$")
  testthat::expect_match(paths[[4L]], "stan[\\\\/]btl_e_b\\.stan$")
})

testthat::test_that("stan_file_for_variant rejects invalid variants", {
  testthat::expect_error(
    pairwiseLLM:::stan_file_for_variant("bad"),
    "model_variant"
  )
})
