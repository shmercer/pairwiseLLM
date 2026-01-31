make_mcmc_results_tbl <- function() {
  tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "B",
    winner_pos = 2L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )
}

testthat::test_that("btl mcmc helpers prepare data and model code", {
  results <- make_mcmc_results_tbl()
  prep <- pairwiseLLM:::.btl_mcmc_v3_prepare_bt_data(results, ids = c("A", "B"))

  expect_equal(prep$N, 2L)
  expect_equal(prep$A, 1L)
  expect_equal(prep$B, 2L)
  expect_equal(prep$Y, 0L)
  expect_equal(prep$item_id, c("A", "B"))

  code <- pairwiseLLM:::.btl_mcmc_v3_model_code()
  expect_true(is.character(code))
  expect_match(code, "data \\{")
  expect_match(code, "parameters \\{")
})

testthat::test_that("btl mcmc helper validates ids coverage", {
  results <- make_mcmc_results_tbl()
  results$B_id <- "C"
  results$better_id <- "C"

  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_prepare_bt_data(results, ids = c("A", "B")),
    "contained in `ids`"
  )
})

testthat::test_that("btl mcmc requires cmdstanr", {
  if ("cmdstanr" %in% loadedNamespaces()) {
    testthat::skip("cmdstanr already loaded; cannot simulate missing namespace safely.")
  }
  empty_lib <- withr::local_tempdir()
  withr::local_libpaths(new = empty_lib, action = "replace")
  expect_error(pairwiseLLM:::.btl_mcmc_require_cmdstanr())
})
