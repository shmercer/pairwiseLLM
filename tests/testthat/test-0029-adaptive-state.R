test_that("legacy scaffold state constructors fail loudly", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_state_new(samples, config = list()),
    "Legacy scaffold state constructors are disabled"
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_state_save(list(), tempfile(fileext = ".rds")),
    "Legacy scaffold state serialization is disabled"
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_state_load(tempfile(fileext = ".rds")),
    "Legacy scaffold state serialization is disabled"
  )
})

test_that("legacy scaffold stopping helpers fail loudly", {
  expect_error(
    pairwiseLLM:::near_stop_from_state(list()),
    "Legacy scaffold stopping helpers are disabled"
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_compute_stop_metrics(list(), list(), tibble::tibble(), list()),
    "Legacy scaffold stopping helpers are disabled"
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_should_stop(list(), list(), list()),
    "Legacy scaffold stopping helpers are disabled"
  )
})
