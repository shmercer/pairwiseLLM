.test_samples_7206 <- function(ids) {
  tibble::tibble(ID = ids, text = paste0("txt-", ids))
}

test_that("bt_run_adaptive_core_linking validates key arguments", {
  ids <- c("A", "B", "C")
  samples <- .test_samples_7206(ids)

  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      fit_engine_running = "bogus",
      judge_fun = function(...) 0L,
      fit_fun = function(...) list(theta = tibble::tibble(ID = ids, theta = 0, pi = 1 / length(ids)))
    ),
    "should be one of"
  )
})
