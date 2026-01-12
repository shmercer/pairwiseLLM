.test_samples_7204 <- function(ids) {
  tibble::tibble(ID = ids, text = paste0("txt-", ids))
}

test_that("bt_run_core_linking validates key arguments", {
  ids <- c("A", "B", "C")
  samples <- .test_samples_7204(ids)
  batches <- list(c("A", "B"), c("C"))

  expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      fit_engine_running = "bogus",
      judge_fun = function(...) 0L
    ),
    "should be one of"
  )

  expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      linking = "sometimes",
      judge_fun = function(...) 0L
    ),
    "should be one of"
  )
})

test_that("bt_run_core_linking upgrades legacy completed checkpoints", {
  ids <- c("A", "B", "C")
  samples <- .test_samples_7204(ids)
  batches <- list(c("A", "B"), c("C"))
  core_ids <- c("A", "B")

  tmp <- withr::local_tempdir()

  chk <- list(
    run_type = "core_linking",
    ids = ids,
    completed = TRUE,
    core_ids = core_ids,
    results = tibble::tibble(ID1 = character(), ID2 = character(), winner = integer()),
    out = list(
      core_ids = core_ids,
      batches = batches,
      results = tibble::tibble(ID1 = character(), ID2 = character(), winner = integer())
    )
  )

  pairwiseLLM:::.bt_write_checkpoint(tmp, chk)

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = function(...) 0L,
    resume_from = tmp
  )

  expect_true(is.list(out))
  expect_true(all(c("results", "fits", "final_fits", "metrics", "state") %in% names(out)))
  expect_true(all(c("estimates", "theta", "pairing_diagnostics") %in% names(out)))
})