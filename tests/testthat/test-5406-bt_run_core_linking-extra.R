test_that("bt_run_core_linking validates key inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      core_size = 2L,
      linking_min_n = 0L
    ),
    "linking_min_n"
  )

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      core_size = 2L,
      reference_max_abs = -1
    ),
    "reference_max_abs"
  )
})

test_that("bt_run_core_linking returns completed checkpoint and upgrades output", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  tmp <- withr::local_tempdir()
  payload <- list(
    run_type = "core_linking",
    ids = c("A", "B"),
    completed = TRUE,
    output = list(
      theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1)),
      diagnostics = tibble::tibble()
    )
  )
  pairwiseLLM:::.bt_write_checkpoint(tmp, payload)

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(c("A", "B")),
    judge_fun = judge_fun,
      core_size = 2L,
    resume_from = tmp,
    checkpoint_dir = tmp,
    round_size = 0L,
    max_rounds_per_batch = 0L,
    return_diagnostics = TRUE
  )

  expect_true(is.list(out))
  expect_true("estimates" %in% names(out))
  expect_true(is.null(out$estimates) || is.data.frame(out$estimates))
})

test_that("bt_run_core_linking errors when engine fit returns malformed theta", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun_bad_theta <- function(results, ...) {
    # Return a theta object with the wrong column names to trigger validation.
    list(theta = tibble::tibble(bad = "A", val = 0.0))
  }

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      judge_fun = judge_fun,
      batches = list(c("A", "B")),
      core_size = 2L,
      fit_fun = fit_fun_bad_theta,
      checkpoint_dir = ckpt_dir
    ),
    "fit_fun\\(\\)\\$theta.*columns ID and theta"
  )

  # fit_bt_model malformed return (engine must be valid for match.arg)
  testthat::local_mocked_bindings(
    fit_bt_model = function(...) list(theta = tibble::tibble(foo = 1)),
    .package = "pairwiseLLM"
  )

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      core_size = 2L,
      engine = "auto",
      round_size = 1L,
      max_rounds_per_batch = 1L,
      min_rounds = 0L
    ),
    "fit_fun\\(\\)\\$theta.*columns ID and theta"
  )
})
