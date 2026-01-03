testthat::test_that("bt_run_adaptive_core_linking validates key inputs", {
  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      theta = tibble::tibble(ID = ids, theta = rep(0, length(ids)), se = rep(NA_real_, length(ids))),
      reliability = 1
    )
  }

  testthat::expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = tibble::tibble(ID = c("A", "A"), text = c("a", "a2")),
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      core_method = "random",
      core_size = 2,
      init_round_size = 0,
      round_size = 0
    ),
    "`samples\\$ID` must be unique"
  )

  testthat::expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = tibble::tibble(ID = c("A", "B"), text = c("a", "b")),
      batches = list(c("A", "C")),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      core_ids = c("A", "B"),
      init_round_size = 0,
      round_size = 0
    ),
    "All batch IDs must be present"
  )
})

testthat::test_that("bt_run_adaptive_core_linking is resilient to malformed fit output", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  batches <- list(c("A", "B"))

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun_bad <- function(bt_data, ...) list(reliability = 1)

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    fit_fun = fit_fun_bad,
    core_ids = c("A", "B"),
    init_round_size = 1,
    round_size = 0,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE,
    return_diagnostics = FALSE,
    include_residuals = FALSE
  )

  testthat::expect_true(length(out$fits) >= 1L)
  th <- out$fits[[1]]$theta
  testthat::expect_true(all(c("ID", "theta", "se") %in% names(th)))
  testthat::expect_equal(nrow(th), 2L)
})

testthat::test_that("bt_run_adaptive_core_linking adds missing se column in fit$theta", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  batches <- list(c("A", "B"))

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun_no_se <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(theta = tibble::tibble(ID = ids, theta = seq_along(ids)), reliability = 1)
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    fit_fun = fit_fun_no_se,
    core_ids = c("A", "B"),
    init_round_size = 1,
    round_size = 0,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE,
    return_diagnostics = FALSE,
    include_residuals = FALSE
  )

  testthat::expect_true(length(out$fits) >= 1L)
  th <- out$fits[[1]]$theta
  testthat::expect_true(all(c("ID", "theta", "se") %in% names(th)))
  testthat::expect_equal(nrow(th), 2L)
})

