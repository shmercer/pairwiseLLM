test_that("bt_run_core_linking supports Rank Centrality as a running engine", {
  testthat::skip_if_not_installed("BradleyTerry2")

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  # deterministic judge: ID1 always wins
  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = list(batch_1 = "C"),
    core_ids = c("A", "B"),
    judge_fun = judge_fun,
    fit_engine_running = "rank_centrality",
    max_rounds_per_batch = 2,
    round_size = 2,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 123,
    verbose = FALSE
  )

  expect_true(is.list(out$fits) && length(out$fits) >= 1L)
  expect_true("engine_running" %in% names(out$fits[[1]]))
  expect_identical(out$fits[[1]]$engine_running, "rank_centrality")
  expect_true(all(c("theta_rc", "pi_rc") %in% names(out$fits[[1]]$theta)))

  expect_true(is.data.frame(out$estimates))
  expect_true(all(c("theta_bt_firth", "se_bt_firth", "theta_rc", "pi_rc") %in% names(out$estimates)))
})
