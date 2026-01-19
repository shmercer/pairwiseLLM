testthat::test_that("e2e adaptive mini-run completes with a valid stop reason", {
  run <- e2e_run_locked_scenario(seed = 1601)
  out <- run$out
  state <- out$state
  v3_config <- state$config$v3
  selection_log <- run$selection_log

  testthat::expect_identical(out$next_action$action, "done")
  testthat::expect_true(out$next_action$reason %in% c(
    "hard_cap_40pct",
    "v3_converged",
    "diagnostics_failed"
  ))
  testthat::expect_identical(as.character(state$stop_reason), as.character(out$next_action$reason))

  testthat::expect_true(any(state$history_pairs$phase == "phase1"))
  testthat::expect_true(any(state$history_pairs$phase %in% c("phase2", "phase3")))

  warm_pairs <- dplyr::filter(state$history_pairs, .data$phase == "phase1")
  warm_deg <- vapply(state$ids, function(id) {
    sum(warm_pairs$A_id == id | warm_pairs$B_id == id)
  }, integer(1L))
  testthat::expect_true(all(warm_deg >= as.integer(v3_config$min_degree)))

  testthat::expect_true(nrow(state$config$round_log) >= 1L)
  testthat::expect_true(nrow(selection_log) > 0L)

  has_exploration <- any(is.na(selection_log$utility))
  has_exploitation <- any(is.finite(selection_log$utility))
  testthat::expect_true(has_exploration)
  testthat::expect_true(has_exploitation)
})
