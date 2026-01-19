testthat::test_that("e2e adaptive run is deterministic under identical seeds", {
  seed <- 1603

  run1 <- e2e_run_locked_scenario(seed = seed)
  run2 <- e2e_run_locked_scenario(seed = seed)

  out1 <- run1$out
  out2 <- run2$out

  seq1 <- dplyr::select(
    out1$state$history_pairs,
    "unordered_key",
    "A_id",
    "B_id",
    "phase",
    "iter"
  )
  seq2 <- dplyr::select(
    out2$state$history_pairs,
    "unordered_key",
    "A_id",
    "B_id",
    "phase",
    "iter"
  )

  testthat::expect_identical(seq1, seq2)
  testthat::expect_identical(as.character(out1$next_action$reason), as.character(out2$next_action$reason))

  sel1 <- dplyr::select(
    run1$selection_log,
    "batch_index",
    "unordered_key",
    "A_id",
    "B_id",
    "utility",
    "p_mean",
    "count_before"
  )
  sel2 <- dplyr::select(
    run2$selection_log,
    "batch_index",
    "unordered_key",
    "A_id",
    "B_id",
    "utility",
    "p_mean",
    "count_before"
  )
  testthat::expect_identical(sel1, sel2)
})
