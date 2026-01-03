test_that("bt_run_core_linking coalesces legacy appearance columns", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("tA", "tB"))
  batches <- list(c("A", "B"))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  td <- withr::local_tempdir()
  chk_path <- file.path(td, "run_state.rds")

  # Minimal checkpoint payload with legacy columns present
  chk <- list(
    run_type = "core_linking",
    ids = samples$ID,
    core_ids = c("A", "B"),
    batches = batches,
    timestamp = Sys.time(),
    completed = FALSE,
    fit_engine_running = "bt",
    state = tibble::tibble(
      batch_index = 0L,
      round_index = 0L,
      # legacy appearance columns:
      median_appearances = 5,
      p90_appearances = 7,
      max_appearances = 9,
      new_median_appearances = 2,
      new_p90_appearances = 3,
      new_max_appearances = 4,
      # new columns present but missing values:
      appear_p50 = NA_real_,
      appear_p90 = NA_real_,
      appear_max = NA_real_,
      new_appear_p50 = NA_real_,
      new_appear_p90 = NA_real_,
      new_appear_max = NA_real_,
      # IMPORTANT for current code path:
      stop_reason = NA_character_
    )
  )
  saveRDS(chk, chk_path)

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    round_size = 0,
    init_round_size = 0,
    max_rounds_per_batch = 0,
    checkpoint_dir = td,
    resume_from = td,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE,
    return_diagnostics = FALSE,
    include_residuals = FALSE
  )

  testthat::expect_equal(out$state$appear_p50[[1]], 5)
  testthat::expect_equal(out$state$appear_p90[[1]], 7)
  testthat::expect_equal(out$state$max_appearances[[1]], 9)
  testthat::expect_equal(out$state$new_appear_p50[[1]], 2)
  testthat::expect_equal(out$state$new_appear_p90[[1]], 3)
  testthat::expect_equal(out$state$new_max_appearances[[1]], 4)
})

