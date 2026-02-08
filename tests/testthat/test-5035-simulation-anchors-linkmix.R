test_that("anchor and link-mix traces are reproducible", {
  withr::local_seed(1)

  run_1 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "anchor_link_mix",
    run_seed = 33L,
    judge_seed = 44L,
    n_steps = 60L
  )
  run_2 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "anchor_link_mix",
    run_seed = 33L,
    judge_seed = 44L,
    n_steps = 60L
  )

  expect_equal(run_1$anchor_trace, run_2$anchor_trace)

  link_1 <- pairwiseLLM:::.adaptive_link_mix_metrics(run_1$step_log)
  link_2 <- pairwiseLLM:::.adaptive_link_mix_metrics(run_2$step_log)
  expect_equal(link_1, link_2)
  expect_true(nrow(link_1) > 0L)

  if (nrow(link_1) > 0L) {
    stage_totals <- link_1 |>
      dplyr::group_by(.data$round_id, .data$round_stage) |>
      dplyr::summarise(total = sum(.data$proportion), .groups = "drop")
    expect_true(all(abs(stage_totals$total - 1) < 1e-8))
  }

  anchors <- run_1$state$round$anchor_ids %||% character()
  expect_true(length(anchors) >= 1L)
  expect_true(length(anchors) < run_1$state$n_items)
})
