test_that("simulation harness reproduces logs for identical seed tuples", {
  withr::local_seed(1)

  run_a <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 111L,
    judge_seed = 222L,
    n_steps = 9L,
    n_items = 10L
  )
  run_b <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 111L,
    judge_seed = 222L,
    n_steps = 9L,
    n_items = 10L
  )

  sig_a <- pairwiseLLM:::.adaptive_simulation_signature(run_a)
  sig_b <- pairwiseLLM:::.adaptive_simulation_signature(run_b)
  expect_equal(sig_a, sig_b)

  step_log <- run_a$step_log
  ok <- step_log[step_log$status == "ok", , drop = FALSE]
  if (nrow(ok) > 0L) {
    expect_true(all(ok$i != ok$j))
    expect_true(all(!is.na(ok$pair_id)))
  }
  bad <- step_log[step_log$status != "ok", , drop = FALSE]
  if (nrow(bad) > 0L) {
    expect_true(all(is.na(bad$pair_id)))
  }
})

test_that("simulation harness changes outcomes when seeds change", {
  withr::local_seed(1)

  base <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 111L,
    judge_seed = 222L,
    n_steps = 9L,
    n_items = 10L
  )
  changed <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 112L,
    judge_seed = 333L,
    n_steps = 9L,
    n_items = 10L
  )

  sig_base <- pairwiseLLM:::.adaptive_simulation_signature(base)
  sig_changed <- pairwiseLLM:::.adaptive_simulation_signature(changed)
  expect_false(identical(sig_base$step, sig_changed$step))

  for (log_tbl in list(base$step_log, changed$step_log)) {
    ok <- log_tbl[log_tbl$status == "ok", , drop = FALSE]
    if (nrow(ok) > 0L) {
      expect_true(all(ok$i != ok$j))
    }
  }
})
