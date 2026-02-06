test_that("adaptive log accessors and history return canonical shapes", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 3L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  logs <- adaptive_get_logs(state)
  expect_true(all(c("step_log", "round_log", "item_log") %in% names(logs)))
  expect_true(tibble::is_tibble(logs$step_log))
  expect_true(tibble::is_tibble(logs$round_log))

  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  expect_true(all(c("step_id", "pair_id", "A", "B", "Y", "status") %in% names(step_log)))
  expect_true(all(c("round_id", "step_id_at_refit") %in% names(round_log)))

  item_log <- adaptive_item_log(state)
  required_cols <- c(
    "refit_id",
    "item_id",
    "theta_mean",
    "theta_p2.5",
    "theta_p5",
    "theta_p50",
    "theta_p95",
    "theta_p97.5",
    "theta_sd",
    "rank_mean",
    "degree",
    "pos_count_A",
    "pos_count_B"
  )
  expect_true(all(required_cols %in% names(item_log)))
  expect_equal(nrow(item_log), state$n_items)
  expect_equal(sort(unique(item_log$item_id)), sort(as.character(state$item_ids)))

  item_log_1 <- adaptive_item_log(state, refit_id = 1L)
  expect_equal(nrow(item_log_1), state$n_items)

  stacked <- adaptive_item_log(state, stack = TRUE)
  expect_true("refit_id" %in% names(stacked))
  expect_equal(nrow(stacked), state$n_items * nrow(round_log))

  history <- adaptive_results_history(state, committed_only = TRUE)
  expect_equal(names(history), c("object1", "object2", "result"))
  expect_true(is.character(history$object1))
  expect_true(is.character(history$object2))
  expect_true(is.numeric(history$result))
  expect_true(all(history$result %in% c(0, 1)))
  expect_equal(nrow(history), sum(!is.na(step_log$pair_id)))

  missing_state <- state
  missing_state$step_log <- NULL
  expect_error(adaptive_step_log(missing_state), "step_log")

  missing_state <- state
  missing_state$item_log <- NULL
  expect_error(adaptive_item_log(missing_state), "item_log")
})
