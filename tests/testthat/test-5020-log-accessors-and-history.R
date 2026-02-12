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
  expect_true(is.list(logs$item_log))
  if (length(logs$item_log) > 0L) {
    expected_item_cols <- pairwiseLLM:::.adaptive_item_log_columns()
    expect_true(all(vapply(logs$item_log, tibble::is_tibble, logical(1))))
    expect_true(all(vapply(
      logs$item_log,
      function(tbl) identical(names(tbl), expected_item_cols),
      logical(1)
    )))
  }

  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  expect_true(all(c("step_id", "pair_id", "A", "B", "Y", "status", "pair_type") %in% names(step_log)))
  expect_true(all(c("refit_id", "round_id_at_refit", "step_id_at_refit") %in% names(round_log)))

  item_log <- adaptive_item_log(state)
  required_cols <- pairwiseLLM:::.adaptive_item_log_columns()
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

test_that("invalid-step rows keep committed-only fields as NA", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items)
  invalid_judge <- function(A, B, state, ...) {
    list(is_valid = FALSE, invalid_reason = "invalid_contract")
  }

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    invalid_judge,
    n_steps = 1L,
    progress = "none"
  )

  step_row <- adaptive_step_log(out)[1L, , drop = FALSE]
  expect_true(is.na(step_row$pair_id[[1L]]))
  expect_true(is.na(step_row$Y[[1L]]))
  expect_true(is.na(step_row$p_ij[[1L]]))
  expect_true(is.na(step_row$U0_ij[[1L]]))
})

test_that("star_override_used follows commit and no-selection semantics", {
  committed_state <- adaptive_rank_start(make_test_items(3))
  judge_ok <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  committed_state <- adaptive_rank_run_live(
    committed_state,
    judge_ok,
    n_steps = 1L,
    progress = "none"
  )
  committed_row <- adaptive_step_log(committed_state)[1L, , drop = FALSE]
  expect_false(is.na(committed_row$pair_id[[1L]]))
  expect_identical(committed_row$star_override_used[[1L]], FALSE)

  invalid_state <- adaptive_rank_start(make_test_items(3))
  judge_invalid <- make_deterministic_judge("invalid")

  withr::local_seed(1)
  invalid_state <- adaptive_rank_run_live(
    invalid_state,
    judge_invalid,
    n_steps = 1L,
    progress = "none"
  )
  invalid_row <- adaptive_step_log(invalid_state)[1L, , drop = FALSE]
  expect_true(is.na(invalid_row$pair_id[[1L]]))
  expect_false(isTRUE(invalid_row$candidate_starved[[1L]]))
  expect_identical(invalid_row$star_override_used[[1L]], FALSE)

  starved_state <- pairwiseLLM:::new_adaptive_state(make_test_items(2))
  starved_state$history_pairs <- tibble::tibble(
    A_id = c("1", "1", "1"),
    B_id = c("2", "2", "2")
  )

  starved_out <- adaptive_rank_run_live(
    starved_state,
    judge_ok,
    n_steps = 1L,
    progress = "none"
  )
  starved_row <- adaptive_step_log(starved_out)[1L, , drop = FALSE]
  expect_true(is.na(starved_row$pair_id[[1L]]))
  expect_true(isTRUE(starved_row$candidate_starved[[1L]]))
  expect_true(is.na(starved_row$star_override_used[[1L]]))
})
