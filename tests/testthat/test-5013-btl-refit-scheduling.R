test_that("maybe_refit_btl follows committed-pair cadence", {
  items <- make_test_items(6)
  state <- pairwiseLLM:::new_adaptive_state(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  for (step in seq_len(9L)) {
    state <- pairwiseLLM:::run_one_step(state, judge)
    refit <- pairwiseLLM:::maybe_refit_btl(
      state,
      config = list(refit_pairs_target = 3L),
      fit_fn = stub$fit_fn
    )
    state <- refit$state
    if (step < 3L) {
      expect_false(refit$refit_performed)
    }
  }

  expect_equal(stub$get_calls(), 3L)
  expect_equal(state$refit_meta$last_refit_M_done, 9L)
  expect_equal(state$refit_meta$last_refit_step, 9L)
})

test_that("invalid steps do not advance refit eligibility", {
  items <- make_test_items(6)
  state <- pairwiseLLM:::new_adaptive_state(items)
  judge <- make_deterministic_judge("i_wins")
  judge_invalid <- make_deterministic_judge("invalid")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- pairwiseLLM:::run_one_step(state, judge)
  state <- pairwiseLLM:::run_one_step(state, judge_invalid)
  refit_1 <- pairwiseLLM:::maybe_refit_btl(
    state,
    config = list(refit_pairs_target = 2L),
    fit_fn = stub$fit_fn
  )
  expect_false(refit_1$refit_performed)
  expect_equal(stub$get_calls(), 0L)

  state <- pairwiseLLM:::run_one_step(state, judge)
  refit_2 <- pairwiseLLM:::maybe_refit_btl(
    state,
    config = list(refit_pairs_target = 2L),
    fit_fn = stub$fit_fn
  )
  expect_true(refit_2$refit_performed)
  expect_equal(stub$get_calls(), 1L)
  expect_equal(refit_2$state$refit_meta$last_refit_M_done, 2L)
})

test_that("linking phase A refit target uses active set size", {
  items <- tibble::tibble(
    item_id = as.character(1:100),
    set_id = c(rep(1L, 50L), rep(2L, 50L)),
    global_item_id = paste0("g", 1:100)
  )
  state <- adaptive_rank_start(
    items,
    seed = 1L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  target <- pairwiseLLM:::.adaptive_refit_pairs_target(state, list())
  expect_identical(target, 25L)
})

test_that("linking phase A refit cadence is tracked per active set", {
  items <- tibble::tibble(
    item_id = as.character(1:8),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = paste0("g", 1:8)
  )
  state <- adaptive_rank_start(
    items,
    seed = 22L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("pending_finalization", "pending_finalization"),
    validation_message = c("pending", "pending"),
    artifact_path = c(NA_character_, NA_character_)
  )
  stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_deterministic_judge("i_wins")

  for (idx in seq_len(2L)) {
    state <- pairwiseLLM:::run_one_step(state, judge)
  }
  refit_1 <- pairwiseLLM:::maybe_refit_btl(
    state,
    config = list(refit_pairs_target = 2L),
    fit_fn = stub$fit_fn
  )
  expect_true(refit_1$refit_performed)
  expect_identical(refit_1$state$refit_meta$last_refit_M_done_by_phase_a_set[["1"]], 2L)
  expect_identical(refit_1$state$refit_meta$last_refit_M_done, 0L)

  state <- refit_1$state
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "pending_finalization"),
    validation_message = c("ready", "pending"),
    artifact_path = c(NA_character_, NA_character_)
  )

  for (idx in seq_len(2L)) {
    state <- pairwiseLLM:::run_one_step(state, judge)
  }
  refit_2 <- pairwiseLLM:::maybe_refit_btl(
    state,
    config = list(refit_pairs_target = 2L),
    fit_fn = stub$fit_fn
  )
  expect_true(refit_2$refit_performed)
  expect_identical(refit_2$state$refit_meta$last_refit_M_done_by_phase_a_set[["2"]], 2L)
  expect_equal(stub$get_calls(), 2L)
})

test_that(".adaptive_btl_extract_fit_contract handles wrapped fit outputs", {
  fit_direct <- list(theta_draws = matrix(0, nrow = 2L, ncol = 2L))
  fit_named <- list(theta_draws = matrix(1, nrow = 2L, ncol = 2L))
  fit_last <- list(theta_draws = matrix(2, nrow = 2L, ncol = 2L))

  out_direct <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(fit_direct)
  out_fit <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(list(fit = fit_named))
  out_fits <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(list(fits = list(fit_named, fit_last)))

  expect_identical(out_direct, fit_direct)
  expect_identical(out_fit, fit_named)
  expect_identical(out_fits, fit_last)
})
