test_that("summarize_refits is a pure view over canonical round_log", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  round_log <- adaptive_round_log(out)
  summary <- summarize_refits(out)
  expect_identical(summary, round_log)

  hacked <- out
  hacked$round_log$global_identified[[1L]] <- TRUE
  hacked$round_log$long_quota_raw[[1L]] <- 99L
  hacked_view <- summarize_refits(hacked)
  expect_identical(hacked_view$global_identified[[1L]], TRUE)
  expect_identical(hacked_view$long_quota_raw[[1L]], 99L)
})

test_that("summarize_refits preserves rows and selected columns deterministically", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(2)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  round_log <- adaptive_round_log(out)
  expected_last <- utils::tail(round_log, 1L)
  last_full <- summarize_refits(out, last_n = 1L)
  expect_identical(last_full, expected_last)

  compact <- summarize_refits(out, last_n = 1L, include_optional = FALSE)
  expected_compact <- expected_last |>
    dplyr::select(dplyr::any_of(names(compact)))
  expect_identical(compact, expected_compact)
})

test_that("adaptive_round_log reconstruction is explicit and summarize_refits stays a pure view", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(3)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  raw_round_log <- adaptive_round_log(out)
  reconstructed_round_log <- adaptive_round_log(out, reconstruct_deferred = TRUE)

  expect_identical(summarize_refits(out), raw_round_log)
  expect_true(all(is.na(raw_round_log$cov_trace_theta)))
  expect_true(all(is.finite(reconstructed_round_log$cov_trace_theta)))
})
