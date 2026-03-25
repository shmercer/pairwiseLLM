test_that("new_adaptive_state builds a stable adaptive scaffold", {
  items <- make_test_items(3)
  now_fn <- function() as.POSIXct("2000-01-01", tz = "UTC")

  state <- pairwiseLLM:::new_adaptive_state(items, now_fn = now_fn)

  expect_true(inherits(state, "adaptive_state"))
  expect_equal(state$meta$schema_version, "adaptive-session")
  expect_equal(state$meta$now_fn(), now_fn())
  expect_equal(state$item_ids, as.character(items$item_id))
  expect_equal(state$items$set_id, rep(1L, nrow(items)))
  expect_equal(state$items$global_item_id, as.character(items$item_id))
  expect_equal(state$n_items, 3L)
  expect_true(tibble::is_tibble(state$history_pairs))
  expect_identical(as.integer(state$history_state$n_pairs), 0L)
  expect_true(inherits(state$trueskill_state, "trueskill_state"))
  expect_true(tibble::is_tibble(state$step_log))
  expect_true(tibble::is_tibble(state$round_log))
  expect_true(is.list(state$item_log))
  expect_true(tibble::is_tibble(state$item_step_log))
  expect_identical(state$controller$link_estimation_mode, "transform")
  expect_identical(state$controller$link_state_frozen_by_spoke, list())
})

test_that("new_adaptive_state rejects non-function now_fn", {
  items <- make_test_items(2)
  expect_error(
    pairwiseLLM:::new_adaptive_state(items, now_fn = NULL),
    "`now_fn` must be a function"
  )
  expect_error(
    pairwiseLLM:::new_adaptive_state(items, now_fn = Sys.time()),
    "`now_fn` must be a function"
  )
})

test_that("new_adaptive_state accepts character item_id values", {
  items <- tibble::tibble(item_id = c("S01", "S02"))
  state <- pairwiseLLM:::new_adaptive_state(items)
  expect_equal(state$item_ids, c("S01", "S02"))
  expect_equal(unname(state$item_index), c(1L, 2L))
})

test_that("new_adaptive_state keeps multi-set identifiers", {
  items <- tibble::tibble(
    item_id = c("a", "b", "c", "d"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("g_a", "g_b", "g_c", "g_d")
  )
  state <- pairwiseLLM:::new_adaptive_state(items)
  expect_equal(state$items$set_id, c(1L, 1L, 2L, 2L))
  expect_equal(state$items$global_item_id, c("g_a", "g_b", "g_c", "g_d"))
})

test_that("adaptive_rank_start stores linking run metadata", {
  items <- tibble::tibble(
    item_id = c("a", "b", "c", "d"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("g_a", "g_b", "g_c", "g_d")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  expect_equal(state$linking$run_mode, "link_one_spoke")
  expect_equal(state$linking$hub_id, 1L)
  expect_equal(state$linking$spoke_ids, 2L)
  expect_true(state$linking$is_multi_set)
})

test_that("adaptive_rank_start defaults multi-spoke linking to concurrent mode", {
  items <- tibble::tibble(
    item_id = c("a", "b", "c", "d", "e", "f"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("g_a", "g_b", "g_c", "g_d", "g_e", "g_f")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 2L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )

  expect_identical(state$controller$multi_spoke_mode, "concurrent")
})

test_that("adaptive_rank_start normalizes anchored-joint defaults", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 3L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "anchored_joint"
    )
  )

  expect_identical(state$controller$link_estimation_mode, "anchored_joint")
  expect_identical(state$controller$hub_lock_mode, "hard_lock")
  expect_true(is.na(state$controller$link_transform_policy))
  expect_true(is.na(state$controller$link_refit_mode))
  expect_true(is.na(state$controller$shift_only_theta_treatment))
  expect_identical(state$controller$anchored_joint_spoke_prior_scale, 1.0)
  expect_identical(state$controller$anchored_joint_sd_floor, 0.02)
  expect_identical(state$controller$anchored_joint_spoke_prior_fallback_sd, 1.0)
})
