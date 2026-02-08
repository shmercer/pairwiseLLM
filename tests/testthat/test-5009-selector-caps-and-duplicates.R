test_that("selector respects star caps and duplicate limits", {
  items <- make_test_items(5)
  trueskill_state <- make_test_trueskill_state(items)

  history <- tibble::tibble(
    A_id = rep(1, 18),
    B_id = rep(2, 18)
  )
  state <- make_test_state(items, trueskill_state, history = history)

  out <- pairwiseLLM:::select_next_pair(state)

  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
  expect_true(out$i != out$j)
  expect_true(out$i != 1L)
  expect_true(out$j != 1L)
})

test_that("selector reports starvation when no pairs are eligible", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items)
  history <- tibble::tibble(
    A_id = c(1, 2, 1),
    B_id = c(2, 1, 2)
  )
  state <- make_test_state(items, trueskill_state, history = history)

  out <- pairwiseLLM:::select_next_pair(state)

  expect_true(out$candidate_starved)
  expect_true(is.na(out$i))
  expect_true(is.na(out$j))
})

test_that("selector excludes items already used in round by default", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$per_round_item_uses[["2"]] <- 1L
  state$round$repeat_in_round_used <- state$round$repeat_in_round_budget

  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)

  expect_false(out$i %in% c(1L, 2L))
  expect_false(out$j %in% c(1L, 2L))
})

test_that("selector blocks repeats while non-repeat candidates are available", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(
      A_id = c("1", "2", "2", "3", "3"),
      B_id = c("4", "4", "5", "5", "6")
    )
  )
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$repeat_in_round_used <- 0L
  state$round$repeat_in_round_budget <- 2L

  out <- pairwiseLLM:::select_next_pair(state, step_id = 2L)

  expect_false(out$candidate_starved)
  expect_false(out$i %in% c(1L))
  expect_false(out$j %in% c(1L))
})

test_that("repeat exposure can still starve when repeated endpoints are not recent-underrepresented", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(
      A_id = c("2", "2", "2", "3", "3", "4"),
      B_id = c("3", "4", "5", "4", "5", "5")
    )
  )
  state$round$staged_active <- TRUE
  state$round$repeat_in_round_used <- 0L
  state$round$repeat_in_round_budget <- 2L
  state$round$per_round_item_uses[] <- 1L

  out <- pairwiseLLM:::select_next_pair(state, step_id = 3L)

  expect_true(out$candidate_starved)
  expect_true(is.na(out$i))
  expect_true(is.na(out$j))
})

test_that("repeat exposure filter enforces repeat slots per repeated endpoint", {
  round <- list(
    per_round_item_uses = c("1" = 1L, "2" = 1L, "3" = 0L, "4" = 0L),
    repeat_in_round_budget = 2L,
    repeat_in_round_used = 1L
  )
  defaults <- pairwiseLLM:::adaptive_defaults(4L)
  recent_deg <- c("1" = 0, "2" = 0, "3" = 0, "4" = 0)
  candidates <- tibble::tibble(
    i = c("1", "1", "3"),
    j = c("2", "3", "4")
  )

  out <- pairwiseLLM:::.adaptive_round_exposure_filter(
    candidates = candidates,
    round = round,
    recent_deg = recent_deg,
    defaults = defaults,
    allow_repeat_pressure = TRUE
  )

  expect_true(any(out$i == "3" & out$j == "4"))
  expect_true(any(out$i == "1" & out$j == "3"))
  expect_false(any(out$i == "1" & out$j == "2"))
})

test_that("repeat exposure filter requires repeated endpoints to be recent-underrepresented", {
  round <- list(
    per_round_item_uses = c("1" = 1L, "2" = 0L, "3" = 0L, "4" = 0L),
    repeat_in_round_budget = 2L,
    repeat_in_round_used = 0L
  )
  defaults <- pairwiseLLM:::adaptive_defaults(4L)
  candidates <- tibble::tibble(i = "1", j = "2")
  recent_deg <- c("1" = 10, "2" = 0, "3" = 0, "4" = 0)

  out <- pairwiseLLM:::.adaptive_round_exposure_filter(
    candidates = candidates,
    round = round,
    recent_deg = recent_deg,
    defaults = defaults,
    allow_repeat_pressure = TRUE
  )

  expect_equal(nrow(out), 0L)
})

test_that("repeat exposure filter uses configured underrepresentation quantile", {
  round <- list(
    per_round_item_uses = c("1" = 1L, "2" = 0L, "3" = 0L, "4" = 0L),
    repeat_in_round_budget = 2L,
    repeat_in_round_used = 0L
  )
  candidates <- tibble::tibble(i = "1", j = "2")
  recent_deg <- c("1" = 5, "2" = 0, "3" = 0, "4" = 0)

  defaults_low <- pairwiseLLM:::adaptive_defaults(4L)
  defaults_low$exposure_underrep_q <- 0.25
  out_low <- pairwiseLLM:::.adaptive_round_exposure_filter(
    candidates = candidates,
    round = round,
    recent_deg = recent_deg,
    defaults = defaults_low,
    allow_repeat_pressure = TRUE
  )
  expect_equal(nrow(out_low), 0L)

  defaults_high <- pairwiseLLM:::adaptive_defaults(4L)
  defaults_high$exposure_underrep_q <- 1
  out_high <- pairwiseLLM:::.adaptive_round_exposure_filter(
    candidates = candidates,
    round = round,
    recent_deg = recent_deg,
    defaults = defaults_high,
    allow_repeat_pressure = TRUE
  )
  expect_equal(nrow(out_high), 1L)
})

test_that("underrepresented set follows min-degree plus one rule", {
  deg <- c("1" = 0L, "2" = 1L, "3" = 2L, "4" = 4L)
  out <- pairwiseLLM:::.adaptive_underrep_set(deg)

  expect_equal(sort(out), c("1", "2"))
})

test_that("repeat fallback is available when base candidates are later filtered out", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(
      A_id = c("2", "3", "2", "4", "3", "4"),
      B_id = c("3", "2", "4", "2", "4", "3")
    )
  )
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$repeat_in_round_budget <- 2L
  state$round$repeat_in_round_used <- 0L
  state$round$stage_index <- 4L

  cand <- tibble::tibble(
    i = c("1", "1", "1", "2", "2", "3"),
    j = c("2", "3", "4", "3", "4", "4")
  )
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand)

  expect_false(out$candidate_starved)
  expect_true(out$i == 1L || out$j == 1L)
})
