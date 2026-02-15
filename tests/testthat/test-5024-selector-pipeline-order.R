test_that("selector stage scores before hard-filter feasibility checks", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  ids <- as.character(state$item_ids)
  named_zero <- stats::setNames(rep.int(0L, length(ids)), ids)
  counts <- list(
    deg = named_zero,
    posA = named_zero,
    posB = named_zero,
    pair_count = stats::setNames(1L, "1:2"),
    pair_last_order = list()
  )
  stage <- list(name = "base_window", W_used = 2L, dup_policy = "default", idx = 1L)
  config <- pairwiseLLM:::adaptive_defaults(length(ids))
  candidates <- tibble::tibble(i = c("1", "1"), j = c("2", "3"))

  scored_n <- 0L
  out <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      scored_n <<- nrow(candidates)
      candidates$u0 <- rep(0.25, nrow(candidates))
      candidates
    },
    trueskill_win_probability = function(i, j, trueskill_state) 0.5,
    pairwiseLLM:::.adaptive_select_stage(
      stage = stage,
      state = state,
      config = config,
      round = state$round,
      history = tibble::tibble(A_id = character(), B_id = character()),
      counts = counts,
      step_id = 1L,
      seed_base = 1L,
      candidates = candidates
    ),
    .env = asNamespace("pairwiseLLM")
  )

  expect_equal(scored_n, 2L)
  expect_equal(out$counts$n_candidates_generated, 2L)
  expect_equal(out$counts$n_candidates_after_hard_filters, 1L)
})

test_that("hard-filter count includes round exposure filtering", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$repeat_in_round_budget <- 0L
  state$round$repeat_in_round_used <- 0L

  ids <- as.character(state$item_ids)
  named_zero <- stats::setNames(rep.int(0L, length(ids)), ids)
  counts <- list(
    deg = named_zero,
    posA = named_zero,
    posB = named_zero,
    pair_count = integer(),
    pair_last_order = list()
  )
  stage <- list(name = "base_window", W_used = 2L, dup_policy = "default", idx = 1L)
  config <- pairwiseLLM:::adaptive_defaults(length(ids))
  candidates <- tibble::tibble(i = c("1", "2"), j = c("2", "3"))

  out <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$u0 <- rep(0.25, nrow(candidates))
      candidates
    },
    trueskill_win_probability = function(i, j, trueskill_state) 0.5,
    pairwiseLLM:::.adaptive_select_stage(
      stage = stage,
      state = state,
      config = config,
      round = state$round,
      history = tibble::tibble(A_id = character(), B_id = character()),
      counts = counts,
      step_id = 1L,
      seed_base = 1L,
      candidates = candidates
    ),
    .env = asNamespace("pairwiseLLM")
  )

  expect_equal(out$counts$n_candidates_generated, 2L)
  expect_equal(out$counts$n_candidates_after_hard_filters, 1L)
})

test_that("first-seen tied-imbalance ordering uses seeded pair tie-break", {
  ids <- as.character(seq_len(12L))
  named_zero <- stats::setNames(rep.int(0L, length(ids)), ids)
  pairs <- utils::combn(ids, 2L, simplify = FALSE)
  pairs <- pairs[seq_len(50L)]

  draw_orientation <- function(seed_base) {
    vapply(pairs, function(pair_ids) {
      out <- pairwiseLLM:::.adaptive_assign_order(
        pair = tibble::tibble(i = pair_ids[[1L]], j = pair_ids[[2L]]),
        posA = named_zero,
        posB = named_zero,
        pair_last_order = list(),
        seed_base = as.integer(seed_base)
      )
      as.integer(out[["A_id"]]) < as.integer(out[["B_id"]])
    }, logical(1L))
  }

  orientation_a <- draw_orientation(seed_base = 17L)
  orientation_b <- draw_orientation(seed_base = 17L)

  expect_identical(orientation_a, orientation_b)
  expect_false(all(orientation_a))
})

test_that("selection paths pass canonical seed_base into adaptive_assign_order", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$meta$seed <- 77L

  captured_seed <- NA_integer_
  out <- testthat::with_mocked_bindings(
    .adaptive_select_stage = function(stage, state, config, controller, generation_stage, round, history, counts,
                                      step_id, seed_base, candidates = NULL) {
      list(
        selected = tibble::tibble(i = "1", j = "2", u0 = 0.25, p = 0.5),
        counts = list(
          n_candidates_generated = 1L,
          n_candidates_after_hard_filters = 1L,
          n_candidates_after_duplicates = 1L,
          n_candidates_after_star_caps = 1L,
          n_candidates_scored = 1L
        ),
        star_caps = list(rejects = 0L, reject_items = character(), reject_items_count = 0L),
        recent_deg = stats::setNames(rep.int(0L, length(state$item_ids)), as.character(state$item_ids)),
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_
      )
    },
    .adaptive_assign_order = function(pair, posA, posB, pair_last_order, seed_base = 1L) {
      captured_seed <<- as.integer(seed_base)
      c(A_id = "1", B_id = "2")
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = tibble::tibble(i = "1", j = "2")),
    .package = "pairwiseLLM"
  )

  expect_identical(captured_seed, 77L)
  expect_identical(out$A, 1L)
  expect_identical(out$B, 2L)
})
