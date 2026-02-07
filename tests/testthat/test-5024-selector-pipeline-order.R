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
