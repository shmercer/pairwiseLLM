test_that("select_next_pair does not leak explore metadata across fallback stages", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(A_id = c("1", "1"), B_id = c("2", "2"))
  )

  candidates <- tibble::tibble(i = "1", j = "2")
  draws <- c(TRUE, FALSE, FALSE)
  draw_idx <- 0L

  out <- testthat::with_mocked_bindings(
    .adaptive_with_seed = function(seed, expr) {
      draw_idx <<- draw_idx + 1L
      if (draw_idx <= length(draws)) {
        return(draws[[draw_idx]])
      }
      eval.parent(substitute(expr))
    },
    .adaptive_underrep_set = function(deg) {
      "3"
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = candidates),
    .env = asNamespace("pairwiseLLM")
  )

  expect_equal(out$fallback_used, "expand_locality")
  if (isTRUE(out$is_explore_step)) {
    expect_true(out$explore_reason %in% c("probabilistic", "coverage_quota_override"))
    expect_true(is.na(out$explore_mode) || out$explore_mode %in% c("local", "nonlocal"))
  } else {
    expect_true(is.na(out$explore_mode))
    expect_true(is.na(out$explore_reason))
  }
})

test_that("select_next_pair reports coherent explore metadata when exploring", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  seen <- NULL
  for (step_id in seq_len(200L)) {
    out <- pairwiseLLM:::select_next_pair(state, step_id = step_id)
    if (isTRUE(out$is_explore_step)) {
      seen <- out
      break
    }
  }

  expect_false(is.null(seen))
  expect_true(seen$explore_reason %in% c("probabilistic", "coverage_quota_override"))
  expect_true(is.na(seen$explore_mode) || seen$explore_mode %in% c("local", "nonlocal"))
})
