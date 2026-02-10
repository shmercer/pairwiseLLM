test_that("benchmark link-mix returns empty output when no staged committed rows", {
  step_log <- tibble::tibble(
    round_id = c(NA_integer_, 1L),
    round_stage = c("local_link", "non_stage"),
    pair_id = c(1L, 2L),
    pair_type = c("local", "mid")
  )

  out <- pairwiseLLM:::.adaptive_link_mix_metrics(step_log)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("round_id", "round_stage", "pair_type", "committed", "proportion"))
})

test_that("benchmark metrics validates state and emits run/fallback/link/quota rows", {
  expect_error(pairwiseLLM:::.adaptive_benchmark_metrics(list()), "adaptive_state")

  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  out <- adaptive_rank_run_live(state, judge, n_steps = 4L, progress = "none")
  metrics <- pairwiseLLM:::.adaptive_benchmark_metrics(out)

  expect_true(all(c("metric_group", "metric", "value", "report_only") %in% names(metrics)))
  expect_true(any(metrics$metric_group == "run"))
  expect_true(any(metrics$metric_group == "fallback"))
  expect_true(any(metrics$metric_group == "link_mix"))
  expect_true(any(metrics$metric_group == "quota"))
})

test_that("candidate validators reject NA/empty ids and invalid numeric controls", {
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c("a", NA_character_),
      W_used = 2L,
      seed = 1L
    ),
    "must not contain missing values"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c("a", ""),
      W_used = 2L,
      seed = 1L
    ),
    "must not contain empty ids"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c("a", "b"),
      W_used = 2L,
      C_max = 0L,
      seed = 1L
    ),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c("a", "b"),
      W_used = 0L,
      seed = 1L
    ),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::generate_candidate_pairs(
      rank_mu = c("a", "b"),
      W_used = 2L,
      seed = NA_integer_
    ),
    "non-missing scalar integer"
  )
})

test_that("candidate generation from state validates state and ranks by trueskill mu", {
  expect_error(
    pairwiseLLM:::generate_candidate_pairs_from_state(list(), W_used = 2L, seed = 1L),
    "adaptive_state"
  )

  items <- make_test_items(4)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$trueskill_state <- NULL
  expect_error(
    pairwiseLLM:::generate_candidate_pairs_from_state(state, W_used = 2L, seed = 1L),
    "must be set"
  )

  state$trueskill_state <- list(items = tibble::tibble(item_id = 1:4))
  expect_error(
    pairwiseLLM:::generate_candidate_pairs_from_state(state, W_used = 2L, seed = 1L),
    "trueskill_state"
  )

  state$trueskill_state <- make_test_trueskill_state(
    items,
    mu = c(1, 4, 3, 2),
    sigma = rep(1, 4)
  )
  out <- pairwiseLLM:::generate_candidate_pairs_from_state(
    state,
    W_used = 3L,
    C_max = 100L,
    seed = 1L
  )

  expect_true(nrow(out) > 0L)
  expect_true(all(out$i < out$j))
  expect_true(any(out$i == "2" | out$j == "2"))
})

test_that("ordered keys and seed helpers cover null/invalid branches", {
  key <- pairwiseLLM:::make_ordered_key(c(1, 2), c(3, 4))
  expect_identical(key, c("1:3", "2:4"))

  expect_error(pairwiseLLM:::.adaptive_validate_seed(NA_integer_), "non-missing scalar integer")

  withr::local_seed(1)
  baseline_pre <- stats::runif(2)
  stats::runif(1)
  baseline_post <- stats::runif(2)

  withr::local_seed(1)
  pre <- stats::runif(2)
  pairwiseLLM:::.adaptive_with_seed(NULL, stats::runif(1))
  post <- stats::runif(2)

  expect_equal(pre, baseline_pre)
  expect_equal(post, baseline_post)
})
