test_that("global-safe candidates stay capped and deterministic", {
  n_items <- 300L
  samples <- tibble::tibble(
    ID = paste0("id", seq_len(n_items)),
    text = paste0("text", seq_len(n_items))
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 1000L),
    seed = NULL
  )
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = seq_along(state$ids),
    theta_sd = rep(1, length(state$ids))
  )
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$C_max <- 100L
  config$W <- as.integer(state$N - 1L)
  anchors <- state$ids

  withr::local_seed(42)
  candidates_a <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors,
    theta_summary,
    state,
    config
  )
  withr::local_seed(42)
  candidates_b <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors,
    theta_summary,
    state,
    config
  )

  expect_true(nrow(candidates_a) <= config$C_max)
  expect_equal(candidates_a, candidates_b)
  if (nrow(candidates_a) > 0L) {
    expect_true(all(candidates_a$i %in% state$ids))
    expect_true(all(candidates_a$j %in% state$ids))
    expect_false(any(candidates_a$i == candidates_a$j))
    keys <- pairwiseLLM:::make_unordered_key(candidates_a$i, candidates_a$j)
    expect_equal(nrow(candidates_a), length(unique(keys)))
  }
})
