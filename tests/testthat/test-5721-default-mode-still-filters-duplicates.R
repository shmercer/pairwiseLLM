testthat::test_that("default candidate generation filters seen pairs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$unordered_count[["A:B"]] <- 2L

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(2, 1, 0, -1),
    theta_sd = rep(0.2, state$N)
  )
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(W = 3L, C_max = 10L))

  candidates <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors = c("A", "B"),
    theta_summary = theta_summary,
    state = state,
    config = config,
    dup_policy = "default"
  )

  keys <- pairwiseLLM:::make_unordered_key(candidates$i, candidates$j)
  testthat::expect_false("A:B" %in% keys)
  testthat::expect_false(any(candidates$i == candidates$j))
})
