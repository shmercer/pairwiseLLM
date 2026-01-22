test_that("generate_candidates caps streamed candidates", {
  samples <- tibble::tibble(
    ID = paste0("id", seq_len(12L)),
    text = paste0("text", seq_len(12L))
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 100L),
    seed = NULL
  )
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = seq_along(state$ids),
    theta_sd = rep(1, length(state$ids))
  )
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$C_max <- 10L
  config$W <- as.integer(state$N - 1L)

  withr::local_seed(123)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)

  expect_true(nrow(candidates) <= config$C_max)
  if (nrow(candidates) > 0L) {
    expect_true(all(candidates$i %in% state$ids))
    expect_true(all(candidates$j %in% state$ids))
    expect_false(any(candidates$i == candidates$j))
    keys <- pairwiseLLM:::make_unordered_key(candidates$i, candidates$j)
    expect_equal(nrow(candidates), length(unique(keys)))
  }
})
