testthat::test_that("fallback ladder attempts stages in order and stops on first success", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E", "F"),
    text = c("alpha", "bravo", "charlie", "delta", "echo", "foxtrot")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
  config$explore_rate <- 0
  config$A_anchors <- 1L
  config$W <- 1L

  theta_draws <- matrix(rep(seq_len(state$N), each = 2L), nrow = 2L, byrow = TRUE)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  counts <- stats::setNames(rep.int(0L, length(keys)), keys)

  config_expand <- config
  config_expand$W <- 2L
  block_candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config_expand)
  block_keys <- pairwiseLLM:::make_unordered_key(block_candidates$i, block_candidates$j)
  counts[block_keys] <- config$dup_max_count
  state$pair_count <- counts

  base_utilities <- tibble::tibble(
    unordered_key = character(),
    i_id = character(),
    j_id = character(),
    i = character(),
    j = character(),
    mean_d = double(),
    var_d = double(),
    p_mean = double(),
    utility = double(),
    utility_raw = double()
  )

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = base_utilities,
    n_candidates_generated = 0L,
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_path, c("base_window", "expand_2x", "expand_4x"))
  testthat::expect_identical(out$fallback_used, "expand_4x")
  testthat::expect_identical(out$fallback_stage, "expand_4x")
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})
