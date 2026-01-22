testthat::test_that("ladder handles primitive select_batch when batch_size is zero", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 0L
  config$explore_rate <- 0

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(theta_draws = theta_draws, theta_mean = stats::setNames(colMeans(theta_draws), state$ids))
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)

  candidates <- tibble::tibble(
    unordered_key = "A:B",
    i_id = "A",
    j_id = "B",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )

  ns <- asNamespace("pairwiseLLM")
  original_select_batch <- get("select_batch", envir = ns)
  base::unlockBinding("select_batch", ns)
  assign("select_batch", base::list, envir = ns)
  base::lockBinding("select_batch", ns)
  on.exit({
    base::unlockBinding("select_batch", ns)
    assign("select_batch", original_select_batch, envir = ns)
    base::lockBinding("select_batch", ns)
  }, add = TRUE)
  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = candidates,
    n_candidates_generated = 1L,
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_used, "base_window")
})

testthat::test_that("global_safe stage can complete and update best", {
  ids <- LETTERS[1:12]
  samples <- tibble::tibble(
    ID = ids,
    text = paste0("text-", ids)
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
  config$explore_rate <- 0
  config$W <- 1L

  theta_draws <- matrix(rep(seq_len(state$N), each = 2L), nrow = 2L, byrow = TRUE)
  colnames(theta_draws) <- state$ids
  fit <- list(theta_draws = theta_draws, theta_mean = stats::setNames(colMeans(theta_draws), state$ids))
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)

  candidates_empty <- tibble::tibble(
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

  all_keys <- pairwiseLLM:::.adaptive_unordered_keys(state$ids)
  counts <- stats::setNames(rep.int(0L, length(all_keys)), all_keys)

  config_expand <- config
  config_expand$W <- min(2L * config$W, state$N - 1L)
  candidates_w2 <- pairwiseLLM:::generate_candidates(theta_summary, state, config_expand)
  blocked_keys <- pairwiseLLM:::make_unordered_key(candidates_w2$i, candidates_w2$j)

  config_expand$W <- min(4L * config$W, state$N - 1L)
  candidates_w4 <- pairwiseLLM:::generate_candidates(theta_summary, state, config_expand)
  blocked_keys <- unique(c(
    blocked_keys,
    pairwiseLLM:::make_unordered_key(candidates_w4$i, candidates_w4$j)
  ))

  uncertainty_anchors <- pairwiseLLM:::select_uncertainty_anchors(state, config, theta_summary = theta_summary)
  config_uncertainty <- config
  config_uncertainty$W <- config$W
  candidates_uncertainty <- pairwiseLLM:::generate_candidates_from_anchors(
    uncertainty_anchors,
    theta_summary,
    state,
    config_uncertainty
  )
  blocked_keys <- unique(c(
    blocked_keys,
    pairwiseLLM:::make_unordered_key(candidates_uncertainty$i, candidates_uncertainty$j)
  ))

  counts[blocked_keys] <- config$dup_max_count
  state$pair_count <- counts

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = candidates_empty,
    n_candidates_generated = 0L,
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_used, "global_safe")
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})

testthat::test_that("fallback wrapper forwards to ladder", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
  config$explore_rate <- 0

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(theta_draws = theta_draws, theta_mean = stats::setNames(colMeans(theta_draws), state$ids))
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  epsilon_mean <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  utilities <- pairwiseLLM:::compute_pair_utility(fit$theta_draws, candidates, epsilon_mean)
  utilities <- pairwiseLLM:::apply_degree_penalty(utilities, state)

  out <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_used, "base_window")
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})
