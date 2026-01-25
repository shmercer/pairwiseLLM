testthat::test_that("batch_log tracks every iteration and required columns", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("text-", seq_len(8))
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 10L,
    budget_max = 20L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 2L, BATCH2 = 2L, BATCH3 = 2L),
    v3 = list(checks_passed_target = 10L, refit_B = 1L)
  )

  make_results <- function(pairs, backend, model) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
      ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-15 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
  }

  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    list(
      results = make_results(pairs, backend, model),
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    list(
      draws = list(theta = theta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = tibble::tibble(epsilon_mean = 0.1),
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
    )
  }

  mock_fit_contract <- function(mcmc_fit, ids) {
    theta_draws <- mcmc_fit$draws$theta
    colnames(theta_draws) <- ids
    list(
      theta_draws = theta_draws,
      theta_mean = stats::setNames(rep(0, length(ids)), ids),
      epsilon_mean = 0.1,
      diagnostics = mcmc_fit$diagnostics %||% list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
    )
  }

  withr::local_seed(444)
  start_out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = adaptive,
      seed = 444
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    as_v3_fit_contract_from_mcmc = mock_fit_contract,
    .env = asNamespace("pairwiseLLM")
  )

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = start_out$state,
      mode = "live",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 444
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    as_v3_fit_contract_from_mcmc = mock_fit_contract,
    .env = asNamespace("pairwiseLLM")
  )

  resume_out2 <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = resume_out$state,
      mode = "live",
      submission_info = resume_out$submission_info,
      adaptive = adaptive,
      seed = 444
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    as_v3_fit_contract_from_mcmc = mock_fit_contract,
    .env = asNamespace("pairwiseLLM")
  )

  state <- resume_out2$state
  testthat::expect_equal(nrow(state$batch_log), state$iter)
  testthat::expect_identical(
    colnames(state$batch_log),
    colnames(pairwiseLLM:::batch_log_schema())
  )
})

testthat::test_that("round_log fills refit fields and matches stop metrics", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("text-", seq_len(8))
  )

  adaptive <- list(
    d1 = 2L,
    M1_target = 10L,
    budget_max = 20L,
    bins = 2L,
    batch_overrides = list(BATCH1 = 2L, BATCH2 = 2L, BATCH3 = 2L),
    v3 = list(checks_passed_target = 10L, refit_B = 1L)
  )

  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    results <- tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
      ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-15 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
    list(
      results = results,
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    list(
      draws = list(theta = theta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = tibble::tibble(epsilon_mean = 0.1),
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
    )
  }
  mock_fit_contract <- function(mcmc_fit, ids) {
    theta_draws <- mcmc_fit$draws$theta
    colnames(theta_draws) <- ids
    list(
      theta_draws = theta_draws,
      theta_mean = stats::setNames(rep(0, length(ids)), ids),
      epsilon_mean = 0.1,
      diagnostics = mcmc_fit$diagnostics %||% list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
    )
  }

  withr::local_seed(777)
  start_out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = adaptive,
      seed = 777
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    as_v3_fit_contract_from_mcmc = mock_fit_contract,
    .env = asNamespace("pairwiseLLM")
  )

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = start_out$state,
      mode = "live",
      submission_info = start_out$submission_info,
      adaptive = adaptive,
      seed = 777
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    .env = asNamespace("pairwiseLLM"),
    .package = "pairwiseLLM"
  )

  resume_out2 <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = resume_out$state,
      mode = "live",
      submission_info = resume_out$submission_info,
      adaptive = adaptive,
      seed = 777
    ),
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    .env = asNamespace("pairwiseLLM"),
    .package = "pairwiseLLM"
  )

  round_log <- resume_out2$state$config$round_log
  testthat::expect_true(nrow(round_log) >= 1L)
  testthat::expect_false(any(is.na(round_log$new_pairs)))
  testthat::expect_false(any(is.na(round_log$batch_size)))
  testthat::expect_false(any(is.na(round_log$window_W)))
  testthat::expect_false(any(is.na(round_log$exploration_rate)))

  last_idx <- nrow(round_log)
  expected_rank_stability_pass <- resume_out2$state$posterior$stop_metrics$rank_stability_pass %||% NA
  testthat::expect_identical(
    as.logical(round_log$rank_stability_pass[[last_idx]]),
    as.logical(expected_rank_stability_pass)
  )
})
