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
    v3 = list(stability_consecutive = 10L, refit_B = 1L)
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
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    epsilon_summary <- if (!is.null(epsilon_draws)) {
      tibble::tibble(
        epsilon_mean = 0.1,
        epsilon_p2.5 = 0.05,
        epsilon_p5 = 0.06,
        epsilon_p50 = 0.1,
        epsilon_p95 = 0.14,
        epsilon_p97.5 = 0.15
      )
    } else {
      tibble::tibble(
        epsilon_mean = NA_real_,
        epsilon_p2.5 = NA_real_,
        epsilon_p5 = NA_real_,
        epsilon_p50 = NA_real_,
        epsilon_p95 = NA_real_,
        epsilon_p97.5 = NA_real_
      )
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary,
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
      model_variant = model_variant
    )
  }

  withr::local_seed(444)
  testthat::local_mocked_bindings(
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    .env = asNamespace("pairwiseLLM")
  )
  start_out <- adaptive_rank_start(
    samples = samples,
    model = "gpt-test",
    trait_name = "quality",
    trait_description = "Which is better?",
    backend = "openai",
    mode = "live",
    adaptive = adaptive,
    seed = 444
  )

  resume_out <- adaptive_rank_resume(
    state = start_out$state,
    mode = "live",
    submission_info = start_out$submission_info,
    adaptive = adaptive,
    seed = 444
  )

  resume_out2 <- adaptive_rank_resume(
    state = resume_out$state,
    mode = "live",
    submission_info = resume_out$submission_info,
    adaptive = adaptive,
    seed = 444
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
    v3 = list(stability_consecutive = 10L, refit_B = 1L)
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
    model_variant <- config$model_variant %||% "btl_e_b"
    epsilon_draws <- if (pairwiseLLM:::model_has_e(model_variant)) {
      rep(0.1, nrow(theta_draws))
    } else {
      NULL
    }
    beta_draws <- if (pairwiseLLM:::model_has_b(model_variant)) {
      rep(0, nrow(theta_draws))
    } else {
      NULL
    }
    epsilon_summary <- if (!is.null(epsilon_draws)) {
      tibble::tibble(
        epsilon_mean = 0.1,
        epsilon_p2.5 = 0.05,
        epsilon_p5 = 0.06,
        epsilon_p50 = 0.1,
        epsilon_p95 = 0.14,
        epsilon_p97.5 = 0.15
      )
    } else {
      tibble::tibble(
        epsilon_mean = NA_real_,
        epsilon_p2.5 = NA_real_,
        epsilon_p5 = NA_real_,
        epsilon_p50 = NA_real_,
        epsilon_p95 = NA_real_,
        epsilon_p97.5 = NA_real_
      )
    }
    list(
      draws = list(theta = theta_draws, epsilon = epsilon_draws, beta = beta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = epsilon_summary,
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
      model_variant = model_variant
    )
  }

  withr::local_seed(777)
  testthat::local_mocked_bindings(
    submit_llm_pairs = mock_submit,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
    .env = asNamespace("pairwiseLLM")
  )
  start_out <- adaptive_rank_start(
    samples = samples,
    model = "gpt-test",
    trait_name = "quality",
    trait_description = "Which is better?",
    backend = "openai",
    mode = "live",
    adaptive = adaptive,
    seed = 777
  )

  resume_out <- adaptive_rank_resume(
    state = start_out$state,
    mode = "live",
    submission_info = start_out$submission_info,
    adaptive = adaptive,
    seed = 777
  )

  resume_out2 <- adaptive_rank_resume(
    state = resume_out$state,
    mode = "live",
    submission_info = resume_out$submission_info,
    adaptive = adaptive,
    seed = 777
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
