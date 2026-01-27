testthat::test_that("phase3 entry follows EAP proximity threshold", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("text-", seq_len(8))
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

  run_phase3 <- function(theta_draws, eap_min) {
    adaptive <- list(
      d1 = 2L,
      M1_target = 2L,
      budget_max = 20L,
      batch_overrides = list(BATCH1 = 2L, BATCH2 = 2L, BATCH3 = 2L),
      v3 = list(
        refit_B = 1L,
        eap_reliability_min = eap_min,
        model_variant = "btl",
        hard_cap_frac = 1
      )
    )

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
      draw_mat <- theta_draws
      colnames(draw_mat) <- ids
      list(
        draws = list(theta = draw_mat),
        theta_summary = tibble::tibble(item_id = ids, theta_mean = colMeans(draw_mat)),
        epsilon_summary = tibble::tibble(
          epsilon_mean = NA_real_,
          epsilon_p2.5 = NA_real_,
          epsilon_p5 = NA_real_,
          epsilon_p50 = NA_real_,
          epsilon_p95 = NA_real_,
          epsilon_p97.5 = NA_real_
        ),
        diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
        model_variant = config$model_variant %||% "btl"
      )
    }

    withr::local_seed(101)
    testthat::with_mocked_bindings(
      {
        start_out <- adaptive_rank_start(
          samples = samples,
          model = "gpt-test",
          trait_name = "quality",
          trait_description = "Which is better?",
          backend = "openai",
          mode = "live",
          adaptive = adaptive,
          seed = 101
        )

        resume_out <- adaptive_rank_resume(
          state = start_out$state,
          mode = "live",
          submission_info = start_out$submission_info,
          adaptive = adaptive,
          seed = 101
        )
        resume_out2 <- adaptive_rank_resume(
          state = resume_out$state,
          mode = "live",
          submission_info = resume_out$submission_info,
          adaptive = adaptive,
          seed = 101
        )

        resume_out2$state
      },
      submit_llm_pairs = mock_submit,
      .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit,
      .package = "pairwiseLLM"
    )
  }

  high_reliability_draws <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8),
    c(1.01, 2.01, 3.01, 4.01, 5.01, 6.01, 7.01, 8.01)
  )
  high_state <- run_phase3(high_reliability_draws, eap_min = 0.95)
  testthat::expect_identical(high_state$phase, "phase3")

  low_reliability_draws <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8),
    c(8, 7, 6, 5, 4, 3, 2, 1)
  )
  low_state <- run_phase3(low_reliability_draws, eap_min = 0.95)
  testthat::expect_identical(low_state$phase, "phase2")
})
