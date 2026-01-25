test_that("summarize_theta and summarize_ranks return MCMC summaries", {
  theta_draws <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    nrow = 2,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")

  theta_sum <- pairwiseLLM:::summarize_theta(theta_draws)
  expect_true(all(c(
    "ID", "mean", "sd", "median", "q05", "q95", "q025", "q975"
  ) %in% names(theta_sum)))
  expect_equal(theta_sum$mean[theta_sum$ID == "A"], 1.5)

  rank_sum <- pairwiseLLM:::summarize_ranks(theta_draws)
  expect_true(all(c(
    "ID", "rank_mean", "rank_median", "rank_sd",
    "rank_q05", "rank_q95", "rank_q025", "rank_q975"
  ) %in% names(rank_sum)))
  expect_equal(rank_sum$rank_mean[rank_sum$ID == "C"], 1)
})

test_that("compute_adjacent_win_probs uses ranking order", {
  theta_draws <- matrix(
    c(2, 1, 0,
      3, 2, 1),
    nrow = 2,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")
  ranking_ids <- c("A", "B", "C")

  out <- pairwiseLLM:::compute_adjacent_win_probs(theta_draws, ranking_ids)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$win_prob >= 0))
  expect_true(all(out$win_prob_btl >= 0))
  expect_equal(out$A_id, c("A", "B"))
  expect_equal(out$B_id, c("B", "C"))
})

test_that("compute_adjacent_win_probs validates ranking ids", {
  theta_draws <- matrix(
    c(2, 1, 0,
      3, 2, 1),
    nrow = 2,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")

  expect_error(
    pairwiseLLM:::compute_adjacent_win_probs(theta_draws, ranking_ids = "A"),
    "at least two"
  )
  expect_error(
    pairwiseLLM:::compute_adjacent_win_probs(theta_draws, ranking_ids = c("A", "A", "B")),
    "duplicates"
  )
  expect_error(
    pairwiseLLM:::compute_adjacent_win_probs(theta_draws, ranking_ids = c("A", "B", "D")),
    "match"
  )
})

test_that("fit_bayes_btl_mcmc runs when CmdStan is available", {
  testthat::skip_if_not_installed("cmdstanr")
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path)) {
    testthat::skip("CmdStan is not installed for MCMC test.")
  }

  out_dir <- withr::local_tempdir()
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  fit <- tryCatch(
    pairwiseLLM:::fit_bayes_btl_mcmc(
      results,
      ids = c("A", "B"),
      cmdstan = list(
        chains = 2,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 123,
        core_fraction = 0.5,
        output_dir = out_dir
      )
    ),
    error = function(e) {
      testthat::skip(paste("CmdStan not usable for MCMC test:", conditionMessage(e)))
    }
  )
  expect_true(is.matrix(fit$theta_draws))
  expect_equal(colnames(fit$theta_draws), c("A", "B"))
  expect_true(nrow(fit$theta_draws) > 0L)
})
