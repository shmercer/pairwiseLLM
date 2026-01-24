testthat::test_that("adaptive config and gini helpers cover edge cases", {
  config <- pairwiseLLM:::adaptive_v3_config(3, list(batch_size = 12L))
  testthat::expect_equal(config$batch_size, 12L)

  testthat::expect_error(pairwiseLLM:::adaptive_v3_config(3, "bad"))

  testthat::expect_true(is.na(pairwiseLLM:::compute_gini_posA(NULL)))
  testthat::expect_true(is.na(pairwiseLLM:::compute_gini_posA(numeric())))
  testthat::expect_error(pairwiseLLM:::compute_gini_posA(c(1, -1)))
  testthat::expect_equal(pairwiseLLM:::compute_gini_posA(c(0, 0)), 0)
  testthat::expect_equal(pairwiseLLM:::compute_gini_posA(1), 0)
  testthat::expect_true(pairwiseLLM:::compute_gini_posA(c(1, 2, 3)) > 0)
})

testthat::test_that("summary schemas and defaults are stable", {
  defaults <- pairwiseLLM:::.adaptive_round_log_defaults()
  testthat::expect_s3_class(defaults, "tbl_df")
  testthat::expect_equal(nrow(defaults), 1L)
  testthat::expect_true("round_id" %in% names(defaults))

  empty_integer <- pairwiseLLM:::.adaptive_summary_empty_value("integer")
  empty_posixct <- pairwiseLLM:::.adaptive_summary_empty_value("posixct")
  testthat::expect_identical(empty_integer, integer())
  testthat::expect_identical(empty_posixct, as.POSIXct(character(), tz = "UTC"))
  testthat::expect_error(pairwiseLLM:::.adaptive_summary_empty_value("unknown"))

  log <- tibble::tibble(a = c(1L, 2L))
  testthat::expect_equal(pairwiseLLM:::.adaptive_summary_col(log, "a", NA_integer_, 2L), log$a)
  testthat::expect_equal(
    pairwiseLLM:::.adaptive_summary_col(log, "a", NA_integer_, 3L),
    rep_len(log$a, 3L)
  )
  testthat::expect_equal(
    pairwiseLLM:::.adaptive_summary_col(log, "b", NA_integer_, 2L),
    rep_len(NA_integer_, 2L)
  )

  testthat::expect_null(pairwiseLLM:::.adaptive_summary_validate_last_n(NULL))
  testthat::expect_error(pairwiseLLM:::.adaptive_summary_validate_last_n("bad"))
  testthat::expect_error(pairwiseLLM:::.adaptive_summary_validate_last_n(0))
  testthat::expect_equal(pairwiseLLM:::.adaptive_summary_validate_last_n(2.2), 2L)

  iter_schema <- pairwiseLLM:::.adaptive_iteration_summary_schema(include_optional = FALSE)
  testthat::expect_false("n_candidates_generated" %in% names(iter_schema))

  refit_schema <- pairwiseLLM:::.adaptive_refit_summary_schema(include_optional = FALSE)
  testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(refit_schema)))
  testthat::expect_false("batch_size" %in% names(refit_schema))

  item_schema <- pairwiseLLM:::.adaptive_item_summary_schema(include_optional = FALSE)
  testthat::expect_false("theta_q975" %in% names(item_schema))
})

testthat::test_that("summary helpers handle draw extraction and repeats", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  theta_draws <- matrix(
    c(
      0.2, -0.1, 0.0,
      0.1, 0.0, -0.2
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )

  testthat::expect_identical(
    pairwiseLLM:::.adaptive_extract_theta_draws(theta_draws, state$ids),
    theta_draws
  )
  testthat::expect_null(pairwiseLLM:::.adaptive_extract_theta_draws(1, state$ids))
  testthat::expect_identical(
    pairwiseLLM:::.adaptive_extract_theta_draws(list(theta_draws = theta_draws), state$ids),
    theta_draws
  )

  testthat::with_mocked_bindings(
    .btl_mcmc_v3_theta_draws = function(draws, item_id) theta_draws,
    .env = asNamespace("pairwiseLLM"),
    {
      out <- pairwiseLLM:::.adaptive_extract_theta_draws(list(draws = "ok"), state$ids)
      testthat::expect_identical(out, theta_draws)
    }
  )
  testthat::with_mocked_bindings(
    .btl_mcmc_v3_theta_draws = function(draws, item_id) stop("nope"),
    .env = asNamespace("pairwiseLLM"),
    {
      testthat::expect_null(pairwiseLLM:::.adaptive_extract_theta_draws(list(draws = "bad"), state$ids))
    }
  )

  state$ids <- character()
  testthat::expect_length(pairwiseLLM:::.adaptive_repeated_pairs_by_item(state), 0L)

  state$ids <- c("A", "B", "C")
  state$pair_count <- integer()
  repeats_missing <- pairwiseLLM:::.adaptive_repeated_pairs_by_item(state)
  testthat::expect_true(all(is.na(repeats_missing)))

  state$pair_count <- c("A:B" = 1L, "A:C" = 1L, "B:C" = 1L)
  repeats_zero <- pairwiseLLM:::.adaptive_repeated_pairs_by_item(state)
  testthat::expect_true(all(repeats_zero == 0L))

  state$pair_count <- c("A:B" = 2L, "A:C" = 3L, "B:C" = 1L)
  repeats <- pairwiseLLM:::.adaptive_repeated_pairs_by_item(state)
  testthat::expect_equal(repeats[["A"]], 3L)
  testthat::expect_equal(repeats[["B"]], 1L)
  testthat::expect_equal(repeats[["C"]], 2L)
})

testthat::test_that("summaries cover validation and logged gini values", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_defaults(state$N)

  state$batch_log <- "bad"
  summary_empty <- pairwiseLLM::summarize_iterations(state)
  testthat::expect_equal(nrow(summary_empty), 0L)

  testthat::expect_error(pairwiseLLM::summarize_iterations(state, last_n = 0))
  testthat::expect_error(pairwiseLLM::summarize_iterations(state, include_optional = NA))

  state$batch_log <- pairwiseLLM:::batch_log_schema()
  row <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase1",
    mode = "warm_start",
    created_at = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 6L,
    n_pairs_selected = 6L,
    n_pairs_completed = 5L,
    n_pairs_failed = 1L,
    backlog_unjudged = 0L,
    n_explore_target = 2L,
    n_explore_selected = 2L,
    n_exploit_target = 4L,
    n_exploit_selected = 4L,
    n_candidates_generated = 10L,
    n_candidates_after_filters = 8L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_,
    W_used = 5L,
    explore_rate_used = 0.2,
    utility_selected_p50 = 0.55,
    utility_selected_p90 = 0.9,
    utility_candidate_p90 = 0.95
  )
  row$gini_degree <- 0.12
  row$gini_pos_A <- 0.34
  state$batch_log <- dplyr::bind_rows(state$batch_log, row)

  summary_logged <- pairwiseLLM::summarize_iterations(state, include_optional = FALSE)
  testthat::expect_equal(summary_logged$gini_degree[[1L]], 0.12)
  testthat::expect_equal(summary_logged$gini_pos_A[[1L]], 0.34)

  state$config$round_log <- "bad"
  refit_empty <- pairwiseLLM::summarize_refits(state)
  testthat::expect_equal(nrow(refit_empty), 0L)

  testthat::expect_error(pairwiseLLM::summarize_refits(state, last_n = 0))
  testthat::expect_error(pairwiseLLM::summarize_refits(state, include_optional = NA))

  state$config$round_log <- pairwiseLLM:::round_log_schema()
  refit_row <- pairwiseLLM:::build_round_log_row(state, new_pairs = 2L)
  refit_row$gini_degree <- 0.21
  refit_row$gini_pos_A <- 0.43
  state$config$round_log <- dplyr::bind_rows(state$config$round_log, refit_row)

  refit_logged <- pairwiseLLM::summarize_refits(state, include_optional = FALSE)
  testthat::expect_equal(refit_logged$gini_degree[[1L]], 0.21)
  testthat::expect_equal(refit_logged$gini_pos_A[[1L]], 0.43)

  testthat::expect_error(pairwiseLLM::summarize_items(state, top_n = 0))
  testthat::expect_error(pairwiseLLM::summarize_items(state, include_optional = NA))

  state$config$item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.1, 0.2, 0.3),
    theta_sd = c(0.1, 0.1, 0.1),
    theta_ci90_lo = c(-0.1, -0.1, -0.1),
    theta_ci90_hi = c(0.2, 0.3, 0.4),
    theta_ci95_lo = c(-0.2, -0.2, -0.2),
    theta_ci95_hi = c(0.3, 0.4, 0.5),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_sd = c(0.1, 0.1, 0.1),
    deg = c(1L, 2L, 3L),
    posA_prop = c(0.5, 0.5, 0.5)
  )
  item_summary <- pairwiseLLM::summarize_items(state, include_optional = FALSE)
  testthat::expect_false("theta_q975" %in% names(item_summary))
})
