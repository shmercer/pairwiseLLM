testthat::test_that("gini posA and stop metric alignment cover edge branches", {
  testthat::expect_error(pairwiseLLM:::compute_gini_posA(c(1, 2), deg = 1))
  testthat::expect_error(pairwiseLLM:::compute_gini_posA(c(1, 2), deg = c(1, -1)))

  defaults <- pairwiseLLM:::.adaptive_stop_metrics_align("bad")
  testthat::expect_true(is.list(defaults))
  testthat::expect_true("hard_cap_reached" %in% names(defaults))

  metrics <- pairwiseLLM:::.adaptive_stop_metrics_align(list(hard_cap_reached = TRUE))
  testthat::expect_true(isTRUE(metrics$hard_cap_reached))
  testthat::expect_true(is.na(metrics$scheduled_pairs))
})

testthat::test_that("round log rollups filter batches since last refit", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_defaults(state$N)

  row1 <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase2",
    mode = "adaptive",
    created_at = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 2L,
    n_pairs_selected = 2L,
    n_pairs_completed = 2L,
    n_pairs_failed = 0L,
    backlog_unjudged = 0L,
    n_explore_target = 1L,
    n_explore_selected = 1L,
    n_exploit_target = 1L,
    n_exploit_selected = 1L,
    n_candidates_generated = 4L,
    n_candidates_after_filters = 4L,
    candidate_starved = TRUE,
    fallback_used = "base_window",
    fallback_path = "base_window",
    a1_stage = "base_window",
    a1_W_used = 2L,
    a1_anchor_pool = "default",
    a1_n_generated = 4L,
    a1_n_survive = 4L,
    a1_n_selected = 2L,
    aN_tried = 0L,
    aN_best_stage = NA_character_,
    aN_best_n_generated = NA_integer_,
    aN_best_n_survive = NA_integer_,
    aN_best_n_selected = NA_integer_,
    starvation_reason = "unknown",
    reason_short_batch = NA_character_,
    W_used = 2L,
    explore_rate_used = 0.5,
    utility_selected_p50 = NA_real_,
    utility_selected_p90 = NA_real_,
    utility_candidate_p90 = NA_real_
  )
  row2 <- row1
  row2$iter <- 3L
  row2$candidate_starved <- FALSE
  row2$fallback_used <- "expand_2x"
  row2$fallback_path <- "base_window>expand_2x"
  row2$starvation_reason <- "unknown"

  state$batch_log <- dplyr::bind_rows(row1, row2)
  state$config$round_log <- tibble::tibble(iter_at_refit = 1L)

  round_row <- pairwiseLLM:::build_round_log_row(state, config = state$config$v3)
  testthat::expect_equal(round_row$starve_rate_since_last_refit[[1L]], 0)
  testthat::expect_equal(round_row$fallback_rate_since_last_refit[[1L]], 1)
  testthat::expect_identical(round_row$fallback_used_mode[[1L]], "expand_2x")
  testthat::expect_identical(round_row$starvation_reason_mode[[1L]], "unknown")
})

testthat::test_that("fallback attempt helpers handle edge paths", {
  attempt_missing <- tibble::tibble(stage = "base_window")
  testthat::expect_true(is.na(pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    attempt_missing,
    batch_size = 5L,
    config = list(C_max = 100L)
  )))

  attempt_full <- tibble::tibble(n_selected = 5L, n_generated = 20L)
  testthat::expect_true(is.na(pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    attempt_full,
    batch_size = 5L,
    config = list(C_max = 100L)
  )))

  attempt_sparse <- tibble::tibble(n_selected = 1L, n_generated = 5L)
  testthat::expect_identical(
    pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
      attempt_sparse,
      batch_size = 5L,
      config = list(C_max = 100L)
    ),
    "few_candidates_generated"
  )
  attempt_ok <- tibble::tibble(n_selected = 1L, n_generated = 15L)
  testthat::expect_identical(
    pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
      attempt_ok,
      batch_size = 5L,
      config = list(C_max = 100L)
    ),
    "unknown"
  )

  stage_attempts <- list(
    tibble::tibble(
      stage = "base_window",
      W_used = 2L,
      anchor_pool = "default",
      n_generated = 5L,
      n_survive = 4L,
      n_selected = 2L
    ),
    tibble::tibble(
      stage = "expand_2x",
      W_used = 4L,
      anchor_pool = "default",
      n_generated = 8L,
      n_survive = 6L,
      n_selected = 3L
    ),
    tibble::tibble(
      stage = "expand_4x",
      W_used = 8L,
      anchor_pool = "default",
      n_generated = 9L,
      n_survive = 9L,
      n_selected = 4L
    ),
    tibble::tibble(
      stage = "uncertainty_pool",
      W_used = 2L,
      anchor_pool = "uncertainty",
      n_generated = 3L,
      n_survive = 2L,
      n_selected = 1L
    )
  )

  fields <- pairwiseLLM:::.adaptive_stage_attempts_fields(
    stage_attempts = stage_attempts,
    batch_size = 5L,
    config = list(C_max = 100L),
    candidate_starved = TRUE
  )
  testthat::expect_equal(fields$aN_tried, 1L)
  testthat::expect_identical(fields$aN_best_stage, "uncertainty_pool")
  testthat::expect_identical(fields$starvation_reason, "few_candidates_generated")
})

testthat::test_that("schema enums expose starvation reasons", {
  levels <- pairwiseLLM:::.adaptive_starvation_reason_levels()
  testthat::expect_true("unknown" %in% levels)
  testthat::expect_true(length(levels) >= 5L)
})

testthat::test_that("summaries cover error branches and missing schema fields", {
  testthat::expect_error(pairwiseLLM::summarize_iterations(list()))
  testthat::expect_error(pairwiseLLM::summarize_refits(list()))
  testthat::expect_error(pairwiseLLM::summarize_items(list()))

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$deg <- c(A = 1L, B = 2L)
  state$pos1 <- c(A = 1L, B = 0L)
  state$pos_count <- state$pos1

  testthat::expect_null(pairwiseLLM:::.adaptive_extract_theta_draws(1, state$ids))

  state$batch_log <- tibble::tibble(
    iter = 1L,
    phase = "phase1",
    mode = "warm_start",
    batch_size_target = 2L,
    n_pairs_selected = 2L,
    n_pairs_completed = 2L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_,
    n_explore_selected = 1L,
    n_exploit_selected = 1L
  )

  testthat::with_mocked_bindings(
    .adaptive_align_log_schema = function(log, schema) {
      log <- tibble::as_tibble(log)
      schema <- tibble::as_tibble(schema)
      missing <- setdiff(names(schema), names(log))
      missing <- setdiff(missing, "created_at")
      for (col in missing) {
        default_val <- pairwiseLLM:::.adaptive_log_default_value(schema[[col]])
        log[[col]] <- rep_len(default_val, nrow(log))
      }
      log
    },
    .env = asNamespace("pairwiseLLM"),
    {
      summary <- pairwiseLLM::summarize_iterations(state, include_optional = FALSE)
      testthat::expect_true(all(is.na(summary$iter_start_time)))
    }
  )

  state$config$round_log <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 0L,
    n_items = 2L,
    total_pairs = 1L,
    new_pairs = 0L,
    batch_size = 2L,
    window_W = 2L,
    exploration_rate = 0.2,
    mean_degree = 0,
    min_degree = 0L,
    pos_balance_sd = 0
  )

  testthat::with_mocked_bindings(
    .adaptive_align_log_schema = function(log, schema) {
      log <- tibble::as_tibble(log)
      schema <- tibble::as_tibble(schema)
      missing <- setdiff(names(schema), names(log))
      missing <- setdiff(missing, c("gini_degree", "gini_pos_A"))
      for (col in missing) {
        default_val <- pairwiseLLM:::.adaptive_log_default_value(schema[[col]])
        log[[col]] <- rep_len(default_val, nrow(log))
      }
      log
    },
    .env = asNamespace("pairwiseLLM"),
    {
      summary <- pairwiseLLM::summarize_refits(state, include_optional = FALSE)
      testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(summary)))
      testthat::expect_true(is.finite(summary$gini_pos_A[[1L]]))
    }
  )

  theta_draws <- matrix(
    c(0.1, 0.2, 0.3, 0.0),
    nrow = 2L,
    ncol = 2L,
    byrow = TRUE
  )
  item_summary <- pairwiseLLM::summarize_items(
    state,
    posterior = theta_draws,
    include_optional = FALSE
  )
  testthat::expect_identical(item_summary$item_id, state$ids)
})
