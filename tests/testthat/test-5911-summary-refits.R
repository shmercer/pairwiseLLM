testthat::test_that("summarize_refits returns stable schema", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_defaults(state$N)
  state$deg <- c(A = 1L, B = 2L, C = 3L)
  state$pos1 <- c(A = 1L, B = 1L, C = 0L)
  state$pos_count <- state$pos1

  fit <- list(
    theta_draws = matrix(
      c(
        0.2, -0.1, 0.0,
        0.1, 0.0, -0.2,
        0.3, -0.2, 0.1
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    ),
    diagnostics = list(
      divergences = 0L,
      min_ess_bulk = 500,
      max_rhat = 1.0
    )
  )

  metrics <- list(
    hard_cap_threshold = 10L,
    n_unique_pairs_seen = 4L,
    rank_stability_pass = TRUE,
    diagnostics_pass = TRUE
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)

  row <- pairwiseLLM:::build_round_log_row(
    state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    new_pairs = 2L
  )
  state$config$round_log <- dplyr::bind_rows(state$config$round_log, row)

  summary <- pairwiseLLM::summarize_refits(state, last_n = 1L)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("reliability_EAP", "epsilon_mean") %in% names(summary)))
})

testthat::test_that("summarize_refits handles missing logs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$round_log <- NULL

  summary <- pairwiseLLM::summarize_refits(state)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_equal(nrow(summary), 0L)
})
