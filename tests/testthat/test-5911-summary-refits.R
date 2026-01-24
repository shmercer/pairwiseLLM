testthat::test_that("summarize_refits returns stable schema with gini metrics", {
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
    theta_sd_median_S = 0.2,
    tau = 1.1,
    theta_sd_pass = TRUE,
    U0 = 0.3,
    U_top_median = 0.4,
    U_pass = TRUE,
    U_abs = 0.1,
    hard_cap_reached = FALSE,
    hard_cap_threshold = 10L,
    n_unique_pairs_seen = 4L,
    rank_stability_pass = TRUE,
    frac_weak_adj = 0.0,
    min_adj_prob = 0.8,
    weak_adj_threshold = 0.95,
    weak_adj_frac_max = 0.05,
    min_adj_prob_threshold = 0.6,
    min_new_pairs_for_check = 5L,
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
  row$gini_degree <- 0.21
  row$gini_pos_A <- 0.43
  state$config$round_log <- dplyr::bind_rows(state$config$round_log, row)

  summary <- pairwiseLLM::summarize_refits(state, last_n = 1L)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_true(all(c("reliability_EAP", "epsilon_mean") %in% names(summary)))
  testthat::expect_equal(summary$gini_degree[[1L]], 0.21)
  testthat::expect_equal(summary$gini_pos_A[[1L]], 0.43)
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
  testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(summary)))
})
