testthat::test_that("summarize_items returns item diagnostics without gini columns", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1, 0.0),
    theta_sd = c(0.1, 0.2, 0.3),
    theta_p2.5 = c(-0.2, -0.3, -0.4),
    theta_p5 = c(-0.1, -0.2, -0.3),
    theta_p50 = c(0.1, -0.05, 0.0),
    theta_p95 = c(0.3, 0.2, 0.1),
    theta_p97.5 = c(0.4, 0.3, 0.2),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_p2.5 = c(1.0, 1.8, 2.7),
    rank_p5 = c(1.0, 1.9, 2.8),
    rank_p50 = c(1.0, 2.0, 3.0),
    rank_p95 = c(1.2, 2.1, 3.2),
    rank_p97.5 = c(1.3, 2.2, 3.3),
    rank_sd = c(0.1, 0.2, 0.3),
    deg = c(1L, 2L, 3L),
    posA_prop = c(1.0, 0.5, 0.0)
  )
  state$config$item_summary <- item_summary

  summary <- pairwiseLLM::summarize_items(state)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("item_id", "theta_mean", "rank_mean", "pos_A_rate") %in% names(summary)))
  testthat::expect_false(any(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_true(setequal(summary$item_id, state$ids))
  testthat::expect_equal(summary$theta_mean[[1L]], item_summary$theta_mean[[1L]])
  testthat::expect_equal(summary$theta_p5[[1L]], item_summary$theta_p5[[1L]])
  testthat::expect_equal(summary$theta_p95[[1L]], item_summary$theta_p95[[1L]])
  testthat::expect_equal(summary$pos_A_rate[[1L]], item_summary$posA_prop[[1L]])
})

testthat::test_that("summarize_items supports sorting and missing posterior", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1, 0.0),
    theta_sd = c(0.1, 0.2, 0.3),
    theta_p2.5 = c(-0.2, -0.3, -0.4),
    theta_p5 = c(-0.1, -0.2, -0.3),
    theta_p50 = c(0.1, -0.05, 0.0),
    theta_p95 = c(0.3, 0.2, 0.1),
    theta_p97.5 = c(0.4, 0.3, 0.2),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_p2.5 = c(1.0, 1.8, 2.7),
    rank_p5 = c(1.0, 1.9, 2.8),
    rank_p50 = c(1.0, 2.0, 3.0),
    rank_p95 = c(1.2, 2.1, 3.2),
    rank_p97.5 = c(1.3, 2.2, 3.3),
    rank_sd = c(0.1, 0.2, 0.3),
    deg = c(1L, 2L, 3L),
    posA_prop = c(1.0, 0.5, 0.0)
  )
  state$config$item_summary <- item_summary

  summary_top <- pairwiseLLM::summarize_items(
    state,
    top_n = 2L,
    sort_by = "theta_mean"
  )
  testthat::expect_equal(nrow(summary_top), 2L)
  testthat::expect_true(summary_top$theta_mean[[1L]] >= summary_top$theta_mean[[2L]])

  state$config$item_summary <- NULL
  summary_missing <- pairwiseLLM::summarize_items(state, posterior = NULL)
  testthat::expect_equal(nrow(summary_missing), 0L)
})

testthat::test_that("summarize_items unwraps list item_summary inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1),
    theta_sd = c(0.1, 0.2),
    theta_p2.5 = c(-0.2, -0.3),
    theta_p5 = c(-0.1, -0.2),
    theta_p50 = c(0.1, -0.05),
    theta_p95 = c(0.3, 0.2),
    theta_p97.5 = c(0.4, 0.3),
    rank_mean = c(1.0, 2.0),
    rank_p2.5 = c(1.0, 1.8),
    rank_p5 = c(1.0, 1.9),
    rank_p50 = c(1.0, 2.0),
    rank_p95 = c(1.2, 2.1),
    rank_p97.5 = c(1.3, 2.2),
    rank_sd = c(0.1, 0.2),
    deg = c(1L, 2L),
    posA_prop = c(1.0, 0.5)
  )

  out <- pairwiseLLM::summarize_items(state, posterior = list(item_summary = item_summary))
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$item_id, state$ids)
})
