testthat::test_that("summarize_items returns item diagnostics without gini columns", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$deg <- c(A = 1L, B = 2L, C = 3L)
  state$pos1 <- c(A = 1L, B = 1L, C = 0L)

  theta_draws <- matrix(
    c(
      0.2, -0.1, 0.0,
      0.1, 0.0, -0.2,
      0.3, -0.2, 0.1,
      0.4, -0.1, 0.2
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- list(theta_draws = theta_draws)

  summary <- pairwiseLLM::summarize_items(state, posterior = fit)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("item_id", "theta_mean", "rank_mean", "pos_A_rate") %in% names(summary)))
  testthat::expect_false(any(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_true(setequal(summary$item_id, state$ids))
  testthat::expect_equal(summary$theta_mean[[1L]], mean(theta_draws[, 1L]))
  rank_A <- summary$rank_mean[summary$item_id == "A"]
  rank_B <- summary$rank_mean[summary$item_id == "B"]
  testthat::expect_true(rank_A <= rank_B)
})

testthat::test_that("summarize_items supports sorting and missing posterior", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$deg <- c(A = 1L, B = 2L, C = 3L)
  state$pos1 <- c(A = 1L, B = 1L, C = 0L)

  theta_draws <- matrix(
    c(
      0.2, -0.1, 0.0,
      0.1, 0.0, -0.2,
      0.3, -0.2, 0.1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- list(theta_draws = theta_draws)

  summary_top <- pairwiseLLM::summarize_items(
    state,
    posterior = fit,
    top_n = 2L,
    sort_by = "theta_mean"
  )
  testthat::expect_equal(nrow(summary_top), 2L)
  testthat::expect_true(summary_top$theta_mean[[1L]] >= summary_top$theta_mean[[2L]])

  summary_missing <- pairwiseLLM::summarize_items(state, posterior = NULL)
  testthat::expect_true(all(is.na(summary_missing$theta_mean)))
  testthat::expect_equal(unname(summary_missing$degree), unname(state$deg))
})
