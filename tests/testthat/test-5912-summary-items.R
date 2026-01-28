testthat::test_that("summarize_items returns item diagnostics without gini columns", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  item_log <- tibble::tibble(
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
  item_log <- dplyr::relocate(
    dplyr::mutate(item_log, refit_id = 1L),
    refit_id,
    .before = 1L
  )
  state$logs <- list(item_log_list = list(item_log))

  summary <- pairwiseLLM::summarize_items(state)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("refit_id", "ID", "theta_mean", "rank_mean", "posA_prop") %in% names(summary)))
  testthat::expect_false(any(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_equal(summary, item_log)
  testthat::expect_identical(names(summary), names(item_log))
})

testthat::test_that("summarize_items supports sorting and missing posterior", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  item_log <- tibble::tibble(
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
  item_log <- dplyr::relocate(
    dplyr::mutate(item_log, refit_id = 1L),
    refit_id,
    .before = 1L
  )
  state$logs <- list(item_log_list = list(item_log))

  summary_top <- pairwiseLLM::summarize_items(
    state,
    top_n = 2L,
    sort_by = "theta_mean"
  )
  testthat::expect_equal(nrow(summary_top), 2L)
  testthat::expect_true(summary_top$theta_mean[[1L]] >= summary_top$theta_mean[[2L]])

  state$logs$item_log_list <- NULL
  summary_missing <- pairwiseLLM::summarize_items(state, posterior = NULL)
  testthat::expect_equal(nrow(summary_missing), 0L)
  testthat::expect_equal(ncol(summary_missing), 0L)
})

testthat::test_that("summarize_items unwraps list item_log_list inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  item_log <- tibble::tibble(
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

  item_log <- dplyr::relocate(
    dplyr::mutate(item_log, refit_id = 1L),
    refit_id,
    .before = 1L
  )
  out <- pairwiseLLM::summarize_items(state, posterior = list(item_log_list = list(item_log)))
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$ID, state$ids)
})

testthat::test_that("summarize_items selects refits and binds when requested", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  item_log_1 <- tibble::tibble(
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
  item_log_2 <- item_log_1
  item_log_2$theta_mean <- c(0.3, -0.2)
  item_log_list <- list(
    dplyr::relocate(dplyr::mutate(item_log_1, refit_id = 1L), refit_id, .before = 1L),
    dplyr::relocate(dplyr::mutate(item_log_2, refit_id = 2L), refit_id, .before = 1L)
  )
  state$logs <- list(item_log_list = item_log_list)

  last_refit <- pairwiseLLM::summarize_items(state)
  testthat::expect_equal(last_refit, item_log_list[[2L]])

  first_refit <- pairwiseLLM::summarize_items(state, refit = 1L)
  testthat::expect_equal(first_refit, item_log_list[[1L]])

  bound <- pairwiseLLM::summarize_items(state, bind = TRUE)
  expected_bound <- pairwiseLLM:::.adaptive_apply_sort_and_top_n(
    dplyr::bind_rows(item_log_list),
    sort_by = "rank_mean",
    top_n = NULL
  )
  testthat::expect_equal(bound, expected_bound)

  testthat::expect_error(pairwiseLLM::summarize_items(state, refit = 3L))
})

testthat::test_that("summarize_items warns on unsupported posterior inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  testthat::expect_warning(
    out <- pairwiseLLM::summarize_items(state, posterior = list(foo = "bar")),
    "item log list"
  )
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_equal(ncol(out), 0L)
})
