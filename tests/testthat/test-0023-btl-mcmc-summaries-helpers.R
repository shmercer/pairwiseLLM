make_summary_legacy_state <- function() {
  ids <- c("A", "B", "C")
  structure(list(
    ids = ids,
    history_pairs = tibble::tibble(A_id = c("A", "B"), B_id = c("B", "C")),
    pair_count = stats::setNames(c(2L, 1L), c("A:B", "B:C"))
  ), class = "adaptive_state")
}

test_that("summary helper value/schema functions validate inputs", {
  expect_error(pairwiseLLM:::.adaptive_summary_empty_value("bogus"), "Unknown summary column type")
  expect_identical(pairwiseLLM:::.adaptive_summary_empty_value("logical"), logical())
  expect_true(inherits(pairwiseLLM:::.adaptive_summary_empty_value("posixct"), "POSIXct"))

  schema_full <- pairwiseLLM:::.adaptive_refit_summary_schema(include_optional = TRUE)
  schema_compact <- pairwiseLLM:::.adaptive_refit_summary_schema(include_optional = FALSE)
  expect_true(ncol(schema_compact) <= ncol(schema_full))

  item_full <- pairwiseLLM:::.adaptive_item_log_schema(include_optional = TRUE)
  item_req <- pairwiseLLM:::.adaptive_item_log_schema(include_optional = FALSE)
  expect_true(ncol(item_full) > ncol(item_req))

  expect_error(pairwiseLLM:::.adaptive_summary_validate_last_n("x"), "single positive")
  expect_error(pairwiseLLM:::.adaptive_summary_validate_last_n(0), "single positive")
  expect_identical(pairwiseLLM:::.adaptive_summary_validate_last_n(2), 2L)
})

test_that("summary source extraction and column fallback behave", {
  runtime <- adaptive_rank_start(make_test_items(3))
  src_runtime <- pairwiseLLM:::.adaptive_summary_extract_source(runtime)
  expect_true(is.data.frame(src_runtime$round_log))

  legacy <- make_summary_legacy_state()
  legacy$config <- list(round_log = tibble::tibble(refit_id = 1L))
  legacy$logs <- list(item_log_list = list(tibble::tibble(refit_id = 1L)))
  src_legacy <- pairwiseLLM:::.adaptive_summary_extract_source(legacy)
  expect_equal(nrow(src_legacy$round_log), 1L)

  src_list <- pairwiseLLM:::.adaptive_summary_extract_source(list(round_log = tibble::tibble(a = 1L)))
  expect_true(is.data.frame(src_list$round_log))
  src_nested <- pairwiseLLM:::.adaptive_summary_extract_source(list(state = runtime))
  expect_true(is.list(src_nested))

  expect_error(pairwiseLLM:::.adaptive_summary_extract_source(1L), "must be an adaptive_state")

  log <- tibble::tibble(x = 1:2)
  out <- pairwiseLLM:::.adaptive_summary_col(log, "x", default = 0L, n = 2L)
  expect_equal(out, 1:2)
  out_len_mismatch <- pairwiseLLM:::.adaptive_summary_col(log, "x", default = 0L, n = 3L)
  expect_equal(length(out_len_mismatch), 3L)
  out2 <- pairwiseLLM:::.adaptive_summary_col(log, "y", default = 9L, n = 3L)
  expect_equal(out2, c(9L, 9L, 9L))
})

test_that("scheduled exposure and repeated-pair summaries compute deterministically", {
  expect_error(pairwiseLLM:::.adaptive_compute_scheduled_exposure(list()), "adaptive_state")

  empty <- make_summary_legacy_state()
  empty$ids <- character()
  expo_empty <- pairwiseLLM:::.adaptive_compute_scheduled_exposure(empty)
  expect_equal(length(expo_empty$deg_scheduled), 0L)

  state <- make_summary_legacy_state()
  expo <- pairwiseLLM:::.adaptive_compute_scheduled_exposure(state)
  expect_equal(expo$deg_scheduled[["B"]], 2L)
  expect_true(is.finite(expo$mean_degree_scheduled))

  bad <- make_summary_legacy_state()
  bad$history_pairs <- tibble::tibble(A_id = "Z", B_id = "B")
  expect_error(pairwiseLLM:::.adaptive_compute_scheduled_exposure(bad), "valid ids")

  rep_counts <- pairwiseLLM:::.adaptive_repeated_pairs_by_item(state)
  expect_equal(rep_counts[["A"]], 1L)
  expect_equal(rep_counts[["C"]], 0L)

  zero_rep <- state
  zero_rep$pair_count <- stats::setNames(c(1L, 1L), c("A:B", "B:C"))
  expect_equal(sum(pairwiseLLM:::.adaptive_repeated_pairs_by_item(zero_rep)), 0L)

  no_names <- state
  no_names$pair_count <- integer(0)
  expect_true(all(is.na(pairwiseLLM:::.adaptive_repeated_pairs_by_item(no_names))))

  empty_ids <- structure(list(ids = character(), pair_count = integer()), class = "adaptive_state")
  expect_identical(pairwiseLLM:::.adaptive_repeated_pairs_by_item(empty_ids), integer())
})

test_that("draw extraction and item sorting helpers respect options", {
  mat <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(mat) <- c("A", "B")

  expect_identical(pairwiseLLM:::.adaptive_extract_theta_draws(mat, ids = c("A", "B")), mat)
  expect_null(pairwiseLLM:::.adaptive_extract_theta_draws(1L, ids = c("A", "B")))
  expect_identical(
    pairwiseLLM:::.adaptive_extract_theta_draws(list(theta_draws = mat), ids = c("A", "B")),
    mat
  )

  from_draws <- pairwiseLLM:::.adaptive_extract_theta_draws(list(draws = list(theta = mat)), ids = c("A", "B"))
  expect_identical(from_draws, mat)

  from_draws_fail <- pairwiseLLM:::.adaptive_extract_theta_draws(list(draws = 1L), ids = c("A", "B"))
  expect_null(from_draws_fail)
  expect_null(pairwiseLLM:::.adaptive_extract_theta_draws(list(other = 1L), ids = c("A", "B")))

  s <- tibble::tibble(ID = c("A", "B", "C"), rank_mean = c(2, 1, NA), theta_mean = c(0.1, 0.3, 0.2))
  sorted <- pairwiseLLM:::.adaptive_apply_sort_and_top_n(s, sort_by = "rank_mean", top_n = 2L)
  expect_equal(nrow(sorted), 2L)
  expect_equal(sorted$ID[[1L]], "B")

  sorted_desc <- pairwiseLLM:::.adaptive_apply_sort_and_top_n(s, sort_by = "theta_mean", top_n = NULL)
  expect_equal(sorted_desc$ID[[1L]], "B")
})

test_that("summarize_items validates bind/refit and sorting constraints", {
  item_log_1 <- tibble::tibble(refit_id = 1L, ID = c("A", "B"), rank_mean = c(1, 2), theta_mean = c(0.2, 0.1))
  item_log_2 <- tibble::tibble(refit_id = 2L, ID = c("A", "B"), rank_mean = c(1.1, 1.9), theta_mean = c(0.3, 0.05))

  logs <- list(item_log_list = list(item_log_1, item_log_2), round_log = tibble::tibble())
  expect_error(summarize_items(logs, bind = TRUE, refit = 1), "must be NULL when `bind` is TRUE")
  expect_error(summarize_items(logs, refit = 9), "only 2 refits are available")

  bad_logs <- list(item_log_list = list(item_log_1, tibble::tibble(other = 1)))
  expect_error(summarize_items(bad_logs, bind = TRUE), "identical columns")

  out <- summarize_items(logs, bind = FALSE, refit = 2, sort_by = "theta_mean", top_n = 1)
  expect_equal(nrow(out), 1L)

  expect_error(summarize_refits(logs, include_optional = NA), "must be TRUE or FALSE")
  weird_refits <- summarize_refits(list(round_log = 1L), include_optional = TRUE)
  expect_true(tibble::is_tibble(weird_refits))
  expect_error(summarize_items(logs, include_optional = NA), "must be TRUE or FALSE")
  expect_error(summarize_items(logs, bind = NA), "must be TRUE or FALSE")
  expect_error(summarize_items(logs, refit = "x"), "single positive integer")
  expect_error(summarize_items(logs, refit = 0), "single positive integer")

  expect_warning(
    summarize_items(list(round_log = tibble::tibble()), posterior = list(bad = TRUE)),
    "must be an item log list"
  )
  warn_out <- suppressWarnings(
    summarize_items(list(round_log = tibble::tibble()), posterior = list(bad = TRUE))
  )
  expect_equal(nrow(warn_out), 0L)

  expect_error(summarize_items(logs, sort_by = "pos_A_rate"), "must be a column in the item log")

  bad_bind <- list(item_log_list = list(1L, item_log_1))
  expect_error(summarize_items(bad_bind, bind = TRUE), "entries must be data frames")
  bad_bind2 <- list(item_log_list = list(item_log_1, 1L))
  expect_error(summarize_items(bad_bind2, bind = TRUE), "entries must be data frames")

  ok_bind <- summarize_items(logs, bind = TRUE)
  expect_true(nrow(ok_bind) >= 2L)

  from_df <- summarize_items(logs, posterior = item_log_1, bind = FALSE, refit = 1)
  expect_true(nrow(from_df) > 0L)

  bad_refit_entry <- list(item_log_list = list(item_log_1, 1L))
  expect_error(summarize_items(bad_refit_entry, bind = FALSE, refit = 2), "entries must be data frames")

  with_optional <- list(item_log_list = list(tibble::tibble(
    refit_id = 1L,
    ID = "A",
    rank_mean = 1,
    theta_mean = 0.1,
    repeated_pairs = 2L,
    adjacent_prev_prob = 0.2,
    adjacent_next_prob = 0.3
  )))
  no_opt <- summarize_items(with_optional, include_optional = FALSE)
  expect_false(any(c("repeated_pairs", "adjacent_prev_prob", "adjacent_next_prob") %in% names(no_opt)))
})
