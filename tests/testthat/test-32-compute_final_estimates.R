test_that("compute_final_estimates returns both BT and Rank Centrality outputs", {
  skip_if_not_installed("BradleyTerry2")

  # Deterministic toy results: A > B > C in a small connected graph
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "A", "B")
  )

  out <- compute_final_estimates(res, bt_bias_reduction = TRUE, rc_damping = 0.05)

  expect_true(is.list(out))
  expect_true(all(c("estimates", "bt_fit", "rc_fit", "diagnostics") %in% names(out)))
  est <- out$estimates

  expect_s3_class(est, "tbl_df")
  expect_true(all(c(
    "ID",
    "theta_bt_firth", "se_bt_firth", "rank_bt_firth",
    "pi_rc", "theta_rc", "rank_rc",
    "wins", "losses", "ties",
    "n_appear", "n_pos1", "n_pos2",
    "component_id"
  ) %in% names(est)))

  # All IDs present
  expect_setequal(est$ID, c("A", "B", "C"))

  # Ranks should reflect transitive ordering A > B > C
  est2 <- dplyr::arrange(est, rank_bt_firth)
  expect_equal(est2$ID, c("A", "B", "C"))

  est3 <- dplyr::arrange(est, rank_rc)
  expect_equal(est3$ID, c("A", "B", "C"))

  # Outputs should be finite
  expect_true(all(is.finite(est$theta_bt_firth)))
  expect_true(all(is.finite(est$theta_rc)))
  expect_true(all(est$pi_rc > 0))
})


test_that("compute_final_estimates reports disconnected components", {
  skip_if_not_installed("BradleyTerry2")

  # Two disconnected pairs: (A,B) and (C,D)
  res <- tibble::tibble(
    ID1 = c("A", "C"),
    ID2 = c("B", "D"),
    better_id = c("A", "C")
  )

  out <- compute_final_estimates(res, rc_damping = 0.05)
  est <- out$estimates

  expect_setequal(est$ID, c("A", "B", "C", "D"))
  expect_true(is.integer(est$component_id) || is.numeric(est$component_id))
  # At least 2 components
  expect_gte(length(unique(est$component_id)), 2)
})

testthat::test_that("compute_final_estimates covers validation and tie logic", {
  # Missing required columns in results_graph
  bad_results <- tibble::tibble(ID1 = "A", ID2 = "B")
  testthat::expect_error(
    pairwiseLLM::compute_final_estimates(bad_results, ids = c("A", "B")),
    "must contain columns"
  )

  # ids validation
  good_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  testthat::expect_error(
    pairwiseLLM::compute_final_estimates(good_results, ids = c(1, 2)),
    "ids"
  )

  # tie handling in summarize_results_graph: better_id neither ID1 nor ID2
  sum_tbl <- pairwiseLLM:::.summarize_results_graph(
    tibble::tibble(ID1 = "A", ID2 = "B", better_id = "TIE"),
    ids = c("A", "B")
  )
  testthat::expect_equal(sum_tbl$wlt$wins[sum_tbl$wlt$ID == "A"], 0L)
  testthat::expect_equal(sum_tbl$wlt$wins[sum_tbl$wlt$ID == "B"], 0L)
  testthat::expect_equal(sum_tbl$wlt$ties[sum_tbl$wlt$ID == "A"], 1L)
  testthat::expect_equal(sum_tbl$wlt$ties[sum_tbl$wlt$ID == "B"], 1L)
})

testthat::test_that("fit BT via BradleyTerry2 covers require-ns and verbose branches", {
  # force requireNamespace failure
  testthat::local_mocked_bindings(
    .require_ns = function(...) FALSE,
    .package = "pairwiseLLM"
  )

  # Use a balanced dataset (each unordered pair appears twice, each side wins once)
  # to avoid degeneracy in BradleyTerry2::BTabilities().
  res <- tibble::tibble(
    ID1 = c("A", "A", "A", "A", "B", "B"),
    ID2 = c("B", "B", "C", "C", "C", "C"),
    better_id = c("A", "B", "A", "C", "B", "C")
  )
  bt_data <- pairwiseLLM::build_bt_data(res)

  testthat::expect_error(
    pairwiseLLM:::.fit_bt_bradleyterry2(
      bt_data,
      ids = c("A", "B", "C")
    ),
    "must be installed"
  )

  # Real verbose path: only run if BradleyTerry2 is installed.
  testthat::skip_if_not_installed("BradleyTerry2")
  testthat::local_mocked_bindings(
    .require_ns = function(...) TRUE,
    .package = "pairwiseLLM"
  )
  testthat::capture_output(
    fit <- pairwiseLLM:::.fit_bt_bradleyterry2(
      bt_data,
      ids = c("A", "B", "C"),
      verbose = TRUE
    )
  )
  testthat::expect_true(all(c("ID", "theta") %in% names(fit$theta)))
})
