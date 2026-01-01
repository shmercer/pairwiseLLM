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
