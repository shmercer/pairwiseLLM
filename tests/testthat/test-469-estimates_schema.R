test_that(".make_estimates_tbl returns required columns and ranks", {
  ids <- c("a", "b", "c")
  theta_bt <- c(0.2, 0.2, 0.1)
  se_bt <- c(0.3, 0.4, 0.5)

  out <- pairwiseLLM:::.make_estimates_tbl(
    ids,
    theta_bt_firth = theta_bt,
    se_bt_firth = se_bt,
    theta_rc = c(0.0, -0.2, -0.3),
    pi_rc = c(0.5, 0.3, 0.2)
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c(
    "ID",
    "theta_bt_firth", "se_bt_firth", "rank_bt_firth",
    "pi_rc", "theta_rc", "rank_rc"
  ) %in% names(out)))

  # theta_bt: 0.2, 0.2, 0.1 -> ranks 1, 1, 3
  expect_equal(out$rank_bt_firth, c(1L, 1L, 3L))
})

test_that(".make_estimates_tbl validates lengths", {
  ids <- c("a", "b", "c")

  expect_error(
    pairwiseLLM:::.make_estimates_tbl(ids, theta_bt_firth = c(0.1, 0.2)),
    "match length(ids)",
    fixed = TRUE
  )

  expect_error(
    pairwiseLLM:::.make_estimates_tbl(
      ids,
      theta_bt_firth = c(0.1, 0.2, 0.3),
      extra = 1:2
    ),
    "All `...` columns must match length(ids)",
    fixed = TRUE
  )
})
