test_that(".make_estimates_tbl() returns required columns", {
  ids <- c("A", "B")
  tbl <- pairwiseLLM:::.make_estimates_tbl(ids)

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$ID, ids)

  expect_true(all(c(
    "theta_bt_firth", "se_bt_firth", "rank_bt_firth",
    "pi_rc", "theta_rc", "rank_rc"
  ) %in% names(tbl)))
})


test_that(".make_estimates_tbl() computes ranks descending with ties", {
  ids <- c("A", "B", "C")

  theta_bt <- c(0.2, 0.2, 0.1)
  theta_rc <- c(0.1, 0.3, 0.2)

  tbl <- pairwiseLLM:::.make_estimates_tbl(
    ids,
    theta_bt_firth = theta_bt,
    se_bt_firth = c(0.01, 0.02, 0.03),
    theta_rc = theta_rc,
    pi_rc = c(0.3, 0.5, 0.2)
  )

  # Descending ranks with ties.method = "min":
  # theta_bt: 0.2, 0.2, 0.1 -> ranks 1, 1, 3
  expect_equal(tbl$rank_bt_firth, c(1L, 1L, 3L))
  # theta_rc: 0.3, 0.2, 0.1 -> ranks 3, 1, 2 for A,B,C ordering
  expect_equal(tbl$rank_rc, c(3L, 1L, 2L))
})


test_that(".make_estimates_tbl() validates column lengths", {
  ids <- c("A", "B", "C")

  expect_error(
    pairwiseLLM:::.make_estimates_tbl(ids, theta_bt_firth = c(0.1, 0.2)),
    "match length(ids)",
    fixed = TRUE
  )

  expect_error(
    pairwiseLLM:::.make_estimates_tbl(ids, theta_bt_firth = c(0.1, 0.2, 0.3), extra = 1:2),
    "All `\\.\\.\\.` columns must match length\(ids\)"
  )
})
