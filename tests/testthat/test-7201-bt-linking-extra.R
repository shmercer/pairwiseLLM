test_that("bt_link_thetas and helpers handle legacy/edge schemas", {
  th_ref <- c(A = 0, B = 1, C = 2)
  th_cur_vec <- c(A = 1, B = 2, C = 3)

  lk <- bt_link_thetas(current = th_cur_vec, reference = th_ref, method = "mean_sd")
  expect_true(is.list(lk))
  expect_true(all(c("a", "b", "method", "n_core", "theta") %in% names(lk)))
  expect_s3_class(lk$theta, "tbl_df")
  expect_true(all(c("ID", "theta", "theta_linked") %in% names(lk$theta)))

  # IDs with insufficient overlap should default to identity transform.
  th_ref2 <- c(A = 0, B = 1, C = 2)
  th_cur2 <- c(A = 10, D = 20)
  lk2 <- bt_link_thetas(current = th_cur2, reference = th_ref2, min_n = 3L)
  expect_identical(lk2$a, 0)
  expect_identical(lk2$b, 1)
})
