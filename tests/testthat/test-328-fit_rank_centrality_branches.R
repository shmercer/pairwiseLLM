test_that("fit_rank_centrality errors when no valid comparisons after filtering", {
  # Self-comparisons are filtered out (i_idx != j_idx), leaving none.
  bt_data <- tibble::tibble(
    object1 = c("A", "B"),
    object2 = c("A", "B"),
    result = c(1, 0)
  )
  expect_error(
    fit_rank_centrality(bt_data),
    "No valid comparisons found after filtering"
  )
})

test_that("fit_rank_centrality works on disconnected graphs when damping > 0", {
  bt_data <- tibble::tibble(
    object1 = c("A", "C"),
    object2 = c("B", "D"),
    result = c(1, 1)
  )
  fit <- fit_rank_centrality(bt_data, damping = 0.15)
  expect_s3_class(fit$theta, "tbl_df")
  expect_true(fit$diagnostics$n_components >= 2)
  # Damping should make the stationary distribution finite.
  expect_true(all(is.finite(fit$theta$theta)))
})
