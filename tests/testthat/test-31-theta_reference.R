testthat::test_that("internal theta reference helpers orient by wins and clamp", {
  bt_data <- tibble::tibble(
    object1 = c("A", "A", "A", "B"),
    object2 = c("B", "C", "D", "C"),
    result = c(1, 1, 1, 1)
  )
  # Win scores: A=3, B=1, C=0, D=0
  theta_tbl <- tibble::tibble(ID = c("A", "B", "C", "D"), theta = c(-3, -2, -1, 0), se = 1)
  oriented <- pairwiseLLM:::.bt_orient_theta_by_wins(theta_tbl, bt_data)
  testthat::expect_true(isTRUE(oriented$flipped))
  testthat::expect_true(oriented$cor >= 0)
  testthat::expect_equal(oriented$theta$theta, c(3, 2, 1, 0))

  fit <- list(theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(100, 0, -100), se = c(2, 2, 2)))
  ref <- pairwiseLLM:::.bt_prepare_reference_fit(
    fit = fit,
    bt_data = bt_data,
    core_ids = c("A", "B", "C"),
    scale_method = "median_iqr",
    max_abs = 6
  )
  testthat::expect_true(all(abs(ref$theta$theta) <= 6))
  testthat::expect_true(is.list(ref$reference_norm))
})


testthat::test_that("bt_link_thetas robust methods are available and stable", {
  ref <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 10, 20), se = c(0.1, 0.1, 0.1))
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = c(0.2, 0.2, 0.2))
  lk <- pairwiseLLM::bt_link_thetas(cur, ref, method = "median_iqr")
  testthat::expect_true(is.finite(lk$a))
  testthat::expect_true(is.finite(lk$b))
  testthat::expect_equal(lk$method, "median_iqr")
  testthat::expect_equal(nrow(lk$theta), 3)
  testthat::expect_true(all(c("theta_linked", "se_linked") %in% names(lk$theta)))
})
