test_that("fit_rank_centrality returns finite, ordered scores on a transitive toy graph", {
  bt_data <- tibble::tibble(
    object1 = c("A", "A", "B"),
    object2 = c("B", "C", "C"),
    result  = c(1, 1, 1)
  )

  fit <- fit_rank_centrality(bt_data, smoothing = 0.5, damping = 0)

  expect_true(is.list(fit))
  expect_equal(fit$engine, "rank_centrality")
  expect_true(is.na(fit$reliability))

  th <- fit$theta
  expect_true(all(c("ID", "theta", "pi") %in% names(th)))
  expect_equal(sum(th$pi), 1, tolerance = 1e-10)
  expect_true(all(is.finite(th$theta)))
  expect_true(all(is.finite(th$pi)))

  # Expect ordering A > B > C
  th <- dplyr::arrange(th, ID)
  theta_map <- setNames(th$theta, th$ID)
  expect_gt(theta_map[["A"]], theta_map[["B"]])
  expect_gt(theta_map[["B"]], theta_map[["C"]])

  # Diagnostics sanity
  expect_true(is.list(fit$diagnostics))
  expect_equal(fit$diagnostics$n_nodes, 3)
  expect_equal(fit$diagnostics$n_edges, 3)
  expect_equal(fit$diagnostics$n_components, 1)
})

test_that("fit_rank_centrality stays finite under extreme dominance (near-separation)", {
  # A dominates B and C many times
  bt_data <- tibble::tibble(
    object1 = c(rep("A", 20), rep("A", 20), rep("B", 5)),
    object2 = c(rep("B", 20), rep("C", 20), rep("C", 5)),
    result  = c(rep(1, 40), rep(1, 5))
  )

  fit <- fit_rank_centrality(bt_data, smoothing = 0.5, damping = 0)

  th <- fit$theta
  expect_true(all(is.finite(th$theta)))
  expect_true(all(is.finite(th$pi)))
  expect_equal(sum(th$pi), 1, tolerance = 1e-10)

  theta_map <- setNames(th$theta, th$ID)
  expect_gt(theta_map[["A"]], theta_map[["B"]])
  expect_gt(theta_map[["A"]], theta_map[["C"]])
})

test_that("fit_rank_centrality reports disconnected components and supports damping", {
  # Two disconnected components: A-B and C-D
  bt_data <- tibble::tibble(
    object1 = c("A", "C"),
    object2 = c("B", "D"),
    result  = c(1, 1)
  )

  fit0 <- fit_rank_centrality(bt_data, smoothing = 0.5, damping = 0)
  expect_equal(fit0$diagnostics$n_components, 2)
  expect_equal(fit0$diagnostics$largest_component_frac, 0.5)

  # With damping, stationary distribution remains well-defined and finite
  fit1 <- fit_rank_centrality(bt_data, smoothing = 0.5, damping = 0.2)
  expect_equal(fit1$diagnostics$n_components, 2)
  expect_true(all(is.finite(fit1$theta$theta)))
  expect_true(all(is.finite(fit1$theta$pi)))
  expect_equal(sum(fit1$theta$pi), 1, tolerance = 1e-10)
})
