test_that("exploration preserves theta-ordered id mapping", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("text_", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0.2, 1.0, -1.0, 0.5),
    se = rep(1, 4)
  )

  # Ensure sample (graph_state) order differs from theta order.
  sample_order <- samples$ID
  theta_order <- dplyr::arrange(theta, theta, ID)$ID
  expect_false(identical(sample_order, theta_order))

  withr::local_seed(123)

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    k_neighbors = 1,
    explore_frac = 1,
    balance_positions = FALSE,
    return_internal = TRUE,
    seed = 123
  )

  # The exploration selector operates on index candidates (i_idx/j_idx) built
  # against theta_order. Returned IDs must reflect that same mapping.
  idx_row <- dplyr::slice(res$candidates$selected, 1)
  expect_true(all(c("i_idx", "j_idx") %in% names(idx_row)))

  expect_identical(res$pairs$ID1[[1]], theta_order[[idx_row$i_idx[[1]]]])
  expect_identical(res$pairs$ID2[[1]], theta_order[[idx_row$j_idx[[1]]]])

  # Regression check: it must *not* accidentally map indices using sample order.
  expect_false(identical(res$pairs$ID1[[1]], sample_order[[idx_row$i_idx[[1]]]]))
  expect_false(identical(res$pairs$ID2[[1]], sample_order[[idx_row$j_idx[[1]]]]))
})
