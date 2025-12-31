test_that("select_core_link_pairs accepts k_neighbors=Inf/NULL and min_judgments=NULL", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("dplyr")

  # Minimal sample set
  samples <- tibble::tibble(
    ID = paste0("id", 1:8),
    text = rep("x", 8)
  )

  # Minimal theta table
  theta <- tibble::tibble(ID = samples$ID, theta = seq(-1.75, 1.75, length.out = 8), se = rep(0.25, 8))
  core_ids <- samples$ID[1:4]

  p1 <- select_core_link_pairs(
    samples = samples, theta = theta, core_ids = core_ids,
    k_neighbors = Inf, min_judgments = NULL, round_size = 6,
    forbid_keys = character(0), balance_positions = TRUE
  )
  expect_true(is.data.frame(p1))
  expect_lte(nrow(p1), 6)

  p2 <- select_core_link_pairs(
    samples = samples, theta = theta, core_ids = core_ids,
    k_neighbors = NULL, min_judgments = NULL, round_size = 6,
    forbid_keys = character(0), balance_positions = TRUE
  )
  expect_true(is.data.frame(p2))
  expect_lte(nrow(p2), 6)
})
