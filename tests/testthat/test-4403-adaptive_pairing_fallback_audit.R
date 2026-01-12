test_that("select_adaptive_pairs exposes controlled-random fallback metadata", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(1, 0.5, 0, -0.5),
    se = rep(0.1, 4)
  )

  # Force the "normal" candidate path to yield zero selectable pairs by
  # capping the candidate pool at 0; controlled-random should still propose
  # feasible pairs and expose auditable metadata.
  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    embedding_neighbors = NULL,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    candidate_pool_cap = 0,
    per_anchor_cap = Inf,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_true(is.data.frame(diag))
  expect_true("used_fallback_random" %in% names(diag))
  expect_true("fallback_reason" %in% names(diag))
  expect_true("candidate_counts" %in% names(diag))

  expect_true(isTRUE(diag$used_fallback_random[[1]]))
  expect_true(is.character(diag$fallback_reason[[1]]))
  expect_true(nchar(diag$fallback_reason[[1]]) > 0)

  cc <- diag$candidate_counts[[1]]
  expect_true(is.data.frame(cc))
  expect_identical(names(cc), c("source", "n_candidates", "n_after_filters"))
})

test_that("select_adaptive_pairs does not trigger fallback in normal conditions", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(1, 0.5, 0, -0.5),
    se = rep(0.1, 4)
  )

  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_true(isFALSE(diag$used_fallback_random[[1]]))
  expect_true(is.na(diag$fallback_reason[[1]]) || diag$fallback_reason[[1]] == "")
})

test_that("controlled-random fallback is deterministic given the same inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(1, 0.5, 0, -0.5),
    se = rep(0.1, 4)
  )

  p1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    candidate_pool_cap = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 123
  )

  p2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    candidate_pool_cap = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 123
  )

  expect_identical(p1, p2)
  expect_identical(attr(p1, "pairing_diagnostics"), attr(p2, "pairing_diagnostics"))
})
