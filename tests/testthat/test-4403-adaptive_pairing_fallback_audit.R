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

  existing_pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("B", "C", "D")
  )

  # Provide a connected existing graph so component-bridge exploration is not
  # required (single component). This lets us deterministically force the
  # normal candidate path to be empty and trigger controlled-random fallback.
  existing_pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("B", "C", "D")
  )

  # Force the "normal" candidate path to yield zero selectable pairs by
  # capping the candidate pool at 0; controlled-random should still propose
  # feasible pairs and expose auditable metadata.
  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    embedding_neighbors = NULL,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    # With a connected existing graph and explore_frac = 0, there is no
    # component-bridge exploration. candidate_pool_cap=0 then exhausts the
    # normal candidate path.
    explore_frac = 0,
    candidate_pool_cap = 0,
    per_anchor_cap = Inf,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_true(is.data.frame(diag))

  # Audit: controlled-random fallback is explicit via path + trigger.
  expect_true("fallback_path" %in% names(diag))
  expect_true("fallback_trigger" %in% names(diag))
  expect_identical(as.character(diag$fallback_path[[1]]), "controlled_random")
  expect_true(is.character(diag$fallback_trigger[[1]]))
  expect_true(nchar(diag$fallback_trigger[[1]]) > 0)

  # No list-columns in diagnostics.
  diag_ok <- diag[1, , drop = FALSE]
  expect_false(any(vapply(diag_ok, is.list, logical(1))))
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

  existing_pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("B", "C", "D")
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
  expect_identical(as.character(diag$fallback_path[[1]]), "normal")
  expect_true(is.na(diag$fallback_trigger[[1]]) || diag$fallback_trigger[[1]] == "")
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
  existing_pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("B", "C", "D")
  )


  p1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    explore_frac = 0,
    candidate_pool_cap = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 123
  )

  p2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 3,
    k_neighbors = 2,
    min_judgments = 0,
    explore_frac = 0,
    candidate_pool_cap = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 123
  )

  expect_identical(p1, p2)
  expect_identical(attr(p1, "pairing_diagnostics"), attr(p2, "pairing_diagnostics"))
})
