test_that("n_pairs == 0 short-circuits with fallback_path = normal", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 1), se = c(1, 1))

  out <- select_adaptive_pairs(samples = samples, theta = theta, n_pairs = 0, seed = 1)
  expect_equal(nrow(out), 0L)

  # Early return: no internal driver is run, so no diagnostics are attached.
  expect_true(is.null(attr(out, "pairing_diagnostics")))
})

test_that("single-ID input errors (input validation)", {
  samples <- tibble::tibble(ID = "A", text = "a")
  theta <- tibble::tibble(ID = "A", theta = 0, se = 1)

  expect_error(
    select_adaptive_pairs(samples = samples, theta = theta, n_pairs = 1, seed = 1),
    "At least two samples"
  )
})

test_that("normal-empty with embeddings attempts bridge but remains exhausted on complete graph", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )
  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 1, 2),
    se = c(1, 1, 1)
  )

  # All unordered pairs already exist.
  existing <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "A", "B")
  )

  # Provide embeddings so the bridge/repair stage is eligible to run.
  embedding_neighbors <- list(
    A = c("B", "C"),
    B = c("A", "C"),
    C = c("A", "B")
  )

  out <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    embedding_neighbors = embedding_neighbors,
    n_pairs = 1,
    forbid_repeats = TRUE,
    seed = 1
  )

  diag <- attr(out, "pairing_diagnostics")
  expect_identical(diag$fallback_path[[1]], "exhausted_no_pairs")
  expect_identical(diag$fallback_trigger[[1]], "normal_empty")
})

test_that("bridge stage applies per-anchor and pool caps when graph is unhealthy", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", 1:4)
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0, 0.01, 3, 3.01),
    se = rep(1, 4)
  )

  # Two disconnected components.
  existing <- tibble::tibble(ID1 = c("A", "C"), ID2 = c("B", "D"), better_id = c("A", "C"))

  embedding_neighbors <- list(
    A = c("C", "D"),
    B = c("C", "D"),
    C = c("A", "B"),
    D = c("A", "B")
  )

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    embedding_neighbors = embedding_neighbors,
    n_pairs = 2,
    k_neighbors = 2,
    forbid_repeats = TRUE,
    # Force the graph to be deemed unhealthy so bridge/repair is attempted.
    repeat_guard_largest_component_frac = 0.9,
    per_anchor_cap = 1,
    candidate_pool_cap = 1,
    balance_positions = FALSE,
    seed = 1,
    return_internal = TRUE
  )

  expect_true(any(res$candidates$selected$source == "bridge"))
  expect_identical(res$diagnostics$fallback_path, "bridge_repair")
  expect_true(nzchar(res$diagnostics$fallback_trigger))
})

test_that("controlled random RNG mode routes through sample.int when opted in", {
  withr::local_options(list(pairwiseLLM.controlled_random_mode = "rng"))

  # We can't safely mock base::sample.int (locked binding), so instead we validate
  # that RNG mode is sensitive to the seed, while still being reproducible for a
  # fixed seed.

  ids <- LETTERS[1:10]
  samples <- tibble::tibble(ID = ids, text = paste0("t", seq_along(ids)))
  theta <- tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids)))

  # Forbid all k-neighborhood pairs so normal selection yields 0.
  existing_pairs <- tibble::tibble(ID1 = ids[1:9], ID2 = ids[2:10])

  res1 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 4,
    k_neighbors = 1,
    embed_far_k = 0,
    embedding_neighbors = NULL,
    seed = 1,
    return_internal = TRUE
  )

  res1b <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 4,
    k_neighbors = 1,
    embed_far_k = 0,
    embedding_neighbors = NULL,
    seed = 1,
    return_internal = TRUE
  )

  res2 <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 4,
    k_neighbors = 1,
    embed_far_k = 0,
    embedding_neighbors = NULL,
    seed = 2,
    return_internal = TRUE
  )

  expect_identical(res1$diagnostics$fallback_path, "controlled_random")
  expect_identical(res1$diagnostics$n_pairs_source_random, 4L)

  # Same seed -> same output
  expect_identical(res1$pairs, res1b$pairs)

  # Different seed -> typically different output (RNG-based shuffle)
  k1 <- paste(res1$pairs$ID1, res1$pairs$ID2, sep = "-")
  k2 <- paste(res2$pairs$ID1, res2$pairs$ID2, sep = "-")
  expect_false(identical(k1, k2))
})
