test_that("compute_ranking_from_theta_mean sorts with stable tie-breaking", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  theta <- c(B = 0.3, C = 0.3, A = 0.2)
  out <- pairwiseLLM:::compute_ranking_from_theta_mean(theta, state)
  expect_equal(out, c("B", "C", "A"))

  expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(c(1, 2), state),
    "named numeric"
  )
  expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(c(A = 1, B = NA), state),
    "missing"
  )
  expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(c(A = 1, B = 2, D = 3), state),
    "match `state\\$ids`"
  )
  expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(c("A", "B"), state),
    "numeric"
  )
  expect_error(
    pairwiseLLM:::compute_ranking_from_theta_mean(c(A = 1, A = 2), state),
    "unique"
  )
})

test_that("select_window_size follows phase rules", {
  expect_equal(pairwiseLLM:::select_window_size(400, phase = "phase2"), 50L)
  expect_equal(pairwiseLLM:::select_window_size(600, phase = "phase2"), 100L)
  expect_equal(pairwiseLLM:::select_window_size(200, phase = "phase3"), 25L)
  expect_equal(
    pairwiseLLM:::select_window_size(600, phase = "phase2", near_stop = TRUE),
    50L
  )
  expect_error(pairwiseLLM:::select_window_size("bad"), "positive numeric")
  expect_error(
    pairwiseLLM:::select_window_size(100, near_stop = "yes"),
    "TRUE or FALSE"
  )
})

test_that("build_candidate_pairs uses window and duplicate filtering", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "beta", "gamma", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$unordered_count[["B:C"]] <- 2L
  state$ordered_seen <- c("B:C" = TRUE, "C:B" = TRUE)

  ranking_ids <- c("A", "B", "C", "D")
  out <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0
  )
  expect_equal(out$unordered_key, c("A:B", "C:D"))
})

test_that("build_candidate_pairs exploration is deterministic under seed", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  ranking_ids <- LETTERS[1:5]

  out1 <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0.5,
    seed = 11
  )
  out2 <- pairwiseLLM:::build_candidate_pairs(
    ranking_ids,
    W = 1,
    state = state,
    exploration_frac = 0.5,
    seed = 11
  )

  expect_equal(out1, out2)
  expect_equal(nrow(out1), 6L)
  expect_equal(length(unique(out1$unordered_key)), nrow(out1))
})

test_that("build_candidate_pairs validates inputs and exploration floor", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_error(
    pairwiseLLM:::build_candidate_pairs("A", W = 1, state = state),
    "at least two ids"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "A", "B"), W = 1, state = state),
    "duplicates"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "B", "D"), W = 1, state = state),
    "match `state\\$ids`"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(c("A", "B", "C"), W = 0, state = state),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::build_candidate_pairs(
      c("A", "B", "C"),
      W = 1,
      state = state,
      exploration_frac = 1.5
    ),
    "between 0 and 1"
  )

  out <- pairwiseLLM:::build_candidate_pairs(
    c("A", "B", "C"),
    W = 1,
    state = state,
    exploration_frac = 0.01,
    seed = 10
  )
  expect_equal(nrow(out), 3L)
  expect_true("A:C" %in% out$unordered_key)
})

test_that("utility and degree penalty match known values", {
  theta_draws <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  colnames(theta_draws) <- c("A", "B")
  candidates <- tibble::tibble(i_id = "A", j_id = "B")
  utility <- pairwiseLLM:::compute_pair_utility(theta_draws, candidates)
  expected <- stats::var(stats::plogis(c(-1, 1)))
  expect_equal(utility$utility_raw, expected)

  samples <- tibble::tibble(ID = c("A", "B"), text = c("alpha", "beta"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos1[["A"]] <- 2L
  state$pos2[["A"]] <- 1L
  state$pos1[["B"]] <- 3L
  state$pos2[["B"]] <- 4L
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2
  penalty <- pairwiseLLM:::apply_degree_penalty(utility, state)
  expected_penalty <- expected / sqrt((3 + 1) * (7 + 1))
  expect_equal(penalty$utility, expected_penalty)
})

test_that("utility helpers validate inputs", {
  theta_draws <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  colnames(theta_draws) <- c("A", "B")

  expect_error(
    pairwiseLLM:::compute_pair_utility(list(), tibble::tibble(i_id = "A", j_id = "B")),
    "numeric matrix"
  )
  expect_error(
    pairwiseLLM:::compute_pair_utility(matrix(1, nrow = 1, ncol = 2), tibble::tibble(i_id = "A", j_id = "B")),
    "at least two draws"
  )
  no_names <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  expect_error(
    pairwiseLLM:::compute_pair_utility(no_names, tibble::tibble(i_id = "A", j_id = "B")),
    "column names"
  )
  expect_error(
    pairwiseLLM:::compute_pair_utility(theta_draws, list()),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::compute_pair_utility(theta_draws, tibble::tibble(i_id = NA, j_id = "B")),
    "non-missing"
  )
  expect_error(
    pairwiseLLM:::compute_pair_utility(theta_draws, tibble::tibble(i_id = "A", j_id = "C")),
    "present in `theta_draws`"
  )

  out <- pairwiseLLM:::compute_pair_utility(
    theta_draws,
    tibble::tibble(i_id = "A", j_id = "B", unordered_key = "A:B")
  )
  expect_equal(out$unordered_key, "A:B")

  samples <- tibble::tibble(ID = c("A", "B"), text = c("alpha", "beta"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  expect_error(
    pairwiseLLM:::apply_degree_penalty(list(), state),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::apply_degree_penalty(
      tibble::tibble(i_id = "A", j_id = "B", utility_raw = "x"),
      state
    ),
    "utility_tbl\\$utility_raw"
  )
  expect_error(
    pairwiseLLM:::apply_degree_penalty(
      tibble::tibble(i_id = "A", j_id = "C", utility_raw = 1),
      state
    ),
    "state\\$ids"
  )
})

test_that("select_pairs_from_candidates respects caps and duplicates", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "beta", "gamma", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$unordered_count[["C:D"]] <- 2L
  state$ordered_seen <- c("C:D" = TRUE, "D:C" = TRUE)

  utilities <- tibble::tibble(
    i_id = c("C", "A", "B", "A"),
    j_id = c("D", "C", "D", "B"),
    unordered_key = c("C:D", "A:C", "B:D", "A:B"),
    utility_raw = c(10, 9, 8, 7),
    utility = c(10, 9, 8, 7)
  )

  out <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 2,
    per_item_cap = 1,
    phase = "phase2",
    iter = 1L,
    seed = 1
  )

  expect_equal(nrow(out$pairs), 2L)
  counts <- table(c(out$pairs$A_id, out$pairs$B_id))
  expect_true(all(counts <= 1L))
  expect_false("C:D" %in% out$pairs$unordered_key)
})

test_that("select_pairs_from_candidates handles validation and empty batches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  utilities <- tibble::tibble(
    i_id = c("A", "A"),
    j_id = c("B", "C"),
    unordered_key = c("A:B", "A:C"),
    utility_raw = c(2, 1),
    utility = c(2, 1)
  )

  expect_error(
    pairwiseLLM:::select_pairs_from_candidates(
      state,
      "bad",
      batch_size = 1,
      iter = 1L
    ),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::select_pairs_from_candidates(
      state,
      utilities,
      batch_size = -1,
      iter = 1L
    ),
    "non-negative"
  )
  expect_error(
    pairwiseLLM:::select_pairs_from_candidates(
      state,
      utilities,
      batch_size = 1,
      iter = 1.5
    ),
    "length-1 integer"
  )
  expect_error(
    pairwiseLLM:::select_pairs_from_candidates(
      state,
      utilities,
      batch_size = 1,
      per_item_cap = 0,
      iter = 1L
    ),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::select_pairs_from_candidates(
      state,
      tibble::tibble(
        i_id = "A",
        j_id = "D",
        unordered_key = "A:D",
        utility_raw = 1,
        utility = 1
      ),
      batch_size = 1,
      iter = 1L
    ),
    "state\\$ids"
  )

  out_empty <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 0,
    iter = 1L
  )
  expect_equal(nrow(out_empty$pairs), 0L)

  out_default_cap <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 1,
    iter = 1L
  )
  expect_equal(nrow(out_default_cap$pairs), 1L)
})

test_that("select_pairs_from_candidates skips when cap reached", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  utilities <- tibble::tibble(
    i_id = c("A", "A"),
    j_id = c("B", "C"),
    unordered_key = c("A:B", "A:C"),
    utility_raw = c(2, 1),
    utility = c(2, 1)
  )

  out <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 2,
    per_item_cap = 1,
    iter = 1L
  )

  expect_equal(nrow(out$pairs), 1L)
  expect_equal(out$pairs$unordered_key, "A:B")
})

test_that("select_pairs_from_candidates flips to reverse order when allowed", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos1[["B"]] <- 2L
  state$pos2[["B"]] <- 0L
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2
  state <- pairwiseLLM:::record_exposure(state, "A", "B")

  utilities <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility_raw = 1,
    utility = 1
  )

  out <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 1,
    per_item_cap = 1,
    iter = 1L
  )

  expect_equal(out$pairs$A_id, "B")
  expect_equal(out$pairs$B_id, "A")
})

test_that("select_pairs_from_candidates drops pairs when duplicates exhausted", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$unordered_count[["A:B"]] <- 2L
  state$ordered_seen <- c("A:B" = TRUE, "B:A" = TRUE)

  utilities <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility_raw = 1,
    utility = 1
  )

  out <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 1,
    per_item_cap = 1,
    iter = 1L
  )

  expect_equal(nrow(out$pairs), 0L)
})

test_that("select_pairs_from_candidates honors position balancing and seed", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos1[["A"]] <- 3L
  state$pos2[["A"]] <- 0L
  state$pos1[["B"]] <- 0L
  state$pos2[["B"]] <- 2L
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2

  utilities <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility_raw = 1,
    utility = 1
  )

  out1 <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 1,
    per_item_cap = 1,
    phase = "phase2",
    iter = 1L,
    seed = 99
  )
  out2 <- pairwiseLLM:::select_pairs_from_candidates(
    state,
    utilities,
    batch_size = 1,
    per_item_cap = 1,
    phase = "phase2",
    iter = 1L,
    seed = 99
  )

  expect_equal(out1$pairs$A_id, "B")
  expect_equal(out1$pairs$B_id, "A")
  expect_equal(out1$pairs, out2$pairs)
})
