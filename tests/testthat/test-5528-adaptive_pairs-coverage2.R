test_that(".ap_gen_candidates filters invalid neighbor indices", {
  id_vec <- c("A", "B")
  th <- c(0, 1)
  se <- c(0.1, 0.1)
  tot <- c(0, 0)

  # Make embed neighbor list include NA and self index when i == 1.
  # This should exercise the invalid-index and self-skip branches and then
  # hit the per-i empty guard.
  embed_nbrs <- list(A = c(NA, "A"), B = character())

  out <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = id_vec,
    th_vec = th,
    se_vec = se,
    tot_vec = tot,
    k_neighbors2 = 0L,
    embed_nbrs = embed_nbrs,
    embed_far_k = 0L,
    hash_round = 0L,
    hash_salt = "pairwiseLLM"
  )

  # With k_neighbors2 == 0 and only invalid/self embed neighbors, no candidates.
  expect_equal(nrow(out), 0L)
})

test_that("select_adaptive_pairs: repeat-guard normalization runs and returns a well-formed result", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(0.1, 0.1, 0.1))

  # Provide a graph_state with unnamed component_id and named degree to trigger
  # defensive normalization logic.
  graph_state <- list(
    degree = stats::setNames(c(0L, 0L, 0L), c("A", "B", "C")),
    component_id = c(1L, 1L, 1L),
    metrics = tibble::tibble(degree_min = 5, largest_component_frac = 1)
  )

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    n_pairs = 2L,
    explore_frac = 0.5,
    repeat_policy = "reverse_only",
    repeat_n = 1L,
    graph_state = graph_state,
    seed = 1
  )

  expect_true(is.data.frame(out))
  expect_true(all(c("ID1", "ID2", "text1", "text2") %in% names(out)))
})

test_that("select_adaptive_pairs: deterministic controlled-random path works", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 0.2), se = c(0.1, 0.1))

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    n_pairs = 1L,
    explore_frac = 0,
    repeat_frac = 0,
    seed = 1
  )
  expect_equal(nrow(out), 1L)

  # n_pairs == 0 should return empty without error
  out0 <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    n_pairs = 0L,
    seed = 1
  )
  expect_equal(nrow(out0), 0L)
})
