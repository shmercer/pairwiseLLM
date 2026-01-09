# Additional branch coverage for R/adaptive_pairs.R

 test_that("select_adaptive_pairs attaches pairing diagnostics and normalizes unnamed graph_state vectors", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(1, 1, 1))

  # component_id and degree are unnamed; ids are provided so names should be normalized.
  graph_state <- list(
    ids = c("A", "B", "C"),
    component_id = c(1L, 1L, 1L),
    degree = c(0L, 0L, 0L),
    metrics = tibble::tibble(degree_min = 0, largest_component_frac = 1)
  )

  # n_pairs must be > 0 to exercise the full pairing path and attach diagnostics.
  out <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1,
    graph_state = graph_state,
    seed = 1
  )

  diag <- attr(out, "pairing_diagnostics")
  expect_true(is.data.frame(diag))
  expect_true(all(c("fallback_path", "fallback_trigger") %in% names(diag)))
 })


 test_that("select_adaptive_pairs validates explore_frac bounds", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 1), se = c(1, 1))

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, explore_frac = -0.01),
    "explore_frac"
  )
  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, explore_frac = 1.01),
    "explore_frac"
  )
 })


 test_that("candidate generators hit defensive guards without mocking base functions", {
  # n == 0 returns an empty tibble
  empty0 <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = character(),
    th_vec = numeric(),
    se_vec = numeric(),
    tot_vec = numeric(),
    k_neighbors2 = 1L
  )
  expect_equal(nrow(empty0), 0L)

  # embed_far_k validation
  expect_error(
    pairwiseLLM:::.ap_gen_candidates(
      id_vec = c("A", "B"),
      th_vec = c(0, 1),
      se_vec = c(1, 1),
      tot_vec = c(0, 0),
      k_neighbors2 = 0L,
      embed_far_k = -1L
    ),
    "embed_far_k"
  )

  # embed_nbrs containing invalid IDs (NA/empty/nonexistent) exercises match/NA filters
  cand <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = c("A", "B"),
    th_vec = c(0, 1),
    se_vec = c(1, 1),
    tot_vec = c(0, 0),
    k_neighbors2 = 0L,
    embed_nbrs = list(A = c(NA_character_, "", "Z", "B"), B = c("A")),
    embed_far_k = 1L,
    hash_round = 1L,
    hash_salt = "x"
  )
  expect_true(is.data.frame(cand))
  expect_true(all(c("i_idx", "j_idx", "source") %in% names(cand)))

  # Controlled-random generator empty return (n_pairs <= 0)
  empty1 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = c("A", "B"),
    n_pairs = 0,
    round_key = 1L,
    salt = "x"
  )
  expect_equal(nrow(empty1), 0L)
 })


 test_that("select_adaptive_pairs diagnostics exercise bridge_repair and controlled_random fallbacks", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(1, 1, 1))

  # Two components, unhealthy graph, and no normal candidates -> should prefer bridge repair.
  graph_state_unhealthy <- list(
    ids = c("A", "B", "C"),
    component_id = stats::setNames(c(1L, 2L, 2L), c("A", "B", "C")),
    degree = stats::setNames(c(0L, 0L, 0L), c("A", "B", "C")),
    metrics = tibble::tibble(degree_min = 0, largest_component_frac = 0)
  )

  # Provide embedding neighbors that allow a bridge pair across components.
  embed_nbrs <- list(A = c("B"), B = c("A", "C"), C = c("B"))

  out_bridge <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1,
    candidate_pool_cap = 0, # forces normal selection empty
    embedding_neighbors = embed_nbrs,
    graph_state = graph_state_unhealthy,
    seed = 1
  )
  diag_bridge <- attr(out_bridge, "pairing_diagnostics")
  expect_true(is.data.frame(diag_bridge))
  expect_true(diag_bridge$fallback_path[[1]] %in% c("bridge_repair", "controlled_random"))

  # Healthy graph, no normal candidates, no bridge candidates -> controlled_random.
  graph_state_healthy <- list(
    ids = c("A", "B", "C"),
    component_id = stats::setNames(c(1L, 1L, 1L), c("A", "B", "C")),
    degree = stats::setNames(c(1L, 1L, 1L), c("A", "B", "C")),
    metrics = tibble::tibble(degree_min = 1, largest_component_frac = 1)
  )

  out_rand <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1,
    candidate_pool_cap = 0,
    embedding_neighbors = NULL,
    graph_state = graph_state_healthy,
    seed = 1
  )
  diag_rand <- attr(out_rand, "pairing_diagnostics")
  expect_true(is.data.frame(diag_rand))
  expect_true(diag_rand$fallback_path[[1]] %in% c("controlled_random", "bridge_repair"))
 })
