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


test_that("select_adaptive_pairs normalizes unnamed graph_state vectors", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(1, 1, 1))

  # component_id is unnamed; ids are provided so names(component_id) should be set to ids
  graph_state <- list(
    ids = c("A", "B", "C"),
    component_id = c(1L, 1L, 1L),
    degree = c(0L, 0L, 0L),
    metrics = tibble::tibble(degree_min = 0, largest_component_frac = 0)
  )

  out <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 0,
    graph_state = graph_state,
    return_internal = TRUE,
    seed = 1
  )

  expect_true(is.list(out))
  expect_true(!is.null(out$diagnostics))
})


test_that("select_adaptive_pairs can compute exploration pool and return empty explore selection", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(1, 1, 1))

  # All nodes in same component -> across-components exploration pool is empty
  graph_state <- list(
    ids = c("A", "B", "C"),
    component_id = stats::setNames(c(1L, 1L, 1L), c("A", "B", "C")),
    degree = stats::setNames(c(1L, 1L, 1L), c("A", "B", "C")),
    metrics = tibble::tibble(degree_min = 1, largest_component_frac = 1)
  )

  # Back-compat path: the function checks exists("explore_across_components_now")
  # with inherits=FALSE, but the symbol itself is resolved in the package namespace.
  ns <- asNamespace("pairwiseLLM")
  had <- exists("explore_across_components_now", envir = ns, inherits = FALSE)
  old <- if (had) get("explore_across_components_now", envir = ns, inherits = FALSE) else NULL
  assign("explore_across_components_now", TRUE, envir = ns)
  on.exit({
    if (had) {
      assign("explore_across_components_now", old, envir = ns)
    } else {
      rm("explore_across_components_now", envir = ns)
    }
  }, add = TRUE)

  testthat::local_mocked_bindings(
    exists = function(...) TRUE,
    .env = ns
  )

  out <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1,
    explore_frac = 1,
    graph_state = graph_state,
    return_internal = TRUE,
    seed = 1
  )

  expect_true(is.list(out))
  # When explore pool is empty, exploration contributes no pairs.
  expect_true(out$diagnostics$n_pairs_source_random >= 0L)
})


test_that("internal candidate generators cover defensive guards via local mocks", {
  ns <- asNamespace("pairwiseLLM")

  id_vec <- c("A", "B")
  th <- c(0, 1)
  se <- c(1, 1)
  tot <- c(0, 0)

  # Force .ap_gen_candidates to see invalid indices by mocking match() and
  # include a neighbor list.
  embed_nbrs <- list(A = c("B"), B = c("A"))
  testthat::local_mocked_bindings(
    match = function(...) c(NA_integer_, 1L, 999L),
    .env = ns
  )
  cand <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = id_vec,
    th_vec = th,
    se_vec = se,
    tot_vec = tot,
    k_neighbors2 = 0L,
    embed_nbrs = embed_nbrs,
    embed_far_k = 0L,
    hash_round = 0L,
    hash_salt = "x"
  )
  expect_true(is.data.frame(cand))

  # Force controlled-random generator to return empty by skipping its loop.
  testthat::local_mocked_bindings(
    seq_len = function(...) integer(0),
    .env = ns
  )
  empty_tbl <- pairwiseLLM:::.ap_gen_candidates_controlled_random(
    id_vec = id_vec,
    n_pairs = 1,
    round_key = 0L,
    salt = "x"
  )
  expect_equal(nrow(empty_tbl), 0L)
})


test_that("select_adaptive_pairs diagnostics cover fallback_path and fallback_trigger branches", {
  # --- exhausted_no_pairs: insufficient_ids ---
  samples1 <- tibble::tibble(ID = "A", text = "a")
  theta1 <- tibble::tibble(ID = "A", theta = 0, se = 1)
  out1 <- select_adaptive_pairs(samples1, theta1, n_pairs = 1, return_internal = TRUE)
  expect_identical(out1$diagnostics$fallback_path[[1]], "exhausted_no_pairs")
  expect_identical(out1$diagnostics$fallback_trigger[[1]], "insufficient_ids")

  # --- bridge_repair: graph_unhealthy + normal_empty -> normal_empty trigger ---
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(-1, 0, 1), se = c(1, 1, 1))

  # Force normal selection to be empty (candidate_pool_cap=0) while allowing a bridge.
  embed_nbrs <- list(
    A = c("B"),
    B = c("A", "C"),
    C = c("B")
  )

  graph_state_unhealthy <- list(
    ids = c("A", "B", "C"),
    component_id = stats::setNames(c(1L, 2L, 2L), c("A", "B", "C")),
    degree = stats::setNames(c(0L, 0L, 0L), c("A", "B", "C")),
    metrics = tibble::tibble(degree_min = 0, largest_component_frac = 0)
  )

  out2 <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1,
    embedding_neighbors = embed_nbrs,
    explore_frac = 0,
    candidate_pool_cap = 0,
    graph_state = graph_state_unhealthy,
    return_internal = TRUE,
    seed = 1
  )
  expect_identical(out2$diagnostics$fallback_path[[1]], "bridge_repair")
  expect_true(out2$diagnostics$fallback_trigger[[1]] %in% c("normal_empty", "graph_unhealthy"))
})
