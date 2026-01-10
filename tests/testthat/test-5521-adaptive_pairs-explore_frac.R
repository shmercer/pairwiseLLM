test_that("select_adaptive_pairs applies explore_frac quota (exploration + exploitation)", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", 1:4)
  )

  # Make A/B highly uncertain so pure exploitation favors pairs involving them,
  # while C/D are low-uncertainty and low-degree (prime exploration targets).
  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(-2, -1, 1, 2),
    se = c(5, 5, 0.1, 0.1)
  )

  # Existing results only involve A/B, leaving C/D with degree 0.
  existing <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "B", "A"),
    better_id = c("A", "A", "B")
  )

  gs <- pairwiseLLM:::.graph_state_from_pairs(existing, ids = samples$ID)

  out_explore <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    graph_state = gs,
    n_pairs = 4,
    k_neighbors = Inf,
    min_judgments = 0,
    repeat_policy = "none",
    explore_frac = 0.5,
    return_internal = TRUE,
    seed = 123
  )

  out_exploit <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    graph_state = gs,
    n_pairs = 4,
    k_neighbors = Inf,
    min_judgments = 0,
    repeat_policy = "none",
    explore_frac = 0,
    return_internal = TRUE,
    seed = 123
  )

  # Helper: unordered key.
  key <- function(a, b) paste0(pmin(a, b), "||", pmax(a, b))

  keys_explore <- key(out_explore$pairs$ID1, out_explore$pairs$ID2)
  keys_exploit <- key(out_exploit$pairs$ID1, out_exploit$pairs$ID2)
  has_component_bridge <- function(out) {
    sel <- out$candidates$selected
    if (!is.list(out$candidates) || is.null(sel) || !is.data.frame(sel)) return(FALSE)
    if (!("pair_type" %in% names(sel))) return(FALSE)
    any(sel$pair_type == "component_bridge", na.rm = TRUE)
  }

  # With exploration enabled on a disconnected graph, component-bridge pairs
  # should be allocated before other exploration candidates.
  expect_true(has_component_bridge(out_explore))
  # With explore_frac = 0, explicit component-bridge pairs should still appear when disconnected.
  expect_true(has_component_bridge(out_exploit))
  sel_exploit <- out_exploit$candidates$selected
  n_bridge <- 0L
  if (is.data.frame(sel_exploit) && "pair_type" %in% names(sel_exploit)) {
    n_bridge <- sum(sel_exploit$pair_type == "component_bridge", na.rm = TRUE)
  }
  expect_equal(n_bridge, 2L)

  # The exploration+exploitation merge should still respect the requested count.
  expect_equal(nrow(out_explore$pairs), 4L)
  expect_equal(nrow(out_exploit$pairs), 4L)
})
