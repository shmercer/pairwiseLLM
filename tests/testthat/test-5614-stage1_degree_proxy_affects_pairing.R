# Workstream C: Stage 1 should use a degree-based uncertainty proxy (not BT SE)

test_that("hybrid Stage 1 uses degree-based uncertainty proxy for adaptive pairing", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("tA", "tB", "tC", "tD")
  )

  # Existing results:
  # - B is connected to A and C (degree 2)
  # - A and C each have degree 1
  # - D is unseen (degree 0)
  # Use symmetric wins so Rank Centrality doesn't strongly separate items.
  # Keep the graph connected so this test isolates the degree-based uncertainty
  # proxy behavior (bridging is tested separately).
  initial_results <- dplyr::bind_rows(
    tibble::tibble(
      ID1 = c("A", "B", "B", "C"),
      ID2 = c("B", "A", "C", "B"),
      better_id = c("A", "B", "B", "C")
    ),
    # Add symmetric judgments connecting D via B while keeping RC ranks degenerate.
    tibble::tibble(
      ID1 = c(rep("B", 10), rep("D", 10)),
      ID2 = c(rep("D", 10), rep("B", 10)),
      better_id = c(rep("B", 10), rep("D", 10))
    )
  )

  captured <- new.env(parent = emptyenv())
  judge_fun <- function(pairs) {
    captured$pairs <- pairs
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  # Mock BT fit: return an extreme SE for B that would dominate selection if Stage 1 used BT SE.
  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    theta_tbl <- tibble::tibble(
      ID = ids,
      theta = rep(0, length(ids)),
      se = ifelse(ids == "B", 100, 0.1)
    )
    list(
      engine = "mock",
      theta = theta_tbl,
      reliability = NA_real_,
      diagnostics = list(sepG = 0)
    )
  }

  bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "hybrid",
    init_round_size = 0,
    round_size = 1,
    min_rounds = 0,
    max_rounds = 1,
    seed_pairs = 1,
    k_neighbors = Inf,
    min_judgments = 0,
    stage1_explore_frac = 0,
    repeat_policy = "none",
    balance_positions = FALSE,
    w_embed = 0,
    # Ensure we remain in Stage 1 when proposing the next pair.
    stage1_k_conn = 999,
    stage1_k_stab = 999,
    # Use damping so rank centrality is well-defined with unseen nodes.
    rc_damping = 0.9
  )

  expect_true(!is.null(captured$pairs))
  expect_equal(nrow(captured$pairs), 1L)

  # With the degree-proxy uncertainty, the selector should prefer lower-degree seen nodes
  # (A and C) over pairing the higher-degree node B with unseen node D, despite B's huge BT SE.
  expect_equal(captured$pairs$ID1[[1]], "A")
  expect_equal(captured$pairs$ID2[[1]], "C")
})
