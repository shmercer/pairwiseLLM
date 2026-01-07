test_that("5510 repeat_reverse generator returns empty on id mismatch and preserves schema", {
  existing_completed <- tibble::tibble(
    ID1 = "X",
    ID2 = "Y"
  )

  out <- pairwiseLLM:::.ap_gen_repeat_reverse(
    existing_completed = existing_completed,
    id_vec = c("A", "B"),
    repeat_cap = 1L
  )

  expect_true(tibble::is_tibble(out))
  expect_identical(
    names(out),
    c(
      "i_idx", "j_idx", "anchor_idx", "pair_key", "source",
      "embed_hit", "embed_rank", "directed",
      "score_info", "score_need", "score_embed", "score_total"
    )
  )
  expect_identical(nrow(out), 0L)
})

test_that("5510 repeat_reverse generator validates required columns", {
  expect_error(
    pairwiseLLM:::.ap_gen_repeat_reverse(
      existing_completed = tibble::tibble(foo = 1),
      id_vec = c("A", "B"),
      repeat_cap = 1L
    ),
    "contain columns ID1 and ID2"
  )
})

test_that("5510 bridge candidate generator handles edge cases and caps", {
  # n < 2 => empty, schema-stable
  out_small <- pairwiseLLM:::.ap_gen_bridge_candidates(
    id_vec = "A",
    embed_nbrs = list(A = "B"),
    component_id = c(A = 1L),
    max_neighbors_per_anchor = 3L
  )
  expect_true(tibble::is_tibble(out_small))
  expect_identical(
    names(out_small),
    c(
      "i_idx", "j_idx", "anchor_idx", "pair_key", "source",
      "embed_hit", "embed_rank", "directed"
    )
  )
  expect_identical(nrow(out_small), 0L)

  # Input validation: embed_nbrs must be named list
  expect_error(
    pairwiseLLM:::.ap_gen_bridge_candidates(
      id_vec = c("A", "B"),
      embed_nbrs = NULL,
      component_id = c(A = 1L, B = 2L)
    ),
    "named list of neighbor IDs"
  )

  expect_error(
    pairwiseLLM:::.ap_gen_bridge_candidates(
      id_vec = c("A", "B"),
      embed_nbrs = list("B"),
      component_id = c(A = 1L, B = 2L)
    ),
    "named list of neighbor IDs"
  )

  expect_error(
    pairwiseLLM:::.ap_gen_bridge_candidates(
      id_vec = c("A", "B"),
      embed_nbrs = list(A = "B", B = "A"),
      component_id = c(A = 1L, B = 2L),
      max_neighbors_per_anchor = -1L
    ),
    "max_neighbors_per_anchor"
  )

  out_cap0 <- pairwiseLLM:::.ap_gen_bridge_candidates(
    id_vec = c("A", "B"),
    embed_nbrs = list(A = "B", B = "A"),
    component_id = c(A = 1L, B = 2L),
    max_neighbors_per_anchor = 0L
  )
  expect_identical(nrow(out_cap0), 0L)
  expect_identical(names(out_cap0), names(out_small))

  # Component IDs with NA trigger the defensive fallback, and per-anchor caps apply.
  id_vec <- c("A", "B", "C")
  embed_nbrs <- list(
    A = c("B", "C"),
    B = c("A", "C"),
    C = c("A", "B")
  )

  out_defensive <- pairwiseLLM:::.ap_gen_bridge_candidates(
    id_vec = id_vec,
    embed_nbrs = embed_nbrs,
    component_id = c(A = 1L, B = 1L, C = NA_integer_),
    max_neighbors_per_anchor = 1L
  )

  expect_true(all(out_defensive$source == "bridge"))
  expect_true(all(out_defensive$i_idx != out_defensive$j_idx))

  # Each anchor should contribute at most 1 edge due to max_neighbors_per_anchor = 1.
  anchor_counts <- table(out_defensive$anchor_idx)
  expect_true(all(as.integer(anchor_counts) <= 1L))

  # If all nodes are in the same component, no bridge edges are possible.
  out_no_cross <- pairwiseLLM:::.ap_gen_bridge_candidates(
    id_vec = id_vec,
    embed_nbrs = embed_nbrs,
    component_id = stats::setNames(rep(1L, length(id_vec)), id_vec),
    max_neighbors_per_anchor = 2L
  )
  expect_identical(nrow(out_no_cross), 0L)
  expect_identical(names(out_no_cross), names(out_small))
})
