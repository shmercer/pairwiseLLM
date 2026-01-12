test_that("ap_gen_candidates validates embed_far_k and filters embed ids", {
  ids <- c("A", "B", "C")
  th <- c(0, 1, 2)
  se <- rep(0.1, 3)
  tot <- rep(1L, 3)

  expect_error(
    pairwiseLLM:::.ap_gen_candidates(
      ids, th, se, tot,
      k_neighbors2 = 1L,
      embed_far_k = -1L
    ),
    "embed_far_k"
  )

  embed_nbrs <- list(
    A = c("B", "", NA_character_, "Z"),
    B = c("A"),
    C = character(0)
  )

  cand <- pairwiseLLM:::.ap_gen_candidates(
    ids, th, se, tot,
    k_neighbors2 = 1L,
    embed_nbrs = embed_nbrs,
    embed_far_k = 1L,
    hash_round = 1L
  )

  expect_true(all(c("i_idx", "j_idx", "source") %in% names(cand)))
  expect_false(any(cand$i_idx == cand$j_idx))
  expect_true(all(cand$i_idx >= 1L & cand$i_idx <= length(ids)))
  expect_true(all(cand$j_idx >= 1L & cand$j_idx <= length(ids)))
})


test_that("ap_map_idx_to_ids validates inputs", {
  cand <- tibble::tibble(i_idx = 1L, j_idx = 2L)

  expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(cand, character(0)),
    "id_vec.*NULL/empty"
  )
  expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(tibble::tibble(x = 1L), c("A", "B")),
    "include i_idx"
  )
  expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(tibble::tibble(i_idx = 0L, j_idx = 2L), c("A", "B")),
    "exceed length\\(id_vec\\)"
  )
  expect_error(
    pairwiseLLM:::.ap_map_idx_to_ids(cand, c("A", NA_character_)),
    "NA after mapping"
  )
})


test_that("ap_choose_id_map_vec and ap_pick_low_degree cover defensive cases", {
  gs <- list(ids = c("X", "Y"), degree = c(X = 1, Y = 2))

  expect_equal(pairwiseLLM:::.ap_choose_id_map_vec(character(0), gs), c("X", "Y"))
  expect_equal(pairwiseLLM:::.ap_choose_id_map_vec(c("A"), gs), "A")

  pool <- tibble::tibble(ID1 = "A", ID2 = "B")
  out0 <- pairwiseLLM:::.ap_pick_low_degree(pool, deg_sum = c(1, 2), n_pick = 0L)
  expect_equal(nrow(out0), 0L)

  out1 <- pairwiseLLM:::.ap_pick_low_degree(pool, deg_sum = c(1, 2), n_pick = 1L)
  expect_equal(nrow(out1), 1L)
})


test_that("select_adaptive_pairs uses default se_fill when all SEs are missing", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 1, 2),
    se = c(NA_real_, NA_real_, NA_real_)
  )

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 2L,
    k_neighbors = 1L,
    explore_frac = 0,
    seed = 123
  )

  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 2L)
  expect_true(all(c("ID1", "ID2", "text1", "text2") %in% names(out)))
})


test_that("select_adaptive_pairs returns stable columns when embed_far_k is 0", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 1, 2),
    se = c(NA_real_, NA_real_, NA_real_)
  )

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 2L,
    explore_frac = 0,
    embed_far_k = 0L,
    seed = 123
  )

  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 2L)
  expect_true(all(c("ID1", "ID2", "text1", "text2") %in% names(out)))
})
