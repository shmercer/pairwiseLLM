test_that(".ap_gen_candidates exercises theta/embed/far candidate loops", {
  ids <- c("A", "B", "C")
  th <- c(0, 1, 2)
  se <- c(1, 1, 1)
  tot <- c(0L, 0L, 0L)

  embed_nbrs <- list(
    A = c("B", "C"),
    B = c("A"),
    C = c("A")
  )

  out <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = ids,
    th_vec = th,
    se_vec = se,
    tot_vec = tot,
    k_neighbors2 = 1L,
    embed_nbrs = embed_nbrs,
    embed_far_k = 1L,
    hash_round = 7L,
    hash_salt = "test"
  )

  expect_true(nrow(out) > 0L)
  expect_true(all(c("i_idx", "j_idx", "pair_key", "source", "anchor_idx") %in% names(out)))
  expect_true(any(out$source %in% c("theta", "embed", "far")))
})

test_that(".ap_gen_repeat_reverse errors on invalid repeat_cap", {
  existing_completed <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  expect_error(
    pairwiseLLM:::.ap_gen_repeat_reverse(
      existing_completed = existing_completed,
      id_vec = c("A", "B"),
      repeat_cap = -1
    ),
    "repeat_cap"
  )
})

test_that(".ap_gen_controlled_random_candidates returns deterministic ordering for a tiny case", {
  out1 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = c("A", "B"),
    n_pairs = 1L,
    round_key = 1L,
    salt = "test"
  )

  out2 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = c("A", "B"),
    n_pairs = 1L,
    round_key = 1L,
    salt = "test"
  )

  expect_equal(nrow(out1), 1L)
  expect_true(all(c("i_idx", "j_idx", "pair_key", "source") %in% names(out1)))
  expect_identical(out1$source[[1]], "random")
  expect_identical(out1, out2)
})

test_that(".ap_select_pairs_from_scored validates repeat_quota_n", {
  scored <- tibble::tibble(
    i_idx = 1L,
    j_idx = 2L,
    anchor_idx = 1L,
    pair_key = "A-B",
    source = "theta",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = FALSE,
    score_info = 0.1,
    score_need = 0,
    score_embed = 0,
    score_total = 0.1
  )

  expect_error(
    pairwiseLLM:::.ap_select_pairs_from_scored(
      scored_tbl = scored,
      n_pairs = 1L,
      repeat_quota_n = -1L
    ),
    "repeat_quota_n"
  )
})
