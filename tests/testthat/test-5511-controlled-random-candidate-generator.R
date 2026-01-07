test_that("5511 controlled_random candidate generator returns empty when n_pairs <= 0 or n < 2", {
  out_n_pairs0 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = c("A", "B"),
    n_pairs = 0L,
    round_key = 1L,
    salt = ""
  )
  expect_true(tibble::is_tibble(out_n_pairs0))
  expect_identical(
    names(out_n_pairs0),
    c(
      "i_idx", "j_idx", "anchor_idx", "pair_key", "source",
      "embed_hit", "embed_rank", "directed",
      "score_info", "score_need", "score_embed", "score_total"
    )
  )
  expect_identical(nrow(out_n_pairs0), 0L)

  out_n1 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = "A",
    n_pairs = 5L,
    round_key = 1L,
    salt = ""
  )
  expect_identical(nrow(out_n1), 0L)
  expect_identical(names(out_n1), names(out_n_pairs0))
})

test_that("5511 controlled_random candidates are deterministic and depend on round_key/salt only via hashing", {
  id_vec <- c("A", "B", "C", "D")

  out1 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = id_vec,
    n_pairs = 4L,
    round_key = NA_integer_, # treated as 0L
    salt = ""
  )

  out2 <- pairwiseLLM:::.ap_gen_controlled_random_candidates(
    id_vec = id_vec,
    n_pairs = 4L,
    round_key = 0L,
    salt = ""
  )

  expect_true(all(out1$source == "random"))
  expect_identical(out1, out2)

  ids1 <- id_vec[out1$i_idx]
  ids2 <- id_vec[out1$j_idx]
  expect_true(all(ids1 %in% id_vec))
  expect_true(all(ids2 %in% id_vec))
  expect_true(all(ids1 != ids2))
})
