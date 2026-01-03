test_that(".ap_gen_candidates handles empty and singleton inputs", {
  out0 <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = character(0),
    th_vec = numeric(0),
    se_vec = numeric(0),
    tot_vec = integer(0),
    k_neighbors2 = 1L,
    embed_nbrs = NULL,
    embed_far_k = 0L
  )

  expect_s3_class(out0, "tbl_df")
  expect_identical(names(out0), c("i_idx", "j_idx", "source"))
  expect_equal(nrow(out0), 0L)

  out1 <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = "A",
    th_vec = 0,
    se_vec = 1,
    tot_vec = 0L,
    k_neighbors2 = 1L,
    embed_nbrs = NULL,
    embed_far_k = 0L
  )

  expect_s3_class(out1, "tbl_df")
  expect_equal(nrow(out1), 0L)
})

test_that(".ap_gen_candidates validates embed_far_k", {
  expect_error(
    pairwiseLLM:::.ap_gen_candidates(
      id_vec = c("A", "B"),
      th_vec = c(0, 0),
      se_vec = c(1, 1),
      tot_vec = c(0L, 0L),
      k_neighbors2 = 1L,
      embed_nbrs = NULL,
      embed_far_k = -1
    ),
    "embed_far_k"
  )
})

test_that(".ap_score_candidates validates w_embed and handles empty candidate tables", {
  expect_error(
    pairwiseLLM:::.ap_score_candidates(
      cand_tbl = tibble::tibble(i_idx = 1L, j_idx = 2L, source = "theta"),
      th_vec = c(0, 0),
      se_vec = c(1, 1),
      tot_vec = c(0L, 0L),
      min_judgments = 0L,
      w_embed = c(1, 2)
    )
  )

  empty_scored <- pairwiseLLM:::.ap_score_candidates(
    cand_tbl = tibble::tibble(i_idx = integer(), j_idx = integer(), source = character()),
    th_vec = numeric(),
    se_vec = numeric(),
    tot_vec = integer(),
    min_judgments = 0L,
    w_embed = 1
  )

  expect_s3_class(empty_scored, "tbl_df")
  expect_identical(
    names(empty_scored),
    c("i_idx", "j_idx", "source", "score_info", "score_need", "score_embed", "score_total")
  )
  expect_equal(nrow(empty_scored), 0L)
})

test_that(".ap_score_candidates supports binary_neighbor and rank_decay embed scoring", {
  cand <- tibble::tibble(i_idx = 1L, j_idx = 2L, source = "embed", embed_rank = 1L)

  out_bin <- pairwiseLLM:::.ap_score_candidates(
    cand_tbl = cand,
    th_vec = c(0, 0),
    se_vec = c(1, 1),
    tot_vec = c(0L, 0L),
    min_judgments = 0L,
    w_embed = 1,
    embed_score_mode = "binary_neighbor"
  )

  expect_equal(out_bin$score_embed, 1)
  expect_true(out_bin$score_total > 0)

  # If embed_rank column is absent, rank_decay treats ranks as NA and gives 0 bonus.
  cand_no_rank <- tibble::tibble(i_idx = 1L, j_idx = 2L, source = "embed")
  testthat::expect_warning(
    out_decay <- pairwiseLLM:::.ap_score_candidates(
      cand_tbl = cand_no_rank,
      th_vec = c(0, 0),
      se_vec = c(1, 1),
      tot_vec = c(0L, 0L),
      min_judgments = 0L,
      w_embed = 1,
      embed_score_mode = "rank_decay"
    ),
    "Unknown or uninitialised column: `embed_rank`\\.?"
  )

  expect_equal(out_decay$score_embed, 0)
})

test_that(".ap_apply_constraints returns well-typed empty output and removes invalid candidates", {
  out0 <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = tibble::tibble(),
    id_vec = c("A", "B")
  )

  expect_s3_class(out0, "tbl_df")
  expect_identical(
    names(out0),
    c(
      "i_idx", "j_idx", "anchor_idx", "pair_key", "source", "embed_hit", "embed_rank", "directed",
      "score_info", "score_need", "score_embed", "score_total"
    )
  )
  expect_equal(nrow(out0), 0L)

  # Self-pairs should be filtered out, leaving an empty tibble.
  self_pair <- tibble::tibble(
    i_idx = 1L,
    j_idx = 1L,
    anchor_idx = 1L,
    source = "theta",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = 0
  )

  out_self <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = self_pair,
    id_vec = c("A", "B"),
    existing_key = character(),
    forbid_repeats = TRUE
  )
  expect_equal(nrow(out_self), 0L)
})

test_that(".ap_apply_constraints computes pair_key for repeat filtering and de-duplicates unordered pairs", {
  id_vec <- c("A", "B")

  existing_key <- pairwiseLLM:::.unordered_pair_key("A", "B")
  cand1 <- tibble::tibble(
    i_idx = 1L,
    j_idx = 2L,
    anchor_idx = 1L,
    source = "theta",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = 0
  )

  out_repeat <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = cand1,
    id_vec = id_vec,
    existing_key = existing_key,
    forbid_repeats = TRUE
  )

  expect_true("pair_key" %in% names(out_repeat))
  expect_equal(nrow(out_repeat), 0L)

  # Without repeat filtering, pair_key is still computed and unordered duplicates are removed.
  cand_dupes <- dplyr::bind_rows(
    cand1,
    dplyr::mutate(cand1, i_idx = 2L, j_idx = 1L, anchor_idx = 2L)
  )

  out_dupes <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = cand_dupes,
    id_vec = id_vec,
    existing_key = character(),
    forbid_repeats = FALSE
  )

  expect_true("pair_key" %in% names(out_dupes))
  expect_equal(nrow(out_dupes), 1L)
})

test_that(".ap_select_pairs_from_scored validates inputs", {
  scored <- tibble::tibble(
    i_idx = 1L,
    j_idx = 2L,
    pair_key = pairwiseLLM:::.unordered_pair_key("A", "B"),
    source = "theta",
    score_total = 1
  )

  expect_error(
    pairwiseLLM:::.ap_select_pairs_from_scored(scored_tbl = scored, n_pairs = 1, embed_quota_frac = -0.1),
    "embed_quota_frac"
  )

  expect_error(
    pairwiseLLM:::.ap_select_pairs_from_scored(scored_tbl = dplyr::select(scored, -pair_key), n_pairs = 1),
    "pair_key"
  )
})

test_that("select_adaptive_pairs validates k_neighbors, embed_quota_frac, caps, and return_internal", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = samples$ID, theta = c(0, 0.1, 0.2), se = c(1, 1, 1))

  # NULL k_neighbors is treated as Inf (all neighbors).
  out_null <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    k_neighbors = NULL,
    balance_positions = FALSE,
    seed = 1
  )
  expect_equal(nrow(out_null), 1L)

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, k_neighbors = "x"),
    "k_neighbors"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, k_neighbors = 1.5),
    "integer"
  )

  # min_judgments can be NULL (treated as 0)
  out_min_null <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    k_neighbors = 1,
    min_judgments = NULL,
    balance_positions = FALSE,
    seed = 1
  )
  expect_equal(nrow(out_min_null), 1L)

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, embed_quota_frac = 1.5),
    "embed_quota_frac"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, candidate_pool_cap = -1),
    "candidate_pool_cap"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, per_anchor_cap = 1.2),
    "per_anchor_cap"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, n_pairs = 1, return_internal = NA),
    "return_internal"
  )
})

test_that("select_adaptive_pairs applies candidate pool caps and can return internals", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = letters[1:5]
  )
  theta <- tibble::tibble(
    ID = samples$ID,
    theta = seq(0, 0.4, length.out = 5),
    se = rep(1, 5)
  )

  out <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 2,
    k_neighbors = Inf,
    per_anchor_cap = 1,
    candidate_pool_cap = 2,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    seed = 1
  )

  diag <- attr(out, "pairing_diagnostics")
  expect_true(inherits(diag, "tbl_df"))
  expect_true(diag$n_candidates_after_constraints >= diag$n_candidates_after_caps)
  expect_true(diag$n_candidates_after_caps <= 2)

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 2,
    k_neighbors = 2,
    per_anchor_cap = 2,
    candidate_pool_cap = 10,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    return_internal = TRUE,
    seed = 1
  )

  expect_true(is.list(res))
  expect_true(all(c("pairs", "diagnostics", "candidates") %in% names(res)))
  expect_s3_class(res$pairs, "tbl_df")
  expect_s3_class(res$diagnostics, "tbl_df")
  expect_true(is.list(res$candidates))
  expect_true(all(c("raw", "scored", "constrained", "capped", "selected") %in% names(res$candidates)))
})

test_that(".compute_embedding_neighbors validates inputs and normalizes embeddings", {
  expect_error(
    pairwiseLLM:::.compute_embedding_neighbors(embeddings = NULL, ids = c("A", "B"), k = 1),
    "embeddings"
  )

  expect_error(
    pairwiseLLM:::.compute_embedding_neighbors(embeddings = matrix(0, nrow = 2, ncol = 2), ids = c("A", NA), k = 1),
    "ids"
  )

  expect_error(
    pairwiseLLM:::.compute_embedding_neighbors(embeddings = matrix(0, nrow = 2, ncol = 2), ids = c("A", "B"), k = -1),
    "k"
  )

  ids <- c("A", "B")
  emb <- matrix(
    c(
      0, 0,
      1, 0
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(ids, c("x", "y"))
  )

  nbrs <- pairwiseLLM:::.compute_embedding_neighbors(embeddings = emb, ids = ids, k = 1, normalize = TRUE)
  expect_identical(names(nbrs), ids)
  expect_identical(nbrs[["A"]], "B")
  expect_identical(nbrs[["B"]], "A")
})
