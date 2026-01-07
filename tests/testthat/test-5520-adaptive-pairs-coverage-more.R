
test_that("5520 per-node cap helper returns early and enforces caps deterministically", {
  id_vec <- c("A", "B", "C")

  scored <- tibble::tibble(
    i_idx = c(1L, 1L, 2L),
    j_idx = c(2L, 3L, 3L),
    anchor_idx = c(1L, 1L, 2L),
    pair_key = pairwiseLLM:::.unordered_pair_key(id_vec[i_idx], id_vec[j_idx]),
    source = c("theta", "theta", "theta"),
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = FALSE,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = c(10, 9, 8)
  )

  out_early <- pairwiseLLM:::.ap_enforce_per_node_cap(scored, id_vec = id_vec, per_node_cap = 0L)
  expect_identical(out_early, scored)

  out_cap1 <- pairwiseLLM:::.ap_enforce_per_node_cap(scored, id_vec = id_vec, per_node_cap = 1L)
  expect_true(tibble::is_tibble(out_cap1))
  expect_identical(nrow(out_cap1), 1L)
  expect_identical(out_cap1$pair_key, scored$pair_key[[1]])
})


test_that("5521 score helper validates w_embed and supports multiple embedding modes", {
  cand_tbl <- tibble::tibble(
    i_idx = c(1L, 1L),
    j_idx = c(2L, 3L),
    anchor_idx = c(1L, 1L),
    pair_key = c("A-B", "A-C"),
    source = c("embed", "theta"),
    embed_hit = c(TRUE, FALSE),
    embed_rank = c(1L, NA_integer_),
    directed = FALSE
  )

  th_vec <- c(0, 0, 0)
  se_vec <- c(1, 1, 1)
  tot_vec <- c(0, 0, 0)

  out_binary <- pairwiseLLM:::.ap_score_candidates(
    cand_tbl,
    th_vec = th_vec,
    se_vec = se_vec,
    tot_vec = tot_vec,
    min_judgments = 0L,
    w_embed = 1,
    embed_score_mode = "binary_neighbor"
  )

  expect_identical(out_binary$score_embed, c(1, 0))

  # Rank-decay handles missing embed_rank column defensively.
  cand_no_rank <- dplyr::select(cand_tbl, -embed_rank)
  out_rank_decay <- pairwiseLLM:::.ap_score_candidates(
    cand_no_rank,
    th_vec = th_vec,
    se_vec = se_vec,
    tot_vec = tot_vec,
    min_judgments = 0L,
    w_embed = 1,
    embed_score_mode = "rank_decay"
  )
  expect_identical(out_rank_decay$score_embed, c(0, 0))

  expect_error(
    pairwiseLLM:::.ap_score_candidates(
      cand_tbl,
      th_vec = th_vec,
      se_vec = se_vec,
      tot_vec = tot_vec,
      min_judgments = 0L,
      w_embed = Inf
    ),
    "w_embed"
  )
})


test_that("5522 constraint helper maps legacy args and validates repeat controls", {
  id_vec <- c("A", "B")
  key_ab <- pairwiseLLM:::.unordered_pair_key("A", "B")

  cand_tbl <- tibble::tibble(
    i_idx = 1L,
    j_idx = 2L,
    anchor_idx = 1L,
    source = "theta",
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = FALSE
  )

  existing_any <- stats::setNames(1L, key_ab)

  out_allow <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl,
    id_vec = id_vec,
    existing_counts_all = existing_any,
    repeat_policy = "allow",
    repeat_cap = 1L
  )
  expect_identical(nrow(out_allow), 0L)

  out_forbid_unordered <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl,
    id_vec = id_vec,
    existing_counts_all = existing_any,
    repeat_policy = "forbid_unordered",
    repeat_cap = 1L
  )
  expect_identical(nrow(out_forbid_unordered), 0L)

  expect_error(
    pairwiseLLM:::.ap_apply_constraints(
      cand_tbl,
      id_vec = id_vec,
      forbid_repeats = "no"
    ),
    "forbid_repeats"
  )

  expect_error(
    pairwiseLLM:::.ap_apply_constraints(
      cand_tbl,
      id_vec = id_vec,
      repeat_cap = -1L
    ),
    "repeat_cap"
  )
})


test_that("5523 scored-pair selector supports repeat bucket and validates schema", {
  id_vec <- c("A", "B", "C")
  scored_tbl <- tibble::tibble(
    i_idx = c(1L, 2L),
    j_idx = c(2L, 3L),
    anchor_idx = c(1L, 2L),
    pair_key = pairwiseLLM:::.unordered_pair_key(id_vec[i_idx], id_vec[j_idx]),
    source = c("repeat_reverse", "theta"),
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = FALSE,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = c(0, 100)
  )

  out_repeat_only <- pairwiseLLM:::.ap_select_pairs_from_scored(
    scored_tbl,
    n_pairs = 1L,
    repeat_quota_n = 1L
  )
  expect_identical(nrow(out_repeat_only), 1L)
  expect_identical(out_repeat_only$source, "repeat_reverse")

  expect_error(
    pairwiseLLM:::.ap_select_pairs_from_scored(
      dplyr::select(scored_tbl, -pair_key),
      n_pairs = 1L
    ),
    "pair_key"
  )

  # Embedding quota ensures embed candidates are included when available.
  scored_tbl2 <- tibble::tibble(
    i_idx = c(1L, 1L, 2L),
    j_idx = c(2L, 3L, 3L),
    anchor_idx = c(1L, 1L, 2L),
    pair_key = pairwiseLLM:::.unordered_pair_key(id_vec[i_idx], id_vec[j_idx]),
    source = c("theta", "embed", "theta"),
    embed_hit = c(FALSE, TRUE, FALSE),
    embed_rank = c(NA_integer_, 1L, NA_integer_),
    directed = FALSE,
    score_info = 0,
    score_need = 0,
    score_embed = 0,
    score_total = c(100, 0, 50)
  )

  out_embed_quota <- pairwiseLLM:::.ap_select_pairs_from_scored(
    scored_tbl2,
    n_pairs = 2L,
    embed_quota_frac = 1
  )
  expect_identical(nrow(out_embed_quota), 2L)
  expect_true(any(out_embed_quota$source == "embed"))
})


test_that("5524 orientation uses directed flag, opt-out switch, and deterministic tie-breaker", {
  id_vec <- c("A", "B")
  samples <- tibble::tibble(ID = id_vec, text = c("a", "b"))

  selected_tbl <- tibble::tibble(i_idx = 1L, j_idx = 2L, directed = FALSE)
  p1 <- c(0L, 0L)
  p2 <- c(0L, 0L)

  pk <- pairwiseLLM:::.unordered_pair_key("A", "B")

  salts <- paste0("salt", seq_len(50))
  coins <- vapply(
    salts,
    function(s) as.integer(pairwiseLLM:::.stable_hash_u32(paste(pk, 0L, s, 1L, sep = "\u001F")) %% 2),
    integer(1)
  )

  salt_even <- salts[which(coins == 0L)[[1]]]
  salt_odd <- salts[which(coins == 1L)[[1]]]

  out_even <- pairwiseLLM:::.ap_orient_pairs(
    selected_tbl,
    id_vec = id_vec,
    samples = samples,
    p1_vec = p1,
    p2_vec = p2,
    balance_positions = TRUE,
    hash_round = 0L,
    hash_salt = salt_even
  )

  out_odd <- pairwiseLLM:::.ap_orient_pairs(
    selected_tbl,
    id_vec = id_vec,
    samples = samples,
    p1_vec = p1,
    p2_vec = p2,
    balance_positions = TRUE,
    hash_round = 0L,
    hash_salt = salt_odd
  )

  expect_identical(nrow(out_even), 1L)
  expect_identical(nrow(out_odd), 1L)
  expect_false(identical(out_even$ID1, out_odd$ID1))

  # Directed pairs always keep the requested orientation.
  out_directed <- pairwiseLLM:::.ap_orient_pairs(
    tibble::tibble(i_idx = 1L, j_idx = 2L, directed = TRUE),
    id_vec = id_vec,
    samples = samples,
    p1_vec = p1,
    p2_vec = p2,
    balance_positions = TRUE
  )
  expect_identical(out_directed$ID1, "A")
  expect_identical(out_directed$ID2, "B")

  # balance_positions = FALSE opts out and keeps forward orientation.
  out_no_balance <- pairwiseLLM:::.ap_orient_pairs(
    selected_tbl,
    id_vec = id_vec,
    samples = samples,
    p1_vec = p1,
    p2_vec = p2,
    balance_positions = FALSE
  )
  expect_identical(out_no_balance$ID1, "A")
  expect_identical(out_no_balance$ID2, "B")
})


test_that("5525 diagnostics contract infers missing fallback fields deterministically", {
  diag_bridge <- tibble::tibble(
    n_pairs_source_bridge = 2L,
    n_pairs_source_random = 0L
  )

  out_bridge <- pairwiseLLM:::.ap_pairing_diagnostics_contract(diag_bridge)
  expect_identical(out_bridge$fallback_path, "bridge_repair")
  expect_identical(out_bridge$fallback_trigger, "normal_empty")

  diag_random <- tibble::tibble(
    n_pairs_source_bridge = 0L,
    n_pairs_source_random = 1L
  )
  out_random <- pairwiseLLM:::.ap_pairing_diagnostics_contract(diag_random)
  expect_identical(out_random$fallback_path, "controlled_random")
  expect_identical(out_random$fallback_trigger, "normal_empty")

  diag_normal <- tibble::tibble(
    n_selected = 3L,
    n_pairs_source_bridge = 0L,
    n_pairs_source_random = 0L
  )
  out_normal <- pairwiseLLM:::.ap_pairing_diagnostics_contract(diag_normal)
  expect_identical(out_normal$fallback_path, "normal")
})


test_that("5526 select_adaptive_pairs reports embed_neighbor_hit_rate when embedding neighbors are provided", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 0.01, 10),
    se = c(1, 1, 1)
  )

  embed_nbrs <- list(
    A = c("B"),
    B = c("A"),
    C = c("B")
  )

  res <- select_adaptive_pairs(
    samples,
    theta,
    n_pairs = 1L,
    k_neighbors = 1L,
    embedding_neighbors = embed_nbrs,
    seed = 1,
    return_internal = TRUE
  )

  expect_true(is.list(res))
  expect_true("diagnostics" %in% names(res))
  expect_true("embed_neighbor_hit_rate" %in% names(res$diagnostics))
  val <- res$diagnostics$embed_neighbor_hit_rate[[1]]
  expect_true(is.numeric(val) && length(val) == 1L)
  expect_true(!is.na(val))
  expect_true(val >= 0 && val <= 1)
})

