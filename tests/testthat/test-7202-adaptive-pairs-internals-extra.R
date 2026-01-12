.test_ids_7202 <- function(n) {
  as.character(utils::head(LETTERS, n))
}

.test_samples_7202 <- function(ids) {
  tibble::tibble(ID = ids, text = paste0("text-", ids))
}


test_that(".ap_gen_candidates exercises far-candidate edge cases", {
  ids <- .test_ids_7202(1)
  out <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = ids,
    th_vec = 0,
    se_vec = 0,
    tot_vec = 0,
    k_neighbors2 = 1L,
    embed_nbrs = NULL,
    embed_far_k = 1L,
    hash_round = 1L
  )
  expect_true(is.data.frame(out))
  expect_identical(nrow(out), 0L)

  ids2 <- .test_ids_7202(2)
  # With k_neighbors2=1, theta neighbors already cover the only base_pool entry,
  # so the far pool is empty and should not add any far candidates.
  out2 <- pairwiseLLM:::.ap_gen_candidates(
    id_vec = ids2,
    th_vec = c(0, 1),
    se_vec = c(0, 0),
    tot_vec = c(0, 0),
    k_neighbors2 = 1L,
    embed_nbrs = NULL,
    embed_far_k = 1L,
    hash_round = 1L
  )
  expect_true(all(out2$source %in% c("theta")))
})


test_that(".ap_choose_id_map_vec prefers stable ID sources", {
  ids <- c("A", "B")
  gs1 <- list(ids = ids)
  expect_identical(pairwiseLLM:::.ap_choose_id_map_vec(id_vec = character(), graph_state = gs1), ids)

  gs2 <- list(degree = stats::setNames(c(1, 2), ids))
  expect_identical(pairwiseLLM:::.ap_choose_id_map_vec(id_vec = character(), graph_state = gs2), character(0))

  expect_identical(
    pairwiseLLM:::.ap_choose_id_map_vec(id_vec = character(), graph_state = list()),
    character(0)
  )
})


test_that(".ap_pick_low_degree falls back deterministically when needed", {
  pool <- tibble::tibble(x = 1:5)
  picked <- pairwiseLLM:::.ap_pick_low_degree(pool, deg_sum = 1:10, n_pick = 2)
  expect_identical(nrow(picked), 2L)

  pool2 <- tibble::tibble(i_idx = c(1, 2, 3), j_idx = c(2, 3, 1), score_total = c(1, 2, 3))
  # Deg vector is too short -> fallback slice_head.
  picked2 <- pairwiseLLM:::.ap_pick_low_degree(pool2, deg_sum = 1, n_pick = 2)
  expect_identical(nrow(picked2), 2L)

  # Normal ordering branch without score_total.
  pool3 <- dplyr::select(pool2, -"score_total")
  deg_sum <- c(10, 0, 1)
  picked3 <- pairwiseLLM:::.ap_pick_low_degree(pool3, deg_sum = deg_sum, n_pick = 1)
  expect_identical(nrow(picked3), 1L)
  # lowest degree sum pair should involve indices 2 and 3 (0+1=1) rather than 1 and 2 (10+0=10)
  expect_identical(picked3$i_idx[[1]], 2)
  expect_identical(picked3$j_idx[[1]], 3)
})
