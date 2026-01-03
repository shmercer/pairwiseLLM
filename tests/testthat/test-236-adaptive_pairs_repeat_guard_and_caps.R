test_that(".ap_select_pairs_from_scored caps reverse repeats at repeat_quota_n", {
  scored <- tibble::tibble(
    i_idx = c(1L, 3L, 1L, 2L),
    j_idx = c(2L, 4L, 3L, 4L),
    pair_key = c(
      pairwiseLLM:::.unordered_pair_key("A", "B"),
      pairwiseLLM:::.unordered_pair_key("C", "D"),
      pairwiseLLM:::.unordered_pair_key("A", "C"),
      pairwiseLLM:::.unordered_pair_key("B", "D")
    ),
    source = c("repeat_reverse", "repeat_reverse", "theta", "embed"),
    score_total = c(100, 90, 80, 70),
    embed_hit = FALSE,
    embed_rank = NA_integer_,
    directed = TRUE
  )

  out <- pairwiseLLM:::.ap_select_pairs_from_scored(
    scored_tbl = scored,
    n_pairs = 2,
    embed_quota_frac = 0,
    repeat_quota_n = 1
  )

  expect_equal(nrow(out), 2L)
  expect_equal(sum(out$source == "repeat_reverse"), 1L)
  expect_equal(
    out$pair_key,
    c(
      pairwiseLLM:::.unordered_pair_key("A", "B"),
      pairwiseLLM:::.unordered_pair_key("A", "C")
    )
  )
})

test_that("select_adaptive_pairs does not append repeat candidates when repeat quota is zero", {
  ids <- c("A", "B")
  samples <- tibble::tibble(ID = ids, text = paste0("t", ids))
  theta <- tibble::tibble(ID = ids, theta = c(0, 0), se = c(1, 1))

  existing <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 1,
    repeat_policy = "reverse_only",
    repeat_frac = 0,
    repeat_guard_min_degree = 1,
    repeat_guard_largest_component_frac = 0.5,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_equal(diag$repeat_quota_n, 0)
  expect_equal(diag$n_repeat_planned, 0)

  planned <- attr(pairs, "planned_repeat_pairs")
  expect_true(is.data.frame(planned))
  expect_equal(nrow(planned), 0L)

  # Only A-B exists, and repeats were disabled => no pairs planned.
  expect_equal(nrow(pairs), 0L)
})
