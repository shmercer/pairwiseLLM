testthat::test_that("select_core_link_pairs yields at least one within and one audit when round_size is large enough", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E", "F"),
    text = letters[1:6]
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = c(0, 1, 2, 3, 4, 5),
    se = rep(0.1, 6)
  )

  pairs <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = c("A", "B", "C"),
    new_ids = c("D", "E", "F"),
    round_size = 10,
    within_batch_frac = 0.05,
    core_audit_frac = 0.05,
    seed = 1
  )

  testthat::expect_s3_class(pairs, "tbl_df")
  testthat::expect_equal(nrow(pairs), 10L)
  testthat::expect_true(all(c("ID1", "ID2", "pair_type") %in% names(pairs)))

  counts <- dplyr::count(pairs, .data$pair_type)

  # With your concrete run, we saw:
  # core_core = 1, new_new = 1, core_new = 8.
  # This enforces the intended “minimum 1” behavior without relying on hidden attrs.
  n_core_core <- counts$n[counts$pair_type == "core_core"]
  n_new_new <- counts$n[counts$pair_type == "new_new"]

  testthat::expect_true(length(n_core_core) == 1L && n_core_core >= 1L)
  testthat::expect_true(length(n_new_new) == 1L && n_new_new >= 1L)
})

testthat::test_that("bt_core_link_round avoids forbidden repeats and can select an alternate neighbor", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  # Make C closest to B (so k_neighbors = 1 selects B), but forbid (B,C) so we must fall back.
  fit <- list(
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0, 9.9, 10.0),
      se = c(0.1, 0.1, 0.1)
    )
  )

  existing_pairs <- tibble::tibble(ID1 = "B", ID2 = "C")

  out <- pairwiseLLM::bt_core_link_round(
    samples = samples,
    fit = fit,
    core_ids = c("A", "B"),
    new_ids = "C",
    existing_pairs = existing_pairs,
    round_size = 1,
    k_neighbors = 1,
    forbid_repeats = TRUE,
    core_audit_frac = 0,
    within_batch_frac = 0,
    seed = 123
  )

  pairs <- out$pairs

  testthat::expect_s3_class(pairs, "tbl_df")
  testthat::expect_equal(nrow(pairs), 1L)

  # Should link C to the alternate neighbor A (order may vary).
  testthat::expect_setequal(c(pairs$ID1[[1]], pairs$ID2[[1]]), c("A", "C"))
  testthat::expect_equal(pairs$pair_type[[1]], "core_new")

  # Confirm fallback happened.
  testthat::expect_equal(attr(pairs, "fallback"), "expanded_k_neighbors")

  # Plan should reflect 1 core_new pair.
  testthat::expect_equal(out$plan$n_core_new[[1]], 1L)
})

