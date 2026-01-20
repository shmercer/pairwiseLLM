testthat::test_that("ordering reverses duplicates and balances positions", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(), seed = 101)

  state$deg[["A"]] <- 10L
  state$deg[["B"]] <- 10L
  state$pos_count[["A"]] <- 8L
  state$pos_count[["B"]] <- 1L
  state$pos1[["A"]] <- 8L
  state$pos2[["A"]] <- 2L
  state$pos1[["B"]] <- 1L
  state$pos2[["B"]] <- 9L
  state$imb <- state$pos1 - state$pos2

  new_pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B")
  )
  ordered_new <- pairwiseLLM:::assign_order(new_pairs, state)
  expect_equal(ordered_new$A_id, "B")
  expect_equal(ordered_new$B_id, "A")

  state$deg[["A"]] <- 100L
  state$deg[["B"]] <- 100L
  state$deg[["C"]] <- 100L
  state$deg[["D"]] <- 100L
  state$pos_count[["A"]] <- 50L
  state$pos_count[["B"]] <- 50L
  state$pos_count[["C"]] <- 50L
  state$pos_count[["D"]] <- 50L
  state$pos1[["A"]] <- 50L
  state$pos2[["A"]] <- 50L
  state$pos1[["B"]] <- 50L
  state$pos2[["B"]] <- 50L
  state$pos1[["C"]] <- 50L
  state$pos2[["C"]] <- 50L
  state$pos1[["D"]] <- 50L
  state$pos2[["D"]] <- 50L
  state$imb <- state$pos1 - state$pos2

  tie_pairs <- tibble::tibble(
    i_id = c("A", "A", "A", "B", "B", "C"),
    j_id = c("B", "C", "D", "C", "D", "D")
  )
  tie_pairs <- dplyr::mutate(
    tie_pairs,
    unordered_key = pairwiseLLM:::make_unordered_key(.data$i_id, .data$j_id)
  )
  ordered_tie_1 <- pairwiseLLM:::assign_order(tie_pairs, state)
  ordered_tie_2 <- pairwiseLLM:::assign_order(tie_pairs, state)
  expect_identical(ordered_tie_1$A_id, ordered_tie_2$A_id)
  expect_true(any(ordered_tie_1$A_id == tie_pairs$i_id))
  expect_true(any(ordered_tie_1$A_id == tie_pairs$j_id))

  state$pair_count[["A:B"]] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  dup_pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B"
  )
  ordered_dup <- pairwiseLLM:::assign_order(dup_pairs, state)
  expect_equal(ordered_dup$A_id, "B")
  expect_equal(ordered_dup$B_id, "A")
})
