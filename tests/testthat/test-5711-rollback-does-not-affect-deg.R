testthat::test_that("rollback_presentation does not touch degree or position exposure", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 3L)
  )
  baseline_deg <- state$deg
  baseline_pos_count <- state$pos_count
  baseline_pos1 <- state$pos1
  baseline_pos2 <- state$pos2

  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")

  testthat::expect_identical(state$deg, baseline_deg)
  testthat::expect_identical(state$pos_count, baseline_pos_count)
  testthat::expect_identical(state$pos1, baseline_pos1)
  testthat::expect_identical(state$pos2, baseline_pos2)

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 3L)
  )
  baseline_deg <- state$deg
  baseline_pos_count <- state$pos_count
  baseline_pos1 <- state$pos1
  baseline_pos2 <- state$pos2

  state <- pairwiseLLM:::record_exposure(state, "A", "B")

  testthat::expect_equal(state$deg[["A"]], baseline_deg[["A"]] + 1L)
  testthat::expect_equal(state$deg[["B"]], baseline_deg[["B"]] + 1L)
  testthat::expect_equal(state$pos_count[["A"]], baseline_pos_count[["A"]] + 1L)
  testthat::expect_equal(state$pos1[["A"]], baseline_pos1[["A"]] + 1L)
  testthat::expect_equal(state$pos2[["B"]], baseline_pos2[["B"]] + 1L)
  testthat::expect_equal(state$unordered_count[["A:B"]], 1L)
})
