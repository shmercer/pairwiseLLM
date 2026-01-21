testthat::test_that("adaptive_schedule_next_pairs uses warm start in phase1", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$budget_max <- 3L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)

  out <- pairwiseLLM:::.adaptive_schedule_next_pairs(
    state = state,
    target_pairs = 2L,
    adaptive = list(),
    seed = 1
  )

  testthat::expect_equal(out$state$phase, "phase2")
  testthat::expect_equal(out$state$mode, "adaptive")
  testthat::expect_true(nrow(out$pairs) > 0L)
  testthat::expect_true(all(out$pairs$phase == "phase1"))
})
