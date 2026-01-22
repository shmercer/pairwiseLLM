testthat::test_that("rollback_presentation validates inputs and required keys", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )

  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "C"),
    "`A_id` and `B_id` must exist"
  )

  state$unordered_count <- integer()
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "unordered_count"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state$unordered_count[["A:B"]] <- 0L
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "cannot be rolled back below zero"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$pair_ordered_count <- integer()
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "pair_ordered_count"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$pair_ordered_count[["A:B"]] <- 0L
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "pair_ordered_count"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$ordered_seen <- logical()
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "ordered_seen"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$ordered_seen[["A:B"]] <- FALSE
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "ordered_seen"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$ordered_seen <- new.env(parent = emptyenv())
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "ordered_seen"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state$ordered_seen <- new.env(parent = emptyenv())
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$ordered_seen[["A:B"]] <- FALSE
  testthat::expect_error(
    pairwiseLLM:::rollback_presentation(state, "A", "B"),
    "ordered_seen"
  )
})

testthat::test_that("rollback_presentation handles ordered_seen environments", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 3L)
  )
  state$ordered_seen <- new.env(parent = emptyenv())

  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  testthat::expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  testthat::expect_equal(state$pair_ordered_count[["A:B"]], 2L)

  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  testthat::expect_equal(state$pair_ordered_count[["A:B"]], 1L)

  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_false(exists("A:B", envir = state$ordered_seen, inherits = FALSE))
  testthat::expect_false("A:B" %in% names(state$pair_ordered_count))
})

testthat::test_that("record_judgment_exposure and ordering helpers cover edge paths", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state$pos_count <- NULL
  state$pair_count <- integer()

  state <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")
  testthat::expect_true("A:B" %in% names(state$pair_count))
  testthat::expect_equal(state$pos_count[["A"]], state$pos1[["A"]])

  state$imb[["A"]] <- 1L
  state$imb[["B"]] <- 0L
  out <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 1L)
  testthat::expect_identical(out$A_id, "B")

  state$imb[["A"]] <- 0L
  state$imb[["B"]] <- 1L
  out <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 1L)
  testthat::expect_identical(out$A_id, "A")

  state$imb[["A"]] <- 0L
  state$imb[["B"]] <- 0L
  pick_first <- withr::with_seed(1L, stats::runif(1) < 0.5)
  out <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 1L)
  expected_A <- if (isTRUE(pick_first)) "A" else "B"
  testthat::expect_identical(out$A_id, expected_A)
})
