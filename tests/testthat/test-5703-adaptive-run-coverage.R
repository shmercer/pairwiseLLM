testthat::test_that("adaptive_get_refit_fit validates refit_B and batch_size", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$config$v3 <- adaptive_v3_config(state$N, list(refit_B = 1L))

  state_bad <- state
  state_bad$config$v3$refit_B <- 0L
  testthat::expect_error(
    .adaptive_get_refit_fit(
      state_bad,
      adaptive = list(),
      batch_size = 1L,
      seed = 1L
    ),
    "`refit_B` must be a positive integer."
  )

  testthat::expect_error(
    .adaptive_get_refit_fit(
      state,
      adaptive = list(),
      batch_size = -1L,
      seed = 1L
    ),
    "`batch_size` must be non-negative."
  )
})

testthat::test_that("adaptive_run_stopping_checks respects allow_refit and existing fit", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$phase <- "phase2"
  state$config$CW <- 1L

  unordered_key <- make_unordered_key("A", "B")
  ordered_key <- make_ordered_key("A", "B")
  pair_uid <- pair_uid_from_state(state, unordered_key)
  state <- record_exposure(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    A_text = state$texts[["A"]],
    B_text = state$texts[["B"]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2024-01-01", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L
  state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2024-01-01", tz = "UTC"),
    backend = "mock",
    model = "mock"
  )
  state$comparisons_observed <- 1L

  out <- .adaptive_run_stopping_checks(
    state,
    adaptive = list(),
    seed = 1L,
    allow_refit = FALSE
  )
  testthat::expect_null(out$state$fit)

  state$fit <- list(
    theta_draws = matrix(0, nrow = 1L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids)
  )
  state$config$v3 <- adaptive_v3_config(state$N, list(refit_B = 1L))

  out2 <- testthat::with_mocked_bindings(
    generate_candidates = function(...) tibble::tibble(),
    .adaptive_run_stopping_checks(
      state,
      adaptive = list(),
      seed = 1L,
      allow_refit = FALSE
    )
  )
  testthat::expect_false(identical(out2$state$mode, "stopped"))
})

testthat::test_that("adaptive_warm_start_order follows imbalance and index rules", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))

  state$imb[["A"]] <- 2L
  state$imb[["B"]] <- 1L
  order1 <- .adaptive_warm_start_order(state, "A", "B", pair_index = 1L)
  testthat::expect_identical(order1, list(A_id = "B", B_id = "A"))

  state$imb[["A"]] <- 0L
  state$imb[["B"]] <- 3L
  order2 <- .adaptive_warm_start_order(state, "A", "B", pair_index = 1L)
  testthat::expect_identical(order2, list(A_id = "A", B_id = "B"))

  state$imb[["A"]] <- 0L
  state$imb[["B"]] <- 0L
  order3 <- .adaptive_warm_start_order(state, "A", "B", pair_index = 2L)
  testthat::expect_identical(order3, list(A_id = "B", B_id = "A"))
})
