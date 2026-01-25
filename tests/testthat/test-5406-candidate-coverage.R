testthat::test_that("adaptive_v3_theta_summary covers validation branches", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary("bad", state),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(
      tibble::tibble(
        item_id = c("A", "", "C"),
        theta_mean = c(1, 0, -1),
        theta_sd = c(0.2, 0.3, 0.4)
      ),
      state
    ),
    "non-empty"
  )
  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(
      tibble::tibble(
        item_id = c("A", "A", "B"),
        theta_mean = c(1, 0, -1),
        theta_sd = c(0.2, 0.3, 0.4)
      ),
      state
    ),
    "unique"
  )
  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(
      tibble::tibble(
        item_id = state$ids,
        theta_mean = c(1, NA, -1),
        theta_sd = c(0.2, 0.3, 0.4)
      ),
      state
    ),
    "missing values"
  )

  bad_tbl <- function(x) {
    structure(
      list(
        item_id = c("A", "B", "C"),
        theta_mean = c(1, 2),
        theta_sd = c(0.2, 0.3, 0.4)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -3L)
    )
  }
  expect_error(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.adaptive_v3_theta_summary(tibble::tibble(), state),
      as_tibble = bad_tbl,
      .package = "tibble"
    ),
    "align with `item_id` length"
  )
})

testthat::test_that("adaptive_theta_summary_from_fit rejects bad inputs", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  fit <- make_v3_fit_contract(state$ids)

  bad_mean <- fit
  bad_mean$theta_mean <- "nope"
  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(bad_mean, state),
    "theta_mean"
  )

  missing_names <- fit
  names(missing_names$theta_mean) <- NULL
  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(missing_names, state),
    "named"
  )

  wrong_names <- fit
  names(wrong_names$theta_sd) <- rev(state$ids)
  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(wrong_names, state),
    "names must match"
  )

  short_sd <- fit
  short_sd$theta_sd <- stats::setNames(0.1, "A")
  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(short_sd, state),
    "align with `state\\$ids`"
  )
})

testthat::test_that("candidate helpers cover invalid config branches", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(1, 0, -1),
    theta_sd = c(0.2, 0.3, 0.4)
  )

  anchors <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      pairwiseLLM:::select_anchors(theta_summary, state, list(A_anchors = 0L)),
      validate_state = function(...) NULL,
      .package = "pairwiseLLM"
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(anchors, character())

  empty <- pairwiseLLM:::enumerate_candidates(
    anchors = "Z",
    theta_summary = theta_summary,
    state = state,
    config = pairwiseLLM:::adaptive_v3_config(state$N)
  )
  expect_equal(nrow(empty), 0L)

  expect_error(
    testthat::with_mocked_bindings(
      pairwiseLLM:::enumerate_candidates(
        anchors = "A",
        theta_summary = theta_summary,
        state = state,
        config = list(W = 0L)
      ),
      validate_state = function(...) NULL,
      .package = "pairwiseLLM"
    ),
    "config\\$W"
  )

  empty_window <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      pairwiseLLM:::enumerate_candidates(
        anchors = "A",
        theta_summary = theta_summary,
        state = state,
        config = list(W = 0L)
      ),
      validate_state = function(...) NULL,
      .package = "pairwiseLLM"
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(empty_window), 0L)

  expect_error(
    testthat::with_mocked_bindings(
      pairwiseLLM:::generate_candidates(
        theta_summary,
        state,
        list(W = 1L, A_anchors = 1L, C_max = 0L)
      ),
      validate_state = function(...) NULL,
      .package = "pairwiseLLM"
    ),
    "config\\$C_max"
  )
})

testthat::test_that("build_candidate_pairs covers empty window branches", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::build_candidate_pairs(
      ranking_ids = state$ids,
      W = 0,
      state = state,
      exploration_frac = 0.01,
      seed = 2
    ),
    abort = function(...) NULL,
    .package = "rlang"
  )
  expect_equal(nrow(out), 1L)
})
