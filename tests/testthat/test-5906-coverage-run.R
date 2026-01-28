testthat::test_that("adaptive_write_v3_artifacts validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(write_outputs = TRUE))

  testthat::expect_error(
    pairwiseLLM:::.adaptive_write_v3_artifacts(list(), fit = NULL),
    "`state` must be an adaptive_state."
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_write_v3_artifacts(state, fit = NULL, output_dir = NULL),
    "`output_dir` must be provided"
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_write_v3_artifacts(state, fit = NULL, output_dir = 1),
    "`output_dir` must be a length-1 character path."
  )

  state$config$v3$keep_draws <- TRUE
  state$config$v3$thin_draws <- 0L
  testthat::expect_error(
    pairwiseLLM:::.adaptive_write_v3_artifacts(
      state,
      fit = list(theta_draws = matrix(0, nrow = 2, ncol = 2)),
      output_dir = withr::local_tempdir()
    ),
    "`thin_draws` must be a positive integer."
  )
})

testthat::test_that("adaptive_write_v3_artifacts returns state when outputs disabled", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(write_outputs = FALSE))

  out <- pairwiseLLM:::.adaptive_write_v3_artifacts(state, fit = NULL, output_dir = NULL)

  testthat::expect_true(inherits(out$state, "adaptive_state"))
  testthat::expect_false("item_summary" %in% names(out))
  testthat::expect_false("item_summary" %in% names(out$state))
  config <- out$state$config
  if (is.null(config)) {
    config <- list()
  }
  testthat::expect_false("item_summary" %in% names(config))
})

testthat::test_that("adaptive_write_v3_artifacts thins draws with epsilon", {
  output_dir <- withr::local_tempdir()
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(write_outputs = TRUE, keep_draws = TRUE, thin_draws = 2L, output_dir = output_dir)
  )

  theta_draws <- matrix(
    c(
      0.1, 0.0, -0.1,
      0.2, -0.1, -0.1,
      0.3, -0.2, -0.1,
      0.4, -0.3, -0.1
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = theta_draws,
    epsilon_draws = c(0.01, 0.02, 0.03, 0.04)
  )

  pairwiseLLM:::.adaptive_write_v3_artifacts(state, fit = fit, output_dir = output_dir)

  draws <- readRDS(file.path(output_dir, "theta_draws.rds"))
  testthat::expect_equal(nrow(draws$theta), 2L)
  testthat::expect_equal(length(draws$epsilon), 2L)
})

testthat::test_that("adaptive_run_stopping_checks appends round log on refit", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$phase <- "phase2"
  state$config$CW <- 1L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(refit_B = 1L, progress = FALSE)
  )

  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)
  state <- pairwiseLLM:::record_exposure(state, "A", "B")
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

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )

  state$new_since_refit <- 1L

  adaptive <- pairwiseLLM:::.adaptive_merge_config(list())
  out <- NULL
  testthat::expect_silent(
    {
      out <- testthat::with_mocked_bindings(
        generate_candidates = function(...) tibble::tibble(),
        .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
          list(state = state, fit = fit, refit_performed = TRUE)
        },
        pairwiseLLM:::.adaptive_run_stopping_checks(state, adaptive = adaptive, seed = 1L),
        .package = "pairwiseLLM"
      )
    }
  )

  round_log <- out$state$config$round_log
  testthat::expect_equal(nrow(round_log), 1L)
  testthat::expect_true("round_id" %in% names(round_log))
})
