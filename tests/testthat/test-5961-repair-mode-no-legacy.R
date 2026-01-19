testthat::test_that("repair mode uses canonical selection and skips legacy scheduler", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$iter <- 0L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    explore_rate = 0,
    batch_size = 1L
  ))
  state$config$skip_stop_checks <- TRUE

  candidate_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i = "A",
    j = "B"
  )
  utilities_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i_id = "A",
    j_id = "B",
    p_mean = 0.5,
    utility = 0.4,
    utility_raw = 0.4
  )
  selection_tbl <- tibble::tibble(
    A_id = "A",
    B_id = "B",
    unordered_key = "A:B",
    utility = 0.4,
    utility_raw = 0.4
  )
  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N),
    diagnostics = list(ok = FALSE)
  )

  out <- NULL
  testthat::expect_warning(
    {
      out <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
        .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
          list(state = state, fit = fit, refit_performed = TRUE)
        },
        diagnostics_gate = function(fit, config, near_stop) FALSE,
        .adaptive_theta_summary_from_fit = function(fit, state) list(),
        generate_candidates = function(theta_summary, state, config) candidate_tbl,
        compute_pair_utility = function(draws, candidates, epsilon_mean) utilities_tbl,
        apply_degree_penalty = function(utility_tbl, state) utility_tbl,
        select_batch = function(state, candidates_with_utility, config, seed = NULL,
                                exploration_only = FALSE) {
          testthat::expect_true(isTRUE(exploration_only))
          selection_tbl
        },
        .adaptive_schedule_repair_pairs = function(...) {
          rlang::abort("legacy repair scheduler called")
        }
      )
    },
    "Diagnostics gate failed; entering repair mode"
  )

  testthat::expect_equal(out$state$mode, "repair")
  testthat::expect_equal(nrow(out$pairs), 1L)
  testthat::expect_equal(out$state$iter, 1L)
  testthat::expect_equal(out$state$comparisons_scheduled, 1L)
})
