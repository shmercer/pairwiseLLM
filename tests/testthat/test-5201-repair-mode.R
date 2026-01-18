testthat::test_that("diagnostics failures trigger repair mode and exploration-only batches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  adaptive <- list(bins = 2L, mix_struct = 0.7, within_adj_split = 0.5, exploration_frac = 0.1)

  called <- new.env(parent = emptyenv())
  called$selection <- FALSE
  called$exploration_only <- FALSE

  out <- NULL
  testthat::expect_warning(
    {
      out <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive, seed = 1),
        .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
          list(
            state = state,
            fit = list(
              theta_mean = stats::setNames(c(0, 0, 0), state$ids),
              theta_draws = matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, state$ids)),
              diagnostics = list(divergences = 1L, max_rhat = 1.5, min_ess_bulk = 10)
            )
          )
        },
        diagnostics_gate_v3 = function(...) FALSE,
        generate_candidates_v3 = function(...) {
          tibble::tibble(i = "A", j = "B")
        },
        compute_pair_utility_v3 = function(...) {
          tibble::tibble(
            i_id = "A",
            j_id = "B",
            unordered_key = "A:B",
            p_mean = 0.5,
            utility = 0.2,
            utility_raw = 0.2
          )
        },
        apply_degree_penalty = function(utilities, state) utilities,
        select_batch_v3 = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
          called$selection <- TRUE
          called$exploration_only <- exploration_only
          tibble::tibble(
            i_id = character(),
            j_id = character(),
            unordered_key = character(),
            utility = double(),
            utility_raw = double(),
            p_mean = double(),
            A_id = character(),
            B_id = character()
          )
        }
      )
    },
    "Diagnostics gate failed; entering repair mode"
  )

  expect_true(called$selection)
  expect_true(called$exploration_only)
  expect_equal(out$state$mode, "repair")
  expect_equal(out$state$repair_attempts, 1L)

  called$selection <- FALSE
  called$exploration_only <- FALSE
  out2 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(out$state, 1L, adaptive, seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0, 0), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, state$ids)),
          diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
        )
      )
    },
    diagnostics_gate_v3 = function(...) TRUE,
    generate_candidates_v3 = function(...) tibble::tibble(i = "A", j = "B"),
    compute_pair_utility_v3 = function(...) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        p_mean = 0.5,
        utility = 0.2,
        utility_raw = 0.2
      )
    },
    apply_degree_penalty = function(utilities, state) utilities,
    select_batch_v3 = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
      called$selection <- TRUE
      called$exploration_only <- exploration_only
      tibble::tibble(
        i_id = character(),
        j_id = character(),
        unordered_key = character(),
        utility = double(),
        utility_raw = double(),
        p_mean = double(),
        A_id = character(),
        B_id = character()
      )
    }
  )

  expect_true(called$selection)
  expect_false(called$exploration_only)
  expect_equal(out2$state$mode, "adaptive")
  expect_equal(out2$state$repair_attempts, 0L)
})
