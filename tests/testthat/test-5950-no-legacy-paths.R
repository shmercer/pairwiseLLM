testthat::test_that("adaptive scheduling avoids legacy paths", {
  withr::local_seed(123)
  legacy_names <- c(
    paste0("phase1", "_generate_pairs"),
    paste0("select_pairs", "_from_candidates")
  )
  ns <- asNamespace("pairwiseLLM")

  for (nm in legacy_names) {
    if (exists(nm, envir = ns, inherits = FALSE)) {
      assign(nm, function(...) testthat::fail(paste0(nm, " should not be called.")), envir = ns)
    }
  }

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
    duplicate_allowed = function(...) {
      testthat::fail("duplicate_allowed should not be called in v3 scheduling.")
    },
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(rep(0, state$N), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = state$N, dimnames = list(NULL, state$ids)),
          diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
        ),
        refit_performed = TRUE
      )
    },
    generate_candidates = function(...) tibble::tibble(i = "A", j = "B"),
    compute_pair_utility = function(...) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        utility = 0.2,
        utility_raw = 0.2,
        p_mean = 0.5
      )
    },
    apply_degree_penalty = function(utilities, state) utilities,
    select_batch = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
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
    },
    .package = "pairwiseLLM"
  )

  testthat::expect_equal(nrow(out$pairs), 0L)
})
