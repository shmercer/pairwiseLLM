test_that("fit_bayes_btl_fast guards against non-finite theta estimates", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )
  ids <- c("A", "B", "C")

  with_mocked_bindings(
    `.btl_fast_fit` = function(edges, ids, max_iter = 200L, tol = 1e-6, win_prior = 0.5) {
      list(
        theta_raw = stats::setNames(c(Inf, -Inf, 0), ids),
        se_raw = stats::setNames(rep(1, length(ids)), ids),
        converged = TRUE
      )
    },
    {
      fit <- expect_warning(
        fit_bayes_btl_fast(results, ids = ids, n_draws = 10, seed = 1),
        "Non-finite theta estimates"
      )
      expect_true(all(is.finite(fit$theta_mean)))
      expect_true(all(is.finite(fit$theta_draws)))
    }
  )
})
