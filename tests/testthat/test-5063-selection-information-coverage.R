test_that("blocked posterior probabilities agree with exact scalar calculations", {
  state <- task09_link_state()
  state$btl_fit$beta_draws <- c(-0.2, 0, 0.1, 0.3)
  state$btl_fit$epsilon_draws <- c(0, 0.1, 0.2, 0.3)
  # More than 32 requested pairs exercises blocking without increasing panel size.
  i <- rep(c("a", "b", "c", "unknown"), 9L)
  j <- rep(c("d", "a", "d", "a"), 9L)
  scalar <- vapply(seq_along(i), function(k) {
    pairwiseLLM:::.adaptive_long_link_gate_posterior_prob(state, i[k], j[k])
  }, numeric(1))
  f <- pairwiseLLM:::.adaptive_long_link_gate_posterior_prob_vec
  expect_equal(f(state, i, j, block_size = 5L), scalar)
  expect_true(all(is.na(f(state, rep("unknown", 33), rep("a", 33)))))
  expect_error(f(state, "a", c("b", "c")), "same length")
  state$btl_fit <- NULL
  expect_true(all(is.na(f(state, i, j))))
  state <- task09_link_state()
  expect_false(pairwiseLLM:::.adaptive_long_link_gate_has_posterior(state))
  state$round_log <- tibble::tibble(diagnostics_pass = FALSE)
  expect_false(pairwiseLLM:::.adaptive_long_link_gate_has_posterior(state))
  state$btl_fit$beta_draws <- 1
  state$btl_fit$beta_mean <- 0.3
  state$btl_fit$epsilon_draws <- 1
  state$btl_fit$epsilon_mean <- 0.2
  payload <- pairwiseLLM:::.adaptive_long_link_gate_draws_payload(state)
  expect_equal(payload$beta_draws, rep(0.3, 4))
  expect_equal(payload$epsilon_draws, rep(0.2, 4))
})

test_that("D-optimal updates agree with determinant and diagonal calculations", {
  prepare <- pairwiseLLM:::.adaptive_link_d_opt_rank1_prepare
  f <- pairwiseLLM:::.adaptive_link_d_opt_rank1_gain_transform
  m <- matrix(c(2, 0.2, 0.2, 3), 2)
  prepared <- prepare(m, ridge = 0.01)
  x <- c(-1, 0, 1)
  gain <- f(prepared, rep(0.25, 3), "shift_scale", 2, x)
  expected <- vapply(x, function(value) {
    g <- c(1, 2 * value)
    log(det(m + diag(0.01, 2) + 0.25 * tcrossprod(g))) - log(det(m + diag(0.01, 2)))
  }, numeric(1))
  expect_equal(gain, expected)
  expect_false(prepare(matrix(-1, 1))$ok)
  expect_false(prepare(matrix("x", 1))$ok)
  expect_equal(prepare(m, ridge = NA_real_)$inv, solve(m + diag(1e-6, 2)))
  expect_true(all(is.na(f(prepared, 0.2, "shift_only", 1, 0))))
  expect_true(all(is.na(f(prepare(matrix(1, 1)), 0.2, "shift_scale", 1, 0))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_d_opt_gain_from_quadform(Inf, 1)))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_d_opt_rank1_gain_diag(prepared, 1, 99L)))
  dp <- pairwiseLLM:::.adaptive_link_d_opt_diag_prepare
  expect_equal(dp(c(2, 3), ridge = NA_real_)$inv_diag, 1 / (c(2, 3) + 1e-6))
  expect_false(dp(c(-1, 2))$ok)
  expect_false(dp(c(Inf, 2))$ok)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_d_opt_gain_diag_state(c(1, 2), 1, 9L)))
  expect_false(pairwiseLLM:::.adaptive_link_d_opt_entry_uses_diag(list()))
  expect_error(pairwiseLLM:::.adaptive_link_d_opt_matrix_dim(NA, "anchored_joint", 0),
    class = "pairwiseLLM_link_selector_unvalidated")
  expect_error(pairwiseLLM:::.adaptive_link_d_opt_state_get(list(), 1L, 2L, NA,
    "anchored_joint", 2L), class = "pairwiseLLM_link_selector_unvalidated")
})
