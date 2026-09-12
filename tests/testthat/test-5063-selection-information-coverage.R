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
    "positive.*free_block_dim")
  get_state <- pairwiseLLM:::.adaptive_link_d_opt_state_get
  controller <- list(link_d_opt_it_by_spoke = list(`1::2` = list(it = diag(c(2, 3)),
    it_n_pairs_accumulated = -1L)))
  out <- get_state(controller, 1L, 2L, NA, "anchored_joint", 2L)
  expect_equal(out$it_diag, c(2, 3))
  expect_identical(out$it_n_pairs_accumulated, 0L)
  controller$link_d_opt_it_by_spoke[[1]]$it <- m
  expect_equal(get_state(controller, 1L, 2L, NA, "anchored_joint", 2L)$it, m)
})

test_that("committed anchored information updates preserve other spokes and discard old windows", {
  state <- task09_link_state()
  state$linking$anchored_joint$accepted_state_by_spoke[["2"]] <-
    pairwiseLLM:::.adaptive_anchored_joint_new_accepted_state(state, 1L, 2L,
      c(a = -1, b = 1), c(c = -0.5, d = 0.5), c(c = 0.2, d = 0.2),
      list(beta = 0, epsilon = 0), "artifact_copy_init", NA_character_, NA_character_)
  refit <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  key <- paste0(refit, "::2")
  other <- paste0(refit, "::3")
  state$controller$link_d_opt_it_by_spoke <- stats::setNames(
    list(list(it = matrix(c(2, 0.1, 0.1, 3), 2)), list(it_diag = c(5, 6)), list(it_diag = 9)),
    c(key, other, "-1::2"))
  row <- tibble::tibble(is_cross_set = TRUE, run_mode = "link_multi_spoke",
    utility_mode = "linking_d_optimal", is_probe_step = FALSE, link_spoke_id = 2L, i = 1L, j = 3L)
  f <- pairwiseLLM:::.adaptive_link_d_opt_update_after_commit
  out <- f(state, state, row)
  expected <- matrix(c(2, 0.1, 0.1, 3), 2)
  # Existing Phase B uses the average of the two presentation probabilities.
  p <- (plogis(-0.5) + plogis(0.5)) / 2
  expected[1, 1] <- expected[1, 1] + p * (1 - p)
  expect_equal(out$controller$link_d_opt_it_by_spoke[[key]]$it, expected)
  expect_identical(out$controller$link_d_opt_it_by_spoke[[other]],
    state$controller$link_d_opt_it_by_spoke[[other]])
  expect_false("-1::2" %in% names(out$controller$link_d_opt_it_by_spoke))
  expect_identical(out$controller$link_d_opt_it_by_spoke[[key]]$it_n_pairs_accumulated, 1L)
  row$is_probe_step <- TRUE
  expect_identical(f(state, state, row), state)
  row$is_probe_step <- FALSE
  row$link_spoke_id <- NA_integer_
  expect_identical(f(state, state, row), state)
  row$link_spoke_id <- 2L
  row$i <- NA_integer_
  expect_identical(f(state, state, row), state)
})
test_that("predictive candidate utility agrees with covariance determinant updates", {
  state <- .adaptive_anchored_joint_sync_scaffolding(task09_link_state())
  cand <- tibble::tibble(i = c("a", "b"), j = c("c", "d"))
  key <- paste0(.adaptive_link_refit_window_id(state), "::2")
  information <- matrix(c(2, 0.1, 0.1, 3), 2)
  state$controller$link_d_opt_it_by_spoke[[key]] <- list(it = information)
  out <- .adaptive_link_attach_predictive_utility(cand, state, state$controller, 2L)
  accepted <- state$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  eta <- unname(accepted$theta_hub_fixed[cand$i] - accepted$theta_spoke_global_mean[cand$j])
  expect_equal(out$link_p, plogis(eta))
  expect_equal(out$link_u, plogis(eta) * (1 - plogis(eta)))
  expected <- vapply(1:2, function(idx) {
    update <- matrix(0, 2, 2)
    update[idx, idx] <- 0.25
    log(det(information + diag(1e-6, 2) + update)) - log(det(information + diag(1e-6, 2)))
  }, numeric(1))
  expect_equal(out$link_d_opt_gain, expected)
  state$controller$link_d_opt_it_by_spoke[[key]] <- list(it_diag = c(-1, -1))
  expect_true(all(is.na(.adaptive_link_attach_predictive_utility(cand, state,
    state$controller, 2L)$link_d_opt_gain)))
  controller <- task09_transform_controller(state, "shift_scale")
  controller$link_d_opt_it_by_spoke[[key]] <- list(it = information)
  transformed <- .adaptive_link_attach_predictive_utility(cand, state, controller, 2L)
  expect_true(all(is.finite(transformed$link_d_opt_gain)))
  controller$link_d_opt_it_by_spoke[[key]] <- list(it = -diag(2))
  expect_true(all(is.na(.adaptive_link_attach_predictive_utility(cand, state,
    controller, 2L)$link_d_opt_gain)))
})
