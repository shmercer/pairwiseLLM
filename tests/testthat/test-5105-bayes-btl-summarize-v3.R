testthat::test_that("btl mcmc validate draws rejects invalid input", {
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_draws("bad"),
    "numeric matrix"
  )

  one_draw <- matrix(c(1, 2), nrow = 1)
  colnames(one_draw) <- c("A", "B")
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_draws(one_draw),
    "at least two draws"
  )

  missing_cols <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_draws(missing_cols),
    "non-empty column names"
  )
})

testthat::test_that("finalize_adaptive_ranking supports v3 draws", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 1L, M1_target = 1L, budget_max = 4L),
    seed = 1
  )

  draws <- matrix(
    c(2, 1, 0,
      2, 1, 0),
    nrow = 2,
    byrow = TRUE
  )
  colnames(draws) <- state$ids
  mcmc_fit <- list(
    draws = list(theta = draws, epsilon = c(0.1, 0.2))
  )

  out <- pairwiseLLM:::finalize_adaptive_ranking(state, mcmc_fit)
  expect_true(is.list(out$theta_summary))
  expect_true(is.list(out$rank_summary))
  expect_true(all(c("win_prob", "win_prob_btl") %in% names(out$adjacent_win_probs)))
})
