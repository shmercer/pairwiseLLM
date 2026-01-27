testthat::test_that("btl mcmc v3 validates bt_data inputs", {
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data("bad"),
    "`bt_data` must be a list"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(A = 1L)),
    "missing"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(A = 1L, B = 1L, Y = 1L, N = 1L)),
    "N` must be an integer >= 2"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(
      A = integer(),
      B = integer(),
      Y = integer(),
      N = 2L
    )),
    "at least one comparison"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(A = c(1L, 2L), B = 1L, Y = 1L, N = 2L)),
    "must match in length"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(A = 0L, B = 1L, Y = 1L, N = 2L)),
    "in 1..N"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(list(A = 1L, B = 1L, Y = 2L, N = 2L)),
    "only 0/1"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(
      list(A = 1L, B = 1L, Y = 1L, N = 2L, item_id = "A")
    ),
    "length N"
  )
})

testthat::test_that("btl mcmc v3 prepares bt_data from results", {
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
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  out <- pairwiseLLM:::.btl_mcmc_v3_prepare_bt_data(results, ids = c("A", "B"))
  expect_equal(out$A, 1L)
  expect_equal(out$B, 2L)
  expect_equal(out$Y, 1L)
  expect_equal(out$N, 2L)
  expect_equal(out$item_id, c("A", "B"))
})

testthat::test_that("btl mcmc v3 exposes model code from Stan file", {
  code <- pairwiseLLM:::.btl_mcmc_v3_model_code()
  expect_true(is.character(code))
  expect_true(nchar(code) > 0L)
  expect_match(code, "epsilon")
})

testthat::test_that("btl mcmc v3 unpacking handles list and matrix draws", {
  draws_list <- list(
    theta = matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE),
    epsilon = c(0.1, 0.2)
  )
  unpacked_list <- pairwiseLLM:::.btl_mcmc_v3_unpack_draws(draws_list)
  expect_true(is.matrix(unpacked_list$theta_draws))
  expect_equal(length(unpacked_list$epsilon_draws), 2L)

  draws_mat <- matrix(c(0, 1, 0.1, 1, 2, 0.2), nrow = 2, byrow = TRUE)
  colnames(draws_mat) <- c("theta[1]", "theta[2]", "epsilon")
  unpacked_mat <- pairwiseLLM:::.btl_mcmc_v3_unpack_draws(draws_mat)
  expect_true(is.matrix(unpacked_mat$theta_draws))
  expect_equal(length(unpacked_mat$epsilon_draws), 2L)

  bad_mat <- matrix(c(0, 1), nrow = 1)
  colnames(bad_mat) <- c("theta[1]", "theta[2]")
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(bad_mat, model_variant = "btl_e"),
    "epsilon"
  )

  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws("bad"),
    "list or matrix"
  )
})

testthat::test_that("btl mcmc v3 unpacking validates theta and beta draws", {
  bad_theta <- matrix(c(0, 1, 0.1, 0.2), nrow = 2, byrow = TRUE)
  colnames(bad_theta) <- c("epsilon", "beta")
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(bad_theta),
    "theta columns"
  )

  draws_beta <- matrix(c(0, 1, 0.5, 1, 2, 0.6), nrow = 2, byrow = TRUE)
  colnames(draws_beta) <- c("theta[1]", "theta[2]", "beta")
  unpacked_beta <- pairwiseLLM:::.btl_mcmc_v3_unpack_draws(draws_beta)
  testthat::expect_equal(length(unpacked_beta$beta_draws), 2L)

  draws_list <- list(theta = matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE))
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(draws_list, model_variant = "btl_b"),
    "beta"
  )

  draws_bad_beta <- list(
    theta = matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE),
    beta = "bad"
  )
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(draws_bad_beta, model_variant = "btl_b"),
    "beta"
  )
})

testthat::test_that("btl mcmc v3 collects diagnostics and notes missing fields", {
  fit_ok <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = c(0L, 1L)),
    summary = function(variables = NULL) {
      tibble::tibble(
        rhat = c(1.01, 1.0),
        ess_bulk = c(200, 300),
        ess_tail = c(150, 400)
      )
    }
  )
  out_ok <- pairwiseLLM:::.btl_mcmc_v3_collect_diagnostics(fit_ok, model_variant = "btl_e_b")
  expect_equal(out_ok$divergences, 1L)
  expect_equal(out_ok$max_rhat, 1.01)
  expect_equal(out_ok$min_ess_bulk, 200)
  expect_equal(out_ok$min_ess_tail, 150)

  fit_missing <- list(
    diagnostic_summary = function() tibble::tibble(other = 1L),
    summary = function(variables = NULL) tibble::tibble(value = 1)
  )
  out_missing <- pairwiseLLM:::.btl_mcmc_v3_collect_diagnostics(fit_missing, model_variant = "btl_e_b")
  expect_true(is.na(out_missing$divergences))
  expect_true(is.na(out_missing$max_rhat))
  expect_true(is.na(out_missing$min_ess_bulk))
  expect_true(is.na(out_missing$min_ess_tail))
  expect_true(length(out_missing$notes) > 0L)
})

testthat::test_that("btl mcmc v3 infers model variant from matrix columns", {
  base_draws <- matrix(0, nrow = 2, ncol = 2)
  colnames(base_draws) <- c("theta[1]", "theta[2]")
  testthat::expect_identical(pairwiseLLM:::.btl_mcmc_v3_infer_variant(base_draws), "btl")

  draws_e <- base_draws
  draws_e <- cbind(draws_e, epsilon = c(0.1, 0.2))
  testthat::expect_identical(pairwiseLLM:::.btl_mcmc_v3_infer_variant(draws_e), "btl_e")

  draws_b <- base_draws
  draws_b <- cbind(draws_b, beta = c(0.1, 0.2))
  testthat::expect_identical(pairwiseLLM:::.btl_mcmc_v3_infer_variant(draws_b), "btl_b")

  draws_eb <- cbind(draws_e, beta = c(0.1, 0.2))
  testthat::expect_identical(pairwiseLLM:::.btl_mcmc_v3_infer_variant(draws_eb), "btl_e_b")
})
