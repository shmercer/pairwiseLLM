make_legacy_state_for_finalize <- function(ids = c("A", "B", "C")) {
  ids <- as.character(ids)
  z <- stats::setNames(integer(length(ids)), ids)
  structure(list(
    schema_version = 1L,
    ids = ids,
    N = as.integer(length(ids)),
    texts = stats::setNames(paste("text", ids), ids),
    fit = NULL,
    pos1 = z,
    pos2 = z,
    pos_count = z,
    deg = z,
    imb = z,
    unordered_count = stats::setNames(c(1L), "A:B"),
    pair_count = stats::setNames(c(1L), "A:B"),
    pair_ordered_count = stats::setNames(c(1L), "A:B"),
    ordered_seen = stats::setNames(TRUE, "A:B"),
    history_pairs = tibble::tibble(
      pair_uid = "A:B#1",
      unordered_key = "A:B",
      ordered_key = "A:B",
      A_id = "A",
      B_id = "B",
      A_text = "a",
      B_text = "b",
      phase = "phase1",
      iter = 1L,
      created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    ),
    history_results = tibble::tibble(
      pair_uid = "A:B#1",
      unordered_key = "A:B",
      ordered_key = "A:B",
      A_id = "A",
      B_id = "B",
      better_id = "A",
      winner_pos = 1L,
      phase = "phase1",
      iter = 1L,
      received_at = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
      backend = "openai",
      model = "gpt-test"
    ),
    failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
    results_seen = stats::setNames(TRUE, "A:B#1"),
    comparisons_scheduled = 0L,
    comparisons_observed = 0L,
    phase = "phase1",
    iter = 1L,
    budget_max = 1L,
    M1_target = 1L,
    last_check_at = 0L,
    new_since_refit = 0L,
    last_refit_at = 0L,
    posterior = list(U_dup_threshold = 0.5),
    mode = "adaptive",
    repair_attempts = 0L,
    stop_reason = NULL
  ), class = "adaptive_state")
}

test_that("bayes_btl_summarize helpers validate draws and summarize outputs", {
  expect_error(pairwiseLLM:::summarize_theta(1:3), "numeric matrix")

  draws <- matrix(c(0.4, 0.2, 0.1, 0.3, 0.2, 0.8), nrow = 3)
  colnames(draws) <- c("A", "B")

  th <- pairwiseLLM:::summarize_theta(draws)
  rk <- pairwiseLLM:::summarize_ranks(draws)
  expect_equal(nrow(th), 2L)
  expect_equal(nrow(rk), 2L)
  expect_true(all(c("q025", "q975") %in% names(th)))

  expect_error(pairwiseLLM:::compute_adjacent_win_probs(draws, "A"), "at least two")
  expect_error(pairwiseLLM:::compute_adjacent_win_probs(draws, c("A", "A")), "duplicates")
  expect_error(pairwiseLLM:::compute_adjacent_win_probs(draws, c("A", "C")), "must match")

  probs <- pairwiseLLM:::compute_adjacent_win_probs(draws, c("A", "B"))
  expect_equal(nrow(probs), 1L)
  expect_true(probs$win_prob_btl[[1L]] >= 0 && probs$win_prob_btl[[1L]] <= 1)
})

test_that("finalize_adaptive_ranking builds all summary blocks", {
  state <- make_legacy_state_for_finalize()
  state$comparisons_scheduled <- 1L
  state$comparisons_observed <- 1L
  state$new_since_refit <- 1L
  draws <- matrix(c(0.1, 0.2, 0.3, 0.5, 0.4, 0.6, 0.9, 0.8, 0.7), nrow = 3)
  colnames(draws) <- c("A", "B", "C")

  out <- pairwiseLLM:::finalize_adaptive_ranking(
    state,
    mcmc_fit = list(theta_draws = draws)
  )

  expect_true(is.list(out))
  expect_true(all(c("theta_summary", "rank_summary", "adjacent_win_probs", "diagnostics") %in% names(out)))

  expect_error(
    pairwiseLLM:::finalize_adaptive_ranking(state, mcmc_fit = list()),
    "must contain `theta_draws` or `draws`"
  )

  mcmc_fit_from_list <- list(draws = list(theta = draws))
  out2 <- pairwiseLLM:::finalize_adaptive_ranking(state, mcmc_fit = mcmc_fit_from_list)
  expect_true(is.list(out2$diagnostics))

  expect_error(pairwiseLLM:::finalize_adaptive_ranking(state, mcmc_fit = 1L), "must be a list")
})

test_that("bayes_btl_mcmc_adaptive helper validators and unpackers work", {
  expect_true(pairwiseLLM:::.btl_mcmc_intish_vec(c(1, 2, 3)))
  expect_false(pairwiseLLM:::.btl_mcmc_intish_vec(c(1, 2.1)))

  expect_error(pairwiseLLM:::.btl_validate_ids(c("A", "A")), "unique")

  bt <- pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("A", "B")))
  expect_identical(bt$N, 2L)

  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 2L, N = 2L)),
    "only 0/1"
  )

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
  prep <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(results, ids = c("A", "B"))
  expect_identical(prep$Y[[1L]], 1L)
  expect_identical(prep$phase[[1L]], "phase2")
  expect_true("judge_scope" %in% names(prep))

  expect_error(pairwiseLLM:::.btl_mcmc_unpack_draws(list(theta = 1:3), model_variant = "btl"), "numeric matrix")

  mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  colnames(mat) <- c("theta[1]", "theta[2]")
  unpacked <- pairwiseLLM:::.btl_mcmc_unpack_draws(mat, model_variant = "btl")
  expect_true(is.matrix(unpacked$theta_draws))

  inferred <- pairwiseLLM:::.btl_mcmc_infer_variant(list(theta = mat, epsilon = c(0.1, 0.2), beta = c(0.3, 0.4)))
  expect_identical(inferred, "btl_e_b")

  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 2L, core_fraction = 0.5))
  expect_identical(cfg$chains, 2L)
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config("bad"), "`cmdstan` must be a list")
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(threads_per_chain = 0L)), "positive integer")
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(core_fraction = 2)), "core_fraction")
})

test_that("summarize_draws and fit-contract conversion return canonical structure", {
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- c("A", "B")
  draws <- list(theta = theta, epsilon = c(0.1, 0.2), beta = c(-0.1, 0.1))

  s <- pairwiseLLM:::summarize_draws(draws, model_variant = "btl_e_b")
  expect_true(all(c("theta_summary", "epsilon_summary") %in% names(s)))

  mcmc_fit <- list(
    model_variant = "btl_e_b",
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
    mcmc_config_used = list(
      chains = 2L,
      parallel_chains = 2L,
      core_fraction = 0.8,
      cores_detected_physical = 2L,
      cores_detected_logical = 4L,
      threads_per_chain = 1L,
      cmdstanr_version = "test"
    )
  )
  fit <- pairwiseLLM:::as_btl_fit_contract_from_mcmc(mcmc_fit, ids = c("A", "B"))
  expect_true(is.matrix(fit$theta_draws))
  expect_identical(colnames(fit$theta_draws), c("A", "B"))
  expect_true(is.list(fit$inference_contract))

  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(list(draws = list(theta = theta)), ids = c("A", "A")), "unique")
  expect_error(
    pairwiseLLM:::as_btl_fit_contract_from_mcmc(
      list(model_variant = "btl_e", draws = list(theta = theta)),
      ids = c("A", "B")
    ),
    "epsilon"
  )
})

test_that("inference contract metadata resolves from results and overrides", {
  results <- tibble::tibble(
    pair_uid = c("A:B#1", "A:B#2"),
    unordered_key = c("A:B", "A:B"),
    ordered_key = c("A:B", "A:B"),
    A_id = c("A", "A"),
    B_id = c("B", "B"),
    better_id = c("A", "B"),
    winner_pos = c(1L, 2L),
    phase = c("phase2", "phase3"),
    judge_scope = c("within", "link"),
    iter = c(1L, 2L),
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 1),
    backend = c("adaptive", "adaptive"),
    model = c("adaptive", "adaptive")
  )

  inferred <- pairwiseLLM:::.btl_mcmc_inference_contract_from_results(results)
  expect_identical(inferred$judge_param_mode, "phase_specific")
  expect_true(isTRUE(inferred$phase_boundary_detected))

  overridden <- pairwiseLLM:::.btl_mcmc_inference_contract_from_results(
    results,
    inference_contract = list(judge_param_mode = "global_shared", phase_boundary_detected = FALSE)
  )
  expect_identical(overridden$judge_param_mode, "global_shared")
  expect_false(isTRUE(overridden$phase_boundary_detected))
})

test_that("fit contract builders and validators enforce schema", {
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- c("A", "B")

  expect_error(pairwiseLLM:::reorder_theta_draws(theta, c("A", "A")), "unique")

  fit <- pairwiseLLM:::build_btl_fit_contract(
    theta_draws = theta,
    epsilon_draws = c(0.1, 0.2),
    beta_draws = c(-0.1, 0.1),
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500),
    model_variant = "btl_e_b",
    diagnostics_pass = TRUE,
    mcmc_config_used = list(
      chains = 2L,
      parallel_chains = 2L,
      core_fraction = 0.8,
      cores_detected_physical = 2L,
      cores_detected_logical = 4L,
      threads_per_chain = 1L,
      cmdstanr_version = "test"
    )
  )
  expect_true(is.list(fit))
  expect_true(isTRUE(pairwiseLLM:::validate_btl_fit_contract(fit, ids = c("A", "B"))))

  bad <- fit
  bad$theta_mean <- c(A = 0.1)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad, ids = c("A", "B")), "length `ids`")

  expect_error(pairwiseLLM:::.btl_contract_vector_summary(c(0.1), "x", NULL, c(0.5)), "at least two finite")
  expect_error(pairwiseLLM:::.btl_contract_diagnostics_pass(1L), "TRUE, FALSE, or NA")
  expect_error(pairwiseLLM:::.btl_contract_mcmc_defaults(1L), "must be a list")
})

test_that("adaptive utility candidate scoring validates shape and handles empty rows", {
  ts <- make_test_trueskill_state(make_test_items(3), mu = c(3, 2, 1), sigma = c(1, 1, 1))

  expect_error(pairwiseLLM:::score_candidates_u0(1:3, ts), "data frame")
  expect_error(pairwiseLLM:::score_candidates_u0(tibble::tibble(i = 1), ts), "include `i` and `j`")

  empty <- pairwiseLLM:::score_candidates_u0(tibble::tibble(i = integer(), j = integer()), ts)
  expect_equal(nrow(empty), 0L)

  cand <- pairwiseLLM:::score_candidates_u0(tibble::tibble(i = c(1L, 1L), j = c(2L, 3L)), ts)
  expect_true(all(cand$u0 >= 0))
})
