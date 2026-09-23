test_that("Phase B selection gates precede judges, fitting and candidate generation", {
  state <- task09_link_state()
  before <- state
  calls <- 0L
  judge <- function(...) { calls <<- calls + 1L; stop("judge must not run") }
  for (mode in c("link_one_spoke", "link_multi_spoke")) {
    for (frozen in c(FALSE, TRUE)) {
      state$controller$run_mode <- mode
      state$controller$link_stopped_by_spoke <- list(`2` = frozen, `3` = frozen)
      expect_error(select_next_pair(state), class = "pairwiseLLM_link_selector_unvalidated")
      expect_error(run_one_step(state, judge), class = "pairwiseLLM_link_selector_unvalidated")
      expect_error(.adaptive_linking_refit_update_state(state, list()),
        class = "pairwiseLLM_link_selector_unvalidated")
    }
  }
  expect_identical(calls, 0L)
  # The gate does not mutate evidence or make a partial step commit.
  original <- before
  expect_identical(state$step_log, original$step_log)
  expect_identical(state$linking, original$linking)
})

test_that("public Phase B execution rejects before provider or fit callbacks", {
  items <- make_test_items(4)
  items$set_id <- rep(1:2, each = 2L)
  items$global_item_id <- paste0("g", items$item_id)
  state <- task09_link_state()
  # Public preflight must guard an existing Phase B state before preparing/refitting it.
  calls <- 0L
  callback <- function(...) { calls <<- calls + 1L; stop("callback must not run") }
  expect_error(adaptive_rank_run_live(state, callback, n_steps = 1L, fit_fn = callback,
    progress = "none"), class = "pairwiseLLM_link_selector_unvalidated")
  expect_identical(calls, 0L)
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    expect_error(.adaptive_normalize_link_estimation_mode(id),
      class = "pairwiseLLM_link_selector_unvalidated")
    expect_error(adaptive_rank_start(items, seed = 280L,
      adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = id)),
      class = "pairwiseLLM_link_selector_unvalidated")
  }
})

test_that("adaptive item IDs are mapped explicitly to common global identities", {
  state <- add_test_link_results(task09_link_state())
  result <- .link_orchestration_result(state, "2")
  scores <- .adaptive_link_phase_b_routing_scores(state, state$controller,
    c("a", "c"), 1L, 2L)
  expect_identical(names(scores), c("a", "c"))
  expect_identical(unname(scores), result$items$theta_link_mean[c(1, 3)])
  expect_equal(.adaptive_link_predictive_prob_oriented(state, state$controller, 2L, "a", "c"),
    predict_link(result, .link_global_pairs(result, "ga", "gc")))
  expect_error(.link_adaptive_global_ids(state, "unknown"), "Unknown adaptive")
  state$items$global_item_id <- NULL
  expect_error(.link_adaptive_global_ids(state, "a"), "explicit global")
})

test_that("E3-MCMC supplies common diagnostics and prediction but cannot select adaptively", {
  skip_if_not_installed("posterior")
  args <- link_contract_args("joint_offset", 2L)
  args$control <- list(estimator = list(engine = "mcmc"))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  state <- start_link_session(input)
  result <- .link_orchestration_result(state)
  expect_true(result$diagnostics$fit_valid)
  expect_identical(.link_orchestration_view(result)$covariance, result$uncertainty$covariance)
  expect_error(select_next_pair(state), class = "pairwiseLLM_link_selector_unvalidated")
  probes <- data.frame(observation_id = "probe", A_set = "H", A_item = "b",
    B_set = "S", B_item = "a", y_A = 0L)
  expect_equal(.link_probe_metrics(result, probes)$probe_brier,
    predict_link(result, probes[, -6])^2)
  expect_identical(resume_link_session(state, input), state)
})

test_that("selection and routing consumers do not inspect anchored-joint state", {
  consumers <- c(".adaptive_link_theta_global_map_for_items", ".adaptive_link_phase_b_routing_scores",
    ".adaptive_link_predictive_utility_context", ".adaptive_link_attach_predictive_utility",
    ".adaptive_link_predictive_prob_oriented", ".adaptive_link_d_opt_state_get",
    ".adaptive_link_d_opt_update_after_commit", "select_next_pair")
  for (name in consumers) {
    source <- paste(deparse(body(getFromNamespace(name, "pairwiseLLM"))), collapse = "\n")
    expect_false(grepl("anchored_joint|fisher_t0_by_spoke|free_block_dim", source), info = name)
  }
})

test_that("held-out caches preserve ordered observations and estimator identity", {
  state <- task09_link_state()
  result <- .link_orchestration_result(state, 2L)
  candidates <- tibble::tibble(i = c("a", "c"), j = c("c", "a"))
  pred <- .link_attach_probe_predictions(candidates, state, state$controller, 2L)
  expected <- predict_link(result, .link_global_pairs(result, c("ga", "gc"), c("gc", "ga")))
  expect_equal(pred$link_p, expected)
  expect_equal(pred$link_u, expected * (1 - expected))
  expect_gt(abs(sum(expected) - 1), 1e-4)
  for (i in 1:2) {
    state$step_log <- append_step_log(state$step_log, list(step_id = i, pair_id = i,
      A = match(candidates$i[i], state$item_ids), B = match(candidates$j[i], state$item_ids),
      Y = i - 1L, run_mode = "link_probe_holdout", is_holdout_probe_step = TRUE))
  }
  realized <- tibble::tibble(step_id = 1:2, spoke_id = 2L, link_epoch_id = 1L,
    probe_panel_id = "heldout", pair_key = make_unordered_key("a", "c"),
    hub_item_id = "a", spoke_item_id = "c")
  observations <- .link_adaptive_probe_observations(state, result, realized)
  expect_identical(observations$A_item, candidates$i)
  expect_identical(observations$y_A, 0:1)
  bad <- realized; bad$step_id[1] <- 99L
  expect_error(.link_adaptive_probe_observations(state, result, bad), "original ordered")
  bad <- state; bad$step_log$run_mode[1] <- "link_multi_spoke"
  bad$step_log$is_holdout_probe_step[1] <- FALSE
  expect_error(.link_adaptive_probe_observations(bad, result, realized), "active step")
  local_mocked_bindings(.adaptive_link_probe_realized_log_for_panel = function(...) realized,
    .adaptive_link_probe_epoch_for_spoke = function(...) 1L, .package = "pairwiseLLM")
  before <- state$linking$estimator
  cached <- .adaptive_link_probe_cache_predictions(state, 1L, 2L)
  rows <- cached$linking$probe$prediction_cache
  expect_equal(rows$pred_prob, expected)
  expect_identical(rows$estimator_id, rep(result$estimator_id, 2))
  expect_identical(rows$input_hash, rep(result$provenance$hashes$input, 2))
  expect_identical(.adaptive_link_probe_cache_predictions(cached, 1L, 2L), cached)
  expect_identical(cached$linking$estimator, before)
  expect_equal(.adaptive_link_probe_metrics_current(cached, 1L, 2L)$probe_brier,
    mean((expected - 0:1)^2))
  expect_true(is.na(.adaptive_link_probe_metrics_current(cached, 2L, 2L)$probe_brier))
  expect_true(is.na(.adaptive_link_probe_metrics_current(state, 1L, 2L)$probe_brier))
  expect_true(is.na(.adaptive_link_probe_pred_rmse_lagged(state, 2L, 2L, 1L, 1L)))
  expect_true(is.na(.adaptive_link_probe_pred_rmse_lagged(cached, 2L, 2L, 1L, 1L)))
  cached <- .adaptive_link_probe_cache_predictions(cached, 2L, 2L)
  expect_equal(.adaptive_link_probe_pred_rmse_lagged(cached, 2L, 2L, 1L, 1L), 0)
  changed <- cached; changed$linking$probe$prediction_cache$history_hash[1] <- "changed-phase-a"
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "identity mismatch")
  changed <- cached; changed$linking$probe$prediction_cache$cross_evidence_hash[1] <- "changed-cross"
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "evidence prefix")
  changed <- cached; changed$linking$probe$prediction_cache$active_edges[1:2] <- 100L
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "evidence count")
  changed <- cached; changed$linking$probe$prediction_cache$input_hash[3] <- "other-result"
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "accepted estimator")
  changed <- cached; changed$linking$probe$prediction_cache$y_A[1] <- 1L
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "orientation or outcome")
  changed <- cached; changed$linking$probe$prediction_cache$input_hash[1] <- "other"
  expect_error(.adaptive_link_probe_metrics_current(changed, 1L, 2L), "evidence mismatch")
  changed <- cached; changed$linking$probe$prediction_cache$observation_id[1:2] <- "duplicate"
  expect_error(.adaptive_link_probe_metrics_current(changed, 1L, 2L), "repeats")
  expect_error(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L), "repeats")
  changed <- cached; changed$linking$probe$prediction_cache$observation_id[1:2] <- c("other-1", "other-2")
  expect_true(is.na(.adaptive_link_probe_pred_rmse_lagged(changed, 2L, 2L, 1L, 1L)))
})

test_that("routing memo identity includes the selected common result", {
  state <- task09_link_state()
  before <- .adaptive_link_refit_local_context(state, state$controller, 2L)
  other_before <- .adaptive_link_refit_local_context(state, state$controller, 3L)
  state$linking$estimator$accepted_state_by_spoke[["2"]]$items$theta_link_mean[1] <- 10
  after <- .adaptive_link_refit_local_context(state, state$controller, 2L)
  expect_false(.adaptive_link_refit_local_context_matches(before, after))
  expect_true(.adaptive_link_refit_local_context_matches(other_before,
    .adaptive_link_refit_local_context(state, state$controller, 3L)))
})
