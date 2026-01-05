test_that("core-linking records graph-gated blocked stop candidates in state", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )
  batches <- list(c("C", "D"))
  core_ids <- c("A", "B")

  true_theta <- c(A = 1, B = 0, C = -1, D = -2)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  # Deterministic fit for all IDs so stability metrics are finite and easy to satisfy.
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(
        ID = samples$ID,
        theta = seq_along(samples$ID),
        se = rep(0.05, length(samples$ID))
      ),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    linking = "never",
    seed = 1,
    # Two rounds so runner-level stability can become eligible.
    max_rounds_per_batch = 2,
    min_rounds = 1,
    round_size = 1,
    # Allow repeats so we don't hard-stop on "no_new_pairs".
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    within_batch_frac = 0,
    core_audit_frac = 0,
    k_neighbors = Inf,
    min_judgments = 1,
    # Disable bt_should_stop() thresholds (we're testing runner-level stability + graph gating).
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    # Make stability easy to satisfy on round 2.
    stop_stability_consecutive = 1L,
    stop_stability_rms = 999,
    stop_topk_overlap = 0,
    # Force graph gating to remain unhealthy even if the bootstrap/core pairs
    # connect the graph. With 4 active IDs and only a few edges, degree_min will
    # remain < 2, so graph gating should block soft stopping.
    stop_min_degree = 2,
    stop_min_largest_component_frac = 1,
    verbose = FALSE
  )

  # Stability should have been eligible, but blocked by graph gating, so the run
  # falls back to the post-loop max-round reason.
  expect_equal(tail(out$state$stop_reason, 1), "max_rounds_reached")

  # The runner should record that graph gating blocked a soft stop.
  expect_true(any(grepl("graph_unhealthy", dplyr::coalesce(out$state$stop_blocked_by, ""))))
  expect_true(any(grepl("stability_reached", dplyr::coalesce(out$state$stop_blocked_candidates, ""))))
})


test_that("core-linking drift gating handles empty non-drift criteria set", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )
  batches <- list("C")
  core_ids <- c("A", "B")

  true_theta <- c(A = 1, B = 0, C = -1)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(
        ID = samples$ID,
        theta = seq_along(samples$ID),
        se = rep(0.05, length(samples$ID))
      ),
      diagnostics = list(sepG = NA_real_)
    )
  }

  # Mock bt_should_stop() so its `details` contains only drift + precision/stability
  # criteria. This forces the runner's det_non_drift subset to be empty.
  testthat::local_mocked_bindings(
    bt_should_stop = function(metrics, prev_metrics = NULL, ...) {
      details <- tibble::tibble(
        criterion = c("core_theta_cor", "rel_se_p90_precision", "rel_se_p90_stability"),
        threshold = c(0.90, 0.50, NA_real_),
        pass = c(FALSE, TRUE, TRUE)
      )
      list(stop = FALSE, details = details)
    },
    .package = "pairwiseLLM"
  )

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    linking = "never",
    seed = 1,
    max_rounds_per_batch = 1,
    min_rounds = 1,
    round_size = 2,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    within_batch_frac = 0,
    core_audit_frac = 0,
    k_neighbors = Inf,
    min_judgments = 1,
    # Ensure graph gating doesn't suppress drift explainability.
    stop_min_degree = 0,
    stop_min_largest_component_frac = 0,
    # Disable non-drift thresholds (runner shouldn't error if they are absent).
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  # Drift should block stopping and be recorded, along with the candidate
  # that would have stopped absent drift.
  expect_true(grepl("drift_guardrails", tail(out$state$stop_blocked_by, 1)))
  expect_true(grepl("precision_reached", tail(out$state$stop_blocked_candidates, 1)))
})
