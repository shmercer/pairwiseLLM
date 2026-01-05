test_that("core-linking: drift guardrails block stability soft stopping", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )
  batches <- list(c("C", "D"))
  core_ids <- c("A", "B")

  true_theta <- c(A = 1, B = 0, C = -1, D = -2)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  # Lightweight deterministic fit.
  #
  # IMPORTANT: bt_run_core_linking() deterministically orients the theta scale
  # based on observed wins, and (when linking != "never") may also apply
  # linking transforms. Both behaviors can defeat attempts to induce a drift
  # failure by simply flipping the sign in a mock fit.
  #
  # To make this test robust, we instead force the drift guardrail to fail via
  # an impossible target (core_theta_cor_target > 1), and we disable precision
  # stopping so that the only soft-stop candidate is the runner-level
  # stability signal.
  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(
        ID = ids,
        theta = seq_along(ids),
        se = rep(0.05, length(ids))
      ),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    linking = "never",
    seed = 1,
    round_size = 2,
    max_rounds_per_batch = 2,
    min_rounds = 1,
    within_batch_frac = 0,
    core_audit_frac = 0,
    k_neighbors = Inf,
    min_judgments = 1,
    # Make runner-level stability easy to satisfy on round 2.
    stop_stability_consecutive = 1L,
    stop_stability_rms = 999,
    stop_topk_overlap = 0,
    # Ensure graph gating isn't the blocker.
    stop_min_degree = 0,
    stop_min_largest_component_frac = 0,
    # Disable non-drift thresholds that depend on real sirt diagnostics.
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    # Activate drift guardrails.
    core_theta_cor_target = 1.1,
    verbose = FALSE
  )

  # Without drift gating, round 2 would stop on stability_reached.
  expect_equal(tail(out$state$stop_reason, 1), "max_rounds_reached")

  # Stability signal should have been reached, but blocked by drift.
  expect_true(tail(out$metrics$stability_streak, 1) >= 1L)
  expect_true(grepl("drift_guardrails", tail(out$state$stop_blocked_by, 1)))
  expect_true(grepl("stability_reached", tail(out$state$stop_blocked_candidates, 1)))
})
