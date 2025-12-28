testthat::test_that("metrics schema is identical between runners", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )
  core_ids <- c("A", "B", "C")
  batches <- list(c("D", "E"))

  true_theta <- c(A = 3, B = 2, C = 1, D = 0, E = -1, F = -2)
  judge_fun <- function(pairs) {
    simulate_bt_judge(pairs, true_theta, deterministic = TRUE)
  }

  round <- 0L
  mock_fit <- function(bt_data, ...) {
    round <<- round + 1L
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    # Decreasing SE across rounds to mimic improving precision.
    se <- rep(max(0.60 - 0.10 * round, 0.05), length(ids))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out_core <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 10,
    max_rounds_per_batch = 1,
    within_batch_frac = 0.25,
    core_audit_frac = 0.10,
    allocation = "fixed",
    # disable stopping thresholds requiring diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  out_adapt <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    init_round_size = 6,
    round_size = 10,
    max_rounds_per_batch = 1,
    within_batch_frac = 0.25,
    core_audit_frac = 0.10,
    allocation = "fixed",
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  testthat::expect_identical(names(out_core$metrics), names(out_adapt$metrics))
  testthat::expect_true(all(c(
    "allocation",
    "allocation_source",
    "stage",
    "stop",
    "stop_reason",
    "linking_max_abs_shift"
  ) %in% names(out_core$metrics)))
})
