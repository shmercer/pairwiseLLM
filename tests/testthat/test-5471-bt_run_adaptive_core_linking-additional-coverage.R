# Additional bt_run_adaptive_core_linking coverage for PR8.2.4


test_that("5471-01 core set selection runs when core_ids is NULL (auto allocation_fun)", {
  samples <- tibble::tibble(ID = c("a", "b", "c"), text = c("A", "B", "C"))

  testthat::local_mocked_bindings(
    select_core_set = function(samples, ...) {
      tibble::tibble(ID = c("a", "b"))
    },
    .env = asNamespace("pairwiseLLM")
  )

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("c")),
    core_ids = NULL,
    allocation = "precision_ramp",
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = samples$ID, theta = 0)),
    verbose = FALSE
  )

  expect_equal(out$core_ids, c("a", "b"))
  expect_equal(out$stop_reason, "pair_budget_exhausted")
})


test_that("5471-02 reference_max_abs must be positive", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(batch1 = character()),
      core_ids = c("a", "b"),
      reference_max_abs = 0,
      init_round_size = 0,
      round_size = 0,
      max_rounds_per_batch = 0,
      judge_fun = function(pairs, ...) tibble::tibble(),
      fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = samples$ID, theta = 0)),
      verbose = FALSE
    )
  )
})


test_that("5471-03 fit_from_results returns NULL fit when fit_fun returns an invalid object", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
  init <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(batch1 = c("a", "b")),
      core_ids = c("a", "b"),
      initial_results = init,
      init_round_size = 0,
      round_size = 0,
      max_rounds_per_batch = 0,
      judge_fun = function(pairs, ...) tibble::tibble(),
      fit_fun = function(bt_data, ...) 1,
      verbose = FALSE
    ),
    regexp = "fit"
  )
})


test_that("5471-04 resume_from errors when checkpoint core_ids do not match requested core_ids", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  chk_dir <- withr::local_tempdir()
  chk <- list(
    run_type = "adaptive_core_linking",
    ids = samples$ID,
    core_ids = c("a"),
    batches = list(batch1 = c("b")),
    completed = FALSE
  )
  saveRDS(chk, file.path(chk_dir, "run_state.rds"))

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(batch1 = c("b")),
      core_ids = c("a", "b"),
      resume_from = chk_dir,
      init_round_size = 0,
      round_size = 0,
      max_rounds_per_batch = 0,
      judge_fun = function(pairs, ...) tibble::tibble(),
      fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = samples$ID, theta = 0)),
      verbose = FALSE
    ),
    regexp = "core_ids"
  )
})


test_that("5471-05 resume_from returns cached output when checkpoint is completed", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  chk_dir <- withr::local_tempdir()
  cached <- list(
    results = tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a"),
    estimates = NULL,
    theta = tibble::tibble(ID = c("a", "b"), theta = c(0.1, -0.1), se = c(NA_real_, NA_real_), rank = c(1L, 2L)),
    theta_engine = "bt",
    fit_provenance = list(cached = TRUE),
    stop_reason = "precision_reached",
    stop_round = 1L,
    pairing_diagnostics = NULL,
    core_ids = c("a", "b"),
    batches = list(batch1 = c("a", "b"))
  )

  chk <- list(
    run_type = "adaptive_core_linking",
    ids = samples$ID,
    core_ids = c("a", "b"),
    batches = list(batch1 = c("a", "b")),
    completed = TRUE,
    out = cached
  )
  saveRDS(chk, file.path(chk_dir, "run_state.rds"))

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("a", "b")),
    core_ids = c("a", "b"),
    resume_from = chk_dir,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = samples$ID, theta = 0)),
    verbose = FALSE
  )

  expect_s3_class(out, "pairwiseLLM_run")
  expect_equal(out$fit_provenance$cached, TRUE)
  expect_equal(out$core_ids, c("a", "b"))
})


test_that("5471-06 resume_from recomputes current fit when checkpoint lacks it", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  fit_called <- 0L

  chk_dir <- withr::local_tempdir()
  chk <- list(
    run_type = "adaptive_core_linking",
    ids = samples$ID,
    core_ids = c("a", "b"),
    batches = list(batch1 = c("a", "b")),
    completed = FALSE,
    results = tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a"),
    current_fit = NULL
  )
  saveRDS(chk, file.path(chk_dir, "run_state.rds"))

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("a", "b")),
    core_ids = c("a", "b"),
    resume_from = chk_dir,
    final_refit = FALSE,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) {
      fit_called <<- fit_called + 1L
      list(
        theta = tibble::tibble(ID = samples$ID, theta = c(0.1, -0.1), se = NA_real_, rank = c(1L, 2L)),
        engine_used = "bt"
      )
    },
    verbose = FALSE
  )

  expect_s3_class(out, "pairwiseLLM_run")
  expect_equal(out$stop_reason, "no_new_ids")
  # When resuming without stored fits, the runner may recompute BOTH:
  #   (1) a baseline/bootstrap reference fit (for linking/drift), and
  #   (2) the current running fit.
  expect_gte(fit_called, 1L)
  expect_true(is.data.frame(out$theta))
  expect_equal(sort(out$theta$ID), sort(samples$ID))
})


test_that("5471-07 drift-active mode uses bootstrap fit in the first batch (no rounds)", {
  samples <- tibble::tibble(ID = c("a", "b", "c"), text = c("A", "B", "C"))
  init <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  # Keep drift computations lightweight but structurally valid.
  testthat::local_mocked_bindings(
    bt_drift_metrics = function(...) {
      tibble::tibble(linking_p90_abs_shift = 0, linking_max_abs_shift = 0)
    },
    .env = asNamespace("pairwiseLLM")
  )

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("a", "b", "c")),
    core_ids = c("a", "b"),
    initial_results = init,
    core_theta_cor_target = 0.5,
    final_refit = FALSE,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) {
      list(
        theta = tibble::tibble(ID = c("a", "b", "c"), theta = c(0.2, -0.2, 0), se = NA_real_)
      )
    },
    verbose = FALSE
  )

  expect_true(is.list(out$fit_provenance))
  expect_equal(out$stop_reason, "max_rounds_reached")
})


test_that("5471-08 fit_provenance is normalized to a list when final_refit returns NULL provenance", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
  init <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  testthat::local_mocked_bindings(
    compute_final_estimates = function(...) {
      est <- pairwiseLLM:::.make_estimates_tbl(
        ids = c("a", "b"),
        theta_rc = c(NA_real_, NA_real_),
        rank_rc = c(NA_integer_, NA_integer_),
        pi_rc = c(NA_real_, NA_real_),
        theta_bt_firth = c(0.1, -0.1),
        se_bt_firth = c(0.1, 0.1),
        rank_bt_firth = c(1L, 2L),
        bt_engine_requested = c("bt_firth", "bt_firth"),
        bt_engine_used = c("bt_firth", "bt_firth"),
        bt_status = c("succeeded", "succeeded"),
        bt_failure_reason = c(NA_character_, NA_character_)
      )
      list(
        estimates = est,
        bt_fit = NULL,
        rc_fit = NULL,
        diagnostics = list(bt_status = "succeeded", bt_engine_used = "bt_firth"),
        provenance = NULL
      )
    },
    .env = asNamespace("pairwiseLLM")
  )

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("a", "b")),
    core_ids = c("a", "b"),
    initial_results = init,
    final_refit = TRUE,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = c("a", "b"), theta = c(0.2, -0.2), se = NA_real_)),
    verbose = FALSE
  )

  expect_true(is.list(out$fit_provenance))
  expect_equal(out$theta_engine, "bt_firth")
})


test_that("5471-09 theta falls back to last running fit when final_refit is FALSE", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
  init <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(batch1 = c("a", "b")),
    core_ids = c("a", "b"),
    initial_results = init,
    final_refit = FALSE,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    judge_fun = function(pairs, ...) tibble::tibble(),
    fit_fun = function(bt_data, ...) {
      list(
        theta = tibble::tibble(ID = c("a", "b"), theta = c(0.2, -0.2), se = NA_real_, rank = c(1L, 2L)),
        engine_used = "bt"
      )
    },
    verbose = FALSE
  )

  expect_true(is.data.frame(out$theta))
  expect_equal(nrow(out$theta), 2)
  expect_true(is.list(out$fit_provenance))
})
