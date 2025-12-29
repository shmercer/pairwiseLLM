# --- helpers ---
.unordered_key <- function(df) {
  paste(pmin(df$ID1, df$ID2), pmax(df$ID1, df$ID2), sep = "__")
}

.mock_fit_all <- function(bt_data, ...) {
  ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
  list(
    engine = "mock",
    reliability = NA_real_,
    theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
    diagnostics = list(sepG = NA_real_)
  )
}

.make_core_initial_results <- function(samples, core_ids, true_theta, n_pairs = 8, seed = 1) {
  core_samples <- dplyr::filter(samples, .data$ID %in% core_ids)
  pairs0 <- pairwiseLLM::make_pairs(core_samples)
  pairs0 <- pairwiseLLM::sample_pairs(pairs0, n_pairs = min(n_pairs, nrow(pairs0)), seed = seed)
  judged0 <- pairwiseLLM::simulate_bt_judge(pairs0, true_theta = true_theta, deterministic = TRUE, seed = seed)
  judged0 <- tibble::as_tibble(judged0)
  judged0 <- judged0[, intersect(c("ID1", "ID2", "better_id"), names(judged0))]
  judged0
}

.strip_checkpoint_fits <- function(tmp_dir) {
  f <- file.path(tmp_dir, "run_state.rds")
  chk <- readRDS(f)
  # remove fit objects so resume MUST recompute
  chk$current_fit <- NULL
  chk$bootstrap_fit <- NULL
  chk$baseline_fit <- NULL
  chk$fits <- NULL
  chk$final_fits <- NULL
  saveRDS(chk, f)
  invisible(chk)
}

.expect_checkpoint_payload <- function(chk, run_type = NULL) {
  testthat::expect_type(chk, "list")
  testthat::expect_true("run_type" %in% names(chk))
  if (!is.null(run_type)) testthat::expect_equal(chk$run_type, run_type)

  # common fields
  testthat::expect_true("results" %in% names(chk))
  testthat::expect_true(is.data.frame(chk$results))
  testthat::expect_true("completed" %in% names(chk))
  testthat::expect_true(is.logical(chk$completed) && length(chk$completed) == 1L)

  if (identical(chk$run_type, "adaptive")) {
    testthat::expect_true("next_round" %in% names(chk))
    testthat::expect_true(is.numeric(chk$next_round) && length(chk$next_round) == 1L)
  } else if (identical(chk$run_type, "core_linking") || identical(chk$run_type, "adaptive_core_linking")) {
    testthat::expect_true(all(c("next_batch_index", "next_round_index", "in_batch") %in% names(chk)))
    testthat::expect_true(is.numeric(chk$next_batch_index) && length(chk$next_batch_index) == 1L)
    testthat::expect_true(is.numeric(chk$next_round_index) && length(chk$next_round_index) == 1L)
    testthat::expect_true(is.logical(chk$in_batch) && length(chk$in_batch) == 1L)
  }

  invisible(chk)
}

test_that("bt_run_adaptive writes checkpoints and can resume without duplicating prior rounds", {
  samples <- tibble::tibble(ID = LETTERS[1:20], text = paste0("t", LETTERS[1:20]))
  true_theta <- stats::setNames(seq(2, -2.5, length.out = nrow(samples)), samples$ID)

  calls <- 0L
  judge_fun_err <- function(pairs) {
    calls <<- calls + 1L
    # call 1 = bootstrap, call 2 = round 1, call 3 = round 2 -> error after round 1 completed
    if (calls >= 3L) stop("boom")
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  # run errors during round 2; should have a checkpoint from last completed round
  expect_error(
    bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all,
      engine = "mock",
      round_size = 10,
      init_round_size = 10,
      max_rounds = 2,
      seed_pairs = 1,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      min_judgments = 1, # critical for allowing multi-round progression in tests
      k_neighbors = 5,
      reliability_target = Inf,
      sepG_target = NA_real_,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_,
      checkpoint_dir = tmp,
      checkpoint_every = 1L,
      checkpoint_store_fits = FALSE
    ),
    "boom"
  )

  expect_true(file.exists(file.path(tmp, "run_state.rds")))
  chk1 <- readRDS(file.path(tmp, "run_state.rds"))
  .expect_checkpoint_payload(chk1, run_type = "adaptive")
  expect_false(isTRUE(chk1$completed))
  expect_equal(chk1$next_round, 2L)

  # resume should complete round 2 and write a completed checkpoint
  judge_fun_ok <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all,
    engine = "mock",
    round_size = 10,
    max_rounds = 2,
    seed_pairs = 1,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    min_judgments = 1,
    k_neighbors = 5,
    reliability_target = Inf,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    resume_from = tmp,
    checkpoint_dir = tmp,
    checkpoint_every = 1L,
    checkpoint_store_fits = FALSE
  )

  chk2 <- readRDS(file.path(tmp, "run_state.rds"))
  .expect_checkpoint_payload(chk2, run_type = "adaptive")
  expect_true(isTRUE(chk2$completed))
  expect_equal(chk2$stop_reason, "max_rounds")
  expect_equal(chk2$stop_round, 2L)
  expect_equal(chk2$next_round, 3L)

  # No unordered duplicates in accumulated results
  keys <- .unordered_key(chk2$results)
  expect_equal(length(keys), length(unique(keys)))
})

test_that("bt_run_adaptive does not checkpoint if error occurs before any round completes", {
  samples <- tibble::tibble(ID = LETTERS[1:12], text = paste0("t", LETTERS[1:12]))
  true_theta <- stats::setNames(seq(2, -2, length.out = nrow(samples)), samples$ID)

  judge_fun_err_early <- function(pairs) {
    stop("boom")
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun_err_early,
      fit_fun = .mock_fit_all,
      engine = "mock",
      round_size = 6,
      init_round_size = 6,
      max_rounds = 2,
      seed_pairs = 1,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      min_judgments = 1,
      reliability_target = Inf,
      sepG_target = NA_real_,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_,
      checkpoint_dir = tmp,
      checkpoint_every = 1L,
      checkpoint_store_fits = FALSE
    ),
    "boom"
  )

  expect_false(file.exists(file.path(tmp, "run_state.rds")))
})

test_that("bt_run_adaptive checkpoint_every controls per-round snapshots", {
  samples <- tibble::tibble(ID = LETTERS[1:10], text = paste0("t", LETTERS[1:10]))
  true_theta <- stats::setNames(seq(2, -2.5, length.out = nrow(samples)), samples$ID)

  judge_fun <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = .mock_fit_all,
    engine = "mock",
    round_size = 6,
    init_round_size = 6,
    max_rounds = 3,
    seed_pairs = 1,
    forbid_repeats = TRUE,
    balance_positions = TRUE,

    # ensure we run to max_rounds (not early stop)
    reliability_target = Inf,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    checkpoint_dir = tmp,
    checkpoint_every = 2L,
    checkpoint_store_fits = FALSE
  )

  expect_true(file.exists(file.path(tmp, "run_state.rds")))

  snaps <- sort(list.files(tmp, pattern = "^run_state_round_\\d{3}\\.rds$", full.names = FALSE))
  # With checkpoint_every=2: snapshots at round 2 (periodic) and round 3 (final max_rounds)
  expect_equal(snaps, c("run_state_round_002.rds", "run_state_round_003.rds"))
})

test_that("bt_run_core_linking checkpoints at batch boundary and resume continues after batch-2 error", {
  samples <- tibble::tibble(ID = LETTERS[1:16], text = paste0("t", LETTERS[1:16]))
  batches <- list(
    c("F", "G", "H"),
    c("I", "J", "K")
  )

  true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

  calls <- 0L
  judge_fun_err <- function(pairs) {
    calls <<- calls + 1L
    # call 1 = bootstrap, call 2 = batch 1, call 3 = batch 2 -> error after batch 1 boundary checkpoint
    if (calls >= 3L) stop("boom")
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = NULL,
      core_method = "random",
      core_size = 5,
      embeddings = NULL,
      linking = "never",
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all,
      engine = "mock",
      round_size = 8,
      max_rounds_per_batch = 1,
      within_batch_frac = 1,
      core_audit_frac = 0,
      allocation = "fixed",
      k_neighbors = 5,
      min_judgments = 1,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      reliability_target = Inf,
      sepG_target = NA_real_,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_,
      drift_reference = "baseline",
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed = 1,
      verbose = FALSE
    ),
    "boom"
  )

  expect_true(file.exists(file.path(tmp, "run_state.rds")))
  chk <- readRDS(file.path(tmp, "run_state.rds"))
  .expect_checkpoint_payload(chk, run_type = "core_linking")
  expect_true(is.null(chk$current_fit))
  expect_true(is.null(chk$baseline_fit))
  expect_true(!is.null(chk$baseline_results_n))
  expect_equal(chk$next_batch_index, 2L)
  expect_equal(chk$next_round_index, 1L)

  judge_fun_ok <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = NULL,
    core_method = "random",
    core_size = 5,
    embeddings = NULL,
    linking = "never",
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all,
    engine = "mock",
    round_size = 8,
    max_rounds_per_batch = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    allocation = "fixed",
    k_neighbors = 5,
    min_judgments = 1,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    reliability_target = Inf,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    drift_reference = "baseline",
    resume_from = tmp,
    checkpoint_dir = tmp,
    checkpoint_store_fits = FALSE,
    seed = 1,
    verbose = FALSE
  )

  expect_equal(nrow(out$batch_summary), 2L)
})

test_that("bt_run_adaptive_core_linking checkpoints at batch boundary and resume continues after batch-2 error", {
  samples <- tibble::tibble(ID = LETTERS[1:12], text = paste0("t", LETTERS[1:12]))
  batches <- list(c("F", "G", "H"), c("I", "J", "K"))
  core_ids_fixed <- LETTERS[1:5]
  batch2_ids <- c("I", "J", "K")

  true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

  initial_results <- .make_core_initial_results(
    samples = samples,
    core_ids = core_ids_fixed,
    true_theta = true_theta,
    n_pairs = 8,
    seed = 1
  )

  judge_fun_err <- function(pairs) {
    if (any(pairs$ID1 %in% batch2_ids) || any(pairs$ID2 %in% batch2_ids)) stop("boom")
    pairwiseLLM::simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  old_opt <- options(warnPartialMatchArgs = TRUE)
  on.exit(options(old_opt), add = TRUE)

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = batches,
      core_ids = core_ids_fixed,
      linking = "never",
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all,
      engine = "mock",
      initial_results = initial_results,

      # keep it simple/deterministic
      round_size = 8,
      init_round_size = 0, # ignored since initial_results provided
      max_rounds_per_batch = 1,
      within_batch_frac = 0,
      core_audit_frac = 0,
      min_judgments = 1,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_every = 1L,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    ),
    "boom"
  )

  expect_true(file.exists(file.path(tmp, "run_state.rds")))
  chk <- readRDS(file.path(tmp, "run_state.rds"))

  # We should have finished batch 1 and failed at the start of batch 2:
  expect_false(isTRUE(chk$completed))
  expect_equal(chk$next_batch_index, 2L)
  expect_equal(chk$next_round_index, 1L)
  expect_false(isTRUE(chk$in_batch)) # checkpoint written at batch boundary

  judge_fun_ok <- function(pairs) {
    pairwiseLLM::simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids_fixed,
    linking = "never",
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all,
    engine = "mock",
    initial_results = initial_results,
    round_size = 8,
    init_round_size = 0,
    max_rounds_per_batch = 1,
    within_batch_frac = 0,
    core_audit_frac = 0,
    min_judgments = 1,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    reliability_target = Inf,
    resume_from = tmp,
    checkpoint_dir = tmp,
    checkpoint_store_fits = FALSE,
    seed_pairs = 1
  )

  expect_equal(nrow(out$batch_summary), 2L)
})

test_that("resume_from errors clearly when missing or incompatible", {
  samples <- tibble::tibble(ID = LETTERS[1:6], text = paste0("t", LETTERS[1:6]))
  true_theta <- stats::setNames(seq(1, -1, length.out = nrow(samples)), samples$ID)

  judge_fun <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  # ---- missing checkpoint file ----
  missing_dir <- tempfile("pairwiseLLM_missing_chk_")
  dir.create(missing_dir, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun,
      fit_fun = .mock_fit_all,
      engine = "mock",
      max_rounds = 1,
      resume_from = missing_dir
    ),
    "No checkpoint found"
  )

  # ---- incompatible IDs ----
  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = .mock_fit_all,
    engine = "mock",
    round_size = 4,
    init_round_size = 4,
    max_rounds = 1,
    seed_pairs = 1,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    checkpoint_dir = tmp,
    checkpoint_store_fits = FALSE
  )

  samples_other <- tibble::tibble(ID = c("X", "Y", "Z"), text = c("x", "y", "z"))

  expect_error(
    bt_run_adaptive(
      samples = samples_other,
      judge_fun = function(pairs) pairs,
      fit_fun = .mock_fit_all,
      engine = "mock",
      max_rounds = 1,
      resume_from = tmp
    ),
    "Checkpoint sample IDs do not match"
  )
})

test_that("bt_run_core_linking resume works when checkpoint_store_fits=FALSE (recomputes fits on resume)", {
  samples <- tibble::tibble(ID = LETTERS[1:16], text = paste0("t", LETTERS[1:16]))
  batches <- list(c("F", "G", "H"), c("I", "J", "K"))
  true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

  calls <- 0L
  judge_fun_err <- function(pairs) {
    calls <<- calls + 1L
    if (calls >= 3L) stop("boom")
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    bt_run_core_linking(
      samples = samples, batches = batches,
      core_ids = NULL, core_method = "random", core_size = 5,
      linking = "never",
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all, engine = "mock",
      round_size = 8, max_rounds_per_batch = 1, within_batch_frac = 1, core_audit_frac = 0,
      min_judgments = 1, forbid_repeats = TRUE, balance_positions = TRUE,
      reliability_target = Inf,
      checkpoint_dir = tmp, checkpoint_store_fits = FALSE,
      seed = 1, verbose = FALSE
    ),
    "boom"
  )

  chk <- readRDS(file.path(tmp, "run_state.rds"))
  expect_true(is.null(chk$current_fit))
  expect_true(is.null(chk$baseline_fit))

  judge_fun_ok <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  out <- bt_run_core_linking(
    samples = samples, batches = batches,
    core_ids = NULL, core_method = "random", core_size = 5,
    linking = "never",
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all, engine = "mock",
    round_size = 8, max_rounds_per_batch = 1, within_batch_frac = 1, core_audit_frac = 0,
    min_judgments = 1, forbid_repeats = TRUE, balance_positions = TRUE,
    reliability_target = Inf,
    resume_from = tmp, checkpoint_dir = tmp, checkpoint_store_fits = FALSE,
    seed = 1, verbose = FALSE
  )
  expect_equal(nrow(out$batch_summary), 2L)
})

test_that("bt_run_core_linking recomputes baseline/current fits on resume when checkpoint_store_fits=FALSE", {
  samples <- tibble::tibble(ID = LETTERS[1:16], text = paste0("t", LETTERS[1:16]))
  batches <- list(c("F", "G", "H"), c("I", "J", "K"))
  true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

  calls <- 0L
  judge_fun_err <- function(pairs) {
    calls <<- calls + 1L
    if (calls >= 3L) stop("boom") # error in batch 2, after batch 1 boundary
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    bt_run_core_linking(
      samples = samples, batches = batches,
      core_ids = NULL, core_method = "random", core_size = 5, embeddings = NULL,
      linking = "never",
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all, engine = "mock",
      round_size = 8, max_rounds_per_batch = 1, within_batch_frac = 1, core_audit_frac = 0,
      min_judgments = 1, forbid_repeats = TRUE, balance_positions = TRUE,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed = 1,
      verbose = FALSE
    ),
    "boom"
  )

  chk <- readRDS(file.path(tmp, "run_state.rds"))
  expect_false(isTRUE(chk$completed))
  # fits should NOT be stored
  expect_true(is.null(chk$baseline_fit) || isFALSE("baseline_fit" %in% names(chk)))
  expect_true(is.null(chk$current_fit) || isFALSE("current_fit" %in% names(chk)))

  judge_fun_ok <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  out <- bt_run_core_linking(
    samples = samples, batches = batches,
    core_ids = NULL, core_method = "random", core_size = 5, embeddings = NULL,
    linking = "never",
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all, engine = "mock",
    round_size = 8, max_rounds_per_batch = 1, within_batch_frac = 1, core_audit_frac = 0,
    min_judgments = 1, forbid_repeats = TRUE, balance_positions = TRUE,
    reliability_target = Inf,
    resume_from = tmp,
    checkpoint_dir = tmp,
    checkpoint_store_fits = FALSE,
    seed = 1,
    verbose = FALSE
  )

  expect_equal(nrow(out$batch_summary), 2L)
})

test_that("bt_run_adaptive_core_linking recomputes fits on resume when checkpoint_store_fits=FALSE", {
  samples <- tibble::tibble(ID = LETTERS[1:12], text = paste0("t", LETTERS[1:12]))
  batches <- list(c("F", "G", "H"), c("I", "J", "K"))
  core_ids_fixed <- LETTERS[1:5]
  batch2_ids <- c("I", "J", "K")

  true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

  initial_results <- .make_core_initial_results(
    samples = samples,
    core_ids = core_ids_fixed,
    true_theta = true_theta,
    n_pairs = 8,
    seed = 1
  )

  judge_fun_err <- function(pairs) {
    if (any(pairs$ID1 %in% batch2_ids) || any(pairs$ID2 %in% batch2_ids)) stop("boom")
    pairwiseLLM::simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  tmp <- tempfile("pairwiseLLM_chk_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = batches,
      core_ids = core_ids_fixed,
      linking = "never",
      judge_fun = judge_fun_err,
      fit_fun = .mock_fit_all,
      engine = "mock",
      initial_results = initial_results,
      round_size = 8,
      init_round_size = 0,
      max_rounds_per_batch = 1,
      within_batch_frac = 0,
      core_audit_frac = 0,
      min_judgments = 1,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_every = 1L,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    ),
    "boom"
  )

  # Strip fits so resume must recompute from results
  .strip_checkpoint_fits(tmp)

  judge_fun_ok <- function(pairs) {
    pairwiseLLM::simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids_fixed,
    linking = "never",
    judge_fun = judge_fun_ok,
    fit_fun = .mock_fit_all,
    engine = "mock",
    initial_results = initial_results,
    round_size = 8,
    init_round_size = 0,
    max_rounds_per_batch = 1,
    within_batch_frac = 0,
    core_audit_frac = 0,
    min_judgments = 1,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    reliability_target = Inf,
    resume_from = tmp,
    checkpoint_dir = tmp,
    checkpoint_store_fits = FALSE,
    seed_pairs = 1
  )

  expect_equal(nrow(out$batch_summary), 2L)
})
