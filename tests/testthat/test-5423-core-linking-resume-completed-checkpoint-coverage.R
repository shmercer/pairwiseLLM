test_that("bt_run_core_linking validates linking/core thresholds", {
  samples <- tibble::tibble(ID = LETTERS[1:3], text = paste0("t", 1:3))
  batches <- list(c("C"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(engine = "mock", reliability = 0.9, theta = tibble::tibble(ID = ids, theta = rep(0, length(ids)), se = rep(1, length(ids))), diagnostics = list(sepG = 3.5))
  }

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = c("A", "B"),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      round_size = 0,
      max_rounds_per_batch = 0,
      linking_p90_abs_shift_target = "nope"
    ),
    "linking_p90_abs_shift_target"
  )

  expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = c("A", "B"),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      round_size = 0,
      max_rounds_per_batch = 0,
      exhaustion_min_pairs_frac = 1.5
    ),
    "exhaustion_min_pairs_frac"
  )
})


test_that("bt_run_core_linking upgrades completed checkpoints missing PR8 contract fields", {
  td <- withr::local_tempdir()

  # Minimal checkpoint with completed=TRUE and an older-style out list that drops
  # some required contract fields.
  chk <- list(
    run_type = "core_linking",
    ids = c("A", "B", "C"),
    core_ids = c("A", "B"),
    batches = list(c("C")),
    linking = "auto",
    linking_method = "shift",
    completed = TRUE,
    next_batch_index = NA_integer_,
    next_round_index = NA_integer_,
    results = tibble::tibble(),
    batch_summary = tibble::tibble(),
    fits = list(),
    out = list(
      # These NULLs emulate older checkpoints that didn't store PR8 contract fields.
      estimates = NULL,
      theta = NULL,
      theta_engine = NULL,
      fit_provenance = NULL,
      stop_reason = NA_character_,
      stop_round = NULL,
      pairing_diagnostics = NULL
    )
  )

  saveRDS(chk, file = file.path(td, "run_state.rds"))

  samples <- tibble::tibble(ID = LETTERS[1:3], text = paste0("t", 1:3))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(engine = "mock", reliability = 0.9, theta = tibble::tibble(ID = ids, theta = rep(0, length(ids)), se = rep(1, length(ids))), diagnostics = list(sepG = 3.5))
  }

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(c("C")),
    core_ids = c("A", "B"),
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    round_size = 0,
    max_rounds_per_batch = 0,
    checkpoint_dir = td,
    resume_from = td,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE
  )

  expect_true(is.list(out$fit_provenance))
  expect_true("theta_engine" %in% names(out))
  expect_true("pairing_diagnostics" %in% names(out))

  expect_silent(pairwiseLLM:::validate_pairwise_run_output(out))
})
