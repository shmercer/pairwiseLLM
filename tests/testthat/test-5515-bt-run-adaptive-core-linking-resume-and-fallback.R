test_that("5515 bt_run_adaptive_core_linking validates init_round_size when there are no results", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      judge = "j1"
    )
  }

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      core_ids = c("A", "B"),
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      init_round_size = NA_integer_,
      round_size = 0L,
      max_rounds_per_batch = 0L,
      final_refit = FALSE
    ),
    "init_round_size.*non-negative integer"
  )
})


test_that("5515 bt_run_adaptive_core_linking aborts when resuming from a checkpoint with no fit", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )

  resume_dir <- withr::local_tempdir(pattern = "pwllm-resume-nofit")

  chk_payload <- list(
    run_type = "adaptive_core_linking",
    ids = samples$ID,
    core_ids = c("A", "B"),
    batches = list(c("A", "B")),
    results = tibble::tibble(),
    completed = FALSE,
    state = list(),
    selection_state = list(),
    completed_keys = character(0)
    # critically: no current_fit / baseline_fit in this payload
  )

  saveRDS(chk_payload, file.path(resume_dir, "run_state.rds"))

  judge_fun <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      judge = "j1"
    )
  }

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      core_ids = c("A", "B"),
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      resume_from = resume_dir,
      init_round_size = 0L,
      round_size = 0L,
      max_rounds_per_batch = 0L,
      final_refit = FALSE
    ),
    "start_resume"
  )
})


test_that("5515 bt_run_adaptive_core_linking can resume within a batch and executes exhaustion call-site", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  core_ids <- c("A", "B")
  batches <- list(c("A", "B", "C"))

  # Include all within-batch pairs so selection yields empty under forbid_repeats=TRUE.
  results <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "A", "B"),
    judge = "j1"
  )

  # Provide a fit that covers all IDs to satisfy downstream stop-metric checks.
  fit_theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0.2, -0.1, 0.0)
  )
  current_fit <- list(theta = fit_theta)

  resume_dir <- withr::local_tempdir(pattern = "pwllm-resume")
  chk_payload <- list(
    run_type = "adaptive_core_linking",
    ids = samples$ID,
    core_ids = core_ids,
    batches = batches,
    results = results,
    completed = FALSE,
    state = list(),
    selection_state = list(),
    completed_keys = character(0),
    # Mark all IDs as already seen so there are no new_ids in this batch.
    seen_ids = samples$ID,
    in_batch = TRUE,
    next_batch_index = 1L,
    next_round_index = 1L,
    new_ids_current = NULL,
    current_fit = current_fit,
    current_fit_running = NULL,
    bootstrap_fit = current_fit,
    baseline_fit = current_fit
  )

  saveRDS(chk_payload, file.path(resume_dir, "run_state.rds"))

  judge_fun <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      judge = "j1"
    )
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    core_ids = core_ids,
    batches = batches,
    judge_fun = judge_fun,
    resume_from = resume_dir,
    init_round_size = 0L,
    round_size = 1L,
    max_rounds_per_batch = 1L,
    final_refit = FALSE,
    forbid_repeats = TRUE,
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 0.5
  )

  expect_true(tibble::is_tibble(out$results))
  expect_true(tibble::is_tibble(out$pairing_diagnostics))
  expect_silent(pairwiseLLM:::validate_pairwise_run_output(out, strict = TRUE))
})
