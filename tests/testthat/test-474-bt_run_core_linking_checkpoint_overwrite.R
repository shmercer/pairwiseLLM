testthat::test_that("bt_run_core_linking: checkpoint_overwrite=FALSE does not overwrite run_state", {
  set.seed(1)

  samples <- tibble::tibble(ID = LETTERS[1:4], text = paste0("t", LETTERS[1:4]))

  ckpt_dir <- file.path(tempdir(), paste0("pairwiseLLM_ckpt_", as.integer(runif(1, 1e8, 1e9))))
  dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

  run_state_path <- file.path(ckpt_dir, "run_state.rds")

  payload0 <- list(dummy = TRUE, stamp = "original")
  saveRDS(payload0, run_state_path)

  mtime0 <- file.info(run_state_path)$mtime
  bytes0 <- file.size(run_state_path)
  obj0   <- readRDS(run_state_path)

  judge_pick_id1 <- function(pairs, ...) {
    dplyr::mutate(pairs, better_id = .data$ID1)
  }

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(LETTERS[1:4]),
    judge_fun = judge_pick_id1,

    core_size = nrow(samples),
    core_method = "random",

    # Keep the run tiny
    round_size = 1,
    max_rounds_per_batch = 1,

    checkpoint_dir = ckpt_dir,
    checkpoint_overwrite = FALSE,

    verbose = FALSE
  )

  testthat::expect_true(file.exists(run_state_path))
  testthat::expect_identical(readRDS(run_state_path), obj0)
  testthat::expect_identical(file.size(run_state_path), bytes0)
  testthat::expect_identical(file.info(run_state_path)$mtime, mtime0)

  testthat::expect_true(is.list(out))
})

testthat::test_that("bt_run_core_linking: checkpoint_overwrite=TRUE overwrites run_state", {
  set.seed(1)

  samples <- tibble::tibble(ID = LETTERS[1:4], text = paste0("t", LETTERS[1:4]))
  ckpt_dir <- file.path(tempdir(), paste0("pairwiseLLM_ckpt_", as.integer(runif(1, 1e8, 1e9))))
  dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

  run_state_path <- file.path(ckpt_dir, "run_state.rds")
  saveRDS(list(dummy = TRUE, stamp = "original"), run_state_path)

  judge_pick_id1 <- function(pairs, ...) {
    dplyr::mutate(pairs, better_id = .data$ID1)
  }

  pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(LETTERS[1:4]),
    judge_fun = judge_pick_id1,

    core_size = nrow(samples),
    core_method = "random",

    round_size = 1,
    max_rounds_per_batch = 1,

    checkpoint_dir = ckpt_dir,
    checkpoint_overwrite = TRUE,

    verbose = FALSE
  )

  obj <- readRDS(run_state_path)
  testthat::expect_false(identical(obj, list(dummy = TRUE, stamp = "original")))
})
