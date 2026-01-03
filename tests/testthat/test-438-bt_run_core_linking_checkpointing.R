testthat::test_that("bt_run_core_linking validates linking bounds", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  batches <- list(c("A", "B"))
  judge <- function(pairs) dplyr::mutate(pairs, better_id = .data$ID1)

  testthat::expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      core_ids = "A",
      batches = batches,
      judge_fun = judge,
      round_size = 0,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      reference_max_abs = -1
    ),
    "reference_max_abs"
  )
})

test_that("bt_run_core_linking writes checkpoints and checks resume compatibility", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("tA", "tB"))
  batches <- list(c("A", "B"))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  td <- withr::local_tempdir()
  chk_path <- file.path(td, "run_state.rds")

  out0 <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    round_size = 0,
    init_round_size = 0,
    max_rounds_per_batch = 0,
    checkpoint_dir = td,
    checkpoint_every = 1,
    checkpoint_overwrite = TRUE,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE,
    return_diagnostics = FALSE,
    include_residuals = FALSE
  )

  testthat::expect_true(file.exists(chk_path))

  chk <- readRDS(chk_path)
  chk$fit_engine_running <- "rank_centrality"
  saveRDS(chk, chk_path)

  testthat::expect_error(
    pairwiseLLM::bt_run_core_linking(
      samples = samples,
      batches = batches,
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      round_size = 0,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      checkpoint_dir = td,
      resume_from = td,
      fit_engine_running = "bt",
      final_refit = FALSE,
      final_bt_bias_reduction = FALSE,
      return_diagnostics = FALSE,
      include_residuals = FALSE
    ),
    "Resume checkpoint does not match `fit_engine_running`"
  )
})

testthat::test_that("bt_run_core_linking fills missing se in fit output", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  batches <- list(c("A", "B"))
  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  fit_fun <- function(bt_data, ...) {
    list(
      theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1)),
      reliability = 1,
      diagnostics = list()
    )
  }

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    core_ids = c("A","B"),
    batches = batches,
    judge_fun = function(pairs) dplyr::mutate(pairs, better_id = .data$ID1),
    initial_results = initial_results,
    round_size = 0,
    init_round_size = 0,
    max_rounds_per_batch = 0,
    fit_fun = fit_fun,
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE
  )

  testthat::expect_equal(out$stop_reason, "no_new_ids")
  testthat::expect_true("se" %in% names(out$fits[[1]]$theta))
})
