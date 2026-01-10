# Workstream F: n_pairs_total/n_pairs_new semantics are consistent across runners

testthat::test_that("runner pair counts use unique unordered edges and new-id involvement", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = paste0("text", c("A", "B", "C"))
  )
  batches <- list(c("C"))
  core_ids <- c("A", "B")

  # Include duplicate observations for the same unordered pair to ensure
  # n_pairs_total is not simply nrow(results).
  initial_results <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("B", "A"),
    better_id = c("A", "B")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = as.character(pairs$ID1),
      ID2 = as.character(pairs$ID2),
      better_id = as.character(pairs$ID1)
    )
  }

  round <- 0L
  mock_fit <- function(bt_data, ...) {
    round <<- round + 1L
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.1, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out_core <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 2,
    max_rounds_per_batch = 1,
    min_rounds = 1L,
    min_judgments = 1L,
    forbid_repeats = FALSE,
    final_refit = FALSE,
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  )

  last_core <- nrow(out_core$metrics)
  pc_core <- pairwiseLLM:::bt_count_unique_pairs(
    results = out_core$results,
    ids = samples$ID,
    new_ids = "C"
  )

  testthat::expect_equal(out_core$metrics$n_pairs_total[[last_core]], pc_core$n_pairs_total[[1]])
  testthat::expect_equal(out_core$metrics$n_pairs_new[[last_core]], pc_core$n_pairs_new[[1]])
  testthat::expect_lt(out_core$metrics$n_pairs_total[[last_core]], nrow(out_core$results))

  out_adaptive <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 2,
    init_round_size = 2,
    max_rounds_per_batch = 1,
    min_rounds = 1L,
    min_judgments = 1L,
    forbid_repeats = FALSE,
    final_refit = FALSE,
    linking = "never",
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    fit_verbose = FALSE
  )

  last_adaptive <- nrow(out_adaptive$metrics)
  pc_adaptive <- pairwiseLLM:::bt_count_unique_pairs(
    results = out_adaptive$results,
    ids = samples$ID,
    new_ids = "C"
  )

  testthat::expect_equal(out_adaptive$metrics$n_pairs_total[[last_adaptive]], pc_adaptive$n_pairs_total[[1]])
  testthat::expect_equal(out_adaptive$metrics$n_pairs_new[[last_adaptive]], pc_adaptive$n_pairs_new[[1]])
  testthat::expect_lt(out_adaptive$metrics$n_pairs_total[[last_adaptive]], nrow(out_adaptive$results))
})
