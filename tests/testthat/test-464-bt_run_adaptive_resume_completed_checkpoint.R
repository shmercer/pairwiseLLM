test_that("bt_run_adaptive can resume a completed checkpoint (completed=TRUE) and returns early", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  ids <- samples$ID

  # minimal judge_fun; should not be invoked on completed checkpoints, but must pass validation
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  chk_dir <- file.path(tempdir(), paste0("pairwiseLLM_chk_", as.integer(stats::runif(1) * 1e9)))
  dir.create(chk_dir, recursive = TRUE, showWarnings = FALSE)

  # Build a checkpoint payload that forces the "completed" early-return branch.
  payload <- list(
    run_type = "adaptive",
    ids = ids,
    results = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    pairs_bootstrap = tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()),
    fits = list(),
    final_fit = NULL,
    prev_metrics = NULL,
    stop_reason = "completed_from_checkpoint",
    stop_round = 1L,
    rounds = tibble::tibble(round = integer(), n_pairs = integer(), stop_reason = character()),
    state = tibble::tibble(round = integer()),
    # Force start_round sanitization branch:
    next_round = NA_integer_,
    completed = TRUE,
    out = NULL
  )

  pairwiseLLM:::.bt_write_checkpoint(chk_dir, payload, basename = "run_state", overwrite = TRUE)

  out <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    resume_from = chk_dir,
    # keep run lightweight; should never enter loop
    round_size = 1,
    init_round_size = 0,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  expect_true(inherits(out, "pairwiseLLM_run"))
  expect_identical(attr(out, "run_type"), "adaptive")
  expect_identical(out$stop_reason, "completed_from_checkpoint")
  expect_identical(out$stop_round, 1L)
  expect_true(is.data.frame(out$results))
  expect_equal(nrow(out$results), 0L)
})
