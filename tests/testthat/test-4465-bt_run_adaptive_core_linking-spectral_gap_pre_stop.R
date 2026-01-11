testthat::test_that("bt_run_adaptive_core_linking records pre-stop spectral-gap diagnostics", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )

  # Deterministic preference ordering.
  true_theta <- stats::setNames(seq(1, -1, length.out = nrow(samples)), samples$ID)
  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids) - stats::median(seq_along(ids)), se = 0.1),
      diagnostics = list(sepG = 4)
    )
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(samples$ID[4:6]),
    judge_fun = judge_fun,
    core_ids = samples$ID[1:3],
    core_method = "random",
    round_size = 4,
    init_round_size = 4,
    max_rounds_per_batch = 1,
    min_rounds = 1,
    forbid_repeats = FALSE,
    fit_fun = fit_fun,
    engine = "mock",
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = 0.99,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    spectral_gap_check = "pre_stop",
    spectral_gap_weights = "binary",
    spectral_gap_warn_below = 1
  )

  # The pre-stop check should record at least one row.
  testthat::expect_true("spectral_gap_checks" %in% names(out))
  testthat::expect_true(nrow(out$spectral_gap_checks) >= 1)
  testthat::expect_true(any(out$spectral_gap_checks$when == "pre_stop"))
  testthat::expect_true(all(c("spectral_gap_est", "lambda2_est", "spectral_gap_warn") %in% names(out$spectral_gap_checks)))
})
