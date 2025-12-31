test_that("exhaustion fallback can unlock cross-batch new-new pairs", {
  samples <- tibble::tibble(
    ID = c("C1", "C2", "A1", "A2", "B1", "B2"),
    text = paste0("t", ID)
  )

  batches <- list(c("A1", "A2"), c("B1", "B2"))

  # Deterministic judge: ID1 always wins
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Minimal fit: theta from win counts; drift metrics not used in this test.
  fit_fun <- function(bt_data, ...) {
    winners <- dplyr::if_else(bt_data$result == 1, bt_data$object1, bt_data$object2)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- purrr::map_dbl(ids, ~ sum(winners == .x))

    theta <- tibble::tibble(
      ID = ids,
      theta = wins - stats::median(wins),
      se = 1
    )

    list(theta = theta, reliability = 0.90, diagnostics = list(sepG = 3))
  }

  set.seed(1)
  out_none <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = c("C1", "C2"),
    within_batch_frac = 1,
    core_audit_frac = 0,
    round_size = 2,
    init_round_size = 2,
    max_rounds_per_batch = 2,
    fit_fun = fit_fun,
    rel_se_p90_target = NA_real_,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    exhaustion_fallback = "none"
  )

  expect_equal(out_none$batch_summary$stop_reason[[2]], "no_pairs")

  set.seed(1)
  out_fb <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = c("C1", "C2"),
    within_batch_frac = 1,
    core_audit_frac = 0,
    round_size = 2,
    init_round_size = 2,
    max_rounds_per_batch = 2,
    fit_fun = fit_fun,
    rel_se_p90_target = NA_real_,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    exhaustion_fallback = "cross_batch_new_new"
  )

  expect_equal(out_fb$batch_summary$stop_reason[[2]], "max_rounds")

  # The fallback should introduce cross-batch comparisons involving A1/A2.
  batch2 <- dplyr::filter(out_fb$results, batch_i == 2)
  expect_true(any(c(batch2$ID1, batch2$ID2) %in% c("A1", "A2")))
})
