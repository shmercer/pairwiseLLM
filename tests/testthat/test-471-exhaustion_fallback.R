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
  fit_fun <- function(results, samples, ...) {
    ids <- sort(unique(c(results$ID1, results$ID2)))
    wins <- setNames(rep(0, length(ids)), ids)
    for (i in seq_len(nrow(results))) {
      wins[results$better_id[[i]]] <- wins[results$better_id[[i]]] + 1
    }
    theta <- tibble::tibble(ID = names(wins), theta = wins - stats::median(wins), se = 1)
    list(theta = theta, reliability = 0.90, diagnostics = list(sepG = 3))
  }

  set.seed(1)
  out_none <- quietly(bt_run_core_linking(
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
    exhaustion_fallback = "none",
    verbose = FALSE
  ))

  expect_equal(out_none$batch_summary$stop_reason[[2]], "no_pairs")

  set.seed(1)
  out_fb <- quietly(bt_run_core_linking(
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
    exhaustion_fallback = "cross_batch_new_new",
    verbose = FALSE
  ))

  expect_equal(out_fb$batch_summary$stop_reason[[2]], "max_rounds")

  # The fallback should introduce cross-batch comparisons involving A1/A2.
  batch2 <- dplyr::filter(out_fb$results, batch_i == 2)
  expect_true(any(c(batch2$ID1, batch2$ID2) %in% c("A1", "A2")))
})
