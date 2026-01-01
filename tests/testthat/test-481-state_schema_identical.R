testthat::test_that("state output schema is identical between core and adaptive core-linking runners", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("ta", "tb", "tc", "td")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  build_bt_fun <- function(results, judge = NULL) {
    tibble::tibble(object1 = results$ID1, object2 = results$ID2, result = 1L)
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(
        ID = samples$ID,
        theta = c(0.2, 0.1, 0.0, -0.1),
        se = rep(0.5, 4L)
      ),
      diagnostics = list(sepG = 3.5)
    )
  }

  out_core <- quietly(bt_run_core_linking(
    samples = samples,
    batches = list(c("C", "D")),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    init_round_size = 1,
    max_rounds_per_batch = 0,
    round_size = 1,
    rel_se_p90_target = 0,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  ))

  out_adapt <- quietly(bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("C", "D")),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    init_round_size = 1,
    max_rounds_per_batch = 0,
    round_size = 1,
    rel_se_p90_target = 0,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  ))

  testthat::expect_true(is.data.frame(out_core$state))
  testthat::expect_true(is.data.frame(out_adapt$state))

  testthat::expect_identical(names(out_core$state), names(out_adapt$state))
})
