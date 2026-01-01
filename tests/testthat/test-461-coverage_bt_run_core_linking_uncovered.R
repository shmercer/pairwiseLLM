testthat::test_that("bt_run_core_linking validation branches are covered", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("ta", "tb", "tc")
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
      theta = tibble::tibble(ID = samples$ID, theta = c(0.1, 0.0, -0.1), se = rep(0.5, 3L)),
      diagnostics = list(sepG = 3.5)
    )
  }

  # function args must be functions
  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      fit_fun = "nope",
      build_bt_fun = build_bt_fun,
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "fit_fun.*function"
  )

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      build_bt_fun = "nope",
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "build_bt_fun.*function"
  )

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      allocation_fun = "nope",
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "allocation_fun"
  )

  # linking target scalars must validate
  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      linking_cor_target = c(0.9, 0.8),
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "linking_cor_target"
  )

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),   # avoid select_core_set()/core_size path
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      linking_cor_target = c(0.8, 0.7),  # <- triggers validation error
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "linking_cor_target.*single numeric"
  )

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      linking_max_abs_shift_target = c(0.30, 0.25),
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "linking_max_abs_shift_target"
  )
})

testthat::test_that("bt_run_core_linking core_ids selection & validation branches are covered", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("ta", "tb", "tc"))

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
      theta = tibble::tibble(ID = samples$ID, theta = c(0.1, 0.0, -0.1), se = rep(0.5, 3L)),
      diagnostics = list(sepG = 3.5)
    )
  }

  base_call <- function() {
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      core_ids = NULL,
      core_method = "random",
      core_size = 2,
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    )
  }

  # core_ids from select_core_set are not allowed to contain NA
  testthat::with_mocked_bindings(
    select_core_set = function(...) c("A", NA_character_),
    {
      testthat::expect_error(base_call(), "core_ids.*non-missing")
    }
  )

  # duplicates rejected
  testthat::with_mocked_bindings(
    select_core_set = function(...) c("A", "A"),
    {
      testthat::expect_error(base_call(), "core_ids.*unique")
    }
  )

  # IDs must be in samples$ID
  testthat::with_mocked_bindings(
    select_core_set = function(...) c("A", "Z"),
    {
      testthat::expect_error(base_call(), "present in `samples\\$ID`")
    }
  )
})

testthat::test_that("bt_run_core_linking validates provided core_ids and can short-circuit linking=never", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("ta", "tb", "tc"))

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
      theta = tibble::tibble(ID = samples$ID, theta = c(0.1, 0.0, -0.1), se = rep(0.5, 3L)),
      diagnostics = list(sepG = 3.5)
    )
  }

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      core_ids = c("A", NA_character_),
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      round_size = 1,
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_ids.*non-missing"
  )

  # linking='never' branch (no linking applied, but linking metadata recorded)
  out <- quietly(bt_run_core_linking(
    samples = samples,
    batches = list("C"),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    initial_results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    linking = "never",
    round_size = 1,
    init_round_size = 0,
    max_rounds_per_batch = 0,
    rel_se_p90_target = 0.7,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  ))

  testthat::expect_true(length(out$fits) >= 1)
  testthat::expect_identical(out$fits[[1]]$linking$reason, "never")
})

testthat::test_that("bt_run_core_linking errors when warm_start fit has theta = NULL", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("ta", "tb", "tc"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  build_bt_fun <- function(results, judge = NULL) {
    tibble::tibble(object1 = results$ID1, object2 = results$ID2, result = 1L)
  }

  fit_fun <- function(bt_data, ...) {
    list(engine = "mock", reliability = 0.95, theta = NULL, diagnostics = list(sepG = 3.5))
  }

  testthat::expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = list("C"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      initial_results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      fit_fun = fit_fun,
      build_bt_fun = build_bt_fun,
      round_size = 1,
      init_round_size = 0,
      max_rounds_per_batch = 1,
      rel_se_p90_target = 10,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "\\$theta.*tibble|contain a \\$theta tibble|fit_bt_model\\(\\)"
  )
})

testthat::test_that("bt_run_core_linking errors if core bootstrap produces 0 pairs", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("ta", "tb", "tc"))
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = c(0.1, 0.0, -0.1), se = rep(0.5, 3L)),
      diagnostics = list(sepG = 3.5)
    )
  }

  build_bt_fun <- function(results, judge = NULL) {
    tibble::tibble(object1 = results$ID1, object2 = results$ID2, result = 1L)
  }

  testthat::with_mocked_bindings(
    bt_core_link_round = function(...) list(pairs = tibble::tibble()),
    {
      testthat::expect_error(
        bt_run_core_linking(
          samples = samples,
          batches = list("C"),
          judge_fun = judge_fun,
          core_ids = c("A", "B"),
          fit_fun = fit_fun,
          build_bt_fun = build_bt_fun,
          round_size = 1,
          init_round_size = 1,
          max_rounds_per_batch = 0,
          rel_se_p90_target = 0.7,
          reliability_target = NA_real_,
          sepG_target = NA_real_,
          rel_se_p90_min_improve = NA_real_,
          max_item_misfit_prop = NA_real_,
          max_judge_misfit_prop = NA_real_
        ),
        "Core bootstrap produced 0 pairs"
      )
    }
  )
})
