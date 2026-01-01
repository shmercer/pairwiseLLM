testthat::test_that("allocation preset 'precision_ramp' updates within_batch_frac between rounds", {
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

  # Improve precision from round 1 -> 2 by reducing SE on 2nd fit call.
  # The allocation update happens after round 2, so round 3 reflects the bump.
  fit_calls <- 0L
  fit_fun <- function(bt_data, ...) {
    fit_calls <<- fit_calls + 1L
    se_val <- if (fit_calls == 1L) 0.60 else 0.30

    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(
        ID = samples$ID,
        theta = c(0.2, 0.1, 0.0, -0.1),
        se = rep(se_val, 4L)
      ),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- quietly(bt_run_core_linking(
    samples = samples,
    batches = list(c("C", "D")), # <- 2 new IDs ensures >=3 unique rounds possible
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    allocation = "precision_ramp",
    within_batch_frac = 0.10,
    core_audit_frac = 0.10,
    round_size = 1,
    init_round_size = 0,
    max_rounds_per_batch = 3,
    # Force continuing (rel_se_p90 > 0, so target 0 won't be met)
    rel_se_p90_target = 0,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  ))

  m <- dplyr::as_tibble(out$metrics) |>
    dplyr::arrange(.data$batch_index, .data$round_index) |>
    dplyr::filter(.data$batch_index == 1L)

  testthat::expect_true("within_batch_frac" %in% names(m))
  testthat::expect_gte(nrow(m), 3L)

  testthat::expect_equal(m$within_batch_frac[[1]], 0.10)
  testthat::expect_equal(m$within_batch_frac[[2]], 0.10)
  # allocation_precision_ramp default step is 0.05
  testthat::expect_equal(m$within_batch_frac[[3]], 0.15)
})

testthat::test_that("allocation_fun takes precedence over allocation preset", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  all_ids <- samples$ID

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  build_bt_fun <- function(results, judge = NULL) {
    tibble::tibble(object1 = results$ID1, object2 = results$ID2, result = 1L)
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = all_ids, theta = seq_along(all_ids), se = rep(1, length(all_ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  alloc <- function(state) list(within_batch_frac = state$within_batch_frac + 0.20)

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    seed_pairs = 11,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    engine = "mock",
    round_size = 4,
    init_round_size = 4,
    max_rounds_per_batch = 2,
    forbid_repeats = FALSE,
    within_batch_frac = 0.10,
    core_audit_frac = 0.10,
    allocation = "precision_ramp", # should be ignored because allocation_fun is set
    allocation_fun = alloc,
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  )

  m <- dplyr::filter(out$metrics, batch_index == 1L, stage == "round")
  testthat::expect_equal(nrow(m), 2L)
  testthat::expect_equal(m$within_batch_frac[[1]], 0.10)
  testthat::expect_equal(m$within_batch_frac[[2]], 0.30)
})
