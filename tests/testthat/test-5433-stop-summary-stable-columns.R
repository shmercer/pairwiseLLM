testthat::test_that("bt_stop_summary() always returns stable columns (adaptive + early-exit)", {
  withr::local_seed(1)

  expected_cols <- c(
    "stop_reason", "stop_round", "theta_engine",
    "degree_min", "largest_component_frac", "rms_theta_delta", "topk_overlap",
    "stop_blocked_by", "stop_blocked_candidates"
  )

  expect_stop_schema <- function(x) {
    testthat::expect_true(inherits(x, "tbl_df"))
    testthat::expect_identical(names(x), expected_cols)
    testthat::expect_identical(nrow(x), 1L)
  }

  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  true_theta <- c(A = 1, B = 0, C = -1)

  judge_fun <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.5, length(ids))),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 2,
    round_size = 2,
    max_rounds = 1,
    min_rounds = 0L,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  )

  s <- bt_stop_summary(out)
  expect_stop_schema(s)
  testthat::expect_identical(s$stop_reason[[1]], as.character(out$stop_reason))
  testthat::expect_identical(s$stop_round[[1]], as.integer(out$stop_round))
  testthat::expect_identical(s$theta_engine[[1]], as.character(out$theta_engine))

  # Pairing diagnostics present -> use last row
  diag_last <- out$pairing_diagnostics[nrow(out$pairing_diagnostics), , drop = FALSE]
  testthat::expect_identical(s$degree_min[[1]], as.double(diag_last$degree_min[[1]]))
  testthat::expect_identical(s$largest_component_frac[[1]], as.double(diag_last$largest_component_frac[[1]]))
  testthat::expect_identical(s$rms_theta_delta[[1]], as.double(diag_last$rms_theta_delta[[1]]))
  testthat::expect_identical(s$topk_overlap[[1]], as.double(diag_last$topk_overlap[[1]]))
  testthat::expect_identical(s$stop_blocked_by[[1]], as.character(diag_last$stop_blocked_by[[1]]))
  testthat::expect_identical(s$stop_blocked_candidates[[1]], as.character(diag_last$stop_blocked_candidates[[1]]))

  # Early-exit: no bootstrap and no initial_results
  out_empty <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 0,
    round_size = 0,
    max_rounds = 1,
    min_rounds = 0L,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  )

  s2 <- bt_stop_summary(out_empty)
  expect_stop_schema(s2)

  # No per-round diagnostics -> NAs but stable schema
  testthat::expect_true(is.na(s2$degree_min[[1]]))
  testthat::expect_true(is.na(s2$largest_component_frac[[1]]))
  testthat::expect_true(is.na(s2$rms_theta_delta[[1]]))
  testthat::expect_true(is.na(s2$topk_overlap[[1]]))
  testthat::expect_true(is.na(s2$stop_blocked_by[[1]]))
  testthat::expect_true(is.na(s2$stop_blocked_candidates[[1]]))
})

testthat::test_that("bt_stop_summary() returns NAs for linking runs with NULL pairing_diagnostics", {
  withr::local_seed(1)

  expected_cols <- c(
    "stop_reason", "stop_round", "theta_engine",
    "degree_min", "largest_component_frac", "rms_theta_delta", "topk_overlap",
    "stop_blocked_by", "stop_blocked_candidates"
  )

  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  batches <- list(c("D", "E"), c("F"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2, F = -3)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  fit_fun <- function(bt_data, ...) {
    round <<- round + 1
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    se <- rep(max(0.6 - 0.15 * round, 0.05), length(ids))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    core_ids = core_ids,
    batches = batches,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 2,
    round_size = 2,
    max_rounds = 2,
    min_rounds = 0L,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  s <- bt_stop_summary(out)
  testthat::expect_true(inherits(s, "tbl_df"))
  testthat::expect_identical(names(s), expected_cols)
  testthat::expect_identical(nrow(s), 1L)

  testthat::expect_true(is.null(out$pairing_diagnostics))
  testthat::expect_true(is.na(s$degree_min[[1]]))
  testthat::expect_true(is.na(s$largest_component_frac[[1]]))
  testthat::expect_true(is.na(s$rms_theta_delta[[1]]))
  testthat::expect_true(is.na(s$topk_overlap[[1]]))
  testthat::expect_true(is.na(s$stop_blocked_by[[1]]))
  testthat::expect_true(is.na(s$stop_blocked_candidates[[1]]))
})

testthat::test_that("bt_stop_summary() uses the last row of pairing_diagnostics when present (adaptive_core_linking)", {
  withr::local_seed(1)

  expected_cols <- c(
    "stop_reason", "stop_round", "theta_engine",
    "degree_min", "largest_component_frac", "rms_theta_delta", "topk_overlap",
    "stop_blocked_by", "stop_blocked_candidates"
  )

  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)
  core_ids <- samples$ID[1:4]
  batches <- list(samples$ID[5:6], samples$ID[7:8])

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  fit_fun <- function(bt_data, ...) {
    round <<- round + 1
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    se <- rep(max(0.7 - 0.15 * round, 0.05), length(ids))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    core_ids = core_ids,
    batches = batches,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 2,
    round_size = 2,
    max_rounds = 2,
    min_rounds = 0L,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  s <- bt_stop_summary(out)
  testthat::expect_true(inherits(s, "tbl_df"))
  testthat::expect_identical(names(s), expected_cols)
  testthat::expect_identical(nrow(s), 1L)

  diag_last <- out$pairing_diagnostics[nrow(out$pairing_diagnostics), , drop = FALSE]
  testthat::expect_identical(s$degree_min[[1]], as.double(diag_last$degree_min[[1]]))
  testthat::expect_identical(s$largest_component_frac[[1]], as.double(diag_last$largest_component_frac[[1]]))
  testthat::expect_identical(s$rms_theta_delta[[1]], as.double(diag_last$rms_theta_delta[[1]]))
  testthat::expect_identical(s$topk_overlap[[1]], as.double(diag_last$topk_overlap[[1]]))
  testthat::expect_identical(s$stop_blocked_by[[1]], as.character(diag_last$stop_blocked_by[[1]]))
  testthat::expect_identical(s$stop_blocked_candidates[[1]], as.character(diag_last$stop_blocked_candidates[[1]]))
})
