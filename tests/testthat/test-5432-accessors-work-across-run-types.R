testthat::test_that("accessors work for bt_run_adaptive and early-exit runs", {
  withr::local_seed(1)

  expect_tbl_or_null <- function(x) {
    testthat::expect_true(is.null(x) || inherits(x, "tbl_df"))
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

  testthat::expect_identical(bt_get_theta(out), out$theta)
  testthat::expect_identical(bt_get_estimates(out), out$estimates)
  testthat::expect_identical(bt_get_pairing_diagnostics(out), out$pairing_diagnostics)

  expect_tbl_or_null(bt_get_theta(out))
  expect_tbl_or_null(bt_get_estimates(out))
  expect_tbl_or_null(bt_get_pairing_diagnostics(out))

  # Early-exit: no bootstrap and no initial_results
  out_empty <- bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) pairs, # never called
    fit_fun = function(bt_data, ...) stop("should not be called"),
    engine = "mock",
    init_round_size = 0,
    round_size = 1,
    max_rounds = 5,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    verbose = FALSE
  )

  testthat::expect_identical(bt_get_theta(out_empty), out_empty$theta)
  testthat::expect_identical(bt_get_estimates(out_empty), out_empty$estimates)
  testthat::expect_identical(bt_get_pairing_diagnostics(out_empty), out_empty$pairing_diagnostics)

  expect_tbl_or_null(bt_get_theta(out_empty))
  expect_tbl_or_null(bt_get_estimates(out_empty))
  expect_tbl_or_null(bt_get_pairing_diagnostics(out_empty))
})


testthat::test_that("accessors work for bt_run_core_linking", {
  withr::local_seed(1)

  expect_tbl_or_null <- function(x) {
    testthat::expect_true(is.null(x) || inherits(x, "tbl_df"))
  }

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
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 8,
    max_rounds_per_batch = 1,
    min_rounds = 0L,
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    rel_se_p90_min_improve = NA_real_,
    verbose = FALSE
  )

  testthat::expect_identical(bt_get_theta(out), out$theta)
  testthat::expect_identical(bt_get_estimates(out), out$estimates)
  testthat::expect_identical(bt_get_pairing_diagnostics(out), out$pairing_diagnostics)

  expect_tbl_or_null(bt_get_theta(out))
  expect_tbl_or_null(bt_get_estimates(out))
  expect_tbl_or_null(bt_get_pairing_diagnostics(out))
})


testthat::test_that("accessors work for bt_run_adaptive_core_linking", {
  withr::local_seed(1)

  expect_tbl_or_null <- function(x) {
    testthat::expect_true(is.null(x) || inherits(x, "tbl_df"))
  }

  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- stats::setNames(rep(0L, length(ids)), ids)
    n_j <- stats::setNames(rep(0L, length(ids)), ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }
    theta <- as.numeric(wins - stats::median(wins))
    se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("E", "F", "G", "H")),
    judge_fun = judge_fun,
    core_ids = NULL,
    core_method = "random",
    core_size = 3,
    seed_core = 123,
    seed_pairs = 1,
    fit_fun = fit_fun,
    engine = "mock",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 1,
    min_rounds = 0L,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  testthat::expect_identical(bt_get_theta(out), out$theta)
  testthat::expect_identical(bt_get_estimates(out), out$estimates)
  testthat::expect_identical(bt_get_pairing_diagnostics(out), out$pairing_diagnostics)

  expect_tbl_or_null(bt_get_theta(out))
  expect_tbl_or_null(bt_get_estimates(out))
  expect_tbl_or_null(bt_get_pairing_diagnostics(out))
})
