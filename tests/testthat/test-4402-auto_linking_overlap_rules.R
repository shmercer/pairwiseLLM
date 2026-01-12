# Checklist C: auto-linking overlap rules

testthat::test_that("linking=auto does not link when overlap < linking_min_n", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  # Warm start for baseline reference fit
  initial_results <- tibble::tibble(
    ID1 = c("A", "B", "A"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "B", "A")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  build_bt_fun <- function(res_tbl, judge = NULL) {
    tibble::tibble(object1 = res_tbl$ID1, object2 = res_tbl$ID2, winner = res_tbl$better_id)
  }

  fit_fun <- function(bt_data, ...) {
    ids <- c("A", "B", "C", "D")
    theta <- c(A = 0, B = 1, C = 2, D = -1)
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = as.double(theta), se = rep(0.2, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = list(batch1 = c("D")),
    core_ids = c("A", "B", "C"),
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    engine = "mock",
    round_size = 4,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.0001,
    linking = "auto",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    linking_min_n = 10L,
    verbose = FALSE
  )

  ff <- out$final_fits[["batch_1"]]
  testthat::expect_false(isTRUE(ff$linking$applied))
  testthat::expect_identical(ff$linking$reason, "insufficient_overlap")
  testthat::expect_false("theta_linked" %in% names(ff$theta))
})


testthat::test_that("linking=auto links when overlap sufficient and drift triggers exceeded", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  initial_results <- tibble::tibble(
    ID1 = c("A", "B", "A"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "B", "A")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  build_bt_fun <- function(res_tbl, judge = NULL) {
    tibble::tibble(object1 = res_tbl$ID1, object2 = res_tbl$ID2, winner = res_tbl$better_id)
  }

  counter <- 0L
  fit_fun <- function(bt_data, ...) {
    counter <<- counter + 1L
    ids <- c("A", "B", "C", "D")
    theta <- c(A = 0, B = 1, C = 2, D = -1)
    if (counter >= 2L) {
      theta <- theta + 5
    }
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = as.double(theta), se = rep(0.2, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = list(batch1 = c("D")),
    core_ids = c("A", "B", "C"),
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    engine = "mock",
    round_size = 4,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.0001,
    linking = "auto",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    linking_min_n = 3L,
    linking_p90_abs_shift_target = 0.1,
    linking_max_abs_shift_target = 0.1,
    verbose = FALSE
  )

  ff <- out$final_fits[["batch_1"]]
  testthat::expect_true(isTRUE(ff$linking$applied))
  testthat::expect_identical(ff$linking$reason, "auto_trigger")
  testthat::expect_true(all(c("theta_linked", "se_linked") %in% names(ff$theta)))
})


testthat::test_that("auto-linking overlap rule is consistent across runners", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    # simple deterministic score proxy: win counts
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

  out_core <- bt_run_core_linking(
    samples = samples,
    batches = list(batch1 = c("D", "E", "F")),
    core_ids = c("A", "B", "C"),
    judge_fun = judge_fun,
    initial_results = tibble::tibble(ID1 = c("A"), ID2 = c("B"), better_id = c("A")),
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    linking = "auto",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    linking_min_n = 10L,
    verbose = FALSE
  )

  out_adapt <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E", "F")),
    core_ids = c("A", "B", "C"),
    linking = "auto",
    linking_min_n = 10L,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    seed_pairs = 1
  )

  ff_core <- out_core$final_fits[["batch_1"]]
  ff_adapt <- out_adapt$final_fits[["batch1"]]

  testthat::expect_identical(ff_core$linking$reason, "insufficient_overlap")
  testthat::expect_identical(ff_adapt$linking$reason, "insufficient_overlap")
  testthat::expect_false(isTRUE(ff_core$linking$applied))
  testthat::expect_false(isTRUE(ff_adapt$linking$applied))
})
