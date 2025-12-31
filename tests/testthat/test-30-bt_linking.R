testthat::test_that("bt_link_thetas mean_sd links by shift/scale", {
  ref <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = c(0.1, 0.1, 0.1))
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 2, 3), se = c(0.2, 0.2, 0.2))

  lk <- bt_link_thetas(cur, ref, ids = c("A", "B", "C"))

  testthat::expect_equal(lk$a, -1)
  testthat::expect_equal(lk$b, 1)
  testthat::expect_equal(lk$n_core, 3L)

  # Core thetas align to reference after linking
  out <- lk$theta
  testthat::expect_equal(out$theta_linked, ref$theta)
  # SE scales by |b| (here 1)
  testthat::expect_equal(out$se_linked, cur$se)
})


testthat::test_that("bt_run_core_linking adds theta_linked when linking is enabled", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )

  # Minimal initial results (warm start) - content doesn't matter for mock fit
  initial_results <- tibble::tibble(
    ID1 = c("A", "B", "A"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "B", "A")
  )

  # Deterministic judge: always picks ID1
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Mock BT data builder
  build_bt_fun <- function(res_tbl, judge = NULL) {
    tibble::tibble(object1 = res_tbl$ID1, object2 = res_tbl$ID2, winner = res_tbl$better_id)
  }

  # Mock fit: returns all IDs every time, but shifts scale after the warm start
  counter <- 0
  fit_fun <- function(bt_data, ...) {
    counter <<- counter + 1
    ids <- c("A", "B", "C", "D")
    base <- c(A = 0, B = 1, C = 2, D = -1)
    theta <- base
    if (counter >= 2) theta <- theta + 1
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
    embeddings = NULL,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun,
    engine = "mock",
    # ensure at least one round
    round_size = 4,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    balance_positions = TRUE,
    # disable sirt-dependent thresholds
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.0001,
    linking = "always",
    linking_method = "mean_sd",
    reference_scale_method = "mean_sd",
    verbose = FALSE
  )

  ff <- out$final_fits[["batch_1"]]
  testthat::expect_true("theta_linked" %in% names(ff$theta))
  testthat::expect_true("se_linked" %in% names(ff$theta))

  # With deterministic orientation-by-wins + centering/scaling, the baseline is
  # flipped so that higher theta corresponds to more wins. In this mock setup:
  # A beats B and C, and B beats C, so A > B > C and the normalized core baseline
  # becomes c(1, 0, -1).
  core_tbl <- ff$theta[match(c("A", "B", "C"), ff$theta$ID), ]
  testthat::expect_equal(core_tbl$theta_linked, c(1, 0, -1))
})


testthat::test_that("linking=auto only applies when drift triggers", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("text", c("A", "B", "C", "D"))
  )
  initial_results <- tibble::tibble(
    ID1 = c("A", "B", "A"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "B", "A")
  )
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  build_bt_fun <- function(res_tbl, judge = NULL) {
    tibble::tibble(object1 = res_tbl$ID1, object2 = res_tbl$ID2, winner = res_tbl$better_id)
  }

  counter <- 0
  fit_fun <- function(bt_data, ...) {
    counter <<- counter + 1
    ids <- c("A", "B", "C", "D")
    base <- c(A = 0, B = 1, C = 2, D = -1)
    theta <- base
    # Small drift in second fit
    if (counter >= 2) theta <- theta + 0.001
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
    # require a strong trigger so this small drift does not apply
    linking_cor_target = 0.50,
    linking_p90_abs_shift_target = 100,
    linking_max_abs_shift_target = 100,
    verbose = FALSE
  )

  ff <- out$final_fits[["batch_1"]]
  # auto should not have applied (no linked columns when not applied)
  testthat::expect_false("theta_linked" %in% names(ff$theta))
})
