test_that("bt_run_adaptive_core_linking selects core_ids when NULL (random) and returns fits", {
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
    # Batch has 4 IDs while core_size=3 => at least 1 new ID always
    batches = list(c("E", "F", "G", "H")),
    judge_fun = judge_fun,
    core_ids = NULL,
    core_method = "random",
    core_size = 3,
    seed_core = 123,
    seed_pairs = 1,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  expect_true(is.character(out$core_ids))
  expect_equal(length(out$core_ids), 3)
  expect_true(all(out$core_ids %in% samples$ID))
  expect_true(length(out$fits) >= 1)
  expect_true("batch1" %in% names(out$final_fits))
  expect_true("bootstrap" %in% names(out$final_fits))
  expect_equal(nrow(out$batch_summary), 1)
  expect_true(all(c("metrics", "state", "bt_data") %in% names(out)))
  expect_true(nrow(out$metrics) >= 1)
})

test_that("bt_run_adaptive_core_linking drift gating can prevent stopping (hits max_rounds)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  all_ids <- samples$ID
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  # Fit fun that returns theta for all IDs and forces large core shifts each call.
  counter <- 0L
  fit_fun <- function(bt_data, ...) {
    counter <<- counter + 1L
    bt_data <- tibble::as_tibble(bt_data)

    wins <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    n_j  <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }

    theta <- as.numeric(wins - stats::median(wins))

    # Force big, alternating shifts on A/B/C so max abs shift is large
    if (counter %% 2L == 0L) {
      theta[all_ids %in% c("A", "B", "C")] <- c(5, -5, 0)
    } else {
      theta[all_ids %in% c("A", "B", "C")] <- c(-5, 5, 0)
    }

    se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = all_ids, theta = theta, se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    seed_pairs = 11,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 3,
    forbid_repeats = FALSE,
    # Make non-drift stopping trivially satisfied
    rel_se_p90_target = 10,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    # Drift guardrail that will always fail given the forced shifts
    core_max_abs_shift_target = 1e-6
  )

  expect_equal(out$batch_summary$stop_reason[[1]], "max_rounds")
  expect_true(any(out$metrics$stop == FALSE))
})

test_that("bt_run_adaptive_core_linking validates core_ids uniqueness", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste0("t", LETTERS[1:5])
  )

  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(engine = "mock", reliability = 0.95, theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))), diagnostics = list(sepG = 3.5))
  }

  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("D"),
      judge_fun = judge_fun,
      core_ids = c("A", "A"),
      fit_fun = fit_fun,
      engine = "mock",
      round_size = 2,
      init_round_size = 2,
      max_rounds_per_batch = 1,
      rel_se_p90_target = 10,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_ids.*unique"
  )
})
