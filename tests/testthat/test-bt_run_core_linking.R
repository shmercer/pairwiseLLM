test_that("bt_run_core_linking validates inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  batches <- list(c("A"))

  expect_error(
    bt_run_core_linking(samples[, "ID"], batches = batches, judge_fun = function(x) x),
    "samples.*ID.*text"
  )

  expect_error(
    bt_run_core_linking(samples, batches = "not a list", judge_fun = function(x) x),
    "batches"
  )

  expect_error(
    bt_run_core_linking(samples, batches = list(c("Z")), judge_fun = function(x) x),
    "present in `samples\\$ID`"
  )

  expect_error(
    bt_run_core_linking(samples, batches = batches, judge_fun = "nope"),
    "judge_fun.*function"
  )
})

test_that("bt_run_core_linking requires judge column when judge is provided", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  batches <- list(c("C"))
  core_ids <- c("A", "B")

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  expect_error(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = core_ids,
      judge_fun = judge_fun,
      judge = "model",
      fit_fun = function(bt_data, ...) {
        ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
        list(engine = "mock", theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.5, length(ids))))
      },
      engine = "mock",
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "include a `model` column|judge.*provided"
  )
})

test_that("bt_run_core_linking runs batches and stops based on precision on new IDs", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  batches <- list(c("D", "E"), c("F"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2, F = -3)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  mock_fit <- function(bt_data, ...) {
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
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 8,
    max_rounds_per_batch = 5,
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    verbose = FALSE
  )

  expect_true(is.list(out))
  expect_equal(out$core_ids, core_ids)
  expect_true(all(c("results", "metrics", "batch_summary") %in% names(out)))

  expect_equal(nrow(out$batch_summary), 2L)
  expect_true(all(out$batch_summary$n_new > 0))

  # metrics should be computed on new IDs per batch (n_items equals batch new count)
  expect_true(all(out$metrics$n_items %in% c(1L, 2L)))
  expect_true(all(c("batch_index", "round_index") %in% names(out$metrics)))
})

test_that("bt_run_core_linking can gate stopping on core drift (prevents stop until max rounds)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )
  batches <- list(c("D", "E"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  mock_fit_drift <- function(bt_data, ...) {
    round <<- round + 1
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))

    # Make core thetas drift heavily each round (large absolute shifts)
    theta_vals <- seq_along(ids)
    names(theta_vals) <- ids
    theta_vals[core_ids] <- theta_vals[core_ids] + (round * 100)

    se <- rep(0.05, length(ids)) # would normally meet precision quickly
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = as.numeric(theta_vals[ids]), se = se),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit_drift,
    engine = "mock",
    round_size = 8,
    max_rounds_per_batch = 3,
    # allow repeats so we don't run out of pairs in a tiny toy example
    forbid_repeats = FALSE,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    # Drift guardrail: prevent stopping because core shifts are huge
    core_max_abs_shift_target = 0.1,
    drift_reference = "previous_round",
    verbose = FALSE
  )

  expect_equal(nrow(out$batch_summary), 1L)
  expect_equal(out$batch_summary$stop_reason[[1]], "max_rounds")
})


