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

test_that("bt_run_core_linking populates per-round fits and per-batch final_fits (bootstrap path)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )
  batches <- list(c("D"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  mock_fit <- function(bt_data, ...) {
    round <<- round + 1
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    se <- rep(max(0.60 - 0.15 * round, 0.05), length(ids))
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
    max_rounds_per_batch = 2,
    forbid_repeats = FALSE, # avoid exhausting pairs in tiny toy example
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.80,
    verbose = FALSE
  )

  # New return elements exist and are populated
  expect_true("fits" %in% names(out))
  expect_true("final_fits" %in% names(out))
  expect_true(is.list(out$fits))
  expect_true(is.list(out$final_fits))
  expect_true(length(out$fits) >= 2L) # bootstrap + at least one batch round
  expect_true("bootstrap" %in% names(out$final_fits))
  expect_true("batch_1" %in% names(out$final_fits))

  # Metadata attribute exists on fits
  meta1 <- attr(out$fits[[1]], "bt_run_core_linking")
  expect_true(is.list(meta1))
  expect_true(all(c("batch_index", "round_index", "stage", "n_results", "n_pairs_this_round", "new_ids") %in% names(meta1)))
  expect_equal(meta1$batch_index, 0L)

  # Ensure we have at least one tagged batch_round fit
  stages <- vapply(out$fits, function(f) {
    m <- attr(f, "bt_run_core_linking")
    if (is.null(m)) NA_character_ else as.character(m$stage)
  }, character(1))
  expect_true(any(stages %in% c("bootstrap", "warm_start")))
  expect_true(any(stages == "batch_round"))

  # final_fits are fits and carry metadata
  meta_boot <- attr(out$final_fits[["bootstrap"]], "bt_run_core_linking")
  expect_true(is.list(meta_boot))
  expect_equal(meta_boot$batch_index, 0L)

  meta_b1 <- attr(out$final_fits[["batch_1"]], "bt_run_core_linking")
  expect_true(is.list(meta_b1))
  expect_equal(meta_b1$batch_index, 1L)
})

test_that("bt_run_core_linking populates warm_start fit metadata and sets final_fits for no_new_ids batch", {
  samples <- tibble::tibble(
    ID = LETTERS[1:4],
    text = paste("text", LETTERS[1:4])
  )
  core_ids <- c("A", "B", "C")

  # Warm start: provide one existing judgment
  initial_results <- tibble::tibble(
    custom_id = "c1",
    ID1 = "A",
    ID2 = "B",
    better_id = "A"
  )

  # Batch requests only core IDs -> no_new_ids branch should trigger
  batches <- list(c("A", "B"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.5, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 6,
    max_rounds_per_batch = 2,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.80,
    verbose = FALSE
  )

  # Warm-start should create one fit and tag it
  expect_true(length(out$fits) >= 1L)
  meta <- attr(out$fits[[1]], "bt_run_core_linking")
  expect_true(is.list(meta))
  expect_equal(meta$stage, "warm_start")
  expect_equal(meta$batch_index, 0L)

  # no_new_ids batch should still set final_fits for that batch
  expect_true("bootstrap" %in% names(out$final_fits))
  expect_true("batch_1" %in% names(out$final_fits))
  expect_equal(out$batch_summary$stop_reason[[1]], "no_new_ids")

  meta_b1 <- attr(out$final_fits[["batch_1"]], "bt_run_core_linking")
  expect_true(is.list(meta_b1))
  # remains the most recent fit at that point (warm_start fit), but batch index in meta stays 0
  expect_equal(meta_b1$batch_index, 0L)
})

test_that("bt_run_core_linking validates samples$ID and core_ids strictly", {
  ok_samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  batches <- list(c("C"))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  # Missing / empty IDs
  bad1 <- ok_samples
  bad1$ID[1] <- NA_character_
  expect_error(
    bt_run_core_linking(bad1, batches = batches, core_ids = c("B", "C"), judge_fun = judge_fun),
    "samples\\$ID.*non-missing"
  )

  bad2 <- ok_samples
  bad2$ID[1] <- ""
  expect_error(
    bt_run_core_linking(bad2, batches = batches, core_ids = c("B", "C"), judge_fun = judge_fun),
    "samples\\$ID.*non-missing"
  )

  # Duplicate IDs
  bad3 <- ok_samples
  bad3$ID[3] <- "B"
  expect_error(
    bt_run_core_linking(bad3, batches = batches, core_ids = c("B", "C"), judge_fun = judge_fun),
    "samples\\$ID.*unique"
  )

  # core_ids validation: duplicates / not present / too short
  expect_error(
    bt_run_core_linking(ok_samples, batches = batches, core_ids = c("A", "B", "B"), judge_fun = judge_fun),
    "core_ids.*unique"
  )
  expect_error(
    bt_run_core_linking(ok_samples, batches = batches, core_ids = c("A", "Z"), judge_fun = judge_fun),
    "present in `samples\\$ID`"
  )
  expect_error(
    bt_run_core_linking(ok_samples, batches = batches, core_ids = c("A"), judge_fun = judge_fun),
    "must include at least 2 IDs"
  )
})

test_that(".normalize_batches_list errors on empty/invalid batches", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  core_ids <- c("A", "B")
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  # Empty batch element
  expect_error(
    bt_run_core_linking(samples, batches = list(character(0)), core_ids = core_ids, judge_fun = judge_fun),
    "Each batch must contain at least one ID"
  )

  # NA / empty string in batch
  expect_error(
    bt_run_core_linking(samples, batches = list(c("C", NA_character_)), core_ids = core_ids, judge_fun = judge_fun),
    "non-missing and non-empty"
  )
  expect_error(
    bt_run_core_linking(samples, batches = list(c("C", "")), core_ids = core_ids, judge_fun = judge_fun),
    "non-missing and non-empty"
  )

  # Unknown ID
  expect_error(
    bt_run_core_linking(samples, batches = list(c("Z")), core_ids = core_ids, judge_fun = judge_fun),
    "present in `samples\\$ID`"
  )
})

test_that("bt_run_core_linking can select core_ids when core_ids is NULL (core_method random)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:10],
    text = paste("text", LETTERS[1:10])
  )
  batches <- list(c("I", "J"))  # include some new IDs (they're in samples)

  # deterministic simulated judge + mock fit
  true_theta <- setNames(seq(10, 1), LETTERS[1:10])
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.5, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = NULL,                # cover core selection branch
    core_method = "random",
    core_size = 3,
    seed = 123,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 12,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 999,
    verbose = FALSE
  )

  expect_true(is.character(out$core_ids))
  expect_equal(length(out$core_ids), 3L)
  expect_true(all(out$core_ids %in% samples$ID))
  expect_false(anyDuplicated(out$core_ids) > 0L)
})

test_that("bt_run_core_linking covers drift_reference = baseline branch", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  batches <- list(c("D"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 3, B = 2, C = 1, D = 0, E = -1, F = -2)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.25, length(ids))),
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
    round_size = 10,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.0001,
    drift_reference = "baseline", # cover baseline branch
    verbose = FALSE
  )

  # We don't assert drift values; we just ensure the run completes with baseline reference.
  expect_true(nrow(out$batch_summary) == 1L)
})

test_that("bt_run_core_linking emits progress messages when verbose = TRUE", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  batches <- list(c("D"))
  core_ids <- c("A", "B", "C")
  true_theta <- c(A = 3, B = 2, C = 1, D = 0, E = -1, F = -2)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.25, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  # Cover the "Batch ..." message line
  expect_message(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = core_ids,
      judge_fun = judge_fun,
      fit_fun = mock_fit,
      engine = "mock",
      round_size = 10,
      max_rounds_per_batch = 1,
      forbid_repeats = FALSE,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_,
      rel_se_p90_target = 999,
      verbose = TRUE
    ),
    "Batch 1:"
  )

  # Cover the "Round ..." message line
  expect_message(
    bt_run_core_linking(
      samples = samples,
      batches = batches,
      core_ids = core_ids,
      judge_fun = judge_fun,
      fit_fun = mock_fit,
      engine = "mock",
      round_size = 10,
      max_rounds_per_batch = 1,
      forbid_repeats = FALSE,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_,
      rel_se_p90_target = 999,
      verbose = TRUE
    ),
    "Round 1:"
  )
})
