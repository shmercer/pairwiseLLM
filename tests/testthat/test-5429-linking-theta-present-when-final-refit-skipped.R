
# PR8.2.4 — Meaningful-fit ⇒ theta exists (even when final_refit = FALSE)

test_that("linking runners return theta when final_refit is FALSE and a running fit exists", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  core_ids <- c("A", "B", "C")
  batches <- list(c("D", "E", "F"))

  true_theta <- setNames(seq(6, 1), LETTERS[1:6])
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.5, length(ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out_core <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    final_refit = FALSE,
    max_rounds_per_batch = 1,
    round_size = 50,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.30,
    verbose = FALSE
  )

  expect_false(is.null(out_core$theta))
  expect_true(all(c("ID", "theta", "se", "rank") %in% names(out_core$theta)))
  expect_equal(nrow(out_core$theta), nrow(samples))
  expect_equal(out_core$theta$ID, samples$ID)
  expect_true(is.character(out_core$theta_engine))
  expect_equal(length(out_core$theta_engine), 1L)
  expect_true(nzchar(out_core$theta_engine))

  out_adapt <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = core_ids,
    fit_fun = mock_fit,
    engine = "mock",
    final_refit = FALSE,
    max_rounds_per_batch = 1,
    min_rounds = 1,
    round_size = 50,
    init_round_size = 50,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.30,
    verbose = FALSE
  )

  expect_false(is.null(out_adapt$theta))
  expect_true(all(c("ID", "theta", "se", "rank") %in% names(out_adapt$theta)))
  expect_equal(nrow(out_adapt$theta), nrow(samples))
  expect_equal(out_adapt$theta$ID, samples$ID)
  expect_true(is.character(out_adapt$theta_engine))
  expect_equal(length(out_adapt$theta_engine), 1L)
  expect_true(nzchar(out_adapt$theta_engine))
})
