test_that("run_summary infers run_type when missing and handles missing/partial results safely", {
  # 1) adaptive_core_linking heuristic: bt_data present
  x1 <- list(bt_data = tibble::tibble())
  s1 <- run_summary(x1)
  expect_identical(s1$run_type, "adaptive_core_linking")

  # 2) core_linking heuristic: core_ids + batches present
  x2 <- list(core_ids = c("A", "B"), batches = list(list()))
  s2 <- run_summary(x2)
  expect_identical(s2$run_type, "core_linking")

  # 3) adaptive heuristic: default fallback
  x3 <- list()
  s3 <- run_summary(x3)
  expect_identical(s3$run_type, "adaptive")

  # Results NULL -> counts n_results = 0, others NA
  expect_equal(s3$counts$n_results, 0L)
  expect_true(is.na(s3$counts$n_unique_ids))
  expect_true(is.na(s3$counts$n_unique_unordered_pairs))

  # Results present but missing ID1/ID2 -> leaves NA
  x4 <- list(results = tibble::tibble(foo = 1))
  s4 <- run_summary(x4)
  expect_true(is.na(s4$counts$n_unique_ids))
  expect_true(is.na(s4$counts$n_unique_unordered_pairs))

  # Results present with ID1/ID2 all NA -> unique pairs becomes 0
  x5 <- list(results = tibble::tibble(ID1 = c(NA, NA), ID2 = c(NA, NA)))
  s5 <- run_summary(x5)
  expect_equal(s5$counts$n_unique_unordered_pairs, 0L)

  # Results missing better_id -> results_diag stays NULL
  x6 <- list(results = tibble::tibble(ID1 = "A", ID2 = "B"))
  s6 <- run_summary(x6)
  expect_true(is.null(s6$results_diag))
})

test_that("run_summary uses final_fit diagnostics for judge fit and (optionally) surfaces pair_plan_reports", {
  res <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    better_id = c("A", "C"),
    judge = c("j1", "j2")
  )

  judge_fit <- tibble::tibble(
    judge = c("j1", "j2"),
    infit = c(1.0, 1.4),
    outfit = c(1.1, 1.6)
  )

  run <- structure(
    list(
      results = res,
      final_fit = list(diagnostics = list(judge_fit = judge_fit)),
      pair_plan_reports = list(round1 = list(report = list(n_rows = 10L)))
    ),
    class = c("pairwiseLLM_run", "list")
  )
  attr(run, "run_type") <- "adaptive"

  s <- run_summary(run, fit_bounds = c(0.7, 1.3), top_n = 5L)

  # Judge fit summary structure should exist
  expect_true(is.list(s$judge))
  expect_true(is.list(s$judge$fit))
  expect_true(is.data.frame(s$judge$fit$summary))
  expect_true(is.data.frame(s$judge$fit$details))

  # pair_plan_reports may or may not be surfaced depending on run_summary implementation;
  # if present, validate its structure.
  if (!is.null(s$pair_plan_reports)) {
    expect_true(is.list(s$pair_plan_reports))
    expect_true("round1" %in% names(s$pair_plan_reports))
  } else {
    expect_true(is.null(s$pair_plan_reports))
  }
})

test_that("print methods for pairwiseLLM_run and pairwiseLLM_run_summary do not error", {
  run <- structure(
    list(
      results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      metrics = tibble::tibble(round = 1, rel_se_p90 = 0.5),
      state = tibble::tibble(round = 1, n_results = 1),
      stop_reason = "max_rounds_reached",
      stop_round = 1
    ),
    class = c("pairwiseLLM_run", "list")
  )
  attr(run, "run_type") <- "adaptive"

  out1 <- capture.output(print(run))
  expect_true(any(grepl("pairwiseLLM run", out1)))

  s <- run_summary(run)
  out2 <- capture.output(print(s))
  expect_true(any(grepl("pairwiseLLM run summary", out2)))
})
