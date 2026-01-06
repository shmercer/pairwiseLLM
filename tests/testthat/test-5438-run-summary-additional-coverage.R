testthat::test_that("5438-01 summary.pairwiseLLM_run dispatches to run_summary()", {
  run <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  )
  class(run) <- "pairwiseLLM_run"

  s <- summary(run)
  testthat::expect_s3_class(s, "pairwiseLLM_run_summary")
})

testthat::test_that("5438-02 .as_pairwise_run default run_type is NA", {
  as_run <- getFromNamespace(".as_pairwise_run", "pairwiseLLM")
  x <- list()
  y <- as_run(x)
  testthat::expect_true(inherits(y, "pairwiseLLM_run"))
  testthat::expect_true(is.na(attr(y, "run_type")))
})

testthat::test_that("5438-03 run_summary derives rounds from state when metrics missing", {
  run <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    state = tibble::tibble(round = c(1, 2))
  )
  class(run) <- "pairwiseLLM_run"

  s <- run_summary(run)
  testthat::expect_equal(s$rounds$n_rounds, 2)
})

testthat::test_that("5438-04 run_summary uses final_fits fit_judges path and print shows judge count", {
  fit_tbl <- tibble::tibble(
    judge = c("j1", "j2"),
    infit = c(1.0, 0.8),
    outfit = c(1.1, 0.9)
  )

  run <- list(
    results = tibble::tibble(
      ID1 = "A",
      ID2 = "B",
      better_id = "A",
      judge = c("j1", "j2")
    ),
    state = tibble::tibble(round = 1L),
    final_fits = list(
      list(fit = list(fit_judges = fit_tbl)),
      # last fit should be used
      list(fit = list(fit_judges = fit_tbl))
    ),
    stop_reason = "max_rounds",
    stop_round = 1L
  )
  class(run) <- "pairwiseLLM_run"

  s <- run_summary(run)
  testthat::expect_true(isTRUE(s$judge$has_judges))
  testthat::expect_false(is.null(s$judge$fit))

  out <- testthat::capture_output(print(s))
  testthat::expect_true(any(grepl("judges:", out, fixed = TRUE)))
})
