test_that("run_summary summarizes run-like objects", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "C", "C"),
    judge = c("j1", "j1", "j2")
  )

  fit <- list(diagnostics = list(judge_fit = tibble::tibble(
    judge = c("j1", "j2"),
    infit = c(1.0, 1.4),
    outfit = c(1.1, 1.5)
  )))

  run <- structure(
    list(
      results = res,
      final_fit = fit,
      rounds = list(),
      state = list()
    ),
    class = c("pairwiseLLM_run", "list")
  )
  attr(run, "run_type") <- "adaptive"

  s <- run_summary(run)

  expect_s3_class(s, "pairwiseLLM_run_summary")
  expect_true(is.list(s$judge))
  expect_true(isTRUE(s$judge$has_judges))

  # judge_summary() returns a list with $by_judge and $overall
  expect_true(is.list(s$judge$per_judge))
  expect_true(is.data.frame(s$judge$per_judge$by_judge))
  expect_true(is.data.frame(s$judge$per_judge$overall))

  # judge_fit_summary() returns a list with $summary and $details
  expect_true(is.list(s$judge$fit))
  expect_true(is.data.frame(s$judge$fit$summary))
  expect_true(is.data.frame(s$judge$fit$details))
})

test_that("bt_run_adaptive returns an object with class pairwiseLLM_run", {
  skip_if_not_installed("sirt")

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )

  # Not expected to be called with round_size=0, but define anyway
  judge_fun <- function(pairs, samples, ...) {
    dplyr::mutate(pairs, better_id = ID1)
  }

  fit_fun <- function(bt_data, ...) fit_bt_model(bt_data)

  run <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    initial_results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    fit_fun = fit_fun,
    round_size = 0,
    init_round_size = 0,
    max_rounds = 1,
    stopping_tier = "good",
    seed = 1
  )

  expect_true(inherits(run, "pairwiseLLM_run"))
})
