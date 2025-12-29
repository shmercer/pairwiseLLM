test_that("judge_summary validates inputs", {
  res <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A", judge = "m1")

  expect_error(judge_summary(res, judge_col = NA_character_), "judge_col")
  expect_error(judge_summary(res, judge_col = ""), "judge_col")

  bad <- dplyr::select(res, -better_id)
  expect_error(judge_summary(bad, judge_col = "judge"), "must contain columns")

  expect_error(judge_summary(res, judge_col = "missing"), "must contain columns")
})


test_that("judge_summary returns per-judge counts and reverse consistency when available", {
  res <- tibble::tibble(
    ID1 = c("A", "B", "A", "A", "A", "A"),
    ID2 = c("B", "A", "C", "C", "B", "B"),
    better_id = c("A", "A", "X", NA, "B", "B"),
    judge = c("m1", "m1", "m1", "m1", "m2", "m2")
  )

  out <- judge_summary(res, judge_col = "judge", compute_reverse = TRUE)
  expect_true(is.list(out))
  expect_true(all(c("by_judge", "overall") %in% names(out)))

  bj <- out$by_judge
  expect_s3_class(bj, "tbl_df")
  expect_equal(sort(unique(bj$judge)), c("m1", "m2"))

  m1 <- dplyr::filter(bj, judge == "m1")
  expect_equal(m1$n_total, 4L)
  expect_equal(m1$n_missing_winner, 1L)
  expect_equal(m1$n_invalid_winner, 1L)
  expect_equal(m1$n_valid, 2L)
  expect_equal(m1$prop_valid, 0.5)
  expect_equal(m1$pos1_win_rate, 0.5)

  # reverse consistency should be computable for A-B since both A<=B and B>A exist
  expect_equal(m1$rc_n_pairs, 1L)
  expect_equal(m1$rc_prop_consistent, 1)

  m2 <- dplyr::filter(bj, judge == "m2")
  expect_equal(m2$n_total, 2L)
  expect_equal(m2$n_valid, 2L)
  expect_equal(m2$pos1_win_rate, 0)
  expect_true(is.na(m2$rc_n_pairs) || m2$rc_n_pairs == 0L)

  # compute_reverse = FALSE should yield NA reverse-consistency fields
  out2 <- judge_summary(res, judge_col = "judge", compute_reverse = FALSE)
  expect_true(all(is.na(out2$by_judge$rc_n_pairs)))
  expect_true(all(is.na(out2$by_judge$rc_prop_consistent)))
})


test_that("judge_fit_summary returns stable outputs when judge-fit is missing", {
  fit <- list(theta = tibble::tibble(ID = "A", theta = 0, se = 0.1))
  out <- judge_fit_summary(fit)

  expect_true(is.list(out))
  expect_true(all(c("summary", "details") %in% names(out)))

  s <- out$summary
  expect_s3_class(s, "tbl_df")
  expect_false(s$has_judge_fit[[1]])
  expect_equal(s$n_judges[[1]], 0L)
  expect_equal(nrow(out$details), 0L)
})


test_that("judge_fit_summary summarizes misfit and worst judges", {
  fit <- list(
    diagnostics = list(
      judge_fit = tibble::tibble(
        judge = c("mA", "mB"),
        infit = c(1.0, 1.4),
        outfit = c(1.0, 1.2)
      )
    )
  )

  out <- judge_fit_summary(fit, fit_bounds = c(0.7, 1.3), top_n = 1)
  s <- out$summary
  d <- out$details

  expect_true(s$has_judge_fit[[1]])
  expect_equal(s$n_judges[[1]], 2L)
  expect_equal(s$n_misfit[[1]], 1L)
  expect_equal(s$misfit_prop[[1]], 0.5)
  expect_equal(s$worst_judges[[1]], "mB")

  expect_true(all(c("judge", "infit", "outfit", "is_misfit", "deviation") %in% names(d)))
  expect_equal(sum(d$is_misfit), 1L)

  # accept a data.frame directly
  out2 <- judge_fit_summary(fit$diagnostics$judge_fit)
  expect_true(out2$summary$has_judge_fit[[1]])

  # top_n = Inf should be accepted (no integer-coercion warning)
  out3 <- judge_fit_summary(fit, top_n = Inf)
  expect_true(out3$summary$has_judge_fit[[1]])
  expect_length(out3$summary$worst_judges[[1]], 2L)
})


test_that("judge_fit_summary validates inputs and table schema", {
  expect_error(judge_fit_summary(list(), fit_bounds = 1), "fit_bounds")
  expect_error(judge_fit_summary(list(), top_n = -1), "top_n")

  bad_tbl <- tibble::tibble(judge = "mA", infit = 1)
  expect_error(judge_fit_summary(bad_tbl), "must contain columns")
})


test_that("judge_misfit_judges returns misfit judge IDs and handles missing diagnostics", {
  fit <- list(
    diagnostics = list(
      judge_fit = tibble::tibble(
        judge = c("mA", "mB", "mC"),
        infit = c(1.0, 1.4, 0.6),
        outfit = c(1.0, 1.2, 1.0)
      )
    )
  )

  print(pairwiseLLM::judge_fit_summary(fit, fit_bounds = c(0.7, 1.3), top_n = Inf)$details)

  ids <- pairwiseLLM::judge_misfit_judges(fit, fit_bounds = c(0.7, 1.3))
  expect_equal(ids, c("mB", "mC"))

  # max_n limits results
  expect_equal(pairwiseLLM::judge_misfit_judges(fit, max_n = 1L), "mB")

  # missing diagnostics => empty
  expect_equal(pairwiseLLM::judge_misfit_judges(list()), character())
})
