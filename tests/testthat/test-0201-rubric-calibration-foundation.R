test_that("rubric labels align by IDs and retain ordered user categories", {
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  rubric <- tibble::tibble(item_id = c("d", "a", "c", "b"),
    rubric_score = ordered(c("high", "low", "medium", NA), levels = c("low", "medium", "high")))
  object <- prepare(rubric_test_fixed(), rubric, trait = "organization")
  expect_s3_class(object, "pairwiseLLM_rubric_calibration")
  expect_identical(object$method, "ordinal_linear")
  expect_identical(object$calibration_design, "same_set")
  expect_identical(object$levels, c("low", "medium", "high"))
  expect_identical(object$K, 3L)
  expect_identical(object$calibration_data$item_id, letters[1:4])
  expect_identical(object$calibration_data$category, c(1L, NA_integer_, 2L, 3L))
  expect_identical(unname(object$category_counts), rep(1L, 3))
  expect_equal(object$calibration_range, c(-1.5, 1.5))
  expect_identical(object$status, "unfitted")
  expect_null(object$backend)
  expect_null(object$transformation)
  expect_identical(object$cj$trait, "organization")
  expect_identical(prepare(rubric_test_fixed(), rubric[4:1, ], trait = "organization"), object)
  expect_error(stats::predict(object), "unfitted")
  expect_error(stats::predict(object, hard_score = "mean"), "hard_score")
})

test_that("numeric, explicit character, and nonconsecutive labels preserve their mapping", {
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  rubric <- tibble::tibble(item_id = c("c", "a", "d"), rubric_score = c(20, 10, 50))
  object <- prepare(rubric_test_fixed(), rubric, trait = "trait")
  expect_identical(object$levels, c(10, 20, 50))
  expect_equal(object$calibration_data$category, c(1, NA, 2, 3))
  rubric$rubric_score <- c("mid", "low", "high")
  expect_error(prepare(rubric_test_fixed(), rubric, trait = "trait"), "Supply ordered")
  expect_identical(prepare(rubric_test_fixed(), rubric, trait = "trait",
    levels = c("low", "mid", "high"))$levels, c("low", "mid", "high"))
  rubric$rubric_score <- factor(rubric$rubric_score)
  expect_error(prepare(rubric_test_fixed(), rubric, trait = "trait"), "unordered-factor")
  expect_identical(prepare(rubric_test_fixed(), rubric, trait = "trait",
    levels = c("low", "mid", "high"))$levels, c("low", "mid", "high"))
})

test_that("malformed labels, missing categories, and trait conflicts fail clearly", {
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  fixed <- rubric_test_fixed()
  rubric <- tibble::tibble(item_id = letters[1:3], rubric_score = 1:3)
  expect_error(prepare(fixed, NULL, trait = "trait"), "data frame")
  bad <- rubric
  bad$item_id[[2L]] <- "a"
  expect_error(prepare(fixed, bad, trait = "trait"), "unique")
  bad$item_id[[2L]] <- "unknown"
  expect_error(prepare(fixed, bad, trait = "trait"), "present")
  bad <- rubric
  bad$trait <- c("trait", "trait", "another")
  expect_error(prepare(fixed, bad, trait = "trait"), "Trait mismatch")
  expect_error(prepare(fixed, rubric, trait = "trait", K = 4), "K.*match")
  expect_error(prepare(fixed, rubric, trait = "trait", levels = 1:4), "Missing requested")
  expect_error(prepare(fixed, rubric, trait = "trait", levels = 2:4), "outside")
  bad <- rubric
  bad$rubric_score <- ordered(c("low", "high", "high"), levels = c("low", "mid", "high"))
  expect_error(prepare(fixed, bad, trait = "trait"), "Missing requested.*mid")
  expect_error(prepare(fixed, bad, trait = "trait", levels = c("high", "mid", "low")), "must agree")
  bad$rubric_score <- c(1, 2, Inf)
  expect_error(prepare(fixed, bad, trait = "trait"), "finite")
  bad$rubric_score <- list(1, 2, 3)
  expect_error(prepare(fixed, bad, trait = "trait"), "must contain")
  for (K in list(2, 3.5, NA, Inf, c(3, 4), "3")) {
    expect_error(prepare(fixed, rubric, trait = "trait", K = K), "K.*integer")
  }
  for (levels in list(c(1, 1, 2), c("a", "", "c"), c(1, 2, NA), c(1, 2, Inf))) {
    expect_error(prepare(fixed, rubric, trait = "trait", levels = levels), "distinct")
  }
})

test_that("percentile foundation validates category specifications without fitting cutpoints", {
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  fixed <- rubric_test_fixed()
  object <- prepare(fixed, method = "percentile", trait = "trait", K = 4)
  expect_identical(object$levels, 1:4)
  expect_null(object$calibration_data)
  expect_null(object$category_counts)
  expect_null(object$backend)
  proportions <- c(0.2, 0.3, 0.5)
  object <- prepare(fixed, method = "percentile", trait = "trait", target_distribution = proportions)
  expect_identical(object$K, 3L)
  expect_identical(object$target_distribution, proportions)
  expect_error(prepare(fixed, method = "percentile", trait = "trait"), "Supply")
  expect_error(prepare(fixed, data.frame(), method = "percentile", trait = "trait", K = 3), "omit")
  for (p in list(c(0.1, 0.2, 0.3), c(0, 0.5, 0.5), c(-1, 1, 1), c(NA, 0.5, 0.5))) {
    expect_error(prepare(fixed, method = "percentile", trait = "trait", target_distribution = p), "proportion")
  }
  expect_error(prepare(fixed, method = "percentile", trait = "trait", K = 4,
    target_distribution = proportions), "proportion")
  expect_error(prepare(fixed, method = "percentile", trait = "trait", levels = c("low", "mid", "high"),
    target_distribution = c(high = 0.2, mid = 0.3, low = 0.5)), "level order")
})

test_that("public API dispatch reports unimplemented backends without fabricated fits", {
  fit <- pairwiseLLM::fit_rubric_calibration
  fixed <- rubric_test_fixed()
  rubric <- tibble::tibble(item_id = letters[1:3], rubric_score = 1:3)
  for (method in c("ordinal_linear", "ordinal_monotone")) {
    expect_error(fit(fixed, rubric, method = method, trait = "trait"),
      class = "pairwiseLLM_rubric_backend_unavailable")
  }
  expect_error(fit(fixed, method = "percentile", trait = "trait", K = 3),
    class = "pairwiseLLM_rubric_backend_unavailable")
  expect_error(fit(fixed, rubric, method = "auto", trait = "trait"), "method")
  expect_error(fit(fixed, rubric, method = "joint", trait = "trait"), "method")
  expect_error(fit(fixed, rubric, calibration_design = "joint", trait = "trait"), "calibration_design")
  expect_error(fit(fixed, method = "percentile", calibration_design = "linked_anchors", trait = "trait", K = 3),
    "does not support")
  expect_error(fit(fixed, rubric, trait = "trait", unused = TRUE), "must be empty")
  expect_error(fit(fixed, rubric, trait = "trait", target_distribution = rep(1 / 3, 3)), "only used")
  object <- pairwiseLLM:::.rubric_prepare_calibration(fixed, rubric, trait = "trait")
  expect_error(stats::predict(object, unused = TRUE), "must be empty")
  object$status <- "fitted"
  expect_error(stats::predict(object), "requires a statistical backend")
  expect_error(pairwiseLLM:::.rubric_validate_calibration(list()), "Invalid")
  expect_identical(names(formals(fit)), c("cj", "rubric", "method", "calibration_design", "trait",
    "levels", "K", "target_distribution", "..."))
  expect_false(".rubric_normalize_cj" %in% getNamespaceExports("pairwiseLLM"))
})

test_that("calibration fit designs require the correct source scale", {
  withr::local_seed(205)
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  rubric <- tibble::tibble(item_id = letters[1:3], rubric_score = 1:3)
  expect_error(prepare(rubric_test_fixed(), rubric, calibration_design = "linked_anchors", trait = "trait"),
    "inappropriate")
  # Give the reference three items to test a valid three-category API shell.
  fixed <- rubric_test_fixed()
  state <- rubric_test_adaptive()
  state$round_log$diagnostics_pass <- TRUE
  state$step_log <- tibble::tibble(pair_id = 1:3, step_id = 1:3, A = c(1L, 2L, 3L), B = c(2L, 3L, 4L),
    Y = 1L, set_i = 1L, set_j = 1L)
  artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, 1L)
  object <- prepare(artifact, rubric, calibration_design = "linked_anchors", trait = "trait")
  expect_identical(object$cj$scale_status, "phase_a_reference")
  expect_equal(object$reference$items$theta, unname(fixed$fit$theta_mean))
  expect_equal(object$calibration_range, c(-1.5, 0.5))
  expect_error(prepare(rubric_test_linked(2L), rubric, trait = "trait"), "inappropriate")
})
