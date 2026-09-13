# Small completed Bayesian contracts: deterministic draws, no sampler or provider.
rubric_percentile_fixed <- function(theta, ids = paste0("item", seq_along(theta))) {
  draws <- outer(c(-0.125, 0.125), theta, `+`)
  colnames(draws) <- ids
  fit <- pairwiseLLM:::build_btl_fit_contract(draws, model_variant = "btl",
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000), diagnostics_pass = TRUE)
  list(fit = fit, fits = list(fit),
    item_log_list = list(tibble::tibble(refit_id = 1L, ID = ids,
      theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
    round_log = tibble::tibble(round_id = 1L, model_variant = "btl", reliability_EAP = 0.95))
}

test_that("equal-frequency percentile fits use exact type-8 cutpoints for generic K", {
  for (K in 3:7) {
    cj <- rubric_percentile_fixed(seq_len(12L * K))
    fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait", K = K)
    pred <- stats::predict(fit)
    expect_s3_class(fit, "pairwiseLLM_rubric_calibration")
    expect_identical(fit$status, "fitted")
    expect_identical(fit$levels, seq_len(K))
    expect_identical(fit$backend$quantile_type, 8L)
    expect_identical(fit$backend$cutpoints, stats::quantile(fit$cj$items$theta,
      fit$backend$cumulative_probs, type = 8, names = FALSE))
    expect_identical(pred$category, rep(seq_len(K), each = 12L))
    expect_identical(pred$rubric_score, pred$category)
    expect_equal(unname(fit$backend$requested_proportions), rep(1 / K, K))
    expect_equal(fit$backend$achieved_proportions, fit$backend$requested_proportions)
    expect_identical(unname(fit$backend$achieved_counts), rep(12L, K))
    expect_true(all(diff(pred$category) >= 0))
    expect_false(any(pred$extrapolated))
    expect_identical(fit$calibration_range, range(fit$cj$items$theta))
    expect_identical(fit$transformation, list(center = 0, scale = 1))
    expect_identical(fit, pairwiseLLM::fit_rubric_calibration(cj,
      method = "percentile", trait = "trait", K = K))
  }
})

test_that("unequal proportions preserve explicit display labels and requested distributions", {
  cj <- rubric_percentile_fixed(1:10)
  labels <- c("Starting", "On track", "Accomplished")
  proportions <- stats::setNames(c(0.1, 0.3, 0.6), labels)
  fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "organization",
    levels = labels, target_distribution = proportions)
  pred <- stats::predict(fit)
  expect_identical(fit$K, 3L)
  expect_identical(fit$target_distribution, proportions)
  expect_identical(fit$backend$requested_proportions, proportions)
  expect_equal(fit$backend$achieved_proportions, proportions)
  expect_identical(pred$rubric_score, rep(labels, c(1L, 3L, 6L)))
  expect_identical(pred$category, rep(1:3, c(1L, 3L, 6L)))
  expect_identical(fit$backend$cumulative_probs, c(0.1, 0.4))
  expect_identical(pred$item_id, cj$item_log_list[[1L]]$ID)
  expect_identical(pred$theta, fit$cj$items$theta)
  expect_identical(fit$trait, "organization")
  numeric_fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait",
    levels = c(10, 20, 50))
  expect_identical(stats::predict(numeric_fit)$rubric_score, c(10, 20, 50)[stats::predict(numeric_fit)$category])
  inferred <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait",
    target_distribution = c(0.1, 0.3, 0.6))
  expect_identical(inferred$levels, 1:3)
})

test_that("boundary equality and repeated cutpoints preserve all exact-score ties", {
  fit <- pairwiseLLM::fit_rubric_calibration(rubric_percentile_fixed(c(-1, 0, 0, 0, 0, 1)),
    method = "percentile", trait = "trait", K = 3)
  expect_identical(fit$backend$cutpoints, c(0, 0))
  expect_identical(fit$backend$cutpoint_tie_counts, c(4L, 4L))
  expect_identical(stats::predict(fit)$category, c(1L, 3L, 3L, 3L, 3L, 3L))
  expect_identical(unname(fit$backend$achieved_counts), c(1L, 0L, 5L))
  expect_equal(unname(fit$backend$achieved_proportions), c(1, 0, 5) / 6)
  expect_false(isTRUE(all.equal(fit$backend$requested_proportions, fit$backend$achieved_proportions)))
  for (theta in list(rep(0, 4), 0)) {
    all_equal <- pairwiseLLM::fit_rubric_calibration(rubric_percentile_fixed(theta),
      method = "percentile", trait = "trait", K = 4)
    expect_identical(stats::predict(all_equal)$category, rep(4L, length(theta)))
    expect_identical(all_equal$backend$cutpoint_tie_counts, rep(length(theta), 3))
    expect_identical(unname(all_equal$backend$achieved_counts), c(0L, 0L, 0L, length(theta)))
  }
  unique_boundary <- pairwiseLLM::fit_rubric_calibration(rubric_percentile_fixed(-2:2),
    method = "percentile", trait = "trait", target_distribution = c(0.5, 0.25, 0.25))
  expect_identical(unique_boundary$backend$cutpoints[[1L]], 0)
  expect_identical(unique_boundary$backend$cutpoint_tie_counts, c(1L, 0L))
  expect_identical(stats::predict(unique_boundary)$category[[3L]], 2L)
})

test_that("accepted numerical tolerance is normalized before cumulative quantiles", {
  cj <- rubric_percentile_fixed(1:4)
  for (proportions in list(c(0.5, 0.500000005, 1e-12), c(0.5, 0.499999995, 1e-12))) {
    fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait",
      target_distribution = proportions)
    expect_identical(unname(fit$backend$requested_proportions), proportions)
    expect_equal(sum(fit$backend$effective_proportions), 1)
    expect_true(all(fit$backend$cumulative_probs >= 0 & fit$backend$cumulative_probs <= 1))
    expect_identical(fit$backend$cutpoints, stats::quantile(fit$cj$items$theta,
      probs = cumsum(proportions / sum(proportions))[1:2], type = 8, names = FALSE))
  }
  expect_error(pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait",
    target_distribution = c(0.5, 0.50000002, 1e-12)), "sum to one")
})

test_that("percentile validation rejects malformed public inputs and fitted cutpoints", {
  cj <- rubric_percentile_fixed(1:4)
  fit <- function(...) pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait", ...)
  for (K in list(2, 3.1, NA, Inf, -1, c(3, 4), "3")) {
    expect_error(fit(K = K), "K.*integer")
  }
  for (p in list(c(0, 0.5, 0.5), c(-1, 1, 1), c(NA, 0.5, 0.5), c(Inf, 1, 1),
    c(0.1, 0.2, 0.3), matrix(rep(1 / 3, 3)), c("a", "b", "c"))) {
    expect_error(fit(K = 3, target_distribution = p), "proportion")
  }
  for (labels in list(c("a", "a", "b"), c("a", "", "c"), c(1, 2, NA), c(1, 2, Inf), 1:2)) {
    expect_error(fit(levels = labels), "distinct")
  }
  expect_error(fit(), "Supply")
  expect_error(fit(K = 3, levels = 1:4), "K.*match")
  expect_error(fit(K = 4, target_distribution = rep(1 / 3, 3)), "proportion")
  expect_error(fit(levels = c("a", "b", "c"), target_distribution = c(c = 0.2, b = 0.5, a = 0.3)),
    "level order")
  expect_error(fit(K = 3, rubric = data.frame()), "omit")
  expect_error(fit(K = 3, calibration_design = "linked_anchors"), "does not support")
  expect_error(fit(K = 3, quantile_type = 7), "must be empty")
  good <- fit(K = 3)
  for (cutpoints in list(NULL, c(2, 1), NA_real_, c(1, Inf), c("a", "b"), matrix(c(1, 2)))) {
    bad <- good
    bad$backend$cutpoints <- cutpoints
    expect_error(stats::predict(bad), "Invalid fitted percentile")
  }
  bad <- good
  bad$backend <- 1
  expect_error(stats::predict(bad), "Invalid fitted percentile")
  bad <- good
  bad$backend$quantile_type <- 7L
  expect_error(stats::predict(bad), "Invalid fitted percentile")
  bad <- good
  bad$transformation$center <- 1
  expect_error(stats::predict(bad), "Invalid fitted percentile")
})

test_that("prediction reuses the same completed result and permits item reordering", {
  cj <- rubric_percentile_fixed(c(-2, -1, 0, 1, 2))
  fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait", K = 3)
  pred <- stats::predict(fit)
  expect_identical(stats::predict(fit, cj), pred)
  reordered <- rubric_percentile_fixed(c(2, 0, -2, 1, -1), ids = paste0("item", c(5, 3, 1, 4, 2)))
  expect_identical(stats::predict(fit, reordered), pred[c(5, 3, 1, 4, 2), ])
  cj$provenance <- list(collection_mode = "batch")
  expect_identical(stats::predict(fit, cj), pred)
  bad <- rubric_percentile_fixed(c(-2, -1, 1e-10, 1, 2))
  expect_error(stats::predict(fit, bad), "unchanged")
  expect_error(stats::predict(fit, rubric_percentile_fixed(1:5)), "unchanged")
  expect_error(stats::predict(fit, rubric_percentile_fixed(-2:2, ids = letters[1:5])), "unchanged")
  bad <- cj
  bad$fits[[1L]]$mcmc_config_used$seed <- 999L
  expect_error(stats::predict(fit, bad), "fit evidence")
  bad <- cj
  bad$trait <- "another"
  expect_error(stats::predict(fit, bad), "Trait mismatch")
  bad <- cj
  bad$orientation <- "lower_is_better"
  expect_error(stats::predict(fit, bad), "orientation")
  expect_error(stats::predict(fit, data.frame(theta = -2:2)), "completed CJ")
  expect_error(stats::predict(fit, fit$cj), "Unsupported")
  expect_error(stats::predict(fit, hard_score = "mean"), "hard_score")
  expect_error(stats::predict(fit, unused = TRUE), "must be empty")
})

test_that("percentile scoring preserves provenance and diagnostics across current source forms", {
  withr::local_seed(202)
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    fixed <- rubric_test_fixed(variant)
    state <- rubric_test_adaptive(variant)
    a <- pairwiseLLM::fit_rubric_calibration(fixed, method = "percentile", trait = "trait", K = 3)
    b <- pairwiseLLM::fit_rubric_calibration(state, method = "percentile", trait = "trait", K = 3)
    expect_identical(stats::predict(a), stats::predict(b))
    expect_identical(stats::predict(b, list(state = state)), stats::predict(b))
    expect_identical(b$cj$model_variant, variant)
    expect_identical(b$cj$estimation_mode, "adaptive")
    expect_identical(a$cj$estimation_mode, "fixed")
    expect_identical(a$cj$reliability, 0.95)
    expect_error(stats::predict(a, state), "unchanged")
  }
  fixed <- rubric_test_fixed()
  fixed$fits[[1L]]$diagnostics_pass <- FALSE
  expect_warning(fit <- pairwiseLLM::fit_rubric_calibration(fixed, method = "percentile",
    trait = "trait", K = 3), class = "pairwiseLLM_rubric_cj_diagnostics")
  expect_false(fit$diagnostics$cj$diagnostics_pass)
  state <- rubric_test_adaptive()
  state$round_log$diagnostics_pass <- TRUE
  state$step_log <- tibble::tibble(pair_id = 1:3, step_id = 1:3, A = c(1L, 2L, 3L), B = c(2L, 3L, 4L),
    Y = 1L, set_i = 1L, set_j = 1L)
  artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, 1L)
  fit <- pairwiseLLM::fit_rubric_calibration(artifact, method = "percentile", trait = "trait", K = 3)
  expect_identical(fit$cj$scale_status, "phase_a_reference")
  expect_identical(fit$reference, fit$cj$reference)
  expect_identical(stats::predict(fit, artifact), stats::predict(fit))
  expect_error(stats::predict(fit, rubric_test_fixed()), "inappropriate")
  linked <- rubric_test_linked(2L)
  expect_error(stats::predict(fit, linked), "inappropriate")
  expect_error(pairwiseLLM::fit_rubric_calibration(linked, method = "percentile", trait = "trait", K = 3),
    "inappropriate")
})

test_that("point-score binning does not invent probabilities or consume randomness", {
  withr::local_seed(203)
  cj <- rubric_test_fixed()
  before <- .Random.seed
  fit <- pairwiseLLM::fit_rubric_calibration(cj, method = "percentile", trait = "trait", K = 3)
  pred <- stats::predict(fit, cj)
  expect_identical(.Random.seed, before)
  expect_identical(names(pred), c("item_id", "theta", "category", "rubric_score", "extrapolated"))
  expect_identical(stats::predict(fit, hard_score = "mode"), pred)
  expect_false(fit$diagnostics$category_probabilities_available)
  expect_null(fit$calibration_data)
  expect_null(fit$category_counts)
  expect_true(is.matrix(fit$cj$posterior_draws))
  expect_false(any(c("probabilities", "expected_level") %in% names(fit$backend)))
})
