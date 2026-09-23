for (method in c("percentile", "ordinal_linear", "ordinal_monotone")) {
  test_that(paste(method, "public workflow predictions ignore collection provenance"), {
    rubric_skip_method(method)
    withr::local_seed(92010L)
    data <- rubric_monotone_data(n_unique = 9L)
    live <- batch <- data$cj
    live$provenance <- list(collection_mode = "live", backend = "fixture_live")
    batch$provenance <- list(collection_mode = "batch", backend = "fixture_batch")
    state <- rubric_completed_adaptive(live)
    originals <- serialize(list(live, batch, state), NULL)
    rng <- .Random.seed
    fits <- lapply(list(live, batch, state, list(state = state)),
      rubric_workflow_fit, rubric = data$rubric, method = method)
    predictions <- lapply(fits, stats::predict)
    evaluations <- lapply(fits, pairwiseLLM::evaluate_rubric_predictions, rubric = data$rubric)
    for (i in 2:4) {
      expect_equal(predictions[[i]], predictions[[1L]], tolerance = 1e-12)
      expect_equal(evaluations[[i]]$metrics, evaluations[[1L]]$metrics, tolerance = 1e-12)
      expect_equal(evaluations[[i]]$calibration, evaluations[[1L]]$calibration, tolerance = 1e-12)
    }
    expect_identical(fits[[1L]]$cj$provenance$collection, live$provenance)
    expect_identical(fits[[2L]]$cj$provenance$collection, batch$provenance)
    expect_identical(fits[[3L]]$cj$estimation_mode, "adaptive")
    expect_identical(fits[[3L]]$cj$provenance$finalization, "btl_converged")
    expect_identical(stats::predict(fits[[1L]], batch), predictions[[1L]])
    expect_error(stats::predict(fits[[1L]], state), "same|original|source")
    expect_identical(serialize(list(live, batch, state), NULL), originals)
    expect_identical(.Random.seed, rng)
  })

  test_that(paste(method, "independent traits and different K survive interleaved reuse"), {
    rubric_skip_method(method)
    withr::local_seed(92011L)
    a <- rubric_monotone_data(K = 3L, n_unique = 9L)
    b <- rubric_monotone_data(K = 6L, n_unique = 9L)
    b$cj <- rubric_linear_fixed(2 * b$cj$fit$theta_mean + 7)
    a$cj$trait <- a$rubric$trait <- "organization"
    b$cj$trait <- b$rubric$trait <- "mechanics"
    expect_identical(a$rubric$item_id, b$rubric$item_id)
    fit_a <- rubric_workflow_fit(a$cj, a$rubric, method)
    before <- serialize(fit_a, NULL)
    pred_a <- stats::predict(fit_a)
    eval_a <- pairwiseLLM::evaluate_rubric_predictions(fit_a, a$rubric)
    fit_b <- rubric_workflow_fit(b$cj, b$rubric, method, K = 6L, trait = "mechanics")
    pred_b <- stats::predict(fit_b)
    expect_identical(fit_a$K, 3L)
    expect_identical(fit_b$K, 6L)
    expect_identical(fit_b$trait, "mechanics")
    expect_true(all(pred_b$category %in% 1:6))
    if (method != "percentile") expect_true(all(lengths(pred_b$probabilities) == 6L))
    expect_error(stats::predict(fit_a, b$cj), "Trait mismatch")
    expect_error(pairwiseLLM::evaluate_rubric_predictions(fit_a, b$rubric), "Trait mismatch")
    expect_identical(stats::predict(fit_a), pred_a)
    expect_identical(pairwiseLLM::evaluate_rubric_predictions(fit_a, a$rubric), eval_a)
    expect_identical(serialize(fit_a, NULL), before)
    path <- file.path(withr::local_tempdir(), "calibrations.rds")
    saveRDS(list(fit_a, fit_b), path)
    restored <- readRDS(path)
    expect_identical(stats::predict(restored[[1L]]), pred_a)
    expect_identical(stats::predict(restored[[2L]]), pred_b)
    expect_identical(pairwiseLLM::evaluate_rubric_predictions(restored[[1L]], a$rubric), eval_a)
  })

  test_that(paste(method, "saved predictions and evaluation reproduce in a fresh R process"), {
    rubric_skip_method(method)
    data <- rubric_monotone_data(n_unique = 9L)
    fit <- rubric_workflow_fit(data$cj, data$rubric, method)
    expected <- list(prediction = stats::predict(fit),
      evaluation = pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric))
    directory <- withr::local_tempdir()
    config_path <- file.path(directory, "config.rds")
    result_path <- file.path(directory, "result.rds")
    dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) getNamespaceInfo("pairwiseLLM", "path") else NULL
    saveRDS(list(dev_path = dev_path, libpaths = .libPaths(), fit = fit,
      rubric = data$rubric, result_path = result_path), config_path)
    code <- paste(
      "cfg <- readRDS(commandArgs(TRUE)[[1L]])",
      ".libPaths(cfg$libpaths)",
      "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path, quiet = TRUE) else library(pairwiseLLM)",
      "saveRDS(list(prediction = stats::predict(cfg$fit),",
      "evaluation = pairwiseLLM::evaluate_rubric_predictions(cfg$fit, cfg$rubric)), cfg$result_path)",
      sep = "\n")
    output <- system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", "-e", shQuote(code), shQuote(config_path)), stdout = TRUE, stderr = TRUE)
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    expect_true(file.exists(result_path))
    expect_identical(readRDS(result_path), expected)
  })
}

for (method in c("ordinal_linear", "ordinal_monotone")) {
  test_that(paste(method, "public linked output preserves exact resumed provenance"), {
    rubric_skip_method(method)
    withr::local_seed(92012L)
    data <- rubric_linked_fixture(n_sets = 3L)
    fit <- rubric_linked_fit(data, method)
    before <- stats::predict(fit, data$state)
    input <- data$state$linking$estimator$accepted_state_by_spoke[["2"]]$continuation$input
    resumed <- pairwiseLLM::resume_link_session(data$state, input)
    after <- stats::predict(fit, resumed)
    expect_identical(after, before)
    expect_identical(attr(after, "linking")$provenance$estimator_id, "fixed_shape_offset")
    expect_identical(attr(after, "linking")$provenance$uncertainty_scope,
      "offset_only_conditional_on_fixed_shapes")
    rubric <- data.frame(item_id = after$item_id, rubric_score = after$rubric_score)
    assessment <- pairwiseLLM::evaluate_rubric_predictions(fit, rubric, newdata = data$state)
    expect_identical(assessment$metadata$linking, attr(after, "linking"))
  })
}
