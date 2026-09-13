for (method in c("percentile", "ordinal_linear", "ordinal_monotone")) {
  test_that(paste(method, "uses one public scoring path for four fixed and adaptive variants"), {
    rubric_skip_method(method)
    withr::local_seed(92020L)
    baseline <- NULL
    for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
      data <- rubric_monotone_data(n_unique = 9L, variant = variant)
      for (collection in c("live", "batch")) {
        data$cj$provenance <- list(collection_mode = collection)
        fixed <- rubric_workflow_fit(data$cj, data$rubric, method)
        pred <- stats::predict(fixed)
        if (is.null(baseline)) baseline <- pred
        expect_equal(pred, baseline, tolerance = 1e-12)
        expect_identical(fixed$cj$model_variant, variant)
        expect_identical(fixed$cj$provenance$collection$collection_mode, collection)
      }
      # Only completed adaptive within-set state is represented here.
      state <- rubric_completed_adaptive(data$cj)
      adaptive <- rubric_workflow_fit(state, data$rubric, method)
      expect_identical(adaptive$cj$model_variant, variant)
      expect_equal(stats::predict(adaptive), baseline, tolerance = 1e-12)
      expect_equal(adaptive$cj$posterior_draws, fixed$cj$posterior_draws)
      expect_equal(adaptive$cj$items$theta_sd, fixed$cj$items$theta_sd)
      expect_identical(adaptive$cj$diagnostics$diagnostics_pass, TRUE)
      if (method != "percentile") {
        p <- do.call(rbind, stats::predict(adaptive)$probabilities)
        expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
        expect_true(all(is.finite(p) & p >= 0 & p <= 1))
        expect_identical(baseline$category, baseline$median_category)
      } else {
        expect_false("probabilities" %in% names(baseline))
      }
    }
  })
}
