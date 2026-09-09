test_that("numeric prior conversion aligns IDs and SDs and centers active scopes", {
  prior <- make_warm_start_prior(c(a = 2, b = 4, c = 9), ids = c("c", "a", "b"),
    prior_sd = c(b = 0.2, c = 0.3, a = 0.1))
  expect_identical(prior$item_id, c("c", "a", "b"))
  expect_equal(prior$prior_mean, c(4, -3, -1))
  expect_equal(prior$prior_sd, c(0.3, 0.1, 0.2))
  expect_silent(pairwiseLLM:::.validate_warm_start_prior(prior))
  scope <- pairwiseLLM:::.warm_start_prior_scope(prior, c("b", "a"))
  expect_identical(scope$prior_mean, c(1, -1))
  expect_identical(scope$prior_sd, c(0.2, 0.1))
  expect_identical(make_warm_start_prior(c(2, 4), ids = c("a", "b"))$prior_sd, c(0.5, 0.5))
  expect_equal(make_warm_start_prior(c(a = 2, b = 4), c("b", "a"), c(0.2, 0.3))$prior_sd, c(0.3, 0.2))
  expect_null(pairwiseLLM:::.warm_start_prior_scope(NULL, c("a", "b")))
  expect_error(pairwiseLLM:::.warm_start_prior_scope(prior, c("a", "d")), "missing")
  expect_error(pairwiseLLM:::.warm_start_prior_scope(prior, c("a", "b"), TRUE), "exactly")
})

test_that("prior boundaries reject invalid IDs, numbers, SDs and tampering", {
  for (ids in list(NULL, "a", c("a", "a"), c("", "b"), c(NA, "b"), c(" ", "b"), list(1, 2))) {
    expect_error(make_warm_start_prior(c(1, 2), ids), "IDs")
  }
  expect_error(make_warm_start_prior(c(a = 1, b = 2), c("a", "c")), "exactly")
  for (scores in list(c(a = NA, b = 1), c(a = Inf, b = 1), c(a = NaN, b = 1))) {
    expect_error(make_warm_start_prior(scores), "finite")
  }
  expect_error(make_warm_start_prior(c(TRUE, FALSE), c("a", "b")), "Supply")
  expect_error(make_warm_start_prior(c(a = 1.7e308, b = -1.7e308, c = -1.7e308)), "finite")
  for (sd in list(0, -1, Inf, NA_real_, "a", numeric(), c(1, 2, 3))) {
    expect_error(make_warm_start_prior(c(a = 1, b = 2), prior_sd = sd), "SDs")
  }
  for (sd in list(c(a = 1, a = 2), c(a = 1, c = 2), c(a = 1))) {
    expect_error(make_warm_start_prior(c(a = 1, b = 2), prior_sd = sd), "IDs")
  }
  prior <- make_warm_start_prior(c(a = 1, b = 2))
  for (field in c("format_version", "scores", "prior_mean", "prior_sd", "diagnostics", "provenance", "digest")) {
    bad <- prior
    bad[[field]] <- NULL
    expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "contract")
  }
  bad <- prior
  bad$format_version <- 2L
  expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "version")
  bad <- prior
  bad$prior_mean <- c(0, 0)
  expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "centering")
  bad <- prior
  bad$provenance$source <- "changed"
  expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "digest")
  bad <- prior
  bad$diagnostics <- data.frame(x = c(NA_real_, 1))
  expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "finite")
  bad$diagnostics <- data.frame(x = 1)
  expect_error(pairwiseLLM:::.validate_warm_start_prior(bad), "diagnostics")
})

test_that("calibrated model and ensemble predictions retain compact diagnostics", {
  model <- warm_bundle_model()
  features <- warm_core_features()
  pred <- predict(model, features)
  prior <- make_warm_start_prior(pred)
  expect_equal(prior$scores, pred$calibrated_prediction)
  expect_equal(prior$diagnostics$raw_prediction, pred$raw_prediction)
  expect_error(make_warm_start_prior(predict(warm_core_model(), features)), "calibration")
  model2 <- model
  model2$intercept <- model$intercept + 1
  ensemble <- ensemble_warm_start_models(a = model, b = model2)
  pred <- predict(ensemble, features)
  prior <- make_warm_start_prior(pred, rev(features$item_id))
  expect_equal(prior$scores, rev(pred$ensemble_mean))
  expect_equal(prior$diagnostics$ensemble_sd, rev(pred$ensemble_sd))
  expect_identical(prior$prior_sd, rep(0.5, nrow(features)))
  expect_null(attr(prior$diagnostics, "component_predictions"))
  scoped <- pairwiseLLM:::.warm_start_prior_scope(prior, c("2", "1"))
  expect_equal(scoped$diagnostics$component_a, pred$component_a[c(2, 1)])
  expect_equal(scoped$prior_mean, pred$ensemble_mean[c(2, 1)] - mean(pred$ensemble_mean[1:2]))
  bad <- pred
  bad$ensemble_sd <- Inf
  expect_error(make_warm_start_prior(bad), "finite")
  bad$ensemble_sd <- 10
  expect_error(make_warm_start_prior(bad), "disagree")
  bad <- pred
  bad$component_a[1] <- Inf
  expect_error(make_warm_start_prior(bad), "finite")
  bad <- pred
  attr(bad, "component_columns") <- NULL
  expect_error(make_warm_start_prior(bad), "ensemble")
  bad <- pred
  attr(bad, "warm_start_schema") <- "unknown"
  expect_error(make_warm_start_prior(bad), "schema")
  attr(bad, "warm_start_schema") <- NULL
  expect_error(make_warm_start_prior(bad), "schema")
  bad <- pred
  attr(bad, "warm_start_model") <- NULL
  expect_error(make_warm_start_prior(bad), "metadata")
  bad <- predict(model, features)
  attr(bad, "warm_start_model")$runtime <- new.env()
  expect_error(make_warm_start_prior(bad), "portable")
  bad <- predict(model, features)
  attr(bad, "warm_start_model")$format_version <- NULL
  expect_error(make_warm_start_prior(bad), "format")
  expect_error(make_warm_start_prior(data.frame(x = 1)), "Supply")
})

test_that("predictive integrity hashing uses portable bytes without runtime version words", {
  value <- list(ids = c("a", "b"), values = c(1, 2), format = 1L)
  path <- withr::local_tempfile()
  bytes <- serialize(value, NULL, version = 2, xdr = TRUE)
  writeBin(bytes[-(7:14)], path)
  expect_identical(pairwiseLLM:::.warm_start_prior_hash(value), unname(tools::md5sum(path)))
  # Only writer/minimum-reader metadata changes in this simulated runtime upgrade.
  bytes[7:14] <- as.raw(rep(0L, 8))
  writeBin(bytes[-(7:14)], path)
  expect_identical(pairwiseLLM:::.warm_start_prior_hash(value), unname(tools::md5sum(path)))
})
