test_that("OOF calibration learns intercept and slope without clipping", {
  raw <- seq(-2, 2, length.out = 10)
  for (slope in c(-2, 0.4, 1)) {
    z <- 0.7 + slope * raw
    cal <- .warm_start_calibration_fit(raw, z)
    expect_equal(cal$intercept, 0.7)
    expect_equal(cal$slope, slope)
    expect_identical(cal$n, 10L)
    expect_equal(.warm_start_calibration_apply(raw, cal), z)
    expect_invisible(.validate_warm_start_calibration(cal))
  }
})

test_that("degenerate and nonfinite calibration fails explicitly", {
  expect_error(.warm_start_calibration_fit(1:2, 1:2), "three aligned")
  expect_error(.warm_start_calibration_fit(1:3, 1:4), "three aligned")
  expect_error(.warm_start_calibration_fit(rep(1, 5), 1:5), "unidentified")
  expect_error(.warm_start_calibration_fit(1 + (1:5) * 1e-12, 1:5), "degenerate")
  expect_error(.warm_start_calibration_fit(c(1, NA, 3), 1:3), "finite")
  cal <- .warm_start_calibration_fit(1:5, 1:5)
  for (field in names(cal)) {
    bad <- cal
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_calibration(bad), "calibration contract")
  }
  for (value in list(Inf, NA_real_, c(1, 2))) {
    bad <- cal
    bad$slope <- value
    expect_error(.warm_start_calibration_apply(1:5, bad), "contract")
  }
  uncalibrated <- list(status = "uncalibrated", intercept = NULL, slope = NULL)
  expect_invisible(.validate_warm_start_calibration(uncalibrated))
  expect_error(.warm_start_calibration_apply(1:5, uncalibrated), "Learned OOF")
  cal$slope <- 1e300
  expect_error(.warm_start_calibration_apply(rep(1e300, 3), cal), "nonfinite")
  local_mocked_bindings(lm.fit = function(...) list(rank = 2L, coefficients = c(Inf, 1)),
    .package = "stats")
  expect_error(.warm_start_calibration_fit(1:5, 1:5), "nonfinite coefficients")
})

test_that("validation metrics use supplied held-out values and label undefined diagnostics", {
  observed <- c(-2, -1, 0, 1, 2)
  predicted <- c(-1.6, -0.9, 0.3, 1.5, 1.8)
  metrics <- .warm_start_validation_metrics(predicted, observed)
  expect_equal(metrics$pearson_r, cor(predicted, observed))
  expect_equal(metrics$squared_pearson_r, cor(predicted, observed)^2)
  expect_equal(metrics$spearman_rho, cor(predicted, observed, method = "spearman"))
  expect_equal(metrics$rmse, sqrt(mean((predicted - observed)^2)))
  expect_equal(metrics$mae, mean(abs(predicted - observed)))
  expect_equal(c(metrics$calibration_intercept, metrics$calibration_slope),
    unname(coef(lm(observed ~ predicted))))
  expect_length(metrics$undefined_reasons, 0)
  constant <- .warm_start_validation_metrics(rep(0, 5), observed)
  expect_true(is.na(constant$pearson_r))
  expect_true(is.na(constant$calibration_slope))
  expect_length(constant$undefined_reasons, 2)
  expect_equal(constant$rmse, sqrt(2))
  expect_error(.warm_start_validation_metrics(1:2, 1:2), "three aligned")
  expect_error(.warm_start_validation_metrics(1:3, 1:4), "aligned")
  expect_error(.warm_start_validation_metrics(rep(1e300, 3), 1:3), "overflowed")
})
