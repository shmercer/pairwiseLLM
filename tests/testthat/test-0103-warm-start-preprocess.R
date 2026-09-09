test_that("preprocessing learns missingness, imputation, NZV and sample scaling in order", {
  x <- cbind(all_na = rep(NA_real_, 40), too_missing = c(rep(NA_real_, 9), 1:31),
    boundary = c(rep(NA_real_, 8), 1:32), constant = rep(3, 40),
    nzv = c(rep(1, 39), 2), ratio_boundary = c(rep(1, 38), 2, 2),
    unique_boundary = c(rep(1, 77), 2, 3, 4)[1:40], varied = (1:40)^2)
  # Distinct imputed median creates the dominant frequency after imputation.
  x <- cbind(x, imputed_nzv = c(rep(NA_real_, 8), rep(1, 31), 2))
  p <- .warm_start_preprocess_fit(x)
  expect_identical(p$retained, c("boundary", "ratio_boundary", "varied"))
  expect_identical(unname(p$removed), c("all_missing", "missingness", "constant",
    "near_zero_variance", "constant", "near_zero_variance"))
  expect_equal(p$missing_fraction["boundary"], c(boundary = 0.2))
  expect_equal(p$medians["boundary"], c(boundary = 16.5))
  imputed <- c(rep(16.5, 8), 1:32)
  expect_equal(p$centers["boundary"], c(boundary = mean(imputed)))
  expect_equal(p$scales["boundary"], c(boundary = sd(imputed)))
  scaled <- .warm_start_preprocess_apply(x, p)
  expect_equal(unname(colMeans(scaled)), rep(0, 3), tolerance = 1e-12)
  expect_equal(unname(apply(scaled, 2, sd)), rep(1, 3), tolerance = 1e-12)
  expect_identical(p$sd_convention, "sample")
  expect_identical(p, .warm_start_preprocess_fit(x))
})

test_that("NZV unique fraction boundaries and control overrides are explicit", {
  x <- cbind(at = c(rep(1, 77), 2, 3, 4), above = c(rep(1, 76), 2:5), varied = 1:80)
  p <- .warm_start_preprocess_fit(x, unique_threshold = 0.05)
  expect_identical(p$retained, c("above", "varied"))
  expect_identical(p$removed, c(at = "near_zero_variance"))
  expect_length(.warm_start_preprocess_fit(x, unique_threshold = 0)$retained, 3)
  expect_length(.warm_start_preprocess_fit(x, frequency_ratio = 100)$retained, 3)
  x[1:20, "varied"] <- NA
  expect_false("varied" %in% .warm_start_preprocess_fit(x, unique_threshold = 0.05)$retained)
  expect_true("varied" %in% .warm_start_preprocess_fit(x, missing_threshold = 0.25)$retained)
  for (value in list(NULL, NA_real_, Inf, "0.2", -1, 2, c(0, 1))) {
    expect_error(.warm_start_preprocess_fit(x, missing_threshold = value), "threshold")
    expect_error(.warm_start_preprocess_fit(x, unique_threshold = value), "threshold")
  }
  for (value in list(NULL, NA_real_, Inf, "19", 0, c(1, 2))) {
    expect_error(.warm_start_preprocess_fit(x, frequency_ratio = value), "frequency ratio")
  }
})

test_that("apply uses training parameters without examining held-out distributions", {
  x <- cbind(a = 1:10, b = c(NA, 2:10))
  p <- .warm_start_preprocess_fit(x)
  new <- cbind(b = c(NA, 1e6), a = c(-1e6, NA), extra = c(10, 20))
  expected <- cbind(a = (c(-1e6, p$medians["a"]) - p$centers["a"]) / p$scales["a"],
    b = (c(p$medians["b"], 1e6) - p$centers["b"]) / p$scales["b"])
  rownames(expected) <- NULL
  expect_equal(.warm_start_preprocess_apply(new, p), expected)
  expect_equal(.warm_start_preprocess_apply(new[1, , drop = FALSE], p), expected[1, , drop = FALSE])
  expect_identical(p, .warm_start_preprocess_fit(x))
  expect_error(.warm_start_preprocess_apply(new[, "a", drop = FALSE], p), "Missing original")
  expect_error(.warm_start_preprocess_fit(cbind(a = rep(1, 5), b = rep(NA_real_, 5))),
    "No predictors survive.*a=constant.*b=all_missing")
  one <- .warm_start_preprocess_fit(cbind(a = 1:5, b = rep(0, 5)))
  expect_identical(dim(.warm_start_preprocess_apply(cbind(a = 1:5, b = rep(0, 5)), one)), c(5L, 1L))
})

test_that("malformed matrices and stored preprocessing fail defensively", {
  x <- cbind(a = 1:10, b = 11:20)
  for (bad in list(NULL, data.frame(x), matrix(1, 2, 2), matrix("a", 2, 2),
    x[FALSE, ], x[, FALSE], cbind(a = c(Inf, 2:10), b = 1:10))) {
    expect_error(.warm_start_preprocess_fit(bad), "matrix")
  }
  bad <- x
  colnames(bad) <- c("a", "a")
  expect_error(.warm_start_preprocess_fit(bad), "matrix")
  expect_error(.warm_start_preprocess_fit(x[1, , drop = FALSE]), "two training")
  p <- .warm_start_preprocess_fit(x)
  for (field in names(p)) {
    bad <- p
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_preprocess(bad))
  }
  for (field in c("scales", "medians", "centers", "missing_fraction")) {
    bad <- p
    bad[[field]][1] <- Inf
    expect_error(.validate_warm_start_preprocess(bad), "contract")
  }
  bad <- p
  bad$scales[1] <- 0
  expect_error(.validate_warm_start_preprocess(bad), "contract")
  bad$scales[1] <- 1e-300
  expect_error(.warm_start_preprocess_apply(cbind(a = rep(1e300, 10), b = 11:20), bad), "matrix")
})

test_that("outcome scaling is reusable and rejects degenerate or nonfinite values", {
  theta <- c(-10, 0, 20, 30)
  p <- .warm_start_outcome_fit(theta)
  expect_identical(p, list(definition = "within_task_z", mean = mean(theta), sd = sd(theta),
    sd_convention = "sample"))
  z <- .warm_start_outcome_apply(theta, p)
  expect_equal(mean(z), 0, tolerance = 1e-14)
  expect_equal(sd(z), 1, tolerance = 1e-14)
  expect_equal(.warm_start_outcome_apply(c(100, 1000), p), (c(100, 1000) - mean(theta)) / sd(theta))
  for (value in list(NULL, numeric(), NA_real_, Inf, NaN, "1", matrix(1), factor(1))) {
    expect_error(.warm_start_outcome_fit(value), "theta")
  }
  expect_error(.warm_start_outcome_fit(1), "positive sample SD")
  expect_error(.warm_start_outcome_fit(rep(1, 5)), "positive sample SD")
  for (field in names(p)) {
    bad <- p
    bad[field] <- list(NULL)
    expect_error(.warm_start_outcome_apply(theta, bad), "Outcome scaling")
  }
  p$sd <- 1e-300
  expect_error(.warm_start_outcome_apply(1e300, p), "nonfinite")
})
