test_that("PLS accepts only explicit component controls and guards the optional backend", {
  expect_identical(.warm_start_pls_control(NULL), list(ncomp = NULL))
  expect_identical(.warm_start_pls_control(list()), list(ncomp = NULL))
  expect_identical(.warm_start_pls_control(list(ncomp = c(3, 1))), list(ncomp = c(1L, 3L)))
  for (bad in list(1, list(1), list(ncomp = 1, ncomp = 2), list(method = "simpls"))) {
    expect_error(.warm_start_pls_control(bad), "only a named")
  }
  for (bad in list(NULL, numeric(), NA, "1", c(1, 1), c(1, 11), -1, 0, 1.5, Inf, matrix(1))) {
    expect_error(.warm_start_pls_control(list(ncomp = bad)), "unique positive integers")
  }
  f <- warm_pls_fixture()
  expect_error(warm_pls_fit(f, alpha_grid = 0), "glmnet-only")
  expect_error(warm_pls_fit(f, lambda_rule = "lambda.1se"), "glmnet-only")
  local_mocked_bindings(.warm_start_pls_available = function() FALSE,
    extract_warm_start_features = function(...) stop("must not extract"), .package = "pairwiseLLM")
  expect_error(warm_pls_fit(f), "optional package 'pls'. Install it explicitly")
  expect_error(fit_warm_start_model(f$x$item_id, f$theta, "phase4", texts = rep("text", 25),
    engine = "pls", cv_plan = f$plan), "optional package 'pls'")
})

test_that("PLS common grids respect every split rank, p, n-1 and the fixed cap", {
  withr::local_seed(25904)
  x <- matrix(stats::rnorm(300), 15, dimnames = list(NULL, paste0("x", 1:20)))
  expect_identical(.warm_start_pls_bound(x)$max_ncomp, 10L)
  expect_identical(.warm_start_pls_bound(x[1:5, , drop = FALSE])$max_ncomp, 4L)
  expect_identical(.warm_start_pls_bound(x[, 1, drop = FALSE])$max_ncomp, 1L)
  aliased <- cbind(a = x[, 1], b = x[, 1], c = x[, 2])
  expect_identical(.warm_start_pls_bound(aliased)$max_ncomp, 2L)
  near <- cbind(a = x[, 1], b = x[, 1] + 1e-9 * x[, 2])
  expect_identical(.warm_start_pls_bound(near)$rank, 1L)
  near[, 2] <- x[, 1] + 1e-5 * x[, 2]
  expect_identical(.warm_start_pls_bound(near)$rank, 2L)
  bounds <- list(.warm_start_pls_bound(x), .warm_start_pls_bound(aliased))
  expect_identical(.warm_start_pls_grid(bounds, NULL), 1:2)
  expect_identical(.warm_start_pls_grid(bounds, 2L), 2L)
  expect_error(.warm_start_pls_grid(bounds, c(1L, 3L)), "common inner/refit bound of 2")
  expect_error(.warm_start_pls_grid(list(.warm_start_pls_bound(x * 0)), NULL), "no legal")
  skip_if_not_installed("pls")
  folds <- rep(1:3, c(5, 5, 7))
  x <- cbind(a = stats::rnorm(17), b = c(stats::rnorm(5), rep(0, 12)), c = stats::rnorm(17))
  x[6, "c"] <- NA
  tune <- .warm_start_pls_tune(x, stats::rnorm(17), folds, list(ncomp = NULL))
  expect_identical(tune$ncomp_grid, 1:2)
  expect_identical(tune$bounds$inner_1$p, 2L)
  expect_identical(tune$inner_preprocessing[[1]]$removed[["b"]], "constant")
  expect_identical(tune$traces$fold_sizes, c(5L, 5L, 7L))
  expect_equal(tune$traces$cvm, colSums(tune$traces$fold_mse * c(5, 5, 7)) / 17)
  expect_invisible(.validate_warm_start_pls_tuning(tune, 17L))
  expect_error(.warm_start_pls_tune(x, stats::rnorm(17), folds, list(ncomp = 3L)), "bound of 2")
})

test_that("PLS weighted losses and 1-SE use observation counts and favor fewer components", {
  loss <- rbind(c(3, 1, 1), c(1, 2, 4), c(2, 1, 3))
  sizes <- c(1L, 2L, 4L)
  errors <- .warm_start_loss_summary(loss, sizes)
  expected <- colSums(loss * sizes) / sum(sizes)
  expect_equal(errors$cvm, expected)
  expect_equal(errors$cvsd, sqrt(colSums((sweep(loss, 2, expected)^2) * sizes) / sum(sizes) / 2))
  choice <- .warm_start_pls_choice(1:3, c(1.1, 1 + 5e-11, 1), c(0.2, 0.2, 0.1))
  expect_identical(choice, list(index_min = 2L, index_1se = 1L, index = 1L,
    ncomp_min = 2L, ncomp_1se = 1L))
  expect_identical(.warm_start_pls_choice(c(2L, 4L), c(1, 1 - 5e-11), c(0, 0))$ncomp_1se, 2L)
})

test_that("PLS portable linear predictions match the explicit backend including centering", {
  skip_if_not_installed("pls")
  withr::local_seed(82)
  x <- cbind(a = 10 + stats::rnorm(18), b = stats::rnorm(18), c = stats::rnorm(18))
  z <- 5 + x[, 1] - x[, 2] + stats::rnorm(18)
  new <- x[1:4, , drop = FALSE] + 0.17
  fit <- .warm_start_pls_fit(x, z, 3L)
  expect_identical(fit$method, "kernelpls")
  expect_null(fit$validation)
  expect_null(fit$scale)
  for (count in 1:3) {
    payload <- .warm_start_pls_payload(fit, count, colnames(x))
    expect_equal(.warm_start_engine_predict(payload, new),
      as.numeric(predict(fit, newdata = data.frame(x = I(new)), ncomp = count)), tolerance = 1e-12)
    expect_identical(names(payload$coefficients), colnames(x))
    expect_equal(payload$intercept, as.numeric(fit$Ymeans - fit$Xmeans %*% coef(fit, ncomp = count)))
  }
  one <- .warm_start_pls_fit(x[, 1, drop = FALSE], z, 1L)
  expect_equal(.warm_start_engine_predict(.warm_start_pls_payload(one, 1L, "a"), new[, 1, drop = FALSE]),
    as.numeric(predict(one, newdata = data.frame(x = I(new[, 1, drop = FALSE])), ncomp = 1L)))
  for (bad in list(0, 1.5, 4, Inf)) expect_error(.warm_start_pls_fit(x, z, bad), "legal components")
  expect_error(.warm_start_pls_fit(x, rep(1, 18), 1L), "nonconstant")
  expect_error(.warm_start_pls_fit(x, z[-1], 1L), "aligned rows")
})

test_that("PLS audits independently reproduce split preprocessing, OOF and outer predictions", {
  skip_if_not_installed("pls")
  f <- warm_pls_fixture()
  model <- warm_pls_fit(f, engine_control = list(ncomp = c(1, 2, 4)))
  x <- as.matrix(f$x[, -1])
  contexts <- c(model$validation$folds, list(model))
  for (i in seq_along(contexts)) {
    record <- contexts[[i]]
    rows <- if (i <= 5L) which(f$plan$outer_foldid != i) else seq_len(nrow(x))
    theta <- f$theta[rows]
    z <- (theta - mean(theta)) / stats::sd(theta)
    tune <- record$tuning
    expect_equal(record$outcome$mean, mean(theta))
    expect_equal(record$outcome$sd, stats::sd(theta))
    expect_equal(tune$oof$observed, z)
    expect_identical(record$preprocessing, .warm_start_preprocess_fit(x[rows, , drop = FALSE]))
    for (fold in 1:5) {
      train <- which(tune$foldid != fold)
      hold <- which(tune$foldid == fold)
      p <- .warm_start_preprocess_fit(x[rows[train], , drop = FALSE])
      expect_identical(tune$inner_preprocessing[[fold]], p)
      predictors <- .warm_start_preprocess_apply(x[rows[train], , drop = FALSE], p)
      target <- z[train]
      backend <- pls::plsr(target ~ predictors, ncomp = 4L, method = "kernelpls",
        scale = FALSE, validation = "none", center = TRUE)
      held <- .warm_start_preprocess_apply(x[rows[hold], , drop = FALSE], p)
      for (j in seq_along(tune$ncomp_grid)) {
        predicted <- as.numeric(predict(backend, newdata = data.frame(predictors = I(held)),
          ncomp = tune$ncomp_grid[j]))
        expect_equal(tune$traces$candidate_oof[hold, j], predicted, tolerance = 1e-12)
        expect_equal(tune$traces$fold_mse[fold, j], mean((predicted - z[hold])^2))
      }
    }
    calibration <- stats::lm.fit(cbind(1, tune$selected$oof), z, tol = 1e-7)$coefficients
    expect_equal(record$calibration$intercept, unname(calibration[1]))
    expect_equal(record$calibration$slope, unname(calibration[2]))
    if (i <= 5L) {
      held <- which(f$plan$outer_foldid == i)
      raw <- as.numeric(record$intercept +
        .warm_start_preprocess_apply(x[held, , drop = FALSE], record$preprocessing) %*% record$coefficients)
      expect_equal(record$predictions$raw_prediction, raw)
      expect_equal(record$predictions$observed, (f$theta[held] - mean(theta)) / stats::sd(theta))
    }
  }
  p <- model$validation$predictions
  expect_equal(model$validation$metrics$rmse, sqrt(mean((p$calibrated_prediction - p$observed)^2)))
  expect_equal(model$validation$metrics$mae, mean(abs(p$calibrated_prediction - p$observed)))
  expect_equal(model$validation$metrics$pearson_r, stats::cor(p$calibrated_prediction, p$observed))
  # Changing only outer holdout predictors must not change that fold's fitted state.
  altered <- f
  held <- which(f$plan$outer_foldid == 1L)
  altered$x$token_length_mean[held] <- altered$x$token_length_mean[held] + 100
  other <- warm_pls_fit(altered, engine_control = list(ncomp = c(1, 2, 4)))
  fields <- setdiff(names(model$validation$folds[[1]]), "predictions")
  expect_identical(other$validation$folds[[1]][fields], model$validation$folds[[1]][fields])
  expect_false(identical(other$validation$folds[[1]]$predictions, model$validation$folds[[1]]$predictions))
})

test_that("PLS preserves caller RNG and supplied partitions despite backend RNG consumption", {
  skip_if_not_installed("pls")
  f <- warm_pls_fixture()
  withr::local_seed(259)
  before <- .Random.seed
  model <- warm_pls_fit(f)
  expect_identical(model$tuning$ncomp_grid, 1:10)
  expect_identical(model$cv_plan, f$plan)
  expect_identical(.Random.seed, before)
  expect_identical(warm_pls_fit(f), model)
  original <- .warm_start_pls_fit
  local_mocked_bindings(.warm_start_pls_fit = function(...) {
    stats::runif(13)
    original(...)
  }, .warm_start_folds = function(...) stop("must not regenerate supplied plan"),
    .warm_start_require_glmnet = function(...) stop("must not load glmnet"), .package = "pairwiseLLM")
  expect_identical(warm_pls_fit(f), model)
  expect_identical(.Random.seed, before)
})

test_that("PLS degeneracy fails explicitly with fitting context and no candidate fallback", {
  skip_if_not_installed("pls")
  x <- cbind(a = c(-1, -1, 1, 1), b = c(-1, 1, -1, 1))
  expect_error(.warm_start_pls_fit(x, c(1, -1, -1, 1), 1L), "zero covariance or latent saturation")
  expect_error(.warm_start_pls_fit(x, x[, 1], 2L), "zero covariance or latent saturation")
  f <- warm_pls_fixture()
  local_mocked_bindings(.warm_start_pls_fit = function(...) rlang::abort("latent saturation fixture"),
    .package = "pairwiseLLM")
  expect_error(warm_pls_fit(f, engine_control = list(ncomp = 1)),
    "Outer fold 1: Inner fold 1 PLS ncomp 1: latent saturation")
  expect_error(.warm_start_engine_refit("pls", x, x[, 1], list(ncomp = 1)),
    "PLS context refit, ncomp 1: latent saturation")
})

test_that("PLS full audits reject missing and inconsistent evidence", {
  skip_if_not_installed("pls")
  model <- warm_pls_fit(engine_control = list(ncomp = c(1, 3)))
  expect_invisible(.validate_warm_start_model(model))
  for (field in names(model)) {
    bad <- model
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(model$tuning)) {
    bad <- model
    bad$tuning[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(model$tuning$traces)) {
    bad <- model
    bad$tuning$traces[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(model$validation$folds[[1]])) {
    bad <- model
    bad$validation$folds[[1]][field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  changes <- list(
    list(c("tuning", "bounds", "inner_1", "max_ncomp"), 9L),
    list(c("tuning", "bounds", "inner_1", "rank"), 99L),
    list(c("tuning", "bounds", "context_refit", "p"), 1L),
    list(c("tuning", "traces", "index"), 99L),
    list(c("tuning", "selected", "ncomp"), 2L),
    list(c("tuning", "ncomp_requested"), c(1L, 2L)),
    list(c("training", "hyperparameters", "ncomp"), 2L),
    list(c("training", "alpha"), 0.5),
    list(c("calibration", "slope"), 99),
    list(c("validation", "metrics", "rmse"), 99),
    list(c("engine_payload", "intercept"), 99)
  )
  for (change in changes) {
    bad <- model
    bad[[change[[1]]]] <- change[[2]]
    expect_error(.validate_warm_start_model(bad), info = paste(change[[1]], collapse = "$"))
  }
  for (field in c("fold_mse", "candidate_oof")) {
    bad <- model
    bad$tuning$traces[[field]][1, 1] <- bad$tuning$traces[[field]][1, 1] + 0.5
    expect_error(.validate_warm_start_model(bad), "PLS tuning audit")
  }
  bad <- model
  bad$tuning$traces$cvm[1] <- bad$tuning$traces$cvm[1] + 1
  expect_error(.validate_warm_start_model(bad), "PLS tuning audit")
  bad <- model
  bad$validation$folds[[1]]$tuning$ncomp_requested <- rev(bad$tuning$ncomp_requested)
  expect_error(.validate_warm_start_model(bad), "PLS tuning audit")
})
