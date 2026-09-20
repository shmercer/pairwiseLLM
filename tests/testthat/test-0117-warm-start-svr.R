test_that("SVR validates candidate controls and guards only its optional backend", {
  defaults <- list(cost = 2^(-2:4), gamma_multiplier = 2^(-2:2))
  expect_identical(.warm_start_svr_control(NULL), defaults)
  expect_identical(.warm_start_svr_control(list()), defaults)
  expect_identical(.warm_start_svr_control(list(cost = c(2, 1))),
    list(cost = c(1, 2), gamma_multiplier = defaults$gamma_multiplier))
  expect_identical(.warm_start_svr_control(list(gamma_multiplier = 2)),
    list(cost = defaults$cost, gamma_multiplier = 2))
  grid <- .warm_start_svr_grid(defaults)
  expect_identical(nrow(grid), 35L)
  expect_identical(order(grid$cost, grid$gamma_multiplier), 1:35)
  for (bad in list(1, list(1), list(cost = 1, cost = 2), list(epsilon = 0.1))) {
    expect_error(.warm_start_svr_control(bad), "only named")
  }
  for (bad in list(NULL, numeric(), NA, "1", c(1, 1), -1, 0, Inf, matrix(1))) {
    for (name in names(defaults)) {
      expect_error(.warm_start_svr_control(stats::setNames(list(bad), name)), "unique finite positive")
    }
  }
  f <- warm_svr_fixture()
  expect_error(warm_svr_fit(f, alpha_grid = 0), "glmnet-only")
  expect_error(warm_svr_fit(f, lambda_rule = "lambda.1se"), "glmnet-only")
  local_mocked_bindings(.warm_start_svr_available = function() FALSE,
    extract_warm_start_features = function(...) stop("must not extract"), .package = "pairwiseLLM")
  expect_error(warm_svr_fit(f), "optional package 'e1071'. Install it explicitly")
  expect_error(fit_warm_start_model(f$x$item_id, f$theta, "phase5", texts = rep("text", 25),
    engine = "svr_rbf", cv_plan = f$plan), "optional package 'e1071'")
})

test_that("SVR weighted selection resolves minima and 1-SE by cost then multiplier", {
  loss <- rbind(c(3, 1, 1), c(1, 2, 4), c(2, 1, 3))
  sizes <- c(1L, 2L, 4L)
  errors <- .warm_start_loss_summary(loss, sizes)
  expected <- colSums(loss * sizes) / sum(sizes)
  expect_equal(errors$cvm, expected)
  expect_equal(errors$cvsd, sqrt(colSums(sweep(loss, 2, expected)^2 * sizes) / sum(sizes) / 2))
  grid <- .warm_start_svr_grid(list(cost = c(0.25, 1), gamma_multiplier = c(0.5, 2)))
  choice <- .warm_start_svr_choice(c(1.3, 1 + 5e-11, 1, 2), c(0.1, 0.4, 0, 0))
  expect_identical(choice, list(index_min = 2L, index_1se = 1L, index = 1L))
  expect_identical(grid$cost[choice$index_min], 0.25)
  expect_identical(grid$gamma_multiplier[choice$index], 0.5)
  expect_identical(.warm_start_svr_choice(c(1, 1 - 5e-11), c(0, 0))$index, 1L)
})

test_that("SVR numeric payload reproduces backend prediction with singleton dimensions", {
  skip_if_not_installed("e1071")
  withr::local_seed(25905)
  x <- cbind(a = 10 + stats::rnorm(18), b = stats::rnorm(18), c = stats::rnorm(18))
  z <- as.numeric(scale(sin(x[, 1]) - x[, 2]))
  for (p in c(1L, 3L)) {
    train <- x[, seq_len(p), drop = FALSE]
    reference <- e1071::svm(train, z, type = "eps-regression", kernel = "radial", scale = FALSE,
      cost = 2, gamma = 0.5 / p, epsilon = 0.1, cross = 0, probability = FALSE)
    payload <- .warm_start_svr_fit(train, z, 2, 0.5)
    expect_null(reference$x.scale)
    expect_null(reference$y.scale)
    expect_identical(payload$gamma, 0.5 / p)
    expect_identical(colnames(payload$support_vectors), colnames(train))
    expect_identical(ncol(payload$support_vectors), p)
    for (new in list(train, train[1, , drop = FALSE] + 0.17, train[1:4, , drop = FALSE] - 0.3)) {
      expect_equal(.warm_start_engine_predict(payload, new), as.numeric(predict(reference, new)),
        tolerance = 1e-12)
    }
  }
  payload <- list(type = "rbf_svr", support_vectors = matrix(1, 1, 1, dimnames = list(NULL, "a")),
    dual = 2, rho = 0.3, gamma = 0.5)
  expect_equal(.warm_start_engine_predict(payload, matrix(c(1, 1, 3), 3, dimnames = list(NULL, "a"))),
    c(1.7, 1.7, 2 * exp(-2) - 0.3))
  expect_identical(dim(.warm_start_engine_copy(payload)$support_vectors), c(1L, 1L))
  for (bad in list(0, -1, Inf, NA)) {
    expect_error(.warm_start_svr_fit(x, z, bad, 1), "positive cost")
    expect_error(.warm_start_svr_fit(x, z, 1, bad), "positive cost")
  }
  expect_error(.warm_start_svr_fit(x, rep(1, 18), 1, 1), "nonconstant")
  expect_error(.warm_start_svr_fit(x, z[-1], 1, 1), "aligned rows")
  expect_error(.warm_start_svr_fit(x[1:2, ], z[1:2], 1, 1), "aligned rows")
  expect_error(.warm_start_svr_fit(x, z, 1, .Machine$double.xmin * .Machine$double.eps), "actual gamma")
  expect_error(.warm_start_svr_payload(list(SV = x, coefs = matrix(1, 2, 2)), colnames(x)), "incomplete")
  expect_error(.warm_start_engine_predict(payload, matrix(1e308, 1, dimnames = list(NULL, "a"))), "distances")
  payload$support_vectors <- matrix(1, 2, 1, dimnames = list(NULL, "a"))
  payload$dual <- rep(1e308, 2)
  expect_error(.warm_start_engine_predict(payload, matrix(1, 1, dimnames = list(NULL, "a"))), "predictions")
})

test_that("SVR uses each fit's retained count and audits unequal-fold OOF evidence", {
  skip_if_not_installed("e1071")
  withr::local_seed(25905)
  folds <- rep(1:3, c(5, 5, 7))
  x <- cbind(a = stats::rnorm(17), b = c(stats::rnorm(5), rep(0, 12)), c = stats::rnorm(17))
  x[6, "c"] <- NA
  z <- stats::rnorm(17)
  control <- list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1))
  tune <- .warm_start_svr_tune(x, z, folds, control)
  expect_identical(tune$retained_p, c(context_refit = 3L, inner_1 = 2L, inner_2 = 3L, inner_3 = 3L))
  expect_equal(unname(tune$actual_gamma[2, ]), tune$grid$gamma_multiplier / 2)
  expect_identical(tune$inner_preprocessing[[1]]$removed[["b"]], "constant")
  expect_identical(tune$traces$fold_sizes, c(5L, 5L, 7L))
  expect_equal(tune$traces$cvm, colSums(tune$traces$fold_mse * c(5, 5, 7)) / 17)
  expect_invisible(.validate_warm_start_svr_tuning(tune, 17L))
  for (fold in 1:3) {
    rows <- which(folds != fold)
    held <- which(folds == fold)
    p <- .warm_start_preprocess_fit(x[rows, , drop = FALSE])
    expect_identical(tune$inner_preprocessing[[fold]], p)
    train <- .warm_start_preprocess_apply(x[rows, , drop = FALSE], p)
    new <- .warm_start_preprocess_apply(x[held, , drop = FALSE], p)
    for (j in seq_len(nrow(tune$grid))) {
      backend <- e1071::svm(train, z[rows], type = "eps-regression", kernel = "radial", scale = FALSE,
        cost = tune$grid$cost[j], gamma = tune$grid$gamma_multiplier[j] / ncol(train),
        epsilon = 0.1, cross = 0, probability = FALSE)
      predicted <- as.numeric(predict(backend, new))
      expect_equal(tune$traces$candidate_oof[held, j], predicted, tolerance = 1e-12)
      expect_equal(tune$traces$fold_mse[fold, j], mean((predicted - z[held])^2))
    }
  }
  # An inner holdout cannot influence its training preprocessing or candidate fits.
  changed <- x
  changed[folds == 1L, "a"] <- changed[folds == 1L, "a"] + 100
  altered <- .warm_start_svr_tune(changed, z, folds, control)
  expect_identical(tune$inner_preprocessing[[1]], altered$inner_preprocessing[[1]])
  expect_false(identical(tune$traces$candidate_oof[folds == 1L, ],
    altered$traces$candidate_oof[folds == 1L, ]))
})

test_that("SVR outer contexts preserve outcome scales, OOF calibration and leakage boundaries", {
  skip_if_not_installed("e1071")
  f <- warm_svr_fixture()
  control <- list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1))
  model <- warm_svr_fit(f, engine_control = control)
  x <- as.matrix(f$x[, -1])
  for (i in seq_len(6)) {
    record <- if (i <= 5) model$validation$folds[[i]] else model
    rows <- if (i <= 5) which(f$plan$outer_foldid != i) else seq_len(nrow(x))
    theta <- f$theta[rows]
    z <- (theta - mean(theta)) / stats::sd(theta)
    expect_equal(record$tuning$oof$observed, z)
    expect_equal(record$outcome$mean, mean(theta))
    expect_equal(record$outcome$sd, stats::sd(theta))
    expect_identical(record$preprocessing, .warm_start_preprocess_fit(x[rows, , drop = FALSE]))
    calibration <- stats::lm.fit(cbind(1, record$tuning$selected$oof), z, tol = 1e-7)$coefficients
    expect_equal(record$calibration$intercept, unname(calibration[1]))
    expect_equal(record$calibration$slope, unname(calibration[2]))
    train <- .warm_start_preprocess_apply(x[rows, , drop = FALSE], record$preprocessing)
    s <- record$tuning$selected
    backend <- e1071::svm(train, z, type = "eps-regression", kernel = "radial", scale = FALSE,
      cost = s$cost, gamma = s$gamma_multiplier / ncol(train), epsilon = 0.1, cross = 0, probability = FALSE)
    expect_identical(record$engine_payload, .warm_start_svr_payload(backend, colnames(train)))
    if (i <= 5) {
      held <- which(f$plan$outer_foldid == i)
      new <- .warm_start_preprocess_apply(x[held, , drop = FALSE], record$preprocessing)
      expect_equal(record$predictions$raw_prediction, as.numeric(predict(backend, new)), tolerance = 1e-12)
      expect_equal(record$predictions$observed, (f$theta[held] - mean(theta)) / stats::sd(theta))
    }
  }
  p <- model$validation$predictions
  expect_equal(model$validation$metrics$rmse, sqrt(mean((p$calibrated_prediction - p$observed)^2)))
  expect_equal(model$validation$metrics$mae, mean(abs(p$calibrated_prediction - p$observed)))
  expect_equal(model$validation$metrics$pearson_r, stats::cor(p$calibrated_prediction, p$observed))
  altered <- f
  held <- which(f$plan$outer_foldid == 1L)
  altered$x$token_length_mean[held] <- altered$x$token_length_mean[held] + 100
  other <- warm_svr_fit(altered, engine_control = control)
  fields <- setdiff(names(model$validation$folds[[1]]), "predictions")
  expect_identical(other$validation$folds[[1]][fields], model$validation$folds[[1]][fields])
  expect_false(identical(other$validation$folds[[1]]$predictions, model$validation$folds[[1]]$predictions))
  expect_null(model$coefficients)
  expect_null(model$intercept)
  expect_false("n_nonzero" %in% names(model$training))
})

test_that("SVR preserves RNG and shared partitions despite backend RNG consumption", {
  skip_if_not_installed("e1071")
  f <- warm_svr_fixture()
  withr::local_seed(259)
  before <- .Random.seed
  model <- warm_svr_fit(f)
  expect_identical(model$tuning$control, .warm_start_svr_control(NULL))
  expect_identical(model$cv_plan, f$plan)
  expect_identical(.Random.seed, before)
  expect_identical(warm_svr_fit(f), model)
  original <- .warm_start_svr_fit
  local_mocked_bindings(.warm_start_svr_fit = function(...) {
    stats::runif(13)
    original(...)
  }, .warm_start_folds = function(...) stop("must not regenerate supplied plan"),
    .warm_start_require_glmnet = function(...) stop("must not load glmnet"),
    .warm_start_require_pls = function(...) stop("must not load pls"), .package = "pairwiseLLM")
  expect_identical(warm_svr_fit(f), model)
  expect_identical(.Random.seed, before)
})

test_that("SVR backend failures retain fitting context without candidate fallback", {
  skip_if_not_installed("e1071")
  local_mocked_bindings(.warm_start_svr_fit = function(...) rlang::abort("backend fixture failure"),
    .package = "pairwiseLLM")
  expect_error(warm_svr_fit(engine_control = list(cost = 1, gamma_multiplier = 1)),
    "Outer fold 1: Inner fold 1 SVR cost 1 gamma_multiplier 1: backend fixture failure")
  expect_error(.warm_start_engine_refit("svr_rbf", NULL, NULL, list(cost = 1, gamma_multiplier = 1)),
    "SVR context refit, cost 1 gamma_multiplier 1: backend fixture failure")
})

test_that("SVR full audits reject missing or inconsistent evidence and malformed payloads", {
  skip_if_not_installed("e1071")
  model <- warm_svr_fit(engine_control = list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1)))
  expect_invisible(.validate_warm_start_model(model))
  for (prefix in list(character(), "tuning", c("tuning", "traces"), c("validation", "folds", "1"))) {
    target <- if (!length(prefix)) {
      model
    } else if (tail(prefix, 1) == "1") {
      model$validation$folds[[1]]
    } else {
      model[[prefix]]
    }
    for (field in setdiff(names(target), c("coefficients", "intercept"))) {
      bad <- model
      if (!length(prefix)) bad[field] <- list(NULL) else if (tail(prefix, 1) == "1") {
        bad$validation$folds[[1]][field] <- list(NULL)
      } else {
        bad[[c(prefix, field)]] <- NULL
      }
      expect_error(.validate_warm_start_model(bad), info = paste(c(prefix, field), collapse = "$"))
    }
  }
  changes <- list(
    list(c("tuning", "retained_p"), 1L),
    list(c("tuning", "actual_gamma"), matrix(1, 6, 4)),
    list(c("tuning", "traces", "index"), 99L),
    list(c("tuning", "traces", "cvm"), rep(99, 4)),
    list(c("tuning", "selected", "cost"), 999),
    list(c("training", "hyperparameters", "epsilon"), 0.2),
    list(c("training", "hyperparameters", "gamma"), 999),
    list(c("training", "alpha"), 0.5),
    list(c("coefficients"), 0),
    list(c("intercept"), 0),
    list(c("calibration", "slope"), 99),
    list(c("validation", "metrics", "rmse"), 99),
    list(c("engine_payload", "gamma"), 99)
  )
  for (change in changes) {
    bad <- model
    bad[[change[[1]]]] <- change[[2]]
    expect_error(.validate_warm_start_model(bad), info = paste(change[[1]], collapse = "$"))
  }
  for (field in c("fold_mse", "candidate_oof")) {
    bad <- model
    bad$tuning$traces[[field]][1, 1] <- bad$tuning$traces[[field]][1, 1] + 0.5
    expect_error(.validate_warm_start_model(bad), "SVR tuning audit")
  }
  bad <- model
  bad$validation$folds[[1]]$tuning$selected$cost <- bad$validation$folds[[1]]$tuning$selected$cost + 1e-11
  expect_error(.validate_warm_start_model(bad), "SVR tuning audit")
  bad <- model
  bad$validation$folds[[1]]$engine_payload$gamma <- 999
  expect_error(.validate_warm_start_model(bad), "SVR context refit")
  bad <- model
  bad$validation$folds[[1]]$coefficients <- 0
  expect_error(.validate_warm_start_model(bad), "SVR context refit")
  payload <- model$engine_payload
  for (field in names(payload)) {
    bad <- payload
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_svr_payload(bad, model$preprocessing$retained))
  }
  for (dual in list(1, matrix(payload$dual), rep(NA_real_, length(payload$dual)))) {
    bad <- payload
    bad$dual <- dual
    expect_error(.validate_warm_start_svr_payload(bad, model$preprocessing$retained), "deployment payload")
  }
  bad <- payload
  colnames(bad$support_vectors) <- rev(colnames(bad$support_vectors))
  expect_error(.validate_warm_start_svr_payload(bad, model$preprocessing$retained), "deployment payload")
  expect_error(.warm_start_svr_payload(list(SV = payload$support_vectors,
    coefs = matrix(payload$dual), rho = NA_real_, gamma = payload$gamma), model$preprocessing$retained),
    "deployment payload")
})
