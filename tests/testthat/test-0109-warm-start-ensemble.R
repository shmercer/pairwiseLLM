# Fit separate tasks once per test file; core deployment assertions below use no engine.
ensemble_fixture <- function() {
  skip_if_not_installed("glmnet")
  x <- warm_core_features(15)
  a <- fit_warm_start_model(x$item_id, warm_core_theta(x), "assessment-a", features = x, alpha_grid = c(0, 1))
  y <- x
  y$first_order_coherence <- NA_real_
  b <- fit_warm_start_model(y$item_id, 500 + 20 * warm_core_theta(y), "assessment-b",
    features = y, alpha_grid = c(0, 1))
  list(x = x, a = a, b = b)
}

test_that("ensembles preserve independently scaled components and exact sample arithmetic", {
  f <- ensemble_fixture()
  e <- ensemble_warm_start_models(first = f$a, second = f$b)
  expect_s3_class(e, "pairwiseLLM_warm_ensemble")
  expect_false(identical(f$a$outcome$mean, f$b$outcome$mean))
  expect_false(identical(f$a$preprocessing$retained, f$b$preprocessing$retained))
  p <- predict(e, f$x)
  a <- predict(f$a, f$x)
  b <- predict(f$b, f$x)
  expect_identical(p$component_first, a$calibrated_prediction)
  expect_identical(p$component_second, b$calibrated_prediction)
  expect_identical(attr(p, "component_predictions"), list(first = a, second = b))
  expect_equal(p$ensemble_mean, (a$calibrated_prediction + b$calibrated_prediction) / 2)
  expect_equal(p$ensemble_sd, abs(a$calibrated_prediction - b$calibrated_prediction) / sqrt(2))
  three <- ensemble_warm_start_models(first = f$a, second = f$b, third = f$a)
  q <- predict(three, f$x)
  values <- cbind(a$calibrated_prediction, b$calibrated_prediction, a$calibrated_prediction)
  expect_equal(q$ensemble_mean, rowMeans(values))
  expect_equal(q$ensemble_sd, apply(values, 1, sd))
  expect_output(print(e), "not Bayesian prior SD")
  expect_output(print(p), "not Bayesian prior SD")
  expect_identical(summary(e)$audit_status, "full")
  expect_identical(summary(p)$components, c("first", "second"))
  expect_error(summary(e, extra = TRUE), "empty")
  expect_error(summary(p, extra = TRUE), "empty")
  expect_identical(names(ensemble_warm_start_models(f$a, f$b)$components), c("component_1", "component_2"))
  expect_error(ensemble_warm_start_models(f$a), "two")
  expect_error(ensemble_warm_start_models(a = f$a, a = f$b), "unique")
  expect_error(ensemble_warm_start_models(f$a, component_1 = f$b), "unique")
  expect_error(ensemble_warm_start_models(" " = f$a, b = f$b), "nonblank")
  expect_error(ensemble_warm_start_models(a = f$a, core = warm_core_model()), "core.*failed")
  expect_error(ensemble_warm_start_models(a = f$a, nested = e), "nested.*failed")
  expect_error(ensemble_warm_start_models(a = f$a, absent = NULL), "absent.*failed")
  for (field in c("schema", "outcome", "calibration", "features")) {
    bad <- f$b
    bad[[field]] <- NULL
    expect_error(ensemble_warm_start_models(a = f$a, broken = bad), "broken.*failed")
  }
  bad <- e
  bad$components$second$metadata <- list(notes = new.env())
  expect_error(predict(bad, f$x), "second.*failed")
  bad <- e
  bad$format_version <- 99L
  expect_error(predict(bad, f$x), "Unsupported.*ensemble")
  for (field in names(e)) {
    bad <- e
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_ensemble(bad), info = field)
  }
  bad <- e
  bad$weighting <- "stacked"
  expect_error(.validate_warm_start_ensemble(bad), "contract")
  bad <- e
  bad$metadata <- list(unknown = "x")
  expect_error(.validate_warm_start_ensemble(bad))
  expect_error(predict(e), "exactly one")
  expect_error(predict(e, f$x, texts = "x"), "exactly one")
  expect_error(predict(e, texts = "x"), "explicit ids")
  expect_error(predict(e, f$x, ids = f$x$item_id), "only to text")
  expect_error(predict(e, f$x, python = "unused"), "only to text")
  expect_error(predict(e, 1), "data frame")
  expect_error(predict(e, f$x, extra = TRUE), "empty")
  bad <- f$x
  bad$item_id[2] <- bad$item_id[1]
  expect_error(predict(e, bad), "unique|duplicat")
  bad <- f$x[, -2]
  expect_error(predict(e, bad), "feature")
  bad <- f$x
  attr(bad, "warm_start_schema") <- "future"
  expect_error(predict(e, bad), "schema")
  shuffled <- f$x[rev(seq_len(nrow(f$x))), ]
  expect_identical(predict(e, shuffled)$item_id, shuffled$item_id)
  calls <- 0L
  local_mocked_bindings(extract_warm_start_features = function(texts, ids, schema, python) {
    calls <<- calls + 1L
    expect_identical(ids, f$x$item_id)
    expect_identical(texts, rep("text", nrow(f$x)))
    f$x
  })
  expect_identical(predict(e, texts = rep("text", nrow(f$x)), ids = f$x$item_id), p)
  expect_identical(calls, 1L)
})

test_that("failed and misaligned predictions always identify their component", {
  f <- ensemble_fixture()
  e <- ensemble_warm_start_models(good = f$a, broken = f$b)
  original <- predict.pairwiseLLM_warm_model
  corruptions <- list(
    function(p) p[rev(seq_len(nrow(p))), ],
    function(p) p[-1, ],
    function(p) rbind(p, p[1, ]),
    function(p) {
      p$item_id[1] <- "extra"
      p
    },
    function(p) {
      p$item_id[1] <- p$item_id[2]
      p
    },
    function(p) {
      p$calibrated_prediction[1] <- NA_real_
      p
    },
    function(p) {
      p$calibrated_prediction[1] <- Inf
      p
    },
    function(p) {
      p$raw_prediction[1] <- NaN
      p
    },
    function(p) {
      p$calibrated_prediction <- NULL
      p
    },
    function(p) {
      p$calibrated_prediction <- as.character(p$calibrated_prediction)
      p
    },
    function(p) NULL,
    function(p) rlang::abort("engine failed"))
  for (corrupt in corruptions) {
    local_mocked_bindings(predict.pairwiseLLM_warm_model = function(object, newdata, ...) {
      p <- original(object, newdata)
      if (object$training$task_id == "assessment-b") corrupt(p) else p
    })
    expect_error(predict(e, f$x), "broken.*failed")
  }
})

test_that("ensemble storage, references, recursive reduction and registry are portable", {
  f <- ensemble_fixture()
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  local_mocked_bindings(.warm_start_require_glmnet = function() rlang::abort("must not train"),
    extract_warm_start_features = function(...) rlang::abort("must not extract"),
    warm_start_python_status = function(...) rlang::abort("must not initialize Python"))
  a <- prepare_warm_start_model(f$a, list(notes = "component metadata"))
  e <- ensemble_warm_start_models(a = a, b = f$b)
  path <- file.path(root, "ensemble.rds")
  for (artifact in list(e, ensemble_warm_start_models(a = a, b = prepare_warm_start_model(f$b, omit_audit = TRUE)),
                        prepare_warm_start_model(e, omit_audit = TRUE))) {
    save_warm_start_model(artifact, path, overwrite = TRUE)
    expect_identical(load_warm_start_model(path), artifact)
    expect_identical(predict(load_warm_start_model(path), f$x), predict(artifact, f$x))
    register_warm_start_model(artifact, "Test Ensemble", overwrite = TRUE)
    rows <- list_warm_start_models("user")
    expect_identical(rows$artifact_type, "ensemble")
    expect_identical(rows$component_count, 2L)
    expect_identical(rows$n, NA_integer_)
    expect_identical(rows$audit_status, .warm_start_audit_status(artifact))
    expect_identical(rows$calibration, "component_oof_linear")
    expect_identical(rows$validation[[1]], lapply(artifact$components, function(x) x$validation$metrics))
    expect_identical(load_warm_start_model(name = "test_ensemble", source = "user"), artifact)
    expect_error(register_warm_start_model(artifact, "test_ensemble"), "already exists")
    remove_warm_start_model("test ensemble")
  }
  reduced <- prepare_warm_start_model(e, omit_audit = TRUE)
  expect_identical(reduced$format_version, 1L)
  expect_identical(reduced$components$a$metadata, a$metadata)
  expect_identical(vapply(reduced$components, function(x) x$format_version, integer(1)), c(a = 2L, b = 2L))
  expect_null(reduced$components$a$validation$predictions)
  expect_identical(prepare_warm_start_model(reduced, omit_audit = TRUE), reduced)
  expect_identical(predict(reduced, f$x)$ensemble_mean, predict(e, f$x)$ensemble_mean)
  expect_identical(predict(reduced, f$x)$ensemble_sd, predict(e, f$x)$ensemble_sd)
  expect_false(identical(attr(predict(reduced, f$x), "warm_start_model"), attr(predict(e, f$x), "warm_start_model")))
  save_warm_start_model(a, path, overwrite = TRUE)
  register_warm_start_model(f$b, "model b")
  loaded <- ensemble_warm_start_models(a = path, b = list(name = "model_b", source = "user"))
  expect_identical(loaded$components, list(a = a, b = f$b))
  expect_identical(ensemble_warm_start_models(a = list(path = path), b = f$b)$components$a, a)
  expect_error(ensemble_warm_start_models(a = f$a, missing = "model_b"), "missing.*failed")
  expect_error(ensemble_warm_start_models(a = f$a, bad = list(foo = "x")), "bad.*failed")
  expect_error(ensemble_warm_start_models(a = f$a, bad = list(path = path, name = "model b")), "bad.*failed")
  remove_warm_start_model("model b")
  expect_identical(predict(loaded, f$x), predict(e, f$x))
})


test_that("negative calibration and permitted missing features retain component semantics", {
  f <- ensemble_fixture()
  b <- prepare_warm_start_model(f$b, omit_audit = TRUE)
  b$calibration$slope <- -abs(b$calibration$slope)
  e <- ensemble_warm_start_models(a = f$a, b = b)
  x <- f$x
  x$first_order_coherence[1] <- NA_real_
  p <- predict(e, x)
  expect_identical(p$component_b, predict(b, x)$calibrated_prediction)
  expect_equal(p$ensemble_mean, (predict(f$a, x)$calibrated_prediction + p$component_b) / 2)
  expect_identical(summary(e)$audit_status, "mixed")
  expect_identical(summary(prepare_warm_start_model(e, omit_audit = TRUE))$audit_status, "summary_only")
  local_mocked_bindings(predict.pairwiseLLM_warm_model = function(object, newdata, ...) {
    value <- if (object$training$task_id == "assessment-a") 1e308 else -1e308
    tibble::tibble(item_id = newdata$item_id, raw_prediction = value, calibrated_prediction = value)
  })
  expect_error(predict(e, x), "Nonfinite ensemble")
})

test_that("optional audited Python supports ensemble text prediction", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", "")
  skip_if(!nzchar(python) || !file.exists(python), "Set PAIRWISELLM_TEST_PYTHON to the audited interpreter.")
  skip_if_not_installed("reticulate")
  f <- ensemble_fixture()
  e <- ensemble_warm_start_models(a = f$a, b = f$b)
  texts <- c("Students support their argument with evidence.", "A clear example explains the reasoning.")
  ids <- c("one", "two")
  p <- withCallingHandlers(predict(e, texts = texts, ids = ids, python = python), warning = function(w) {
    if (startsWith(conditionMessage(w), "Importing 'parser.split_arg_string' is deprecated")) {
      invokeRestart("muffleWarning")
    }
  })
  features <- extract_warm_start_features(ids, texts, python = python)
  expect_identical(p, predict(e, features))
  expect_identical(p$item_id, ids)
})
