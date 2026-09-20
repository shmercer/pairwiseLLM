test_that("packaged warm-start example separates training and prediction items", {
  example <- readRDS(system.file("extdata", "warm-start-example.rds", package = "pairwiseLLM"))
  expect_length(intersect(example$training$text, example$new_items$text), 0L)
  expect_identical(anyDuplicated(c(example$training$text, example$new_items$text)), 0L)
  schema <- "writing_features_v2"
  expect_length(intersect(example$training$item_id, example$new_items$item_id), 0L)
  for (name in c("training", "new")) {
    x <- example[[paste0(name, "_features")]]
    items <- example[[if (name == "training") "training" else "new_items"]]
    expect_identical(x$item_id, items$item_id)
    expect_identical(names(x), c("item_id", warm_start_feature_schema(schema)$feature))
    expect_no_error(.validate_warm_start_features(x, items$item_id, schema))
  }
  expect_identical(example$provenance$schema_sha256,
    "d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492")
  expect_false("theta" %in% names(example$new_items))
})

test_that("guide chunks carry shared validation into new-item BTL and TrueSkill priors", {
  for (pkg in c("glmnet", "pls", "e1071", "withr", "knitr")) skip_if_not_installed(pkg)
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  path <- file.path(root, "vignettes", "adaptive-warm-start.Rmd")
  skip_if_not(file.exists(path), "Source vignette unavailable in installed-package tests")
  withr::local_seed(9106L)
  seed <- .Random.seed
  lines <- readLines(path)
  env <- new.env(parent = asNamespace("pairwiseLLM"))
  chunks <- c("example-data", "shared-plan", "train", "validation", "algorithm-ensemble",
    "algorithm-prediction", "algorithm-initialization", "storage", "resume", "reduction")
  for (label in chunks) {
    start <- grep(paste0("^```\\{r ", label, "[,}]"), lines)
    expect_length(start, 1L)
    end <- which(seq_along(lines) > start & lines == "```")[[1L]]
    invisible(capture.output(eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), env)))
  }
  expect_identical(.Random.seed, seed)
  components <- env$algorithm_ensemble$components
  for (model in components) expect_identical(model$cv_plan, env$plan)
  outer <- env$algorithm_ensemble$validation$predictions
  component_outer <- vapply(components,
    function(model) model$validation$predictions$calibrated_prediction, numeric(nrow(outer)))
  expect_equal(outer$calibrated_prediction, rowMeans(component_outer))
  expect_equal(env$algorithm_ensemble$validation$metrics$rmse,
    sqrt(mean((rowMeans(component_outer) - outer$observed)^2)))
  prior <- env$algorithm_prior
  expect_identical(prior$item_id, env$new_items$item_id)
  expect_identical(prior$scores, env$algorithm_predictions$ensemble_mean)
  expect_identical(prior$prior_sd, rep(0.5, nrow(env$new_items)))
  for (name in c("btl_state", "trueskill_state", "both_state")) {
    expect_identical(env[[name]]$predictive_prior, prior)
    expect_identical(env[[name]]$warm_start_pairs, env$btl_state$warm_start_pairs)
  }
  expect_false(env$btl_state$meta$trueskill_initialized_from_predictive)
  for (name in c("trueskill_state", "both_state")) {
    expect_equal(env[[name]]$trueskill_state$items$mu, 25 + (25 / 3) * prior$prior_mean)
    expect_identical(env[[name]]$trueskill_state$items$sigma,
      env$btl_state$trueskill_state$items$sigma)
  }
})

test_that("pinned extraction reproduces the guide's precomputed features", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", unset = "")
  skip_if(!nzchar(python), "Set PAIRWISELLM_TEST_PYTHON to an explicitly provisioned interpreter")
  skip_if_not_installed("reticulate", "1.41")
  # Existing spaCy/Click import deprecation is unrelated to feature extraction.
  status <- suppressWarnings(warm_start_python_status("writing_features_v2", python))
  expect_true(status$available, info = paste(status$problems, collapse = "; "))
  example <- readRDS(system.file("extdata", "warm-start-example.rds", package = "pairwiseLLM"))
  for (name in c("training", "new")) {
    items <- example[[if (name == "training") "training" else "new_items"]]
    expected <- example[[paste0(name, "_features")]]
    actual <- extract_warm_start_features(items$item_id, items$text,
      schema = "writing_features_v2", python = python)
    expect_identical(actual$item_id, expected$item_id)
    expect_identical(names(actual), names(expected))
    expect_identical(is.na(actual), is.na(expected))
    expect_equal(actual, expected, tolerance = 1e-7)
  }
})
