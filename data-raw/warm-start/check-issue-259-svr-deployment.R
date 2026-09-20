# Offline maintainer check: a fresh R process with no accessible e1071/pls/glmnet/Python
# bridge package must load and deploy both full and reduced SVR artifacts.
# Rscript --vanilla data-raw/warm-start/check-issue-259-svr-deployment.R
local({
  pkgload::load_all(quiet = TRUE)
  source("tests/testthat/helper-warm-start-core.R")
  source("tests/testthat/helper-warm-start-svr.R")
  root <- tempfile("svr-deployment-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  library <- file.path(root, "library")
  dir.create(library)
  blocked <- c("e1071", "pls", "glmnet", "reticulate", "pairwiseLLM")
  installed <- utils::installed.packages()
  installed <- installed[!duplicated(installed[, "Package"]) &
    !installed[, "Package"] %in% blocked, , drop = FALSE]
  for (i in seq_len(nrow(installed))) {
    package <- installed[i, "Package"]
    source <- file.path(installed[i, "LibPath"], package)
    if (!file.symlink(source, file.path(library, package))) stop("Cannot isolate package library: ", package)
  }
  for (schema in c("writing_features_v1", "writing_features_v2")) {
    fixture <- warm_svr_fixture(schema)
    model <- warm_svr_fit(fixture, engine_control = list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1)))
    saveRDS(list(full = model, reduced = prepare_warm_start_model(model, omit_audit = TRUE),
      features = fixture$x, expected = predict(model, fixture$x),
      engine_payload = model$engine_payload), file.path(root, paste0(schema, ".rds")))
  }
  script <- file.path(root, "deploy.R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(args[1], include.site = FALSE)",
    'blocked <- c("e1071", "pls", "glmnet", "reticulate")',
    "stopifnot(!any(vapply(blocked, requireNamespace, logical(1), quietly = TRUE)))",
    "pkgload::load_all(args[2], quiet = TRUE)",
    'Sys.setenv(R_USER_DATA_DIR = file.path(args[3], "registry"))',
    'for (schema in c("writing_features_v1", "writing_features_v2")) {',
    '  fixture <- readRDS(file.path(args[3], paste0(schema, ".rds")))',
    '  for (kind in c("full", "reduced")) {',
    "    model <- fixture[[kind]]",
    '    path <- file.path(args[3], "model.rds")',
    "    save_warm_start_model(model, path, overwrite = TRUE)",
    "    restored <- load_warm_start_model(path)",
    '    stopifnot(identical(restored, model), identical(summary(model)$engine, "svr_rbf"))',
    "    prediction <- predict(restored, fixture$features)",
    "    stopifnot(identical(prediction, fixture$expected))",
    "    stopifnot(identical(restored$engine_payload, fixture$engine_payload))",
    "    error <- tryCatch(warm_start_coefficients(restored), error = identity)",
    '    stopifnot(inherits(error, "pairwiseLLM_warm_nonlinear_coefficients"))',
    "    prior <- make_warm_start_prior(prediction)",
    "    stopifnot(identical(prior$scores, prediction$calibrated_prediction))",
    '    register_warm_start_model(restored, "svr", overwrite = TRUE)',
    '    stopifnot(identical(load_warm_start_model(name = "svr", source = "user"), model))',
    '    stopifnot(identical(list_warm_start_models("user")$engine, "svr_rbf"))',
    '    cat(schema, kind, "deployment passed without e1071/pls/glmnet/reticulate\n")',
    "  }",
    "}",
    "stopifnot(!any(blocked %in% loadedNamespaces()))"
  ), script)
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(library), shQuote(normalizePath(".")), shQuote(root)))
  if (status != 0L) stop("Isolated deployment check failed.")
})
