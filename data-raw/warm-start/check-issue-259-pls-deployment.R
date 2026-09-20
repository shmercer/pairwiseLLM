# Offline maintainer check: a fresh R process with no accessible pls/glmnet/Python
# bridge package must load and deploy both full and reduced PLS artifacts.
# Rscript --vanilla data-raw/warm-start/check-issue-259-pls-deployment.R
local({
  pkgload::load_all(quiet = TRUE)
  source("tests/testthat/helper-warm-start-core.R")
  source("tests/testthat/helper-warm-start-pls.R")
  root <- tempfile("pls-deployment-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  library <- file.path(root, "library")
  dir.create(library)
  blocked <- c("pls", "glmnet", "reticulate", "pairwiseLLM")
  installed <- utils::installed.packages()
  installed <- installed[!duplicated(installed[, "Package"]) &
    !installed[, "Package"] %in% blocked, , drop = FALSE]
  for (i in seq_len(nrow(installed))) {
    package <- installed[i, "Package"]
    source <- file.path(installed[i, "LibPath"], package)
    if (!file.symlink(source, file.path(library, package))) stop("Cannot isolate package library: ", package)
  }
  for (schema in c("writing_features_v1", "writing_features_v2")) {
    fixture <- warm_pls_fixture(schema)
    model <- warm_pls_fit(fixture, engine_control = list(ncomp = 1:2))
    saveRDS(list(full = model, reduced = prepare_warm_start_model(model, omit_audit = TRUE),
      features = fixture$x, expected = predict(model, fixture$x),
      coefficients = warm_start_coefficients(model)), file.path(root, paste0(schema, ".rds")))
  }
  script <- file.path(root, "deploy.R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(args[1], include.site = FALSE)",
    'blocked <- c("pls", "glmnet", "reticulate")',
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
    '    stopifnot(identical(restored, model), identical(summary(model)$engine, "pls"))',
    "    prediction <- predict(restored, fixture$features)",
    "    stopifnot(identical(prediction, fixture$expected))",
    "    stopifnot(identical(warm_start_coefficients(restored), fixture$coefficients))",
    "    prior <- make_warm_start_prior(prediction)",
    "    stopifnot(identical(prior$scores, prediction$calibrated_prediction))",
    '    register_warm_start_model(restored, "pls", overwrite = TRUE)',
    '    stopifnot(identical(load_warm_start_model(name = "pls", source = "user"), model))',
    '    stopifnot(identical(list_warm_start_models("user")$engine, "pls"))',
    '    cat(schema, kind, "deployment passed without pls/glmnet/reticulate\n")',
    "  }",
    "}",
    "stopifnot(!any(blocked %in% loadedNamespaces()))"
  ), script)
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(library), shQuote(normalizePath(".")), shQuote(root)))
  if (status != 0L) stop("Isolated deployment check failed.")
})
