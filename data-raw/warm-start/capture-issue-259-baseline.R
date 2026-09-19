# Run explicitly from the repository root, before changing package runtime code:
# Rscript --vanilla data-raw/warm-start/capture-issue-259-baseline.R
# This captures synthetic regression evidence, never study inputs or real models.
local({
  baseline <- "013f869b1ba2272b88e85937023d57eb2c2c99fa"
  runtime <- c("R", "inst", "DESCRIPTION", "NAMESPACE")
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) > 1L) stop("Supply at most one empty capture directory.")
  destination <- if (length(arguments)) arguments[[1]] else
    file.path("tests", "testthat", "fixtures", "warm-start-legacy")
  output <- file.path(destination, "baseline-1.5.1.rds")
  if (file.exists(output)) stop("Refusing to overwrite frozen compatibility evidence: ", output)
  untracked <- system2("git", c("ls-files", "--others", "--", runtime), stdout = TRUE)
  # Ignore shell histories/compiled caches, but include ignored source/data files.
  untracked_source <- grep("(^R/.*[.][Rr]$)|(^inst/.*[.](R|r|py|csv|json|rds|lock|ya?ml)$)",
    untracked, value = TRUE)
  if (length(untracked_source) || !is.null(attr(untracked, "status"))) {
    stop("Capture requires no untracked runtime source/data files.")
  }
  status <- system2("git", c("diff", "--quiet", baseline, "--", runtime))
  if (status != 0L) stop("Capture requires unchanged baseline package runtime and resources.")
  for (package in c("pkgload", "glmnet", "withr")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install the capture dependency: ", package)
  }
  if (as.character(utils::packageVersion("glmnet")) != "5.0") {
    stop("This historical capture requires glmnet 5.0; do not refresh fixtures with a new engine.")
  }
  pkgload::load_all(quiet = TRUE, helpers = FALSE)
  withr::local_seed(259L, .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  fields <- warm_start_feature_schema()$feature
  synthetic_features <- function(n) {
    withr::local_seed(3103L)
    x <- as.data.frame(matrix(stats::runif(n * length(fields)), nrow = n))
    names(x) <- fields
    x$n_tokens <- seq_len(n) + 10L
    x$token_length_mean <- 2 + 10 * x$token_length_mean
    x$token_length_std <- 0.2 + x$token_length_std
    x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
    x <- data.frame(item_id = as.character(seq_len(n)), x, check.names = FALSE)
    attr(x, "warm_start_schema") <- "writing_features_v1"
    x
  }
  synthetic_theta <- function(x) {
    10 + 0.4 * x$n_tokens - 2 * x$token_length_mean + x$dale_chall_readability_score / 10
  }
  default <- synthetic_features(15L)
  missing <- synthetic_features(25L)
  missing$first_order_coherence[c(1L, 7L, 13L)] <- NA_real_
  missing$pos_prop_adj <- 0
  missing$pos_prop_adv <- c(1, rep(0, 24))
  one <- synthetic_features(20L)
  one[, setdiff(fields, "n_tokens")] <- 1
  inputs <- list(
    default = list(ids = default$item_id, theta = synthetic_theta(default),
      task_id = "synthetic-default", features = default),
    tied_missing = list(ids = missing$item_id, theta = round(synthetic_theta(missing) / 3),
      task_id = "synthetic-tied-missing", features = missing, seed = 37L,
      alpha_grid = c(0, 0.5, 1), lambda_rule = "lambda.min"),
    one_predictor = list(ids = one$item_id, theta = 2 + 3 * one$n_tokens + sin(one$n_tokens),
      task_id = "synthetic-one-predictor", features = one, seed = 11L, alpha_grid = c(0, 1))
  )
  metadata <- list(prepared_at = "2026-09-19T00:00:00Z",
    preparation_package_version = "1.5.1", extraction_provenance = c(status = "synthetic"))
  newdata <- synthetic_features(7L)
  newdata$first_order_coherence[c(2L, 5L)] <- NA_real_
  cases <- lapply(inputs, function(input) {
    model <- do.call(fit_warm_start_model, input)
    reduced <- prepare_warm_start_model(model, metadata, omit_audit = TRUE)
    list(input = input, model = model, reduced = reduced, prediction = predict(model, newdata))
  })
  ensemble <- ensemble_warm_start_models(default = cases$default$model,
    other_task = cases$tied_missing$model, repeated = cases$default$model)
  fixture <- list(baseline_commit = baseline, package_version = "1.5.1",
    engine_version = as.character(utils::packageVersion("glmnet")),
    r_version = as.character(getRversion()), rng_kind = RNGkind(),
    schema = as.data.frame(warm_start_feature_schema()), newdata = newdata, cases = cases,
    ensemble = ensemble, ensemble_prediction = predict(ensemble, newdata),
    reduced_ensemble = prepare_warm_start_model(ensemble, metadata, omit_audit = TRUE))
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fixture, output, version = 2, compress = "xz")
  cat("Captured", length(cases), "synthetic cases from", baseline, "\n")
})
