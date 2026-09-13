# Maintainer-only sensitivity smoke study. No provider calls or CJ estimation.
# Run from the source checkout: Rscript --vanilla <this-file> /tmp/rubric-smoke

rubric_smoke_scenarios <- function() {
  data.frame(scenario = sprintf("scenario_%02d", 1:16), seed = 92700L + 1:16,
    K = rep(3:6, 4L), n = rep(c(400L, 800L, 800L, 1600L), each = 4L),
    shape = rep(c("linear", "compression", "s_shape", "weak"), each = 4L),
    imbalanced = rep(c(FALSE, TRUE), 8L), spacing = rep(c(1, 2), each = 2L, times = 4L),
    labeled_fraction = rep(c(0.05, 0.10, 0.20, 0.30, 0.50, 0.20, 0.30, 0.50), 2L),
    sampling = rep(c("random", "stratified", "central", "stratified"), 4L),
    theta_noise_sd = rep(c(0, 0.1, 0.4, 0), 4L), stringsAsFactors = FALSE)
}

rubric_smoke_data <- function(scenario) {
  withr::with_seed(scenario$seed, {
    latent <- sort(stats::runif(scenario$n, -3, 3))
    eta <- switch(scenario$shape, linear = 1.2 * latent,
      compression = 3 * tanh(latent), s_shape = 0.35 * latent + 0.5 * latent^3,
      weak = 0.03 * latent)
    thresholds <- seq(-scenario$spacing, scenario$spacing, length.out = scenario$K - 1L) +
      if (scenario$imbalanced) 3 else 0
    cumulative <- stats::plogis(outer(eta, thresholds, function(e, tau) tau - e))
    probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
    labels <- as.integer(1L + rowSums(cumulative < stats::runif(scenario$n)))
    theta <- latent + stats::rnorm(scenario$n, sd = scenario$theta_noise_sd)
    ids <- sprintf("item_%04d", seq_len(scenario$n))
    # Synthetic symmetric draws encode declared precision; no actual MCMC is implied.
    draws <- outer(c(-1, 1) / sqrt(2), theta, function(offset, score) {
      score + offset * scenario$theta_noise_sd
    })
    colnames(draws) <- ids
    fit <- pairwiseLLM:::build_btl_fit_contract(draws, model_variant = "btl",
      diagnostics = list(diagnostics_pass = NA, source = "synthetic_precision_sensitivity"),
      diagnostics_pass = NA)
    cj <- list(fit = fit, fits = list(fit), trait = "simulated_trait",
      provenance = list(source = "synthetic_completed_scores", seed = scenario$seed),
      item_log_list = list(data.frame(refit_id = 1L, ID = ids,
        theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
      round_log = data.frame(round_id = 1L, model_variant = "btl", reliability_EAP = NA_real_))
    n_train <- as.integer(round(scenario$n * scenario$labeled_fraction))
    order_theta <- order(theta, ids)
    train <- switch(scenario$sampling,
      random = sample.int(scenario$n, n_train),
      stratified = order_theta[round(seq(1, scenario$n, length.out = n_train))],
      central = order(abs(theta), ids)[seq_len(n_train)])
    list(cj = cj, rubric = data.frame(item_id = ids, rubric_score = labels),
      train = sort(train), truth = probabilities, thresholds = thresholds,
      latent = latent, theta = theta)
  })
}

rubric_smoke_fit <- function(data, scenario, method) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers({
    fit <- pairwiseLLM::fit_rubric_calibration(data$cj,
      rubric = if (method == "percentile") NULL else data$rubric[data$train, ],
      method = method, levels = seq_len(scenario$K))
    held <- setdiff(seq_len(scenario$n), data$train)
    evaluation <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric[held, ], diagnostics = TRUE)
    prediction <- stats::predict(fit)
    probabilities <- if (method == "percentile") NULL else do.call(rbind, prediction$probabilities)
    slope_error <- threshold_error <- NA_real_
    if (method == "ordinal_linear" && scenario$shape %in% c("linear", "weak")) {
      true_slope <- if (scenario$shape == "linear") 1.2 else 0.03
      raw_slope <- fit$backend$slope / fit$transformation$scale
      raw_thresholds <- fit$backend$thresholds + raw_slope * fit$transformation$center
      slope_error <- raw_slope - true_slope
      threshold_error <- max(abs(raw_thresholds - data$thresholds))
    }
    converged <- method == "percentile" || isTRUE(fit$diagnostics$ordinal$converged)
    list(status = if (converged) "fitted" else "nonconverged", reason = if (converged) NA_character_ else
      "Calibration convergence/identification requires review.",
      evaluation = evaluation, diagnostics = fit$diagnostics, calibration_range = fit$calibration_range,
      transformation = fit$transformation, slope_error = slope_error, threshold_error = threshold_error,
      probability_mae = if (is.null(probabilities)) NA_real_ else
        mean(abs(probabilities[held, ] - data$truth[held, ])),
      extreme_category_accuracy = vapply(c(1L, scenario$K), function(category) {
        selected <- held[data$rubric$rubric_score[held] == category]
        if (!length(selected)) NA_real_ else mean(prediction$category[selected] == category)
      }, numeric(1L)))
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) list(status = "failed", reason = conditionMessage(e)))
  result$warnings <- unique(warnings)
  result$category_counts <- tabulate(data$rubric$rubric_score[data$train], scenario$K)
  result
}

run_rubric_smoke <- function(output_dir, scenarios = rubric_smoke_scenarios()) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L, nzchar(output_dir))
  if (!requireNamespace("withr", quietly = TRUE)) stop("Install optional package 'withr' to run this study.")
  withr::local_envvar(TZ = "UTC")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  details <- rows <- list()
  for (i in seq_len(nrow(scenarios))) {
    scenario <- scenarios[i, ]
    data <- rubric_smoke_data(scenario)
    for (method in c("percentile", "ordinal_linear", "ordinal_monotone")) {
      key <- paste(scenario$scenario, method, sep = "/")
      result <- rubric_smoke_fit(data, scenario, method)
      details[[key]] <- result
      metrics <- result$evaluation$metrics
      if (is.null(metrics)) metrics <- data.frame(n = scenario$n - length(data$train),
        rps = NA_real_, log_loss = NA_real_, exact_accuracy = NA_real_, within_one_accuracy = NA_real_,
        mae = NA_real_, quadratic_weighted_kappa = NA_real_)
      rows[[key]] <- cbind(scenario, data.frame(method = method, status = result$status,
        reason = result$reason, warnings = paste(result$warnings, collapse = " | "),
        n_train = length(data$train), category_counts = paste(result$category_counts, collapse = ","),
        n_extrapolated = if (is.null(result$evaluation)) NA_integer_ else result$evaluation$metadata$n_extrapolated,
        probability_mae = if (is.null(result$probability_mae)) NA_real_ else result$probability_mae,
        slope_error = if (is.null(result$slope_error)) NA_real_ else result$slope_error,
        threshold_max_error = if (is.null(result$threshold_error)) NA_real_ else result$threshold_error), metrics)
      message(key, ": ", result$status)
    }
  }
  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL
  versions <- vapply(c("pairwiseLLM", "ordinal", "mgcv", "withr"), function(package) {
    if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package)) else "unavailable"
  }, character(1L))
  utils::write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)
  saveRDS(list(scenarios = scenarios, summary = summary, details = details,
    versions = versions, r_version = R.version.string), file.path(output_dir, "results.rds"))
  writeLines(capture.output(utils::sessionInfo()), file.path(output_dir, "session-info.txt"))
  invisible(summary)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Supply an output directory, for example /tmp/rubric-smoke.")
  pkgload::load_all(quiet = TRUE)
  run_rubric_smoke(args[[1L]])
}
