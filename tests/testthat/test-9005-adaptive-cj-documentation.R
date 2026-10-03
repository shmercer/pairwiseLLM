# Evaluate the published examples themselves, without copying their calculations.
cj_documentation_example <- function(labels, env = new.env(parent = globalenv())) {
  path <- testthat::test_path("..", "..", "vignettes", "adaptive-cj-workflow.Rmd")
  skip_if_not(file.exists(path), "Source vignette unavailable in installed-package tests")
  lines <- readLines(path, warn = FALSE)
  for (label in labels) {
    start <- grep(paste0("^```\\{r ", label, "[,}]"), lines)
    expect_length(start, 1L)
    end <- which(seq_along(lines) > start & lines == "```")[[1L]]
    invisible(capture.output(eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), env)))
  }
  env
}

test_that("integrated guide runs the public estimation and evaluation workflow", {
  skip_if_not_installed("withr")
  withr::local_seed(9005L)
  rng <- .Random.seed
  env <- new.env(parent = globalenv())
  fitted_inputs <- list()
  # Record the documented fitting inputs while executing the real public API.
  env$fit_bt_model <- function(bt_data, ...) {
    fitted_inputs[[length(fitted_inputs) + 1L]] <<- bt_data
    pairwiseLLM::fit_bt_model(bt_data, ...)
  }
  cj_documentation_example(c(
    "synthetic-data", "alpha-fit", "ssr-components", "adaptive-collection",
    "score-recovery", "scale-expansion", "heldout-prediction", "fixed-bootstrap",
    "adaptive-bootstrap", "lapse-fits"
  ), env)
  expect_identical(.Random.seed, rng)

  expect_identical(fitted_inputs[[1L]], env$fixed_data)
  expect_identical(fitted_inputs[[2L]], env$adaptive_data)
  expect_identical(fitted_inputs[[3L]], env$reference_data)
  expect_length(fitted_inputs, 5L)
  expect_false(any(vapply(fitted_inputs, identical, logical(1L), y = env$heldout_data)))
  expect_identical(env$example_design$outcome_seed, c(30601L, 30602L, 30603L))
  expect_identical(names(env$adaptive_data), c("object1", "object2", "result"))
  expect_equal(nrow(env$adaptive_data), 12L)
  expect_equal(nrow(pairwiseLLM::adaptive_results_history(env$initial)), 0L)
  expect_identical(env$initial$controller$pairing_strategy, "trueskill_p50")
  expect_identical(env$adaptive_fit$provenance$adjustment$method, "hamilton_alpha")
  expect_identical(env$adaptive_fit$alpha, 0.5)
  expect_true(all(env$selection_log$pairing_strategy == "trueskill_p50"))
  expect_equal(nrow(pairwiseLLM::adaptive_round_log(env$observed)), 0L)

  ssr <- env$ssr_components
  expect_equal(ssr$true_score_variance, ssr$observed_variance - ssr$mean_squared_se)
  expect_equal(ssr$ssr, env$fixed_fit$ssr$ssr)
  expect_equal(ssr$ssr, ssr$true_score_variance / ssr$observed_variance)
  expect_equal(env$scale_comparison$pearson_r_squared, c(1, 1))
  expect_equal(env$scale_comparison$rank_correlation, c(1, 1))
  expect_equal(env$scale_comparison$sd_ratio, c(1, 2))
  expect_equal(env$scale_comparison$centered_rmse[1L], 0)
  expect_gt(env$scale_comparison$centered_rmse[2L], 0)

  # Scoring edge cases verify interpretation without asserting method superiority.
  neutral <- env$prediction_metrics(rep(0.5, 4), c(0, 1, 1, 0))
  expect_equal(neutral$log_loss, log(2))
  expect_equal(neutral$brier, 0.25)
  expect_equal(neutral$accuracy, 0.5)
  impossible <- env$prediction_metrics(c(0, 1), c(1, 0))
  expect_identical(impossible$log_loss, Inf)
  expect_equal(impossible$brier, 1)
  expect_equal(impossible$accuracy, 0)
  expect_true(all(is.finite(as.matrix(env$predictive_results))))
  expect_identical(env$p_reference >= 0.5, env$p_expanded >= 0.5)
  expect_equal(env$predictive_results$accuracy[2L], env$predictive_results$accuracy[3L])
  expect_false(isTRUE(all.equal(env$p_reference, env$p_expanded)))

  for (boot in list(env$fixed_boot, env$adaptive_boot)) {
    expect_identical(boot$status, "complete")
    expect_equal(boot$n_failed, 0L)
    expect_equal(boot$theta$bootstrap_mean, unname(colMeans(boot$draws)))
    expect_equal(boot$theta$theta_corrected, 2 * boot$theta$theta_initial - boot$theta$bootstrap_mean)
    expect_equal(boot$theta$bootstrap_sd, unname(apply(boot$draws, 2, sd)))
    expect_equal(boot$theta$mcse_bias, boot$theta$bootstrap_sd / sqrt(boot$n_success))
  }
  expect_identical(env$fixed_boot$provenance$mode, "fixed")
  expect_identical(env$adaptive_boot$provenance$pairing_strategy, "trueskill_p50")
  expect_equal(env$adaptive_boot$replicates$n_comparisons, rep(12L, 4L))
  expect_gt(length(unique(env$adaptive_boot$replicates$schedule_digest)), 1L)
  for (field in c("selector", "outcome", "scheduling_refit", "estimator")) {
    expect_false(anyDuplicated(env$adaptive_boot$replicates[[field]]) > 0L)
  }

  expect_gt(env$lapse_interior$epsilon, 0)
  expect_lt(env$lapse_interior$epsilon, 1)
  expect_identical(env$lapse_interior$provenance$uncertainty$status, "valid")
  expect_true(all(is.finite(env$lapse_interior$theta$se)))
  expect_identical(env$lapse_boundary$epsilon, 0)
  expect_identical(env$lapse_boundary$provenance$convergence$status, "converged_boundary")
  expect_identical(env$lapse_boundary$provenance$uncertainty$status, "nonregular_boundary")
  expect_true(all(is.na(env$lapse_boundary$theta$se)))
  expect_null(env$lapse_boundary$vcov)
  expect_null(env$lapse_boundary$parameter_vcov)
  for (fit in env$lapse_examples) {
    expect_false(fit$ssr$valid)
    expect_identical(fit$ssr$status, "prototype_ssr_unavailable")
    expect_true(all(is.finite(predict(fit, newdata = env$lapse_pairs))))
  }

  # Repeating the published seeds reproduces data, selection, and correction.
  again <- cj_documentation_example(c(
    "synthetic-data", "alpha-fit", "adaptive-collection", "fixed-bootstrap"
  ))
  expect_identical(again$fixed_data, env$fixed_data)
  expect_identical(again$reference_data, env$reference_data)
  expect_identical(again$heldout_data, env$heldout_data)
  expect_identical(again$adaptive_data, env$adaptive_data)
  expect_equal(again$fixed_boot$theta, env$fixed_boot$theta)
  expect_identical(again$fixed_boot$replicates, env$fixed_boot$replicates)
})

test_that("optional conventional and Firth examples retain estimator provenance", {
  for (pkg in c("withr", "sirt", "brglm2")) skip_if_not_installed(pkg)
  withr::local_seed(9005L)
  env <- cj_documentation_example(c("synthetic-data", "conventional-fit", "firth-fit"))
  expect_identical(env$conventional_fit$engine, "sirt")
  expect_identical(env$conventional_fit$provenance$adjustment$eps, 0.3)
  expect_true(env$conventional_fit$ssr$agrees)
  expect_identical(env$firth_fit$engine, "brglm2")
  expect_identical(env$firth_fit$provenance$adjustment$type, "AS_mean")
  expect_true(env$firth_fit$ssr$valid)
})

test_that("guide guards optional engines and leaves Bayesian sampling unevaluated", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("withr")
  path <- testthat::test_path("..", "..", "vignettes", "adaptive-cj-workflow.Rmd")
  skip_if_not(file.exists(path), "Source vignette unavailable in installed-package tests")
  lines <- readLines(path, warn = FALSE)
  headers <- grep("^```\\{r ", lines, value = TRUE)
  guard <- function(label, withr_available, engine_available) {
    header <- headers[grepl(paste0("^```\\{r ", label, "[,}]"), headers)]
    expr <- sub("^.*eval=", "", sub("\\}$", "", header))
    env <- list2env(list(requireNamespace = function(package, ...) {
      if (package == "withr") withr_available else engine_available
    }))
    eval(parse(text = expr), env)
  }
  for (label in c("conventional-fit", "firth-fit")) {
    expect_false(guard(label, FALSE, TRUE))
    expect_false(guard(label, TRUE, FALSE))
    expect_true(guard(label, TRUE, TRUE))
  }
  expect_false(guard("bayesian-fit", TRUE, TRUE))
  opts <- knitr::opts_chunk$get()
  withr::defer(knitr::opts_chunk$restore(opts))
  env <- new.env(parent = globalenv())
  env$requireNamespace <- function(...) FALSE
  cj_documentation_example("setup", env)
  expect_false(knitr::opts_chunk$get("eval"))
})
