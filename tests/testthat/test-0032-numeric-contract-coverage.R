test_that("numeric summaries reject malformed inputs and preserve degenerate conventions", {
  expect_error(pairwiseLLM:::.pairwiseLLM_col_sds(1:3), "numeric matrix")
  expect_error(pairwiseLLM:::.pairwiseLLM_col_quantiles(1:3, 0.5), "numeric matrix")
  expect_error(pairwiseLLM:::.btl_mcmc_validate_draws(matrix(1, 1, 2)), "at least two")
  expect_error(pairwiseLLM:::.btl_mcmc_validate_draws(matrix(1, 2, 2)), "column names")
  expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(matrix(c(1, Inf, 2, 3), 2))))
  gd <- pairwiseLLM:::compute_gini_degree
  gp <- pairwiseLLM:::compute_gini_posA
  expect_true(is.na(gd(NULL)))
  expect_true(is.na(gp(NULL)))
  expect_true(is.na(gp(numeric())))
  expect_equal(gd(4), 0)
  expect_equal(gd(c(0, 1, 3)), 0.5)
  expect_equal(gp(c(0, 1, 3)), 0.5)
  expect_equal(gp(c(1, 2, 0), c(2, 4, 0)), 0)
  expect_equal(gp(2), 0)
  expect_equal(gp(c(0, 0)), 0)
  expect_true(is.na(gp(c(0, 0), c(0, 0))))
  expect_error(gp(1, c(1, 2)), "same length")
  expect_error(gp(1, -1), "non-negative")
  expect_error(pairwiseLLM:::compute_core_budget(max_cores = 0), "positive number")
  expect_true(is.na(pairwiseLLM:::.adaptive_safe_cor(c(NA, 1), c(2, 3))))
})

test_that("fit validation diagnoses malformed named locations and uncertainty", {
  ids <- c("a", "b")
  fit <- make_btl_fit_contract(ids)
  f <- pairwiseLLM:::validate_btl_fit_contract
  expect_invisible(f(fit, ids))
  for (field in c("theta_mean", "theta_sd")) {
    for (value in list("bad", c(a = 1), c(1, 2), c(b = 1, a = 2), c(a = 1, b = Inf))) {
      bad <- fit
      bad[[field]] <- value
      expect_error(f(bad, ids), field, fixed = TRUE)
    }
  }
  for (value in list("bad", matrix(1, 2, 2),
    matrix(1, 2, 2, dimnames = list(NULL, c("b", "a"))))) {
    bad <- fit
    bad$theta_draws <- value
    expect_error(f(bad, ids), "theta_draws", fixed = TRUE)
  }
  for (entry in list(list(field = "epsilon_mean", value = c(1, 2)),
    list(field = "epsilon_mean", value = Inf), list(field = "epsilon_draws", value = "bad"),
    list(field = "model_variant", value = c("btl", "btl_e")))) {
    bad <- fit
    bad[[entry$field]] <- entry$value
    expect_error(f(bad, ids), entry$field, fixed = TRUE)
  }
})

test_that("presentation rollback preserves repeated orientation counts and guards corruption", {
  state <- list(ids = c("a", "b"), unordered_count = integer(), pair_ordered_count = integer(),
    ordered_seen = logical())
  record <- pairwiseLLM:::btl_mcmc_record_presentation
  rollback <- pairwiseLLM:::btl_mcmc_rollback_presentation
  state <- record(record(state, "a", "b"), "a", "b")
  out <- rollback(state, "a", "b")
  expect_equal(unname(out$unordered_count), 1L)
  expect_equal(unname(out$pair_ordered_count), 1L)
  expect_true(all(out$ordered_seen))
  for (field in c("unordered_count", "pair_ordered_count")) {
    bad <- state
    bad[[field]][] <- 0L
    expect_error(rollback(bad, "a", "b"), "below zero")
  }
  state$ordered_seen <- new.env(parent = emptyenv())
  state$ordered_seen[["a:b"]] <- TRUE
  out <- rollback(state, "a", "b")
  expect_true(out$ordered_seen[["a:b"]])
  out$ordered_seen[["a:b"]] <- FALSE
  expect_error(rollback(out, "a", "b"), "below zero")
  seen <- new.env(parent = emptyenv())
  seen[[".hidden"]] <- TRUE
  expect_identical(pairwiseLLM:::btl_mcmc_results_seen_names(list(results_seen = seen)), ".hidden")
  blank <- list(results_seen = NULL, history_results = tibble::tibble(pair_uid = c(NA, "")))
  expect_identical(pairwiseLLM:::btl_mcmc_state_sync_results_seen(blank)$results_seen, logical())
})

test_that("calibration fingerprints identify function environments without evaluating functions", {
  f <- pairwiseLLM:::.adaptive_calibration_function_surface
  for (entry in list(list(env = emptyenv(), label = "emptyenv"),
    list(env = baseenv(), label = "baseenv"), list(env = globalenv(), label = "globalenv"),
    list(env = asNamespace("stats"), label = "namespace:stats"))) {
    fn <- function() NULL
    environment(fn) <- entry$env
    expect_identical(f(fn)$environment, entry$label)
  }
})
test_that("standalone diagnostic policy and inferred judge scopes remain explicit", {
  config <- list(require_divergences_zero = FALSE, max_rhat = 1.01, min_ess_bulk = 100)
  fit <- list(diagnostics = list(divergences = 2L, max_rhat = 1, min_ess_bulk = 200))
  expect_true(.btl_mcmc_standalone_fit_metrics(fit, config)$diagnostics_pass)
  config$require_divergences_zero <- TRUE
  expect_false(.btl_mcmc_standalone_fit_metrics(fit, config)$diagnostics_pass)
  expect_error(.btl_mcmc_standalone_fit_metrics(1, config), "fit_contract.*list")
  expect_error(.btl_mcmc_standalone_fit_metrics(fit, 1), "config.*list")
  results <- tibble::tibble(judge_scope = c("within", "obsolete", "link"))
  expect_identical(.btl_mcmc_inference_contract_from_results(results)$judge_scope_levels,
    c("within", "link"))
  expect_error(.btl_mcmc_inference_contract_from_results(results, 1), "inference_contract")
  expect_error(.btl_mcmc_inference_contract_from_results(results,
    list(judge_scope_levels = "obsolete")), "shared, within, or link")
  posterior <- list(item_log_list = list(tibble::tibble(item_id = c("a", "b"), theta_mean = c(-1, 1))))
  expect_equal(summarize_items(task09_link_state(), posterior = posterior), posterior$item_log_list[[1]][2:1, ])
})
