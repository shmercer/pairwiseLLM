test_that("MCMC resource detection requires parallelly only when used", {
  testthat::local_mocked_bindings(.btl_mcmc_parallelly_available = function() FALSE, .package = "pairwiseLLM")
  expect_error(pairwiseLLM:::.btl_mcmc_available_cores(), "install.packages",
    class = "pairwiseLLM_mcmc_dependency_missing")
  expect_silent(pairwiseLLM:::.btl_contract_mcmc_defaults(NULL))
})

test_that("allocation detection uses parallelly and safely reports failure", {
  skip_if_not_installed("parallelly")
  testthat::local_mocked_bindings(availableCores = function(logical, which) {
    expect_false(logical)
    expect_identical(which, "min")
    c(allocation = 3L)
  }, .package = "parallelly")
  expect_identical(pairwiseLLM:::.btl_mcmc_available_cores(), 3L)
  for (bad in list(NA_integer_, 0L, Inf, 1.5, numeric(), c(1L, 2L), "2", 2^31)) {
    testthat::with_mocked_bindings(
      availableCores = function(...) bad,
      expect_warning(expect_identical(pairwiseLLM:::.btl_mcmc_available_cores(), 1L),
        "using one CPU slot", class = "pairwiseLLM_mcmc_resource_detection"),
      .package = "parallelly"
    )
  }
  testthat::with_mocked_bindings(
    availableCores = function(...) stop("detection failed"),
    expect_warning(expect_identical(pairwiseLLM:::.btl_mcmc_available_cores(), 1L),
      "using one CPU slot"),
    .package = "parallelly"
  )
})

test_that("allocation affects scheduling without changing hardware-based chains", {
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = NA_character_)
  allocation <- 1L
  testthat::local_mocked_bindings(
    .btl_mcmc_available_cores = function() allocation,
    .btl_mcmc_detect_cores = function() list(physical = 32L, logical = 64L, effective = 32L),
    .package = "pairwiseLLM"
  )
  for (allocation in c(1L, 2L, 3L, 8L, 32L)) {
    cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(NULL)
    expect_identical(cfg$chains, 8L)
    expect_identical(cfg$cores_detected_physical, 32L)
    expect_identical(cfg$cores_detected_logical, 64L)
    expect_identical(cfg$cores_available, allocation)
    expect_identical(cfg$parallel_chains, if (allocation <= 2L) 1L else 2L)
    expect_identical(cfg$parallel_chains_requested, NA_integer_)
    expect_identical(cfg$concurrency_used, cfg$parallel_chains)
    expect_lte(cfg$concurrency_used, cfg$concurrency_budget)
    expect_lte(cfg$concurrency_budget, min(allocation, 2L))
  }
  allocation <- 2L
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, core_fraction = 1))
  expect_identical(cfg$chains, 4L)
  expect_identical(cfg$parallel_chains, 2L)
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 1L, core_fraction = 1))
  expect_identical(cfg$parallel_chains, 1L)
})

test_that("explicit requests preserve headroom opt-out but cannot exceed allocation", {
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = NA_character_)
  allocation <- 8L
  testthat::local_mocked_bindings(.btl_mcmc_available_cores = function() allocation, .package = "pairwiseLLM")
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(
    list(chains = 4L, parallel_chains = 4L, core_fraction = 0.1))
  expect_identical(cfg$parallel_chains_requested, 4L)
  expect_identical(cfg$parallel_chains, 4L)
  expect_identical(cfg$concurrency_budget, 8L)
  expect_identical(cfg$concurrency_used, 4L)
  allocation <- 2L
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 2L, parallel_chains = 9L))
  expect_identical(cfg$parallel_chains_requested, 9L)
  expect_identical(cfg$parallel_chains, 2L)
  allocation <- 1L
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, parallel_chains = 4L)),
    "exceeds the available CPU budget", class = "pairwiseLLM_mcmc_resource_limit")
})

test_that("check constraints cap explicit requests independently of detection", {
  testthat::local_mocked_bindings(.btl_mcmc_available_cores = function() 32L, .package = "pairwiseLLM")
  for (flag in c("TRUE", "true", "warn", "1", "unexpected")) {
    withr::with_envvar(c(`_R_CHECK_LIMIT_CORES_` = flag), {
      cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L))
      expect_identical(cfg$cores_available, 2L)
      expect_lte(cfg$concurrency_used, 2L)
      expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, parallel_chains = 4L)),
        class = "pairwiseLLM_mcmc_resource_limit")
    })
  }
  for (flag in c("", "FALSE", "false", "F", "0")) {
    withr::with_envvar(c(`_R_CHECK_LIMIT_CORES_` = flag), {
      cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, parallel_chains = 4L))
      expect_identical(cfg$concurrency_used, 4L)
    })
  }
})

test_that("chain and thread parallelism share one CPU budget", {
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = NA_character_)
  testthat::local_mocked_bindings(.btl_mcmc_available_cores = function() 8L, .package = "pairwiseLLM")
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, threads_per_chain = 2L))
  expect_identical(cfg$parallel_chains, 1L)
  expect_identical(cfg$concurrency_used, 2L)
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, threads_per_chain = 3L)),
    "automatic CPU budget", class = "pairwiseLLM_mcmc_resource_limit")
  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(
    list(chains = 4L, parallel_chains = 2L, threads_per_chain = 4L))
  expect_identical(cfg$concurrency_used, 8L)
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(
    list(chains = 4L, parallel_chains = 3L, threads_per_chain = 4L)), "exceeds")
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(
    list(chains = 1L, parallel_chains = 1L, threads_per_chain = 9L)), "exceeds")
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(
    list(chains = .Machine$integer.max, parallel_chains = .Machine$integer.max,
      threads_per_chain = .Machine$integer.max)), "exceeds")
})

test_that("resource settings reject malformed values before integer coercion", {
  for (name in c("chains", "parallel_chains", "threads_per_chain")) {
    for (bad in list(NA_real_, Inf, 0, -1, 1.5, 2^31, numeric(), c(1L, 2L), TRUE, "2", matrix(1))) {
      expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(stats::setNames(list(bad), name)),
        "must be a positive integer")
    }
  }
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config("bad"), "must be a list")
  for (bad in list(NA_real_, Inf, 0, -1, 2, c(0.5, 0.8), "0.5")) {
    expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(core_fraction = bad)), "must be in")
  }
})

test_that("resource metadata survives fit normalization and historical serialization", {
  fields <- c("cores_available", "parallel_chains_requested", "concurrency_budget", "concurrency_used")
  theta <- cbind(A = c(-0.5, -0.4, -0.6), B = c(0.5, 0.4, 0.6))
  cfg <- list(cores_available = 8L, parallel_chains_requested = 4L,
    concurrency_budget = 8L, concurrency_used = 4L)
  fit <- pairwiseLLM:::build_btl_fit_contract(theta_draws = theta, model_variant = "btl", mcmc_config_used = cfg)
  expect_identical(fit$mcmc_config_used[fields], cfg)
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(fit, path)
  expect_identical(readRDS(path)$mcmc_config_used[fields], cfg)
  fit$mcmc_config_used[fields] <- NULL
  saveRDS(fit, path)
  fit <- readRDS(path)
  expect_silent(pairwiseLLM:::validate_btl_fit_contract(fit, ids = c("A", "B")))
  old <- pairwiseLLM:::.btl_contract_mcmc_defaults(fit$mcmc_config_used)
  expect_identical(old[fields], stats::setNames(rep(list(NA_integer_), 4L), fields))
})

test_that("historical log readers fill allocation fields without modifying saved objects", {
  fields <- c("mcmc_cores_available", "mcmc_parallel_chains_requested",
    "mcmc_concurrency_budget", "mcmc_concurrency_used")
  schema <- pairwiseLLM:::.adaptive_refit_summary_schema(include_optional = FALSE)
  expect_true(all(fields %in% names(schema)))
  for (field in fields) expect_identical(schema[[field]], integer())
  for (n in c(0L, 2L)) {
    old <- list(round_log = tibble::tibble(refit_id = seq_len(n)))
    original <- old
    for (reader in list(pairwiseLLM::summarize_refits, pairwiseLLM::adaptive_round_log)) {
      log <- reader(old)
      expect_identical(log$refit_id, seq_len(n))
      for (field in fields) expect_identical(log[[field]], rep(NA_integer_, n))
    }
    expect_identical(old, original)
  }
})

test_that("all variants forward scheduling without changing sampling inputs", {
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = NA_character_)
  allocation <- 2L
  captured <- list()
  model_fn <- function(stan_file, cpp_options) {
    expect_true(cpp_options$stan_threads)
    list(sample = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      list(
        draws = function(variables, format) {
          cbind(`theta[1]` = c(-0.5, -0.4, -0.6, -0.3),
            `theta[2]` = c(0.5, 0.4, 0.6, 0.3), epsilon = rep(0.1, 4), beta = rep(0, 4))
        },
        diagnostic_summary = function() data.frame(num_divergent = 0L),
        summary = function(variables) data.frame(rhat = 1, ess_bulk = 1000, ess_tail = 1000)
      )
    })
  }
  sampler <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive
  testthat::local_mocked_bindings(
    .btl_mcmc_require_cmdstanr = function() NULL,
    .btl_mcmc_available_cores = function() allocation,
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      sampler(bt_data, config, seed, model_fn = model_fn)
    },
    .package = "pairwiseLLM"
  )
  results <- pairwiseLLM::build_btl_results_data(
    data.frame(ID1 = c("A", "A"), ID2 = c("B", "B"), better_id = c("A", "B")))
  data <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(results, c("A", "B"))
  settings <- list(chains = 4L, core_fraction = 1, iter_warmup = 5L, iter_sampling = 7L, seed = 123L)
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    cfg <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = variant, cmdstan = settings))
    for (allocation in c(1L, 2L)) {
      direct <- sampler(data, cfg, seed = 123L, model_fn = model_fn)
      standalone <- pairwiseLLM::fit_bayes_btl_mcmc(results, c("A", "B"), variant, cmdstan = settings)
      expect_identical(direct$mcmc_config_used$chains, 4L)
      expect_identical(standalone$fit$mcmc_config_used$cores_available, allocation)
      expect_identical(standalone$round_log$mcmc_concurrency_used, allocation)
      expect_identical(pairwiseLLM::summarize_refits(standalone, include_optional = FALSE)$mcmc_cores_available,
        allocation)
      for (args in utils::tail(captured, 2L)) {
        expect_identical(args$chains, 4L)
        expect_identical(args$parallel_chains, allocation)
        expect_identical(args$threads_per_chain, 1L)
        expect_identical(args$iter_warmup, 5L)
        expect_identical(args$iter_sampling, 7L)
        expect_identical(args$seed, 123L)
        expect_identical(args$data$prior_mean, c(0, 0))
        expect_identical(args$data$prior_sd, c(1, 1))
        expect_identical(args$data$Y, data$Y)
      }
    }
  }
  allocation <- 4L
  settings$parallel_chains <- 4L
  messages <- character()
  withCallingHandlers(
    pairwiseLLM::fit_bayes_btl_mcmc(results, c("A", "B"), "btl", cmdstan = settings),
    pairwiseLLM_mcmc_explicit_concurrency = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(messages, 1L)
  expect_match(messages, "user-requested MCMC concurrency: 4 parallel chains x 1 threads")
  expect_silent(pairwiseLLM:::.btl_mcmc_resource_message(list()))
})
