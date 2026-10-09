distribution_state_5116 <- function(mode = "both", strategy = "trueskill_pollitt") {
  prior <- make_warm_start_prior(c(a = -1, b = -0.2, c = 0.2, d = 1),
    prior_sd = c(d = 0.9, a = 0.1, c = 0.6, b = 0.3))
  adaptive_rank_start(prior$item_id, seed = 314L, warm_start_prior = prior,
    warm_start_mode = mode, warm_start_trueskill = "predictive_distribution",
    adaptive_config = list(pairing_strategy = strategy))
}

test_that("predictive distributions align by ID without changing legacy initialization", {
  withr::local_seed(314L)
  rng <- .Random.seed
  items <- data.frame(item_id = c("c", "a", "b"), mu = c(17, 25, 40), sigma = c(2, 3, 4))
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1), prior_sd = c(b = 0.6, c = 0.9, a = 0.1))
  legacy <- adaptive_rank_start(items, warm_start_prior = prior, warm_start_mode = "both")
  expect_identical(legacy$trueskill_state$items$sigma, items$sigma)
  expect_null(legacy$meta$warm_start_trueskill)
  expect_null(legacy$meta$trueskill_mapping)
  states <- lapply(c("trueskill_only", "both"), function(mode) {
    adaptive_rank_start(items, warm_start_prior = prior, warm_start_mode = mode,
      warm_start_trueskill = c(policy = "predictive_distribution"))
  })
  for (state in states) {
    expect_identical(state$trueskill_state$items$mu, 25 + (25 / 3) * c(1, -1, 0))
    expect_identical(state$trueskill_state$items$sigma, (25 / 3) * c(0.9, 0.1, 0.6))
    expect_identical(state$trueskill_state$beta, 25 / 6)
    expect_identical(state$predictive_prior, legacy$predictive_prior)
    expect_identical(state$warm_start_pairs, legacy$warm_start_pairs)
    mapping <- state$meta$trueskill_mapping
    expect_identical(mapping$format_version, 1L)
    expect_identical(mapping$sd_source, "prior_object")
    expect_identical(mapping$sd_rule, "per_item")
    expect_identical(mapping$calibration, "upstream_not_verified")
    expect_identical(mapping$clipping, "none")
    expect_identical(mapping$item_id, items$item_id)
  }
  expect_identical(states[[1]]$trueskill_state, states[[2]]$trueskill_state)
  expect_identical(states[[1]]$meta$trueskill_mapping$distribution_digest,
    states[[2]]$meta$trueskill_mapping$distribution_digest)
  expect_null(pairwiseLLM:::.warm_start_btl_prior_for_state(states[[1]]))
  expect_identical(pairwiseLLM:::.warm_start_btl_prior_for_state(states[[2]]), legacy$predictive_prior)
  for (mode in c("cold", "btl_only")) {
    args <- list(items = items, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior)
    expect_identical(do.call(adaptive_rank_start, args)$trueskill_state$items$sigma, items$sigma)
    expect_error(do.call(adaptive_rank_start, c(args,
      list(warm_start_trueskill = "predictive_distribution"))), "requires warm_start_mode")
  }
  for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
    state <- distribution_state_5116(strategy = strategy)
    expect_identical(state$meta$trueskill_mapping,
      distribution_state_5116()$meta$trueskill_mapping)
  }
  expect_identical(.Random.seed, rng)
})

test_that("distribution inputs fail before model resolution and never silently default SD", {
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
  testthat::local_mocked_bindings(.warm_start_prior_resolve_model = function(...) {
    stop("model resolution must not occur")
  }, .package = "pairwiseLLM")
  for (bad in list("", "mean_only", NA_character_, 1, TRUE, character(),
    c("predictive_distribution", "predictive_distribution"), matrix("predictive_distribution"))) {
    expect_error(adaptive_rank_start(prior$item_id, warm_start_model = "unused",
      warm_start_mode = "both", warm_start_trueskill = bad), "warm_start_trueskill")
  }
  expect_error(adaptive_rank_start(prior$item_id, warm_start_model = "unused",
    warm_start_mode = "both", warm_start_trueskill = "predictive_distribution"), "explicit.*warm_start_prior_sd")
  for (sd in list(0, -1, NA_real_, NaN, Inf, -Inf, c(1, 2), matrix(1), "0.5",
    c(a = 1, b = 1, missing = 1), c(a = 1, a = 2, c = 3))) {
    expect_error(adaptive_rank_start(prior$item_id, warm_start_model = "unused",
      warm_start_mode = "trueskill_only", warm_start_trueskill = "predictive_distribution",
      warm_start_prior_sd = sd), "SDs|IDs")
  }
  expect_error(adaptive_rank_start(prior$item_id, warm_start_prior = prior,
    warm_start_mode = "trueskill_only", warm_start_trueskill = "predictive_distribution",
    warm_start_prior_sd = 0.2), "require.*warm_start_model")
  for (ids in list(c("a", "b"), c("a", "b", "d"))) {
    expect_error(adaptive_rank_start(ids, warm_start_prior = prior, warm_start_mode = "both",
      warm_start_trueskill = "predictive_distribution"), "exactly")
  }
  for (value in c(0, -1, NA_real_, NaN, Inf)) {
    bad <- prior
    bad$prior_sd[[1L]] <- value
    expect_error(adaptive_rank_start(prior$item_id, warm_start_prior = bad,
      warm_start_mode = "both", warm_start_trueskill = "predictive_distribution"), "SD|prior_sd")
  }
  bad <- prior
  bad$prior_sd[[1L]] <- 0.7
  expect_error(adaptive_rank_start(prior$item_id, warm_start_prior = bad,
    warm_start_mode = "both", warm_start_trueskill = "predictive_distribution"), "digest")
  for (overflow_mean in c(FALSE, TRUE)) {
    huge <- make_warm_start_prior(if (overflow_mean) c(a = -1e308, b = 1e308) else c(a = -1, b = 1),
      prior_sd = if (overflow_mean) 0.5 else 1e308)
    expect_error(adaptive_rank_start(huge$item_id, warm_start_prior = huge,
      warm_start_mode = "both", warm_start_trueskill = "predictive_distribution"), "mapped TrueSkill")
  }
})

test_that("model input and the public wrapper retain explicit SD provenance", {
  features <- warm_core_features(6)
  items <- data.frame(item_id = features$item_id, text = "Synthetic text")
  model <- warm_bundle_model()
  count <- 0L
  original <- getS3method("predict", "pairwiseLLM_warm_model")
  testthat::local_mocked_bindings(predict.pairwiseLLM_warm_model = function(...) {
    count <<- count + 1L
    original(...)
  }, adaptive_rank_run_live = function(state, ...) state, .package = "pairwiseLLM")
  for (mode in c("both", "trueskill_only")) {
    for (sd in list(0.4, stats::setNames(seq(0.1, 0.6, 0.1), rev(items$item_id)))) {
      session <- file.path(withr::local_tempdir(), "session")
      out <- adaptive_rank(items, judge = function(...) NULL, progress = "none",
        warm_start_model = model, warm_start_features = features, warm_start_mode = mode,
        warm_start_prior_sd = sd, warm_start_trueskill = "predictive_distribution", session_dir = session)
      state <- out$state %||% out
      expect_identical(state$trueskill_state$items$sigma, (25 / 3) * state$predictive_prior$prior_sd)
      expect_identical(state$meta$trueskill_mapping$sd_source, "model_argument")
      expect_identical(state$meta$trueskill_mapping$sd_rule, if (length(sd) == 1L) "scalar" else "per_item")
      expect_identical(adaptive_rank_resume(session)$trueskill_state, state$trueskill_state)
      expect_error(adaptive_rank(items, judge = function(...) NULL, session_dir = session,
        warm_start_trueskill = "predictive_distribution", progress = "none"), "Omit all warm-start")
    }
  }
  expect_identical(count, 4L)
  legacy <- adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_mode = "both")
  expect_identical(legacy$predictive_prior$prior_sd, rep(0.5, 6L))
  expect_identical(legacy$trueskill_state$items$sigma, rep(25 / 3, 6L))
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_mode = "trueskill_only", warm_start_prior_sd = 0.4), "controls BTL")
})

test_that("distribution metadata is authoritative and rejected saves preserve disk state", {
  state <- distribution_state_5116()
  session <- withr::local_tempdir()
  save_adaptive_session(state, session)
  saved_bytes <- readBin(file.path(session, "state.rds"), "raw", n = 1e7)
  metadata <- readRDS(file.path(session, "metadata.rds"))
  expect_identical(validate_session_dir(session), metadata)
  expect_identical(metadata$trueskill_mapping, state$meta$trueskill_mapping)
  reject <- function(bad, pattern = "TrueSkill|trueskill|warm_start|exactly|digest") {
    expect_error(pairwiseLLM:::.warm_start_adaptive_validate(bad), pattern)
    expect_error(save_adaptive_session(bad, session, overwrite = TRUE), pattern)
    expect_identical(readBin(file.path(session, "state.rds"), "raw", n = 1e7), saved_bytes)
  }
  for (field in names(state$meta$trueskill_mapping)) {
    bad <- state
    bad$meta$trueskill_mapping[field] <- list(NULL)
    reject(bad)
  }
  for (field in c("warm_start_trueskill", "trueskill_mapping", "trueskill_initialized_from_predictive",
    "trueskill_warm_scale", "trueskill_mu0_used", "trueskill_sigma0_used")) {
    bad <- state
    bad$meta[field] <- list(NULL)
    reject(bad)
  }
  for (mode in list(NULL, "cold", "btl_only", "trueskill_only")) {
    bad <- state
    bad$meta$warm_start_mode <- mode
    reject(bad)
  }
  for (field in c("mu_offset", "scale", "beta")) {
    for (value in c(0, NaN, Inf)) {
      bad <- state
      bad$meta$trueskill_mapping[[field]] <- value
      reject(bad)
    }
  }
  bad <- state
  bad$trueskill_state$beta <- 1
  reject(bad)
  for (field in c("mu", "sigma")) {
    bad <- state
    bad$trueskill_state$items[[field]][[1L]] <- 2
    reject(bad)
  }
  bad <- state
  bad$trueskill_state$items$item_id[[1L]] <- "missing"
  reject(bad)
  bad <- state
  bad$predictive_prior <- pairwiseLLM:::.warm_start_prior_scope(state$predictive_prior, rev(state$item_ids))
  bad$meta$predictive_prior_digest <- bad$predictive_prior$digest
  reject(bad)
  for (field in c("warm_start_trueskill", "trueskill_mapping")) {
    bad <- metadata
    bad[[field]] <- NULL
    saveRDS(bad, file.path(session, "metadata.rds"))
    expect_error(validate_session_dir(session), "mapping integrity")
    expect_error(load_adaptive_session(session), "mapping integrity")
    saveRDS(metadata, file.path(session, "metadata.rds"))
    bad_state <- state
    bad_state$meta$warm_start_trueskill <- bad_state$meta$trueskill_mapping <- NULL
    saveRDS(bad_state, file.path(session, "state.rds"))
    expect_error(load_adaptive_session(session), "mapping integrity")
    saveRDS(state, file.path(session, "state.rds"))
  }
  expect_error(pairwiseLLM:::.warm_start_adaptive_init(state, prior = state$predictive_prior,
    mode = "both", trueskill = "predictive_distribution"), "Cannot reinitialize")
  expect_error(pairwiseLLM:::.warm_start_adaptive_init(state, prior = state$predictive_prior,
    mode = "both"), "Cannot reinitialize")
  old <- adaptive_rank_start(state$item_ids, warm_start_prior = state$predictive_prior)
  expect_error(pairwiseLLM:::.warm_start_adaptive_init(old, prior = old$predictive_prior,
    mode = "both", trueskill = "predictive_distribution"), "Cannot reinitialize")
})

test_that("fresh-process resume preserves distribution and subsequent selection", {
  withr::local_seed(315L)
  rng <- .Random.seed
  root <- withr::local_tempdir()
  cases <- list()
  for (mode in c("both", "trueskill_only")) {
    for (steps in c(0L, 2L, 5L)) {
      state <- distribution_state_5116(mode)
      if (steps > 0L) state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
        n_steps = steps, progress = "none")
      session <- file.path(root, paste(mode, steps, sep = "-"))
      save_adaptive_session(state, session)
      expected <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"), n_steps = 1L, progress = "none")
      cases[[length(cases) + 1L]] <- list(session = session, before = state, expected = expected)
    }
  }
  config <- file.path(root, "config.rds")
  result <- file.path(root, "result.rds")
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) getNamespaceInfo("pairwiseLLM", "path") else NULL
  saveRDS(list(dev_path = dev_path, libpaths = .libPaths(), sessions = lapply(cases, `[[`, "session"),
    result = result), config)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])",
    ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path, quiet=TRUE) else library(pairwiseLLM)",
    "stopifnot(!exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE))",
    "initial <- pairwiseLLM::adaptive_rank_start(c('x','y'), warm_start_mode='both',",
    "warm_start_prior=pairwiseLLM::make_warm_start_prior(c(x=-1,y=1)),",
    "warm_start_trueskill='predictive_distribution')",
    "out <- lapply(cfg$sessions, function(path) {",
    "s <- pairwiseLLM::adaptive_rank_resume(path)",
    "list(before=s, after=pairwiseLLM::adaptive_rank_run_live(s,",
    "function(...) list(is_valid=TRUE,Y=1L), n_steps=1L, progress='none')) })",
    "stopifnot(!exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE))",
    "saveRDS(out, cfg$result)", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config)), stdout = TRUE, stderr = TRUE))
  expect_true(is.null(attr(output, "status")), info = paste(output, collapse = "\n"))
  expect_true(file.exists(result))
  actual <- readRDS(result)
  for (i in seq_along(cases)) {
    for (part in c("before", "after")) {
      expected <- if (part == "before") cases[[i]]$before else cases[[i]]$expected
      expect_identical(actual[[i]][[part]]$trueskill_state, expected$trueskill_state)
      expect_identical(actual[[i]][[part]]$meta$trueskill_mapping, expected$meta$trueskill_mapping)
      expect_identical(actual[[i]][[part]]$predictive_prior, expected$predictive_prior)
      fields <- c("A_id", "B_id", "Y", "pairing_strategy", "target_distance")
      expect_identical(actual[[i]][[part]]$step_log[fields], expected$step_log[fields])
    }
  }
  expect_identical(.Random.seed, rng)
})
