test_that("warm modes have contextual defaults and reject contradictory inputs", {
  ids <- c("a", "b", "c")
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
  expect_identical(adaptive_rank_start(ids)$meta$warm_start_mode, "cold")
  expect_identical(adaptive_rank_start(ids, warm_start_prior = prior)$meta$warm_start_mode,
    "btl_only")
  for (bad in list("", " ", NA_character_, c("cold", "both"), 1, TRUE,
                   character(), "Both", matrix("both"))) {
    expect_error(adaptive_rank_start(ids, warm_start_mode = bad), "warm_start_mode")
  }
  expect_error(adaptive_rank_start(ids, warm_start_prior = prior, warm_start_mode = "cold"),
    "cannot be combined")
  expect_error(adaptive_rank_start(ids, warm_start_prior = prior,
    warm_start_mode = c(destination = "cold")), "cannot be combined")
  for (mode in c("btl_only", "trueskill_only", "both")) {
    expect_error(adaptive_rank_start(ids, warm_start_mode = mode), "requires")
  }
  expect_error(adaptive_rank_start(ids, warm_start_prior = prior,
    warm_start_mode = "trueskill_only", warm_start_prior_sd = 0.5), "controls BTL")
  expect_error(adaptive_rank_start(ids, warm_start_prior = prior,
    warm_start_mode = "both", warm_start_features = data.frame()), "require")
  expect_error(adaptive_rank_start(ids, warm_start_prior = prior,
    warm_start_model = "unused", warm_start_mode = "both"), "only one")
})

test_that("TrueSkill mapping aligns IDs exactly and preserves constructor sigma", {
  withr::local_seed(910)
  rng <- .Random.seed
  items <- data.frame(item_id = c("c", "a", "b"), mu = c(17, 25, 40), sigma = c(2, 25 / 3, 4))
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1), prior_sd = c(0.1, 2, 9))
  states <- lapply(c("cold", "btl_only", "trueskill_only", "both"), function(mode) {
    adaptive_rank_start(items, seed = 12, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior)
  })
  expect_identical(.Random.seed, rng)
  expect_identical(states[[1]]$trueskill_state, states[[2]]$trueskill_state)
  expect_identical(states[[3]]$trueskill_state, states[[4]]$trueskill_state)
  expect_identical(states[[3]]$trueskill_state$items$mu, 25 + (25 / 3) * c(1, -1, 0))
  for (k in seq_along(states)) {
    state <- states[[k]]
    expect_identical(state$trueskill_state$items$sigma, c(2, 25 / 3, 4))
    expect_identical(state$trueskill_state$beta, 25 / 6)
    expect_identical(state$warm_start_pairs, states[[1]]$warm_start_pairs)
    expect_identical(state$warm_start_idx, 1L)
    expect_identical(state$meta$trueskill_initialized_from_predictive, k >= 3L)
  }
  expect_identical(states[[4]]$meta$trueskill_warm_scale, 1.0)
  expect_identical(states[[4]]$meta$trueskill_mu0_used, 25)
  expect_identical(states[[4]]$meta$trueskill_sigma0_used, 25 / 3)
  changed_sd <- make_warm_start_prior(c(a = -1, b = 0, c = 1), prior_sd = 20)
  changed <- adaptive_rank_start(items, warm_start_prior = changed_sd, warm_start_mode = "both")
  expect_identical(changed$trueskill_state, states[[4]]$trueskill_state)
  for (bad_ids in list(c("a", "b"), c("a", "b", "d"), c("a", "b", "c", "d"))) {
    expect_error(adaptive_rank_start(bad_ids, warm_start_prior = prior, warm_start_mode = "both"),
      "exactly")
  }
  bad <- prior
  bad$item_id <- c("a", "a", "c")
  expect_error(adaptive_rank_start(items, warm_start_prior = bad, warm_start_mode = "both"), "unique")
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$trueskill_state$items$item_id <- c("a", "b", "d")
  expect_error(pairwiseLLM:::.warm_start_adaptive_init(state, prior = prior, mode = "both"), "exactly")
})

test_that("all modes commit identical bootstrap evidence after invalid retries", {
  prior <- make_warm_start_prior(c(a = -1, b = -0.5, c = 0.5, d = 1))
  states <- lapply(c("cold", "btl_only", "trueskill_only", "both"), function(mode) {
    state <- adaptive_rank_start(prior$item_id, seed = 72, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior)
    initial_ts <- state$trueskill_state
    bad <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("invalid"))
    expect_identical(bad$warm_start_idx, 1L)
    expect_identical(bad$history_pairs, state$history_pairs)
    expect_identical(bad$trueskill_state, initial_ts)
    expect_false(bad$warm_start_done)
    state <- adaptive_rank_run_live(bad, make_deterministic_judge("i_wins"),
      n_steps = length(prior$item_id) - 1L, progress = "none")
    expect_true(state$warm_start_done)
    expect_equal(nrow(state$history_pairs), 3L)
    expect_setequal(c(state$history_pairs$A_id, state$history_pairs$B_id), prior$item_id)
    state
  })
  for (state in states[-1]) {
    expect_identical(state$history_pairs, states[[1]]$history_pairs)
    expect_identical(state$step_log$Y, states[[1]]$step_log$Y)
    expect_identical(state$step_log$Y, c(NA_integer_, 1L, 1L, 1L))
    expect_identical(state$warm_start_idx, states[[1]]$warm_start_idx)
  }
  expect_identical(states[[1]]$trueskill_state, states[[2]]$trueskill_state)
  expect_identical(states[[3]]$trueskill_state, states[[4]]$trueskill_state)
  expect_false(identical(states[[2]]$trueskill_state, states[[4]]$trueskill_state))
})

test_that("default BTL refits consume predictive evidence only in BTL-warm modes", {
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
  received <- list()
  testthat::local_mocked_bindings(fit_bayes_btl_mcmc = function(results, ids,
      model_variant, cmdstan, warm_start_prior) {
    received <<- list(prior = warm_start_prior, ids = ids)
    list(fit = list(theta_draws = matrix(0, 2, length(ids))))
  }, .package = "pairwiseLLM")
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    state <- adaptive_rank_start(prior$item_id, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior)
    expect_error(pairwiseLLM:::default_btl_fit_fn(state, list()), "committed comparison")
    state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
    pairwiseLLM:::default_btl_fit_fn(state, list())
    if (mode %in% c("btl_only", "both")) {
      expect_identical(received$prior, prior)
    } else {
      expect_null(received$prior)
    }
    if (mode == "trueskill_only") expect_identical(state$predictive_prior, prior)
  }
  legacy <- adaptive_rank_start(prior$item_id, warm_start_prior = prior)
  legacy$meta$warm_start_mode <- NULL
  expect_identical(pairwiseLLM:::.warm_start_btl_prior_for_state(legacy), prior)
  legacy$predictive_prior <- NULL
  expect_null(pairwiseLLM:::.warm_start_btl_prior_for_state(legacy))
  expect_error(pairwiseLLM:::default_btl_fit_fn(list(), list()), "adaptive_state")
})

test_that("model destinations resolve once and wrapper forwards and rejects resume mode", {
  features <- warm_core_features(6)
  items <- data.frame(item_id = features$item_id, text = "Synthetic text")
  model <- warm_bundle_model()
  predictions <- 0L
  original <- getS3method("predict", "pairwiseLLM_warm_model")
  testthat::local_mocked_bindings(predict.pairwiseLLM_warm_model = function(...) {
    predictions <<- predictions + 1L
    original(...)
  }, adaptive_rank_run_live = function(state, ...) state, .package = "pairwiseLLM")
  session <- file.path(withr::local_tempdir(), "session")
  out <- adaptive_rank(items, judge = function(...) NULL, progress = "none",
    warm_start_model = model, warm_start_features = features, warm_start_mode = "both",
    session_dir = session)
  state <- out$state %||% out
  expect_identical(predictions, 1L)
  expect_identical(state$meta$warm_start_mode, "both")
  expect_identical(state$trueskill_state$items$mu, 25 + (25 / 3) * state$predictive_prior$prior_mean)
  restored <- adaptive_rank_resume(session)
  expect_identical(restored$meta$warm_start_mode, "both")
  expect_identical(restored$trueskill_state, state$trueskill_state)
  expect_identical(predictions, 1L)
  expect_error(adaptive_rank(items, judge = function(...) NULL, progress = "none",
    session_dir = session, warm_start_mode = "both"), "Omit all warm-start")
  ts <- adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_mode = "trueskill_only")
  expect_identical(predictions, 2L)
  expect_identical(ts$trueskill_state, state$trueskill_state)
  expect_null(pairwiseLLM:::.warm_start_btl_prior_for_state(ts))
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_mode = "trueskill_only", warm_start_prior_sd = 0.5), "controls BTL")
  expect_identical(predictions, 2L)
})

test_that("run-required linking Phase A shares initialization and scoped BTL gating", {
  items <- data.frame(item_id = c("a", "b", "c", "d"), set_id = c(1L, 1L, 2L, 2L))
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1, d = 2))
  received <- NULL
  testthat::local_mocked_bindings(fit_bayes_btl_mcmc = function(results, ids,
      model_variant, cmdstan, warm_start_prior) {
    received <<- list(prior = warm_start_prior, ids = ids)
    list(fit = list(theta_draws = matrix(0, 2, length(ids))))
  }, .package = "pairwiseLLM")
  states <- lapply(c("cold", "btl_only", "trueskill_only", "both"), function(mode) {
    state <- adaptive_rank_start(items, seed = 82, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior,
      adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run"))
    expect_identical(state$controller$link_phase, "phase_a")
    if (mode %in% c("both", "trueskill_only")) {
      expect_identical(state$trueskill_state$items$mu, 25 + (25 / 3) * prior$prior_mean)
    }
    state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
      n_steps = 1L, progress = "none")
    pairwiseLLM:::default_btl_fit_fn(state, list())
    expect_identical(received$ids, c("a", "b"))
    if (mode %in% c("both", "btl_only")) {
      expect_identical(received$prior$prior_mean, c(-0.5, 0.5))
    } else {
      expect_null(received$prior)
    }
    state
  })
  for (state in states[-1]) {
    expect_identical(state$warm_start_pairs, states[[1]]$warm_start_pairs)
    expect_identical(state$history_pairs, states[[1]]$history_pairs)
  }
})
