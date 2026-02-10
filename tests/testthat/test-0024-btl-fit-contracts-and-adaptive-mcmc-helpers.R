make_mcmc_config_fixture <- function() {
  list(
    chains = 2L,
    parallel_chains = 2L,
    core_fraction = 0.8,
    cores_detected_physical = 2L,
    cores_detected_logical = 4L,
    threads_per_chain = 1L,
    cmdstanr_version = "test"
  )
}

make_fit_contract_fixture <- function() {
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- c("A", "B")
  pairwiseLLM:::build_btl_fit_contract(
    theta_draws = theta,
    epsilon_draws = c(0.1, 0.2),
    beta_draws = c(-0.1, 0.1),
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
    model_variant = "btl_e_b",
    diagnostics_pass = TRUE,
    mcmc_config_used = make_mcmc_config_fixture()
  )
}

test_that("fit contract validator catches missing and malformed fields", {
  fit <- make_fit_contract_fixture()

  missing <- fit
  missing$diagnostics <- NULL
  expect_error(pairwiseLLM:::validate_btl_fit_contract(missing, ids = c("A", "B")), "missing required fields")

  bad_draws <- fit
  bad_draws$theta_draws <- matrix(c(1, 2, 3), nrow = 1)
  colnames(bad_draws$theta_draws) <- c("A", "B", "C")
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_draws, ids = c("A", "B")), "at least two draws")

  bad_names <- fit
  colnames(bad_names$theta_draws) <- c("B", "A")
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_names, ids = c("A", "B")), "column names must match")

  bad_theta_sd <- fit
  bad_theta_sd$theta_sd <- c(A = 0.1, B = Inf)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_theta_sd, ids = c("A", "B")), "must be finite")

  bad_diag <- fit
  bad_diag$diagnostics <- list(divergences = 0L)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_diag, ids = c("A", "B")), "must include divergences")

  bad_mcmc <- fit
  bad_mcmc$mcmc_config_used <- list(chains = 1L)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_mcmc, ids = c("A", "B")), "missing")
})

test_that("fit contract validator rejects malformed types across scalar and draw fields", {
  fit <- make_fit_contract_fixture()

  expect_error(pairwiseLLM:::validate_btl_fit_contract(1L, ids = c("A", "B")), "`fit` must be a list")
  expect_error(pairwiseLLM:::validate_btl_fit_contract(fit, ids = c("A", "A")), "`ids` must be unique")

  bad_theta <- fit
  bad_theta$theta_draws <- "x"
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_theta, ids = c("A", "B")), "numeric matrix")

  bad_n_items <- fit
  bad_n_items$n_items <- 3L
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_n_items, ids = c("A", "B")), "must match `theta_draws` columns")

  bad_eps_type <- fit
  bad_eps_type$epsilon_draws <- "x"
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_eps_type, ids = c("A", "B")), "numeric vector or NULL")

  bad_eps_range <- fit
  bad_eps_range$epsilon_draws <- c(-0.1, 0.1)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_eps_range, ids = c("A", "B")), "within \\[0, 1\\]")

  bad_beta_type <- fit
  bad_beta_type$beta_draws <- "x"
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_beta_type, ids = c("A", "B")), "numeric vector or NULL")

  bad_diag_type <- fit
  bad_diag_type$diagnostics_pass <- 1L
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_diag_type, ids = c("A", "B")), "TRUE, FALSE, or NA")

  bad_variant <- fit
  bad_variant$model_variant <- 1L
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_variant, ids = c("A", "B")), "length-1 character")
})

test_that("fit contract utility helpers enforce bounds and types", {
  expect_equal(pairwiseLLM:::percentiles_2_5_50_95_97_5(c(1, 2, 3))[["p50"]], 2)
  expect_true(all(is.na(pairwiseLLM:::na_param_summary())))

  expect_error(
    pairwiseLLM:::.btl_contract_vector_summary(c(-0.1, 0.2), "eps", c(0, 1), c(0.5)),
    "within \\[0, 1\\]"
  )
  expect_error(pairwiseLLM:::.btl_contract_diagnostics_defaults(1L), "must be a list")
  expect_error(pairwiseLLM:::.btl_contract_diagnostics_pass(c(TRUE, FALSE)), "TRUE, FALSE, or NA")
  expect_error(pairwiseLLM:::.btl_contract_mcmc_defaults(1L), "must be a list")

  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- c("A", "B")
  expect_error(pairwiseLLM:::reorder_theta_draws(theta, c("A", "C")), "must match `ids`")

  expect_warning(
    pairwiseLLM:::.btl_contract_vector_summary(c(0.1, Inf, 0.2), "eps", c(0, 1), c(0.5)),
    "Non-finite values detected"
  )
  expect_identical(pairwiseLLM:::.btl_contract_diagnostics_defaults(NULL)$divergences, NA_integer_)
  expect_true(is.na(pairwiseLLM:::.btl_contract_diagnostics_pass(NULL)))
  expect_error(pairwiseLLM:::reorder_theta_draws("x", c("A", "B")), "numeric matrix")
  expect_error(pairwiseLLM:::reorder_theta_draws(theta, c("A", "A")), "must be unique")
  expect_error(pairwiseLLM:::reorder_theta_draws(theta, character()), "non-empty character vector")

  no_cols <- theta
  colnames(no_cols) <- NULL
  expect_error(pairwiseLLM:::reorder_theta_draws(no_cols, c("A", "B")), "must have non-empty column names")

  expect_error(
    pairwiseLLM:::.btl_contract_vector_summary("x", "eps", c(0, 1), c(0.5)),
    "must be numeric or NULL"
  )
})

test_that("build_btl_fit_contract validates theta draw names", {
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  expect_error(
    pairwiseLLM:::build_btl_fit_contract(
      theta_draws = theta,
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 100)
    ),
    "must have non-empty column names"
  )
})

test_that("adaptive mcmc helper functions cover config and diagnostics branches", {
  cores <- pairwiseLLM:::.btl_mcmc_detect_cores()
  expect_true(all(c("physical", "logical", "effective") %in% names(cores)))

  expect_true(is.character(pairwiseLLM:::.btl_mcmc_cmdstanr_version()))

  expect_error(pairwiseLLM:::.btl_mcmc_validate_seed("x", "seed"), "length-1 numeric")
  expect_identical(pairwiseLLM:::.btl_mcmc_validate_seed(NULL, "seed"), NULL)

  expect_error(pairwiseLLM:::.btl_mcmc_validate_pair_counts(c(1, 5), 2), "<= nrow")
  expect_equal(pairwiseLLM:::.btl_mcmc_validate_pair_counts(c(2, 1, 2), 3), c(1L, 2L))

  draws_mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  colnames(draws_mat) <- c("theta[1]", "theta[2]")
  expect_error(pairwiseLLM:::.btl_mcmc_unpack_draws(matrix(1, nrow = 1, ncol = 1), model_variant = "btl"), "column names")

  td <- pairwiseLLM:::.btl_mcmc_theta_draws(list(theta = draws_mat), item_id = c("A", "B"))
  expect_identical(colnames(td), c("A", "B"))

  fake_fit <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = c(0, 1)),
    summary = function(variables) tibble::tibble(rhat = c(1, 1.01), ess_bulk = c(500, 600), ess_tail = c(400, 500))
  )
  diag <- pairwiseLLM:::.btl_mcmc_collect_diagnostics(fake_fit, model_variant = "btl")
  expect_identical(diag$divergences, 1L)
  expect_true(is.finite(diag$max_rhat))

  fake_fit_empty <- list(
    diagnostic_summary = function() NULL,
    summary = function(variables) NULL
  )
  diag2 <- pairwiseLLM:::.btl_mcmc_collect_diagnostics(fake_fit_empty, model_variant = "btl")
  expect_true(length(diag2$notes) > 0L)
})

test_that("adaptive mcmc config, draw summaries, and contract conversion cover edge branches", {
  expect_error(pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config("bad"), "`cmdstan` must be a list")
  expect_error(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(threads_per_chain = 0L)),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(core_fraction = 2)),
    "must be in \\(0, 1\\]"
  )

  cfg <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 2L, core_fraction = 1))
  expect_true(cfg$parallel_chains >= 1L)

  expect_error(pairwiseLLM:::.btl_validate_ids(c("A")), "at least two ids")
  expect_error(pairwiseLLM:::.btl_validate_ids(c("A", "A")), "must be unique")

  expect_error(pairwiseLLM:::.btl_mcmc_validate_bt_data("bad"), "`bt_data` must be a list")
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 1L)),
    "missing: N"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 1L, N = 1L)),
    "must be an integer >= 2"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = c(1, 2), B = 1L, Y = c(1, 0), N = 2L)),
    "must match in length"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 2L, N = 2L)),
    "must contain only 0/1 values"
  )

  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01", tz = "UTC"),
    backend = "openai",
    model = "m"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_prepare_bt_data(results, ids = c("A", "C")),
    "must be contained in `ids`"
  )

  expect_true(is.list(pairwiseLLM:::summarize_draws(list(theta = matrix(1, nrow = 1, ncol = 1)))))
  summarized <- pairwiseLLM:::summarize_draws(list(theta = matrix(1:4, nrow = 2), epsilon = c(0.1, 0.2)), "btl_e")
  expect_true(is.list(summarized))

  bad_theta <- list(draws = list(theta = matrix(1:4, nrow = 2)), model_variant = "btl")
  expect_error(
    pairwiseLLM:::as_btl_fit_contract_from_mcmc(bad_theta, ids = c("A", "B")),
    "must have column names"
  )
})

test_that("mcmc core detection, config resolution, and variant inference hit fallback branches", {
  cores <- testthat::with_mocked_bindings(
    detectCores = function(logical = FALSE) stop("detect fail"),
    pairwiseLLM:::.btl_mcmc_detect_cores(),
    .package = "parallel"
  )
  expect_identical(cores$effective, 1L)

  cfg_na <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = NA_integer_))
  expect_true(is.na(cfg_na$parallel_chains))

  cfg_clamped <- pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(
    chains = 2L,
    parallel_chains = 9L,
    core_fraction = 1
  ))
  expect_identical(cfg_clamped$parallel_chains, 2L)

  mat_e <- matrix(1, nrow = 2, ncol = 2)
  colnames(mat_e) <- c("theta[1]", "epsilon")
  expect_identical(pairwiseLLM:::.btl_mcmc_infer_variant(mat_e), "btl_e")

  mat_b <- matrix(1, nrow = 2, ncol = 2)
  colnames(mat_b) <- c("theta[1]", "beta")
  expect_identical(pairwiseLLM:::.btl_mcmc_infer_variant(mat_b), "btl_b")
})

test_that("fit_bayes_btl_mcmc_adaptive succeeds with deterministic model_fn and validates guard rails", {
  bt_data <- list(
    A = c(1L, 1L),
    B = c(2L, 2L),
    Y = c(1L, 0L),
    N = 2L,
    item_id = c("A", "B")
  )
  cfg <- pairwiseLLM:::btl_mcmc_config(2L, list(
    model_variant = "btl_e_b",
    thin_draws = 2L,
    cmdstan = list(
      chains = 2L,
      parallel_chains = 2L,
      core_fraction = 1,
      threads_per_chain = 1L,
      iter_warmup = 4L,
      iter_sampling = 4L
    )
  ))

  draws_matrix <- cbind(
    `theta[1]` = c(0.1, 0.2, 0.3, 0.4),
    `theta[2]` = c(0.2, 0.3, 0.4, 0.5),
    epsilon = c(0.1, 0.2, 0.3, 0.4),
    beta = c(-0.2, -0.1, 0.1, 0.2)
  )
  fake_fit <- list(
    draws = function(variables, format) draws_matrix,
    diagnostic_summary = function() tibble::tibble(num_divergent = c(0L, 1L)),
    summary = function(variables) tibble::tibble(
      rhat = c(1.0, 1.01),
      ess_bulk = c(500, 600),
      ess_tail = c(400, 500)
    )
  )
  fake_model_fn <- function(stan_file, cpp_options) {
    list(sample = function(...) fake_fit)
  }

  out <- testthat::with_mocked_bindings(
    .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
    stan_file_for_variant = function(model_variant) "fake.stan",
    .package = "pairwiseLLM"
  , {
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(
      bt_data = bt_data,
      config = cfg,
      seed = 123L,
      model_fn = fake_model_fn
    )
  }
  )
  expect_identical(out$model_variant, "btl_e_b")
  expect_identical(nrow(out$draws$theta), 2L)
  expect_true(is.numeric(out$draws$epsilon))
  expect_true(is.numeric(out$draws$beta))
  expect_true(is.list(out$diagnostics))

  bad_cfg <- cfg
  bad_cfg$cmdstan <- "bad"
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      .package = "pairwiseLLM"
    , {
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, bad_cfg, model_fn = fake_model_fn)
    }
    ),
    "must be a list"
  )

  bad_cfg2 <- cfg
  bad_cfg2$thin_draws <- 0L
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      stan_file_for_variant = function(model_variant) "fake.stan",
      .package = "pairwiseLLM"
    , {
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, bad_cfg2, model_fn = fake_model_fn)
    }
    ),
    "thin_draws"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      stan_file_for_variant = function(model_variant) "fake.stan",
      .package = "pairwiseLLM"
    , {
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, cfg, model_fn = 1L)
    }
    ),
    "`model_fn` must be a function"
  )
})

test_that("fit_bayes_btl_mcmc wrapper executes deterministic multi-refit flow and validates errors", {
  results <- tibble::tibble(
    pair_uid = c("A:B#1", "A:B#2"),
    unordered_key = c("A:B", "A:B"),
    ordered_key = c("A:B", "A:B"),
    A_id = c("A", "A"),
    B_id = c("B", "B"),
    better_id = c("A", "B"),
    winner_pos = c(1L, 2L),
    phase = c("phase2", "phase2"),
    iter = c(1L, 2L),
    received_at = as.POSIXct(c("2026-01-01", "2026-01-01"), tz = "UTC"),
    backend = c("openai", "openai"),
    model = c("m", "m")
  )
  fake_mcmc <- list(
    model_variant = "btl",
    draws = list(theta = matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, dimnames = list(NULL, c("A", "B")))),
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 500),
    mcmc_config_used = make_mcmc_config_fixture()
  )

  out <- testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL, model_fn = NULL) fake_mcmc,
    .package = "pairwiseLLM"
  , {
    pairwiseLLM::fit_bayes_btl_mcmc(
      results = results,
      ids = c("A", "B"),
      model_variant = "btl",
      cmdstan = list(
        chains = 2L,
        parallel_chains = 2L,
        core_fraction = 1,
        threads_per_chain = 1L
      ),
      pair_counts = c(1L, 2L),
      subset_method = "sample",
      seed = 11L
    )
  }
  )
  expect_identical(length(out$fits), 2L)
  expect_true(is.data.frame(out$round_log))
  expect_true(is.data.frame(out$item_summary))

  expect_error(
    pairwiseLLM::fit_bayes_btl_mcmc(
      results = results[0, , drop = FALSE],
      ids = c("A", "B"),
      cmdstan = list(chains = 1L, parallel_chains = 1L, core_fraction = 1)
    ),
    "at least one row"
  )

  expect_error(
    pairwiseLLM::fit_bayes_btl_mcmc(results, ids = c("A", "B"), cmdstan = "bad"),
    "`cmdstan` must be a list"
  )
})

test_that("cmdstan requirement helper aborts cleanly when cmdstanr is unavailable", {
  expect_error(
    testthat::with_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) FALSE,
      .package = "base"
    , {
      pairwiseLLM:::.btl_mcmc_require_cmdstanr()
    }
    ),
    "CmdStanR is required"
  )
})

test_that("btl mcmc contract helpers cover remaining guard and gini branches", {
  expect_error(pairwiseLLM:::btl_mcmc_config(4L, 1L), "`overrides` must be a list")

  cfg <- pairwiseLLM:::btl_mcmc_config(4L)
  cfg$cmdstan <- "bad"
  expect_error(pairwiseLLM:::validate_btl_mcmc_config(cfg), "must be a list when provided")

  expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(NULL)))
  expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(matrix(c(1, NA), nrow = 1))))

  expect_error(pairwiseLLM:::compute_gini_degree(c(-1, 2)), "non-negative")
  expect_identical(pairwiseLLM:::compute_gini_degree(c(0, 0, 0)), 0)
  expect_identical(pairwiseLLM:::compute_gini_degree(3), 0)

  expect_error(pairwiseLLM:::compute_gini_posA(c(1, 2), deg = c(1)), "same length")
  expect_error(pairwiseLLM:::compute_gini_posA(c(1, 2), deg = c(1, -1)), "`deg` must be non-negative")
  expect_identical(pairwiseLLM:::compute_gini_posA(c(0, 0, 0)), 0)
  expect_identical(pairwiseLLM:::compute_gini_posA(c(3)), 0)

  defaults_rows <- pairwiseLLM:::.adaptive_item_log_defaults(n_rows = 2L)
  expect_identical(nrow(defaults_rows), 2L)
})

test_that("fit contract validator covers additional scalar and diagnostics branches", {
  fit <- make_fit_contract_fixture()

  bad_theta_mean <- fit
  bad_theta_mean$theta_mean <- unname(bad_theta_mean$theta_mean)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_theta_mean, ids = c("A", "B")), "must be named")

  bad_theta_sd_names <- fit
  names(bad_theta_sd_names$theta_sd) <- c("B", "A")
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_theta_sd_names, ids = c("A", "B")), "names must match")

  bad_n <- fit
  bad_n$n_items <- NA_real_
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_n, ids = c("A", "B")), "must be integer scalars")

  bad_eps_scalar <- fit
  bad_eps_scalar$epsilon_draws <- c(0.1, 0.2)
  bad_eps_scalar$epsilon_mean <- "bad"
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_eps_scalar, ids = c("A", "B")), "numeric scalar")

  bad_beta_draws <- fit
  bad_beta_draws$beta_draws <- 1
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_beta_draws, ids = c("A", "B")), "at least two finite draws")

  bad_diag_list <- fit
  bad_diag_list$diagnostics <- 1L
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_diag_list, ids = c("A", "B")), "must be a list")
})

test_that("build_item_log covers draw-matrix colname fallback branch", {
  ids <- c("A", "B")
  state <- structure(
    list(
      ids = ids,
      deg = stats::setNames(c(1L, 0L), ids),
      pos1 = stats::setNames(c(1L, 0L), ids)
    ),
    class = "adaptive_state"
  )
  draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  fit <- list(theta_draws = draws)
  out <- pairwiseLLM:::build_item_log(state, fit = fit)
  expect_identical(nrow(out), 2L)
  expect_true(all(c("ID", "theta_mean", "rank_mean") %in% names(out)))
})

test_that("mcmc draw unpacking, diagnostics notes, and fit-contract conversion cover additional error branches", {
  expect_error(
    pairwiseLLM:::.btl_mcmc_unpack_draws(list(theta = c(1, 2))),
    "must be a numeric matrix"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_unpack_draws(list(theta = matrix(1:4, nrow = 2)), model_variant = "btl_e"),
    "include epsilon draws"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_unpack_draws(
      list(theta = matrix(1:4, nrow = 2), epsilon = c(0.1, 0.2)),
      model_variant = "btl_e_b"
    ),
    "include beta draws"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_unpack_draws(
      list(theta = matrix(1:4, nrow = 2), epsilon = "bad"),
      model_variant = "btl_e"
    ),
    "must be numeric"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_unpack_draws(
      list(theta = matrix(1:4, nrow = 2), beta = "bad"),
      model_variant = "btl_b"
    ),
    "must be numeric"
  )

  no_theta <- matrix(1, nrow = 2, ncol = 1)
  colnames(no_theta) <- "epsilon"
  expect_error(pairwiseLLM:::.btl_mcmc_unpack_draws(no_theta), "include theta columns")

  fake_fit_nonfinite <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = c(Inf, -Inf)),
    summary = function(variables) tibble::tibble(rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_)
  )
  diag <- pairwiseLLM:::.btl_mcmc_collect_diagnostics(fake_fit_nonfinite, model_variant = "btl_e_b")
  expect_true(any(grepl("Divergence count not finite", diag$notes, fixed = TRUE)))
  expect_true(any(grepl("Rhat values missing", diag$notes, fixed = TRUE)))
  expect_true(any(grepl("ESS bulk values missing", diag$notes, fixed = TRUE)))
  expect_true(any(grepl("ESS tail values missing", diag$notes, fixed = TRUE)))

  fake_fit_missing_cols <- list(
    diagnostic_summary = function() tibble::tibble(other = 1L),
    summary = function(variables) tibble::tibble(x = 1)
  )
  diag2 <- pairwiseLLM:::.btl_mcmc_collect_diagnostics(fake_fit_missing_cols, model_variant = "btl")
  expect_true(any(grepl("missing num_divergent", diag2$notes, fixed = TRUE)))
  expect_true(any(grepl("missing rhat", diag2$notes, fixed = TRUE)))
  expect_true(any(grepl("missing ess_bulk", diag2$notes, fixed = TRUE)))
  expect_true(any(grepl("missing ess_tail", diag2$notes, fixed = TRUE)))

  expect_warning(
    expect_error(
      pairwiseLLM:::summarize_draws(
        list(theta = matrix(1:4, nrow = 2), epsilon = c(Inf, NA_real_)),
        model_variant = "btl_e"
      ),
      "at least two finite values"
    ),
    "Non-finite values detected in `epsilon_draws`"
  )

  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- c("A", "B")
  fit_base <- list(model_variant = "btl_e_b", draws = list(theta = theta, epsilon = c(0.1, 0.2), beta = c(0, 0.1)))

  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(1L, ids = c("A", "B")), "`mcmc_fit` must be a list")
  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(fit_base, ids = c("A", "")), "non-empty character vector")
  bad_draws <- fit_base
  bad_draws$draws <- NULL
  expect_error(
    pairwiseLLM:::as_btl_fit_contract_from_mcmc(bad_draws, ids = c("A", "B")),
    "`mcmc_fit\\$draws` must be a list"
  )

  bad_theta <- fit_base
  bad_theta$draws$theta <- c(1, 2)
  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(bad_theta, ids = c("A", "B")), "numeric matrix")

  bad_beta <- fit_base
  bad_beta$draws$beta <- "bad"
  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(bad_beta, ids = c("A", "B")), "must be numeric")
})

test_that("fit_bayes_btl_mcmc and adaptive fit entrypoints cover input guard rails and compatibility branches", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01", tz = "UTC"),
    backend = "openai",
    model = "m"
  )

  out_compat <- testthat::with_mocked_bindings(
    .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
      list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
    },
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL, model_fn = NULL) {
      theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
      colnames(theta) <- bt_data$item_id
      list(
        draws = list(theta = theta, epsilon = c(0.1, 0.2), beta = c(0, 0.1)),
        diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000, min_ess_tail = 900),
        mcmc_config_used = list(
          chains = 1L, parallel_chains = 1L, core_fraction = 1,
          cores_detected_physical = 1L, cores_detected_logical = 1L,
          threads_per_chain = 1L, cmdstanr_version = "test"
        ),
        model_variant = "btl_e_b"
      )
    },
    btl_mcmc_fill_terminal_stop_metrics = function(state, config) {
      m <- pairwiseLLM:::.adaptive_stop_metrics_defaults()
      m$proposed_pairs <- as.integer(state$comparisons_observed)
      m
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::fit_bayes_btl_mcmc(
        results = results,
        ids = c("A", "B"),
        model_variant = list(chains = 1L),
        pair_counts = 1L
      )
    }
  )
  expect_true(is.list(out_compat$fit))

  expect_error(
    pairwiseLLM::fit_bayes_btl_mcmc(
      results = results,
      ids = c("A", "B"),
      cmdstan = list(output_dir = NA_character_)
    ),
    "length-1 character path"
  )
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) list(parallel_chains = NA_integer_),
      .package = "pairwiseLLM",
      {
        pairwiseLLM::fit_bayes_btl_mcmc(results = results, ids = c("A", "B"), cmdstan = list())
      }
    ),
    "positive integer"
  )

  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("A", "B"))
  cfg <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl_e_b"))
  expect_error(pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = "bad"), "`config` must be a list")

  cfg_bad_cmdstan <- cfg
  cfg_bad_cmdstan$cmdstan <- "bad"
  expect_error(pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = cfg_bad_cmdstan), "must be a list")

  cfg_bad_thin <- cfg
  cfg_bad_thin$thin_draws <- 0L
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      validate_btl_mcmc_config = function(config) invisible(config),
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) list(
        chains = 1L, parallel_chains = 1L, threads_per_chain = 1L
      ),
      stan_file_for_variant = function(model_variant) "fake.stan",
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(
          bt_data = bt_data,
          config = cfg_bad_thin,
          model_fn = function(stan_file, cpp_options) {
            list(sample = function(...) {
              list(
                draws = function(variables, format) {
                  cbind(`theta[1]` = c(0.1, 0.2), `theta[2]` = c(0.2, 0.3), epsilon = c(0.1, 0.2), beta = c(0, 0.1))
                },
                diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
                summary = function(variables) tibble::tibble(rhat = 1, ess_bulk = 1000, ess_tail = 1000)
              )
            })
          }
        )
      }
    ),
    "positive integer"
  )
})

test_that("bayes mcmc helpers cover remaining config, id, data, and summary-state branches", {
  expect_error(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 2L, parallel_chains = 0L)),
    "positive integer"
  )
  ver <- testthat::with_mocked_bindings(
    requireNamespace = function(pkg, quietly = TRUE) FALSE,
    .package = "base",
    {
      pairwiseLLM:::.btl_mcmc_cmdstanr_version()
    }
  )
  expect_true(is.na(ver))

  expect_error(pairwiseLLM:::.btl_validate_ids(c("A", "")), "missing or empty")

  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = c("x"), B = 1L, Y = 1L, N = 2L)),
    "must be integer vectors"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = "x", N = 2L)),
    "`bt_data\\$Y` must be an integer vector"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = integer(), B = integer(), Y = integer(), N = 2L)),
    "at least one comparison"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 3L, B = 1L, Y = 1L, N = 2L)),
    "must be in 1..N"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_bt_data(list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("A", ""))),
    "non-empty strings"
  )

  expect_true(nzchar(pairwiseLLM:::stan_file_for_variant("btl")))

  draws_matrix <- cbind(`theta[1]` = c(0.1, 0.2), `theta[2]` = c(0.2, 0.3), epsilon = c(0.1, 0.2), beta = c(0.0, 0.1))
  unpacked <- pairwiseLLM:::.btl_mcmc_unpack_draws(draws_matrix, model_variant = "btl_e_b")
  expect_true(is.numeric(unpacked$epsilon_draws))
  expect_true(is.numeric(unpacked$beta_draws))

  expect_error(pairwiseLLM:::.btl_mcmc_validate_pair_counts("x", 2), "integer vector or NULL")
  expect_error(pairwiseLLM:::.btl_mcmc_validate_pair_counts(c(NA_real_, 1), 2), "non-empty integer vector")
  expect_error(pairwiseLLM:::.btl_mcmc_validate_pair_counts(c(0, 1), 2), "positive integers")

  empty_counts <- pairwiseLLM:::.btl_mcmc_summary_counts(
    tibble::tibble(A_id = character(), B_id = character()),
    ids = c("A")
  )
  expect_identical(length(empty_counts$pair_count), 0L)

  skip_counts <- pairwiseLLM:::.btl_mcmc_summary_counts(
    tibble::tibble(A_id = c("A", "A"), B_id = c("A", "Z")),
    ids = c("A", "B")
  )
  expect_true(all(skip_counts$deg == c(A = 0L, B = 0L)))

  state_no_iter <- pairwiseLLM:::.btl_mcmc_summary_state(
    results = tibble::tibble(A_id = "A", B_id = "B"),
    ids = c("A", "B"),
    config = list(),
    fit_contract = make_fit_contract_fixture()
  )
  expect_identical(state_no_iter$iter, 0L)
})

test_that("adaptive mcmc fit helper covers cmdstan guards and missing-draw branches", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("A", "B"))

  cfg_bad_int <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl_e_b", cmdstan = list(iter_warmup = 0L)))
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      validate_btl_mcmc_config = function(config) invisible(config),
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
        list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
      },
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = cfg_bad_int, model_fn = function(...) list(sample = function(...) NULL))
      }
    ),
    "positive integers"
  )

  cfg_bad_pc <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl_e_b"))
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      validate_btl_mcmc_config = function(config) invisible(config),
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
        list(chains = 1L, parallel_chains = NA_integer_, threads_per_chain = 1L)
      },
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = cfg_bad_pc, model_fn = function(...) list(sample = function(...) NULL))
      }
    ),
    "positive integer"
  )

  cfg_bad_out <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl_e_b", cmdstan = list(output_dir = NA_character_)))
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      validate_btl_mcmc_config = function(config) invisible(config),
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
        list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
      },
      stan_file_for_variant = function(model_variant) "fake.stan",
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(
          bt_data = bt_data,
          config = cfg_bad_out,
          model_fn = function(stan_file, cpp_options) list(sample = function(...) list(
            draws = function(variables, format) cbind(`theta[1]` = c(0.1, 0.2), `theta[2]` = c(0.2, 0.3), epsilon = c(0.1, 0.2), beta = c(0.0, 0.1)),
            diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
            summary = function(variables) tibble::tibble(rhat = 1, ess_bulk = 1000, ess_tail = 1000)
          ))
        )
      }
    ),
    "length-1 character path"
  )

  cfg <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl_e_b"))
  expect_error(
    testthat::with_mocked_bindings(
      .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
      validate_btl_mcmc_config = function(config) invisible(config),
      .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
        list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
      },
      stan_file_for_variant = function(model_variant) "fake.stan",
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(
          bt_data = bt_data,
          config = cfg,
          model_fn = function(stan_file, cpp_options) list(sample = function(...) list(
            draws = function(variables, format) cbind(epsilon = c(0.1, 0.2), beta = c(0.0, 0.1)),
            diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
            summary = function(variables) tibble::tibble(rhat = 1, ess_bulk = 1000, ess_tail = 1000)
          ))
        )
      }
    ),
    "missing theta draws"
  )

  cfg_btl <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl"))
  out_btl <- testthat::with_mocked_bindings(
    .btl_mcmc_require_cmdstanr = function() invisible(TRUE),
    validate_btl_mcmc_config = function(config) invisible(config),
    .btl_mcmc_resolve_cmdstan_config = function(cmdstan) {
      list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
    },
    stan_file_for_variant = function(model_variant) "fake.stan",
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(
        bt_data = bt_data,
        config = cfg_btl,
        model_fn = function(stan_file, cpp_options) list(sample = function(...) list(
          draws = function(variables, format) cbind(`theta[1]` = c(0.1, 0.2), `theta[2]` = c(0.2, 0.3)),
          diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
          summary = function(variables) tibble::tibble(rhat = 1, ess_bulk = 1000, ess_tail = 1000)
        ))
      )
    }
  )
  expect_null(out_btl$draws$epsilon)
  expect_null(out_btl$draws$beta)
})
