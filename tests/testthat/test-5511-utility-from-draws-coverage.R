local_rebind_namespace <- function(ns, name, value) {
  env <- asNamespace(ns)
  has_old <- exists(name, envir = env, inherits = FALSE)
  old <- if (has_old) get(name, envir = env, inherits = FALSE) else NULL
  locked <- if (has_old) bindingIsLocked(name, env) else FALSE
  if (locked) {
    unlockBinding(name, env)
  }
  assign(name, value, envir = env)
  if (locked) {
    lockBinding(name, env)
  }
  function() {
    if (locked) {
      unlockBinding(name, env)
    }
    if (has_old) {
      assign(name, old, envir = env)
    } else if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
    if (locked) {
      lockBinding(name, env)
    }
  }
}

testthat::test_that("compute_pair_utility_from_draws validates core inputs", {
  theta_draws <- matrix(0, nrow = 2, ncol = 2)
  deg <- c(0, 0)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = c(1, 2),
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "theta_draws"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = matrix(0, nrow = 1, ncol = 2),
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "at least two draws"
  )

  bad_draws <- matrix(c(0, Inf, 0, 0), nrow = 2)
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = bad_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "finite values"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = "A",
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "candidate_i"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = c(1, 2),
      candidate_j = 1,
      deg = deg,
      model_variant = "btl"
    ),
    "same length"
  )

  empty_out <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = numeric(),
    candidate_j = numeric(),
    deg = deg,
    model_variant = "btl"
  )
  testthat::expect_equal(nrow(empty_out), 0L)
  testthat::expect_true(all(c("u0", "u", "p_mean") %in% names(empty_out)))

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = Inf,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "finite"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1.2,
      candidate_j = 2,
      deg = deg,
      model_variant = "btl"
    ),
    "integer-like"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 0,
      candidate_j = 2,
      deg = deg,
      model_variant = "btl"
    ),
    "index"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = 0,
      model_variant = "btl"
    ),
    "deg"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = c(0, NA_real_),
      model_variant = "btl"
    ),
    "finite"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = c(0, -1),
      model_variant = "btl"
    ),
    "non-negative"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl",
      chunk_size = 0L
    ),
    "chunk_size"
  )
})

testthat::test_that("compute_pair_utility_from_draws validates epsilon and beta draws", {
  theta_draws <- matrix(0, nrow = 2, ncol = 2)
  deg <- c(0, 0)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e",
      epsilon_draws = "bad"
    ),
    "epsilon_draws"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e",
      epsilon_draws = c(0.1, 0.2, 0.3)
    ),
    "same length"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e",
      epsilon_draws = c(-0.1, 0.2)
    ),
    "within \\[0, 1\\]"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_b",
      beta_draws = "bad"
    ),
    "beta_draws"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_b",
      beta_draws = c(0.1, 0.2, 0.3)
    ),
    "same length"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_b",
      beta_draws = c(0.1, NA_real_)
    ),
    "finite"
  )
})

testthat::test_that("compute_pair_utility_from_draws aborts on non-finite draw moments", {
  theta_draws <- matrix(0, nrow = 2, ncol = 2)
  deg <- c(0, 0)

  restore_plogis <- local_rebind_namespace("stats", "plogis", function(x) {
    matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
  })
  on.exit(restore_plogis(), add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "Draw-based utility values"
  )
})

testthat::test_that("compute_pair_utility_from_draws aborts on non-finite moments", {
  theta_draws <- matrix(0, nrow = 2, ncol = 2)
  deg <- c(0, 0)

  restore_var <- local_rebind_namespace("stats", "var", function(...) NA_real_)
  on.exit(restore_var(), add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl"
    ),
    "utility moments"
  )
})

testthat::test_that("compute_pair_utility_dispatch validates inputs and ids", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N, model_variant = "btl")

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = matrix(0, nrow = 2, ncol = 2)),
      candidates = "bad",
      state = state,
      config = config
    ),
    "data frame"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = matrix(0, nrow = 2, ncol = 2)),
      candidates = tibble::tibble(x = "A"),
      state = state,
      config = config
    ),
    "must include"
  )

  empty_out <- pairwiseLLM:::compute_pair_utility_dispatch(
    fit = list(theta_draws = matrix(0, nrow = 2, ncol = 2)),
    candidates = tibble::tibble(i_id = character(), j_id = character()),
    state = state,
    config = config
  )
  testthat::expect_equal(nrow(empty_out), 0L)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(),
      candidates = tibble::tibble(i_id = "A", j_id = "B"),
      state = state,
      config = config
    ),
    "theta_draws"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = matrix(0, nrow = 1, ncol = 2)),
      candidates = tibble::tibble(i_id = "A", j_id = "B"),
      state = state,
      config = config
    ),
    "at least two draws"
  )

  no_names <- matrix(0, nrow = 2, ncol = 2)
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = no_names),
      candidates = tibble::tibble(i_id = "A", j_id = "B"),
      state = state,
      config = config,
      diagnostics_pass = TRUE
    ),
    "column names"
  )

  theta_draws <- matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, c("A", "B")))
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = theta_draws),
      candidates = tibble::tibble(i_id = NA_character_, j_id = "B"),
      state = state,
      config = config,
      diagnostics_pass = TRUE
    ),
    "non-missing"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_dispatch(
      fit = list(theta_draws = theta_draws),
      candidates = tibble::tibble(i_id = "A", j_id = "C"),
      state = state,
      config = config,
      diagnostics_pass = TRUE
    ),
    "present in"
  )
})

testthat::test_that("compute_pair_utility_dispatch fills unordered_key when missing", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N, model_variant = "btl")
  theta_draws <- matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, c("A", "B")))
  fit <- list(theta_draws = theta_draws)

  out <- pairwiseLLM:::compute_pair_utility_dispatch(
    fit = fit,
    candidates = tibble::tibble(i_id = "A", j_id = "B"),
    state = state,
    config = config,
    diagnostics_pass = TRUE
  )
  testthat::expect_true("unordered_key" %in% names(out))
})
