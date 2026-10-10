# Unmodified post-#328 scheduler/bootstrap implementations. Changes to shared
# execution must be checked against this scope, never another new runner path.
bt_reference_325 <- function(path = testthat::test_path("fixtures", "bt-schedule-325")) {
  env <- new.env(parent = asNamespace("pairwiseLLM"))
  for (file in c("adaptive_run.R", "bt_bootstrap.R", "bt_bootstrap_schedule.R")) {
    sys.source(file.path(path, file), envir = env)
  }
  env
}

bt_schedule_fit_325 <- function(state, config) {
  # Independent stochastic posterior-contract fixture, not a Stan substitute
  # for numerical qualification. Refit RNG must be identical across batching.
  mu <- state$trueskill_state$items$mu
  draws <- outer(stats::rnorm(20L, sd = .01), mu - mean(mu), "+")
  colnames(draws) <- state$item_ids
  list(btl_posterior_draws = draws, theta_mean = colMeans(draws),
    theta_sd = apply(draws, 2L, stats::sd), model_variant = "btl",
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000))
}
environment(bt_schedule_fit_325) <- baseenv()

bt_schedule_args_325 <- function(strategy = "trueskill_p50", n = 12L, budget = 2L * n,
                                 refit_target = n, ...) {
  ids <- sprintf("item%02d", seq_len(n))
  initial <- pairwiseLLM::adaptive_rank_start(ids, seed = 325L,
    adaptive_config = list(pairing_strategy = strategy), ...)
  list(object = stats::setNames(seq(-1, 1, length.out = n), ids), mode = "adaptive",
    n_rep = 2L, seed = 325L, initial_state = initial, budget = budget,
    estimator = "alpha", estimator_args = list(alpha = .5),
    btl_config = list(refit_pairs_target = refit_target, model_variant = "btl"),
    schedule_fit_fn = bt_schedule_fit_325, keep = "full")
}

bt_scientific_325 <- function(out) {
  # The existing phase-A artifact builder uses Sys.time(), even in within-set
  # bootstrap with its fixed clock. Only this incidental timestamp is normalized.
  for (i in seq_along(out$artifacts)) {
    artifacts <- out$artifacts[[i]]$state$linking$phase_a$artifacts
    for (j in seq_along(artifacts)) artifacts[[j]]$created_at <- "<wall-clock>"
    if (length(artifacts)) out$artifacts[[i]]$state$linking$phase_a$artifacts <- artifacts
  }
  # Mutable memo environments have process identity, not scientific content.
  # Compare their values as well; never drop state fields or log diagnostics.
  normalize <- function(x) {
    if (is.environment(x)) return(normalize(as.list(x, all.names = TRUE)))
    if (is.function(x)) return(list(formals(x), paste(deparse(body(x)), collapse = "\n")))
    if (is.list(x)) for (i in seq_along(x)) x[i] <- list(normalize(x[[i]]))
    x
  }
  normalize(out)
}

bt_result_325 <- function(fun, args) {
  tryCatch(do.call(fun, args), pairwiseLLM_bt_bootstrap_error = function(e) {
    if (is.null(e$result)) stop(e)
    e$result
  })
}
