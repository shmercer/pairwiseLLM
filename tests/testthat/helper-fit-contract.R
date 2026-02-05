make_btl_fit_contract <- function(ids,
    theta_draws = NULL,
    epsilon_draws = NULL,
    beta_draws = NULL,
    diagnostics = NULL,
    model_variant = "btl_e_b",
    mcmc_config_used = NULL,
    diagnostics_pass = NA) {
  ids <- as.character(ids)
  if (is.null(theta_draws)) {
    theta_draws <- matrix(0, nrow = 2L, ncol = length(ids))
  }
  if (is.null(colnames(theta_draws))) {
    colnames(theta_draws) <- ids
  }
  if (is.null(epsilon_draws)) {
    epsilon_draws <- rep(0.1, nrow(theta_draws))
  }
  diagnostics <- diagnostics %||% list(divergences = 0L, max_rhat = 1, min_ess_bulk = 500)
  mcmc_config_used <- mcmc_config_used %||% list(
    chains = 2L,
    parallel_chains = 2L,
    core_fraction = 0.8,
    cores_detected_physical = 2L,
    cores_detected_logical = 2L,
    threads_per_chain = 1L,
    cmdstanr_version = "test"
  )

  fit <- pairwiseLLM:::build_btl_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = epsilon_draws,
    beta_draws = beta_draws,
    diagnostics = diagnostics,
    model_variant = model_variant,
    mcmc_config_used = mcmc_config_used,
    diagnostics_pass = diagnostics_pass
  )
  fit
}
