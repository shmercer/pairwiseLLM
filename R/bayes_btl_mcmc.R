# -------------------------------------------------------------------------
# Full Bayesian BTL inference via CmdStanR (optional/gated).
# -------------------------------------------------------------------------

.btl_mcmc_require_cmdstanr <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    rlang::abort(
      "CmdStanR is required for MCMC audit. Install with:\n",
      "install.packages(\"cmdstanr\", repos = c(\"https://mc-stan.org/r-packages/\", getOption(\"repos\")))\n",
      "Then install CmdStan: cmdstanr::install_cmdstan()"
    )
  }
  cmdstan_path <- tryCatch(
    cmdstanr::cmdstan_path(),
    error = function(e) ""
  )
  if (!nzchar(cmdstan_path)) {
    rlang::abort(
      "CmdStan is not available. Run: cmdstanr::install_cmdstan()"
    )
  }
  invisible(TRUE)
}

.btl_mcmc_prepare_data <- function(results, ids) {
  results <- tibble::as_tibble(results)
  required <- c("A_id", "B_id", "better_id")
  .adaptive_required_cols(results, "results", required)
  validate_results_tbl(results)

  ids <- .btl_fast_validate_ids(ids)
  result_ids <- unique(c(results$A_id, results$B_id, results$better_id))
  missing_ids <- setdiff(result_ids, ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("All ids in `results` must be contained in `ids`.")
  }

  winner <- as.character(results$better_id)
  loser <- ifelse(results$better_id == results$A_id, results$B_id, results$A_id)
  winner_idx <- match(winner, ids)
  loser_idx <- match(loser, ids)

  list(
    data = list(
      N = as.integer(length(ids)),
      K = as.integer(nrow(results)),
      winner = as.integer(winner_idx),
      loser = as.integer(loser_idx)
    ),
    ids = ids
  )
}

.btl_mcmc_model_code <- function() {
  "
data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=1,upper=N> winner[K];
  int<lower=1,upper=N> loser[K];
}
parameters {
  vector[N] theta_raw;
}
transformed parameters {
  vector[N] theta;
  theta = theta_raw - mean(theta_raw);
}
model {
  theta_raw ~ normal(0, 1);
  for (k in 1:K) {
    target += bernoulli_logit_lpmf(1 | theta[winner[k]] - theta[loser[k]]);
  }
}
"
}

#' @keywords internal
#' @noRd
fit_bayes_btl_mcmc <- function(
    results,
    ids,
    cmdstan = list(
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = NULL,
      core_fraction = 0.6
    )
) {
  .btl_mcmc_require_cmdstanr()

  cmdstan <- cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`cmdstan` must be a list.")
  }

  prep <- .btl_mcmc_prepare_data(results, ids)
  data <- prep$data
  ids <- prep$ids

  chains <- as.integer(cmdstan$chains %||% 4L)
  iter_warmup <- as.integer(cmdstan$iter_warmup %||% 1000L)
  iter_sampling <- as.integer(cmdstan$iter_sampling %||% 1000L)
  seed <- cmdstan$seed %||% NULL
  core_fraction <- cmdstan$core_fraction %||% 0.6
  parallel_chains <- compute_core_budget(core_fraction = core_fraction, min_cores = 1L)

  if (any(is.na(c(chains, iter_warmup, iter_sampling))) || chains < 1L ||
    iter_warmup < 1L || iter_sampling < 1L) {
    rlang::abort("CmdStan settings must be positive integers.")
  }

  stan_file <- cmdstanr::write_stan_file(.btl_mcmc_model_code())
  model <- cmdstanr::cmdstan_model(stan_file)

  sample_args <- list(
    data = data,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
    refresh = 0
  )
  if (!is.null(seed)) {
    sample_args$seed <- seed
  }

  fit <- do.call(model$sample, sample_args)

  draws_matrix <- fit$draws(variables = "theta", format = "matrix")
  theta_cols <- paste0("theta[", seq_along(ids), "]")
  if (!all(theta_cols %in% colnames(draws_matrix))) {
    rlang::abort("CmdStan output missing theta draws.")
  }
  theta_draws <- draws_matrix[, theta_cols, drop = FALSE]
  colnames(theta_draws) <- ids

  list(
    theta_draws = theta_draws,
    fit_meta = list(
      converged = TRUE,
      sampler_diagnostics = fit$diagnostic_summary()
    )
  )
}
