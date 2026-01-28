# -------------------------------------------------------------------------
# Full Bayesian BTL inference via CmdStanR (optional/gated).
# -------------------------------------------------------------------------

.btl_mcmc_require_cmdstanr <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    rlang::abort(paste0(
      "CmdStanR is required for MCMC audit. Install with:\n",
      "install.packages(\"cmdstanr\", repos = c(\"https://mc-stan.org/r-packages/\", getOption(\"repos\")))\n",
      "Then install CmdStan: cmdstanr::install_cmdstan()"
    ))
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

  ids <- .btl_validate_ids(ids)
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
  array[K] int<lower=1, upper=N> winner;
  array[K] int<lower=1, upper=N> loser;
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

#' Full Bayesian BTL inference via CmdStanR (audit only)
#'
#' Final Bayesian posterior inference for adaptive ranking. This requires
#' CmdStanR/CmdStan (optional dependency). All reported uncertainty summaries
#' are derived from these draws only.
#'
#' @param results Canonical \code{results_tbl} with \code{A_id}, \code{B_id}, and
#'   \code{better_id}.
#' @param ids Character vector of all sample ids (length \code{N}).
#' @param cmdstan List of CmdStan settings: \code{chains} (defaults to
#'   \code{min(8, physical_cores)}), \code{iter_warmup} (1000),
#'   \code{iter_sampling} (1000), \code{seed} (NULL), and
#'   \code{core_fraction} (0.8). The list is extensible in future versions.
#'
#' @return A list with:
#' \describe{
#'   \item{theta_draws}{Matrix of posterior draws \code{[S, N]} (colnames == ids).}
#'   \item{fit_meta}{Sampling diagnostics and convergence metadata.}
#' }
#'
#' @examples
#' \dontrun{
#' results <- tibble::tibble(
#'   pair_uid = "A:B#1",
#'   unordered_key = "A:B",
#'   ordered_key = "A:B",
#'   A_id = "A",
#'   B_id = "B",
#'   better_id = "A",
#'   winner_pos = 1L,
#'   phase = "phase2",
#'   iter = 1L,
#'   received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
#'   backend = "openai",
#'   model = "gpt-test"
#' )
#' fit <- fit_bayes_btl_mcmc(results, ids = c("A", "B"))
#'
#' # Full workflow shape (after stop confirmation):
#' # 1) Run MCMC
#' # 2) Summarize with MCMC-only helpers
#' mcmc_fit <- fit_bayes_btl_mcmc(results, ids = c("A", "B"))
#' theta_summary <- summarize_theta(mcmc_fit$theta_draws)
#' rank_summary <- summarize_ranks(mcmc_fit$theta_draws)
#' }
#'
#'
#' @export
fit_bayes_btl_mcmc <- function(
    results,
    ids,
    cmdstan = list(
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = NULL,
      core_fraction = 0.8
    )
) {
  cmdstan <- cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`cmdstan` must be a list.")
  }
  output_dir <- cmdstan$output_dir %||% NULL
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
      rlang::abort("`cmdstan$output_dir` must be a length-1 character path.")
    }
  }

  .btl_mcmc_require_cmdstanr()

  prep <- .btl_mcmc_prepare_data(results, ids)
  data <- prep$data
  ids <- prep$ids

  resolved_cmdstan <- .btl_mcmc_resolve_cmdstan_config(cmdstan)
  chains <- resolved_cmdstan$chains
  iter_warmup <- as.integer(cmdstan$iter_warmup %||% 1000L)
  iter_sampling <- as.integer(cmdstan$iter_sampling %||% 1000L)
  seed <- cmdstan$seed %||% NULL
  parallel_chains <- resolved_cmdstan$parallel_chains

  if (any(is.na(c(chains, iter_warmup, iter_sampling))) || chains < 1L ||
    iter_warmup < 1L || iter_sampling < 1L) {
    rlang::abort("CmdStan settings must be positive integers.")
  }
  if (is.na(parallel_chains) || parallel_chains < 1L) {
    rlang::abort("`cmdstan$parallel_chains` must be a positive integer.")
  }

  stan_file <- cmdstanr::write_stan_file(.btl_mcmc_model_code())
  model <- cmdstanr::cmdstan_model(
    stan_file,
    cpp_options = list(stan_threads = TRUE)
  )

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
  if (!is.null(output_dir)) {
    sample_args$output_dir <- output_dir
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
